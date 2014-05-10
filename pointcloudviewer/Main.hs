{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase, MultiWayIf, ScopedTypeVariables, TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, FlexibleContexts, TypeOperators, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (assert)
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 (parseOnly, sepBy1', double, endOfLine, skipSpace)
import           Data.Bits (unsafeShiftR)
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Int (Int64)
import           Data.List (find, intercalate, sortBy, maximumBy)
import           Data.Ord (comparing)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Typeable
import qualified Data.Packed.Matrix as Matrix
import           Data.Packed.Matrix ((><))
import qualified Data.Packed.Vector as HmatrixVec
import qualified Data.Vect.Double as Vect.Double
import           Data.Vect.Float hiding (Vector)
import           Data.Vect.Float.Instances ()
import           Data.Vect.Float.Util.Quaternion
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Data.Word
import           Foreign.C.Types (CInt, CFloat(..))
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (peek)
import           Foreign.Store (Store(..), newStore, lookupStore, readStore, deleteStore)
import           GHC.Generics
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (Plane, Normal3)
import           Linear (V3(..))
import           Numeric.LinearAlgebra.Algorithms (linearSolve)
import qualified PCD.Data as PCD
import qualified PCD.Point as PCD
import           System.Endian (fromBE32)
import           System.FilePath ((</>))
import           System.Random (randomRIO)
import           System.SelfRestart (forkSelfRestartExePollWithAction)
import           System.IO (hPutStrLn, stderr)

import           FitCuboidBFGS hiding (main)
import           HoniHelper (takeDepthSnapshot)

-- Things needed to `show` the Generic representation of our `State`,
-- which we use to check if the State type changed when doing hot code
-- reloading in in-place restarts in ghci.
deriving instance Show (V1 p)
deriving instance Show (U1 p)
deriving instance (Show c) => Show (K1 i c p)
deriving instance Show (f p) => Show (M1 i c f p)
deriving instance (Show (f p), Show (g p)) => Show ((f :*:g) p)
deriving instance (Show (f p), Show (g p)) => Show ((f :+:g) p)
deriving instance Show D
deriving instance Show C
deriving instance Show S

instance (Typeable a) => Show (IORef a) where
  show x = "IORef " ++ show (typeOf x)


-- Orphan instance so that we can derive Ord
instance Ord Vec3 where
-- Really questionable why this isn't there already
instance Eq Normal3 where
  n1 == n2 = fromNormal n1 == fromNormal n2
instance Ord Normal3 where
  n1 `compare` n2 = fromNormal n1 `compare` fromNormal n2
deriving instance Typeable Vec3


data CloudColor
  = OneColor !(Color3 GLfloat)
  | ManyColors (Vector Vec3) -- must be same size as `cloudPoints`
  deriving (Eq, Ord, Show, Generic)

data Cloud = Cloud
  { cloudID :: !ID
  , cloudColor :: !CloudColor -- TODO maybe clean this interface up
  , cloudPoints :: Vector Vec3
  } deriving (Eq, Ord, Show, Generic, Typeable)

data DragMode = Rotate | Translate
  deriving (Eq, Ord, Show, Typeable)


class ShortShow a where
  shortShow :: a -> String

  shortPrint :: a -> IO ()
  shortPrint = putStrLn . shortShow

instance ShortShow CloudColor where
  shortShow = \case
    c@OneColor{} -> show c
    ManyColors cols -> "ManyColors (" ++ show (V.length cols) ++ " points)"

instance ShortShow Cloud where
  shortShow (Cloud i col points) = "Cloud" ++ concat
    [ " ", show i, " (", shortShow col, ")"
    , " (", show (V.length points), " points)"
    ]

instance ShortShow Plane where
  shortShow (Plane i eq col bounds) = "PlaneXXX" ++ concat
    [ " ", show i, " (", show eq, ")"
    , " (", show col, ") ", show bounds
    ]

instance ShortShow Room where
  shortShow (Room i planes cloud corners) = "Room" ++ concat
    [ " ", show i, " ", shortShow planes, " (", shortShow cloud, ")"
    , " ", show corners
    ]

instance ShortShow Word32 where
  shortShow = show

instance (ShortShow a, ShortShow b) => ShortShow (a, b) where
  shortShow (a,b) = "(" ++ shortShow a ++ "," ++ shortShow b ++ ")"

instance (ShortShow a) => ShortShow [a] where
  shortShow l = "[" ++ intercalate ", " (map shortShow l) ++ "]"

instance (ShortShow a, ShortShow b) => ShortShow (Map a b) where
  shortShow = shortShow . Map.toList


-- TODO make all State/TransientState fields strict so that we get an error if not initialized

-- |Application state
data State
  = State { sMouse :: IORef ( GLint, GLint )
          , sDragMode :: IORef (Maybe DragMode)
          , sSize :: IORef ( GLint, GLint )
          , sLookAtPoint :: IORef Vec3 -- ^ focus point around which we rotate
          , sRotUp :: IORef Float -- ^ view angle away from the ground plane
          , sRotY :: IORef Float -- ^ angle around the up axis (Y in OpenGL), orthogonal to ground plane
          , sZoom :: IORef Float
          , queuedClouds :: IORef (Map ID Cloud)
          , sFps :: IORef Int
          -- | Both `display` and `idle` set this to the current time
          -- after running
          , sLastLoopTime :: IORef (Maybe Int64)
          -- Things needed for hot code reloading
          , sRestartRequested :: IORef Bool
          , sGlInitialized :: IORef Bool
          , sRestartFunction :: IORef (IO ())
          -- Object picking
          , sPickingDisabled :: IORef Bool
          , sPickObjectAt :: IORef (Maybe ((Int,Int), Maybe ID -> IO ()))
          , sUnderCursor :: IORef (Maybe ID)
          , sDebugPickingDrawVisible :: IORef Bool
          , sDebugPickingTiming :: IORef Bool
          -- Displaying options
          , sDisplayPlanes :: IORef Bool
          , sDisplayClouds :: IORef Bool
          -- Visual debugging
          , sDebugProjectPlanePointsToEq :: IORef Bool
          -- Transient state
          , transient :: TransientState
          } deriving (Generic)

data TransientState
  = TransientState { sNextID :: IORef ID
                   , sPickingMode :: IORef Bool
                   , sAllocatedClouds :: IORef (Map ID (Cloud, BufferObject, Maybe BufferObject)) -- second is for colours
                   , sPlanes :: IORef (Map ID Plane)
                   , sSelectedPlanes :: IORef [Plane]
                   , sRooms :: IORef (Map ID Room)
                   , sSelectedRoom :: IORef (Maybe Room)
                   , sConnectedWalls :: IORef [(ID, ID)]
                   }

instance Show TransientState where
  show _ = "TransientState"


data Plane = Plane
  { planeID     :: !ID
  , planeEq     :: !PlaneEq
  , planeColor  :: !(Color3 GLfloat)
  , planeBounds :: Vector Vec3
  } deriving (Eq, Ord, Show, Generic)


-- Convenience
planeNormal :: Plane -> Vec3
planeNormal Plane{ planeEq = PlaneEq n _ } = fromNormal n


data Room = Room
  { roomID      :: !ID
  , roomPlanes  :: ![Plane]
  , roomCloud   :: Cloud
  , roomCorners :: [Vec3] -- TODO newtype this
  } deriving (Eq, Ord, Show, Generic)


deriving instance Generic Vec3
instance Binary Vec3
instance Binary Normal3 where
  get = mkNormal <$> Binary.get
  put = Binary.put . fromNormal
deriving instance Generic CFloat
instance Binary CFloat
deriving instance Generic a => Generic (Color3 a)
instance (Binary a, Generic a) => Binary (Color3 a)
instance Binary CloudColor
instance Binary Cloud
instance Binary PlaneEq
instance Binary Plane
instance Binary Room


type ID = Word32

-- We pick maxBound as the ID for "there is no object there".
noID :: ID
noID = maxBound


genID :: State -> IO ID
genID State{ transient = TransientState{ sNextID } } =
  atomicModifyIORef' sNextID (\i -> (i+1 `mod` noID, i))


-- |Sets the vertex color
color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 x y z
  = color $ Color4 x y z 1.0


-- |Sets the vertex position
vertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3 x y z
  = vertex $ Vertex3 x y z


getTimeUs :: IO Int64
getTimeUs = round . (* 1000000.0) <$> getPOSIXTime


withVar :: StateVar a -> a -> IO b -> IO b
withVar var val f = do
  before <- get var
  var $= val
  x <- f
  var $= before
  return x


withDisabled :: [StateVar Capability] -> IO b -> IO b
withDisabled vars f = do
  befores <- mapM get vars
  mapM_ ($= Disabled) vars
  x <- f
  zipWithM_ ($=) vars befores
  return x


upAxis :: Vec3
upAxis = Vec3 0 1 0


-- |Called when stuff needs to be drawn
display :: State -> DisplayCallback
display state@State{..} = do

  ( width, height ) <- get sSize
  rotY              <- get sRotY
  rotUp             <- get sRotUp
  zoom              <- get sZoom
  lookAtPoint       <- get sLookAtPoint

  let buffers = [ ColorBuffer, DepthBuffer ]

  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (fromIntegral width / fromIntegral height) 0.1 500.0

  matrixMode $= Modelview 0
  loadIdentity

  -- Moving around and rotating around the lookAtPoint
  let eye = lookAtPoint &+ zoom *& (vec3Z .* rotMatrixX (toRad (-rotUp))
                                          .* rotMatrixY (toRad (-rotY )))
  lookAt (toGlVertex eye) (toGlVertex lookAtPoint) (toGlVector upAxis)

  -- Do pick rendering (using color picking)
  pickingDisabled <- get sPickingDisabled
  get sPickObjectAt >>= \case
    Just ((x,y), callback) | not pickingDisabled -> do
      i <- colorPicking state (x,y)
      sPickObjectAt $= Nothing
      callback i
    _ -> return ()

  -- Do the normal rendering of all objects
  clear buffers
  preservingMatrix $ drawObjects state
  swapBuffers

  getTimeUs >>= \now -> sLastLoopTime $= Just now


idToColor :: ID -> Color4 GLfloat
idToColor i = Color4 (fromIntegral r / 255.0)
                     (fromIntegral g / 255.0)
                     (fromIntegral b / 255.0)
                     (fromIntegral a / 255.0)
  where
    -- From http://stackoverflow.com/questions/664014
    -- hash(i)=i*2654435761 mod 2^32
    col32 = i `rem` noID -- (2654435761 * i) `rem` noID :: Word32 -- noID == maxBound itself is for "no ID" -- TODO find inverse
    r = fromIntegral $ col32 `unsafeShiftR` 24 :: Word8
    g = fromIntegral $ col32 `unsafeShiftR` 16 :: Word8
    b = fromIntegral $ col32 `unsafeShiftR`  8 :: Word8
    a = fromIntegral $ col32                   :: Word8


-- | Render all objects with a distinct color to find out which object
-- is at a given (x,y) coordinate.
-- (x,y) must not be off-screen since `readPixels` is used.
-- Returns `Nothing` if the background is picked.
colorPicking :: State -> (Int, Int) -> IO (Maybe ID)
colorPicking state@State{ transient = TransientState{..}, ..} (x, y) = do
  timeBefore <- getPOSIXTime

  -- Draw background white
  col <- get clearColor
  clearColor $= Color4 1 1 1 1 -- this gives us 0xffffffff == maxBound == noID
  clear [ ColorBuffer, DepthBuffer ] -- we need color and depth for picking
  clearColor $= col

  -- Note: We could probably use glScissor here to restrict drawing to the
  --       one pixel requested.

  i <- withDisabled [ texture Texture2D -- not sure if we should also disable other texture targets
                    , fog
                    , lighting
                    , blend
                    ] $ do

    sPickingMode $= True
    preservingMatrix $ drawObjects state
    sPickingMode $= False
    flush -- so that readPixels reads what we just drew

    ( _, height ) <- get sSize

    -- Get the ID
    i <- alloca $ \(rgbaPtr :: Ptr Word32) -> do
      -- We disable blending for the pick rendering so we can use the
      -- full 32 bits of RGBA for color picking.
      -- readPixels is undefined for off-screen coordinates, so we
      -- require (x,y) to be on-screen.
      readPixels (Position (i2c x) (height-(i2c y)-1)) (Size 1 1) (PixelData RGBA UnsignedByte rgbaPtr)
      -- The color is stored in memory as R-G-B-A bytes, so we have to convert it to big-endian.
      fromBE32 <$> peek rgbaPtr

    -- For debugging we can actually draw the unique colors.
    -- This must happen after readPixels becaus swapBuffers makes the buffer undefined.
    on sDebugPickingDrawVisible swapBuffers

    return i

  on sDebugPickingTiming $ do
    timeAfter <- getPOSIXTime
    putStrLn $ "Picking took " ++ show (timeAfter - timeBefore) ++ " s"

  return $ if i == noID then Nothing else Just i


on :: HasGetter a => a Bool -> IO () -> IO ()
on var f = get var >>= \enabled -> when enabled f


i2c :: Int -> CInt
i2c = fromIntegral

c2i :: CInt -> Int
c2i = fromIntegral


toFloat :: Double -> Float
toFloat = realToFrac

toDouble :: Float -> Double
toDouble = realToFrac


toGlVector :: Fractional a => Vec3 -> Vector3 a
toGlVector (Vec3 a b c) = Vector3 (realToFrac a) (realToFrac b) (realToFrac c)

toGlVertex :: Fractional a => Vec3 -> Vertex3 a
toGlVertex (Vec3 a b c) = Vertex3 (realToFrac a) (realToFrac b) (realToFrac c)


toRad :: Float -> Float
toRad d = d / 180 * pi


-- |Draws the objects to show
drawObjects :: State -> IO ()
drawObjects state@State{ sDisplayPlanes, sDisplayClouds, transient = TransientState{ sPickingMode } } = do
  picking <- get sPickingMode

  -- Objects must only be drawn in picking mode when they are colour picking
  -- aware, that is they query the picking mode and draw themselves only in
  -- colors generated by `idToColor <$> genID` if we are picking.

  when (not picking) $ drawReferenceSystem

  when (not picking) $ on sDisplayClouds $ drawPointClouds state

  when (not picking) $ drawRoomCorners state

  when (not picking) $ drawWallConnections state

  on sDisplayPlanes $ drawPlanes state


drawReferenceSystem :: IO ()
drawReferenceSystem = do

  -- displayQuad 1 1 1

  renderPrimitive Lines $ do
    color3 1.0 0.0 0.0
    vertex3 0.0 0.0 0.0
    vertex3 20.0 0.0 0.0

    color3 0.0 1.0 0.0
    vertex3 0.0 0.0 0.0
    vertex3 0.0 20.0 0.0

    color3 0.0 0.0 1.0
    vertex3 0.0 0.0 0.0
    vertex3 0.0 0.0 20.0



drawPointClouds :: State -> IO ()
drawPointClouds State{ transient = TransientState{ sAllocatedClouds } } = do

  allocatedClouds <- get sAllocatedClouds

  -- Render all clouds
  forM_ (Map.elems allocatedClouds) $ \(Cloud{ cloudColor = colType, cloudPoints }, bufObj, m'colorObj) -> do

    clientState VertexArray $= Enabled

    case (colType, m'colorObj) of
      (OneColor col, Nothing) -> color col
      (ManyColors _, Just colorObj) -> do
        clientState ColorArray $= Enabled
        bindBuffer ArrayBuffer $= Just colorObj
        arrayPointer ColorArray $= VertexArrayDescriptor 3 Float 0 nullPtr
      _ -> error $ "bad combination of CloudColor and buffer: " ++ show m'colorObj

    bindBuffer ArrayBuffer $= Just bufObj
    arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr

    drawArrays Points 0 (i2c $ V.length cloudPoints)
    bindBuffer ArrayBuffer $= Nothing

    clientState VertexArray $= Disabled
    -- If we dont' disable this, a draw with only 1 color using `color` will segfault
    clientState ColorArray $= Disabled


drawRoomCorners :: State -> IO ()
drawRoomCorners State{ transient = TransientState{ sRooms } } = do
  rooms <- Map.elems <$> get sRooms

  withVar pointSize 8.0 $ do
    renderPrimitive Points $ do
      forM_ rooms $ \Room{ roomCorners } -> do
        if (length roomCorners /= 8)
          then do
            color red
            forM_ roomCorners vertexVec3
          else do
            let [a,b,c,d,e,f,g,h] = roomCorners
            color3 0.3 0.6 0.3 >> vertexVec3 a
            color3 0 0 1 >> vertexVec3 b
            color3 0 1 0 >> vertexVec3 c
            color3 0 1 1 >> vertexVec3 d
            color3 1 0 0 >> vertexVec3 e
            color3 1 0 1 >> vertexVec3 f
            color3 1 1 0 >> vertexVec3 g
            color3 1 1 1 >> vertexVec3 h


drawWallConnections :: State -> IO ()
drawWallConnections  State{ transient = TransientState{ sConnectedWalls, sRooms } } = do
  conns <- get sConnectedWalls
  planes <- concatMap roomPlanes . Map.elems <$> get sRooms

  renderPrimitive Lines $ do
    forM_ conns $ \(pid1, pid2) -> do
      case [ p | p <- planes, planeID p `elem` [pid1,pid2] ] of
        [p1, p2] -> do
          color3 1.0 0.0 0.0
          vertexVec3 (planeMean p1)
          vertexVec3 (planeMean p2)
        _ -> putStrLn $ "Room planes not found: " ++ show (pid1, pid2)


drawPlanes :: State -> IO ()
drawPlanes State{ transient = TransientState{ sPlanes, sRooms, sPickingMode }, ..} = do

  planePols <- Map.elems <$> get sPlanes
  roomPlanes <- concatMap roomPlanes . Map.elems <$> get sRooms

  debugProject <- get sDebugProjectPlanePointsToEq
  let roomPols
        -- This reveals bugs in the plane projection code: It uses the
        -- actual plane equation for drawing the points.
        | debugProject = [ p{ planeBounds = V.map (projectToPlane eq) points
                            } | p@(Plane _ eq _ points) <- roomPlanes ]
        | otherwise = roomPlanes

  let pols = planePols ++ roomPols

  picking <- get sPickingMode
  underCursor <- get sUnderCursor

  let drawPolys = do
        forM_ pols $ \(Plane i _ (Color3 r g b) points) -> do

          renderPrimitive Polygon $ do
            color $ if
              | picking               -> idToColor i
              | underCursor == Just i -> Color4 r g b 0.8
              | otherwise             -> Color4 r g b 0.5
            V.mapM_ vertexVec3 points

  -- Get "real" transparency for overlapping polygons by drawing them last,
  -- and disabling the depth test for their drawing
  -- (transparency must be 0.5 for all polygons for this technique).
  -- From http://stackoverflow.com/questions/4127242
  -- If we are picking, of course we don't want any color blending, so we
  -- keep the depth test on.
  if picking then                          drawPolys
             else withDisabled [depthMask] drawPolys


processCloudQueue :: State -> IO ()
processCloudQueue State{ transient = TransientState{ sAllocatedClouds }, queuedClouds } = do

  -- Get out queued clouds, set queued clouds to []
  queued <- atomicModifyIORef' queuedClouds (\cls -> (Map.empty, Map.elems cls))

  -- Go over the queue contents
  forM_ queued $ \cloud@Cloud{ cloudID = i, cloudPoints, cloudColor } -> do

    -- If the ID is already allocated, deallocate the corresponding buffers
    allocatedClouds <- get sAllocatedClouds
    for_ (Map.lookup i allocatedClouds) $ \(_, bufObj, m'colorObj) -> do
      deleteObjectName bufObj
      for_ m'colorObj deleteObjectName
      sAllocatedClouds $~ Map.delete i

    -- Allocate buffer object containing all these points
    bufObj <- fromVector ArrayBuffer cloudPoints

    -- Allocate color buffer if we don't use only 1 color
    m'colorObj <- case cloudColor of
      OneColor _             -> return Nothing
      ManyColors pointColors -> Just <$> fromVector ArrayBuffer pointColors

    sAllocatedClouds $~ Map.insert i (cloud, bufObj, m'colorObj)


atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\x -> (f x, ()))


addPointCloud :: State -> Cloud -> IO ()
addPointCloud State{ transient = TransientState{..}, ..} cloud@Cloud{ cloudID = i } = do
  -- Make sure a cloud with that id doesn't already exist
  queued <- get queuedClouds
  allocated <- get sAllocatedClouds
  when (i `Map.member` queued || i `Map.member` allocated) $
    error $ "Cloud with id " ++ show i ++ " already exists"

  atomicModifyIORef_ queuedClouds (Map.insert i cloud)


updatePointCloud :: State -> Cloud -> IO ()
updatePointCloud State{ queuedClouds } cloud@Cloud{ cloudID = i } = do
  atomicModifyIORef_ queuedClouds (Map.insert i cloud)


initializeObjects :: State -> IO ()
initializeObjects _state = do
  return ()


-- |Displays a quad
displayQuad :: GLfloat -> GLfloat -> GLfloat -> IO ()
displayQuad w h d = preservingMatrix $ do
  scale w h d

  renderPrimitive Quads $ do
    color3 1.0 0.0 0.0
    vertex3 (-1.0) ( 1.0) ( 1.0)
    vertex3 (-1.0) (-1.0) ( 1.0)
    vertex3 ( 1.0) (-1.0) ( 1.0)
    vertex3 ( 1.0) ( 1.0) ( 1.0)

    color3 1.0 0.0 0.0
    vertex3 (-1.0) (-1.0) (-1.0)
    vertex3 (-1.0) ( 1.0) (-1.0)
    vertex3 ( 1.0) ( 1.0) (-1.0)
    vertex3 ( 1.0) (-1.0) (-1.0)

    color3 0.0 1.0 0.0
    vertex3 ( 1.0) (-1.0) ( 1.0)
    vertex3 ( 1.0) (-1.0) (-1.0)
    vertex3 ( 1.0) ( 1.0) (-1.0)
    vertex3 ( 1.0) ( 1.0) ( 1.0)

    color3 0.0 1.0 0.0
    vertex3 (-1.0) (-1.0) (-1.0)
    vertex3 (-1.0) (-1.0) ( 1.0)
    vertex3 (-1.0) ( 1.0) ( 1.0)
    vertex3 (-1.0) ( 1.0) (-1.0)

    color3 0.0 0.0 1.0
    vertex3 (-1.0) (-1.0) ( 1.0)
    vertex3 (-1.0) (-1.0) (-1.0)
    vertex3 ( 1.0) (-1.0) (-1.0)
    vertex3 ( 1.0) (-1.0) ( 1.0)

    color3 0.0 0.0 1.0
    vertex3 (-1.0) ( 1.0) (-1.0)
    vertex3 (-1.0) ( 1.0) ( 1.0)
    vertex3 ( 1.0) ( 1.0) ( 1.0)
    vertex3 ( 1.0) ( 1.0) (-1.0)

-- |Called when the sSize of the viewport changes
reshape :: State -> ReshapeCallback
reshape State{..} (Size width height) = do
  sSize $= ( width, height )
  viewport $= (Position 0 0, Size width height)
  postRedisplay Nothing


-- |Animation
idle :: State -> IdleCallback
idle state@State{..} = do

  -- Allocate BufferObjects for all queued clouds
  processCloudQueue state

  get sLastLoopTime >>= \case
    Nothing -> return ()
    Just lastLoopTime -> do
      now <- getTimeUs
      fps <- get sFps
      let sleepTime = max 0 $ 1000000 `quot` fps - fromIntegral (now - lastLoopTime)
      threadDelay sleepTime

  postRedisplay Nothing
  getTimeUs >>= \now -> sLastLoopTime $= Just now

  -- If a restart is requested, stop the main loop.
  -- The code after the main loop will do the actual restart.
  shallRestart <- get sRestartRequested
  when shallRestart leaveMainLoop


-- | Called when the OpenGL window is closed.
close :: State -> CloseCallback
close State{..} = do
  putStrLn "window closed"


-- | Mouse motion (with buttons pressed)
motion :: State -> Position -> IO ()
motion State{..} (Position posx posy) = do

  ( oldx, oldy ) <- get sMouse
  let diffH = fromIntegral $ posx - oldx
      diffV = fromIntegral $ posy - oldy

  sMouse $= ( posx, posy )

  get sDragMode >>= \case
    Just Rotate -> do
      let clamp (l,u) x = min u (max l x)
      sRotY  $~! (+ diffH)
      sRotUp $~! (clamp (-80, 80) . (+ diffV))
    Just Translate -> do
      zoom  <- get sZoom
      rotY  <- get sRotY
      rotUp <- get sRotUp
      -- Where left/right/up/down is depends on the rotation around the
      -- up axis (rotY), and how much to move depends on the zoom.
      -- rotUp allows us to go below the ground plane; since we always want to
      -- "drag the ground plane around", we have to invert the Z component then.
      let movVec = Vec3 (-diffH) 0 (-diffV * signum rotUp)
      sLookAtPoint $~! (&+ (0.0025 * zoom) *& (movVec .* rotMatrixY (toRad $ -rotY)))
    _ -> return ()


-- | Mouse motion (without buttons pressed)
passiveMotion :: State -> Position -> IO ()
passiveMotion state@State{..} (Position posx posy) = do

  sPickObjectAt $= Just ((c2i posx, c2i posy), objectHover state)



changeFps :: State -> (Int -> Int) -> IO ()
changeFps State{ sFps } f = do
  sFps $~ f
  putStrLn . ("FPS: " ++) . show =<< get sFps


-- |Button input
input :: State -> Key -> KeyState -> Modifiers -> Position -> IO ()
input state@State{..} (MouseButton LeftButton) Down _ (Position x y) = do
  sPickObjectAt $= Just ((c2i x, c2i y), objectClick state)
  sMouse $= ( x, y )
  sDragMode $= Just Translate
input State{..} (MouseButton LeftButton) Up _ (Position x y) = do
  sMouse $= ( x, y )
  sDragMode $= Nothing
input State{..} (MouseButton RightButton) Down _ (Position x y) = do
  sMouse $= ( x, y )
  sDragMode $= Just Rotate
input State{..} (MouseButton RightButton) Up _ (Position x y) = do
  sMouse $= ( x, y )
  sDragMode $= Nothing
input state (MouseButton WheelDown) Down _ pos
  = wheel state 0 120 pos
input state (MouseButton WheelUp) Down _ pos
  = wheel state 0 (-120) pos
input state (Char '[') Down _ _ = changeFps state pred
input state (Char ']') Down _ _ = changeFps state succ
input state (Char '\r') Down _ _ = addDevicePointCloud state
input state (Char 'm') Down _ _ = addCornerPoint state
input state (Char 'f') Down _ _ = fitCuboidToSelectedRoom state
input state (Char 'r') Down _ _ = rotateSelectedPlanes state
input state (Char 's') Down _ _ = save state
input state (Char 'l') Down _ _ = load state
input state (Char '/') Down _ _ = devSetup state
input state (Char 'd') Down _ _ = sDisplayPlanes state $~ not
input state (Char 'p') Down _ _ = sDisplayClouds state $~ not
input state (Char 'c') Down _ _ = clearRooms state
input state (Char 'w') Down _ _ = connectWalls state
input _state key Down _ _ = putStrLn $ "Unhandled key " ++ show key
input _state _ _ _ _ = return ()


-- | Called when picking notices a hover over an object
objectHover :: State -> Maybe ID -> IO ()
objectHover State{..} m'i = do
  sUnderCursor $= m'i


-- | Called when picking notices a click on an object
objectClick :: State -> Maybe ID -> IO ()
objectClick _      Nothing  = putStrLn $ "Clicked: Background"
objectClick State{ transient = TransientState{..}, ..} (Just i) = do
  putStrLn $ "Clicked: " ++ show i

  rooms <- Map.elems <$> get sRooms
  allPlanes <- do
    planes <- Map.elems <$> get sPlanes
    return (planes ++ concatMap roomPlanes rooms)

  selected <- get sSelectedPlanes

  case findRoomContainingPlane rooms i of
    Nothing -> sSelectedRoom $= Nothing
    Just r -> do
      putStrLn $ "Room: " ++ show (roomID r)
      sSelectedRoom $= Just r

  for_ (find (\Plane{ planeID } -> planeID == i) allPlanes) $ \p -> do
    putStrLn $ "Plane: " ++ show (planeID p)
    putStrLn $ "PlaneEq: " ++ show (planeEq p)
    when (p `notElem` selected) $ do -- could compare by ID only
      sSelectedPlanes $~ (p:)


-- |Mouse wheel movement (sZoom)
wheel :: State -> WheelNumber -> WheelDirection -> Position -> IO ()
wheel State{..} _num dir _pos
  | dir > 0   = get sZoom >>= (\x -> sZoom $= clamp (x * 1.2))
  | otherwise = get sZoom >>= (\x -> sZoom $= clamp (x / 1.2))
  where
    clamp x = 0.5 `max` (300.0 `min` x)


-- | Creates the default state
createState :: IO State
createState = do
  sMouse            <- newIORef ( 0, 0 )
  sDragMode         <- newIORef Nothing
  sSize             <- newIORef ( 0, 1 )
  sRotUp            <- newIORef 0.0
  sRotY             <- newIORef 0.0
  sZoom             <- newIORef 20.0
  sLookAtPoint      <- newIORef zero
  queuedClouds      <- newIORef Map.empty
  sFps              <- newIORef 30
  sLastLoopTime     <- newIORef Nothing
  sRestartRequested <- newIORef False
  sGlInitialized    <- newIORef False
  sRestartFunction  <- newIORef (error "restartFunction called before set")
  sPickingDisabled  <- newIORef False
  sPickObjectAt     <- newIORef Nothing
  sUnderCursor      <- newIORef Nothing
  sDebugPickingDrawVisible <- newIORef False
  sDebugPickingTiming      <- newIORef False
  sDisplayPlanes    <- newIORef True
  sDisplayClouds    <- newIORef True
  sDebugProjectPlanePointsToEq <- newIORef True -- It is a good idea to keep this on, always
  transient         <- createTransientState

  return State{..} -- RecordWildCards for initialisation convenience


createTransientState :: IO TransientState
createTransientState = do
  sNextID <- newIORef 1
  sPickingMode <- newIORef False
  sAllocatedClouds <- newIORef Map.empty
  sPlanes <- newIORef Map.empty
  sSelectedPlanes <- newIORef []
  sRooms <- newIORef Map.empty
  sSelectedRoom <- newIORef Nothing
  sConnectedWalls <- newIORef []
  return TransientState{..}


-- |Main
main :: IO ()
main = do
  state <- createState
  mainState state


-- | Run `main` on a state.
mainState :: State -> IO ()
mainState state@State{..} = do

  _ <- forkSelfRestartExePollWithAction 1.0 $ do
    putStrLn "executable changed, restarting"
    threadDelay 1500000

  -- Initialize OpenGL
  _ <- getArgsAndInitialize

  -- Enable double buffering
  initialDisplayMode $= [RGBAMode, WithDepthBuffer, DoubleBuffered]

  -- Create window
  _ <- createWindow "3D cloud viewer"
  sGlInitialized $= True

  clearColor  $= Color4 0 0 0 1
  shadeModel  $= Smooth
  depthMask   $= Enabled
  depthFunc   $= Just Lequal
  blend       $= Enabled
  blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
  lineWidth   $= 3.0
  pointSize   $= 2.0

  -- Callbacks
  displayCallback       $= display state
  reshapeCallback       $= Just (reshape state)
  idleCallback          $= Just (idle state)
  mouseWheelCallback    $= Just (wheel state)
  motionCallback        $= Just (motion state)
  passiveMotionCallback $= Just (passiveMotion state)
  keyboardMouseCallback $= Just (input state)
  closeCallback         $= Just (close state)

  initializeObjects state

  -- Let's get started
  actionOnWindowClose $= ContinueExecution
  mainLoop -- blocks while windows are open
  exit
  sGlInitialized $= False
  putStrLn "Exited OpenGL loop"

  -- Restart if requested
  on sRestartRequested $ do
    putStrLn "restarting"
    sRestartRequested $= False -- Note: This is for the new state;
                               -- works because this is not transient.
    -- We can't just call `mainState state` here since that would (tail) call
    -- the original function instead of the freshly loaded one. That's why the
    -- function is put into the IORef to be updated by `restart`.
    f <- get sRestartFunction
    f


-- | For debugging / ghci only.
getState :: IO State
getState = lookupStore _STORE_STATE >>= \case
  Just store -> readStore store
  Nothing    -> error "state not available; call restart first"


-- | For debugging / ghci only.
run :: (State -> IO a) -> IO a
run f = getState >>= f


-- Store IDs for Foreign.Store
_STORE_STATE, _STORE_STATE_TYPE_STRING :: Word32
_STORE_STATE = 0
_STORE_STATE_TYPE_STRING = 1


-- For restarting the program in GHCI while keeping the `State` intact.
restart :: (State -> IO ()) -> IO ()
restart mainStateFun = do
  -- Note: We have to pass in the `mainState` function from the global
  --       ghci scope as `mainStateFun` instead of just calling the
  --       `mainState` already visible from here - that would call the
  --       old `mainState`, not the freshly loaded one.
  lookupStore _STORE_STATE >>= \case
    Nothing -> do
      putStrLn "restart: starting for first time"
      state <- createState

      -- Store the state
      newStore state >>= \(Store i) -> when (i /= _STORE_STATE) $
        error "state store has bad store id"

      -- Store the type representation string of the state.
      -- This way we can detect whether the state changed when doing hot code
      -- reloading in in-place restarts in ghci.
      -- Using Generics, this really works on the *structure* of the `State`
      -- type, so hot code reloading even works when the name of a field in
      -- the `State` record changes!
      newStore (show $ from state) >>= \(Store i) -> when (i /= _STORE_STATE_TYPE_STRING) $
        error "state type representation string store has bad store id"

      void $ forkIO (mainStateFun state)

    Just store -> do
      putStrLn "restart: having existing store"

      -- Check state type. If it changed, abort reloading
      -- (otherwise we get a segfault since the memory layout changed).
      lookupStore _STORE_STATE_TYPE_STRING >>= \case
        Nothing -> error "restart: State type representation string missing"
        Just stateTypeStore -> do
          stateTypeString <- readStore stateTypeStore
          tmpState <- createState -- something we can compare with
          -- TODO This might fail to reload even if the types are the same
          --      if we have e.g. an Either in our state:
          --      Flipping it from Left to Right will change the type string.
          --      For now this is fine since our State only has IORefs.
          when (stateTypeString /= show (from tmpState)) $
            -- `error` is fine here since this can only be called from
            -- ghci anyway, and `error` won't terminate ghci.
            error "cannot restart in-place: the State type changed"

      -- All clear, state is safe to load.

      oldState <- readStore store

      -- Only store an empty transient state so that we can't access
      -- things that cannot survive a reload (like GPU buffers).
      emptyTransientState <- createTransientState
      let newState = oldState{ transient = emptyTransientState }

      deleteStore store
      _ <- newStore newState

      -- If OpenGL is (still or already) initialized, just ask it to
      -- shut down in the next `idle` loop.
      get (sGlInitialized oldState) >>= \case
        True  -> do -- Ask the GL loop running on the old state to
                    -- restart for us.
                    sRestartRequested oldState $= True
                    sRestartFunction oldState $= mainStateFun newState
                    -- TODO We should also deallocate all BufferObjects.
        False -> void $ forkIO $ mainStateFun newState


getRandomColor :: IO (Color3 GLfloat)
getRandomColor = Color3 <$> randomRIO (0,1)
                        <*> randomRIO (0,1)
                        <*> randomRIO (0,1)


-- Add some random points as one point cloud
addRandomPoints :: State -> IO ()
addRandomPoints state = do
  x <- randomRIO (0, 10)
  y <- randomRIO (0, 10)
  z <- randomRIO (0, 10)
  i <- genID state
  let points = map mkVec3 [(x+1,y+2,z+3),(x+4,y+5,z+6)]
      colour = Color3 (realToFrac $ x/10) (realToFrac $ y/10) (realToFrac $ z/10)
  addPointCloud state $ Cloud i (OneColor colour) (V.fromList points)

-- addPointCloud globalState Cloud{ cloudColor = Color3 0 0 1, cloudPoints = V.fromList [ Vec3 x y z | x <- [1..4], y <- [1..4], let z = 3 ] }

addDevicePointCloud :: State -> IO ()
addDevicePointCloud state = do
  putStrLn "Depth snapshot: start"
  s <- takeDepthSnapshot
  putStrLn "Depth snapshot: done"

  case s of
    Left err -> hPutStrLn stderr $ "WARNING: " ++ err
    Right (depthVec, (width, _height)) -> do

      r <- randomRIO (0, 1)
      g <- randomRIO (0, 1)
      b <- randomRIO (0, 1)

      let points =   V.map scalePoints
                   . V.filter (\(Vec3 _ _ d) -> d /= 0) -- remove 0 depth points
                   . V.imap (\i depth ->                -- convert x/y/d to floats
                       let (y, x) = i `quotRem` width
                        in Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral depth)
                     )
                   $ depthVec

      i <- genID state
      addPointCloud state $ Cloud i (OneColor $ Color3 r g b) points

  where
    -- Scale the points from the camera so that they appear nicely in 3D space.
    -- TODO remove X/Y scaling by changing the camera in the viewer
    -- TODO Use camera intrinsics + error correction function
    scalePoints (Vec3 x y d) = Vec3 (x / 10.0)
                                    (y / 10.0)
                                    (d / 20.0 - 30.0)


vertexVec3 :: Vec3 -> IO ()
vertexVec3 (Vec3 x y z) = vertex (Vertex3 (realToFrac x) (realToFrac y) (realToFrac z) :: Vertex3 GLfloat)


loadPCDFileXyzFloat :: FilePath -> IO (Vector Vec3)
loadPCDFileXyzFloat file = V.map v3toVec3 <$> PCD.loadXyz file
  where
    v3toVec3 (V3 a b c) = Vec3 a b c

loadPCDFileXyzNormalFloat :: FilePath -> IO (Vector Vec3, Vector Vec3)
loadPCDFileXyzNormalFloat file = do
  ps <- PCD.loadXyzRgbNormal file
  return (V.map (v3toVec3 . PCD.xyz) ps, V.map (rgbToFloats . PCD.rgb) ps)
  where
    rgbToFloats (V3 r g b) = Vec3 (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b / 255.0)
    v3toVec3 (V3 a b c) = Vec3 a b c


cloudFromFile :: State -> FilePath -> IO Cloud
cloudFromFile state file = do
  -- TODO this switching is nasty, pcl-loader needs to be improved
  i <- genID state
  p1 <- loadPCDFileXyzFloat file
  if not (V.null p1)
    then return $ Cloud i (OneColor $ Color3 1 0 0) p1
    else do
      (p2, colors) <- loadPCDFileXyzNormalFloat file
      return $ Cloud i (ManyColors colors) p2


loadPCDFile :: State -> FilePath -> IO ()
loadPCDFile state file = do
  addPointCloud state =<< cloudFromFile state file



-- | Plane equation: ax + by + cz = d, or: n*xyz = d
-- (Hessian normal form). It matters that the d is on the
-- right hand side since we care about plane normal direction.
data PlaneEq = PlaneEq !Normal3 !Float -- parameters: a b c d
  deriving (Eq, Ord, Show, Generic)

mkPlaneEq :: Vec3 -> Float -> PlaneEq
mkPlaneEq abc d = PlaneEq (mkNormal abc) (d / norm abc)

mkPlaneEqABCD :: Float -> Float -> Float -> Float -> PlaneEq
mkPlaneEqABCD a b c d = mkPlaneEq (Vec3 a b c) d


flipPlaneEq :: PlaneEq -> PlaneEq
flipPlaneEq (PlaneEq n d) = PlaneEq (flipNormal n) (-d)


signedDistanceToPlaneEq :: PlaneEq -> Vec3 -> Float
signedDistanceToPlaneEq (PlaneEq n d) p = fromNormal n `dotprod` p - d


projectToPlane :: PlaneEq -> Vec3 -> Vec3
projectToPlane eq@(PlaneEq n _) p = p &- (signedDistanceToPlaneEq eq p *& fromNormal n)


planeEqsFromFile :: FilePath -> IO [PlaneEq]
planeEqsFromFile file = do
  let float = realToFrac <$> double
      floatS = float <* skipSpace
      -- PCL exports plane in the form `ax + by + cz + d = 0`,
      -- we need `ax + by + cz = d`.
      planesParser = (mkPlaneEqABCD <$> floatS <*> floatS <*> floatS <*> (negate <$> float))
                     `sepBy1'` endOfLine
  parseOnly planesParser <$> BS.readFile file >>= \case
    Left err -> error $ "Could not load planes: " ++ show err
    Right planes -> return planes


planesFromDir :: State -> FilePath -> IO [Plane]
planesFromDir state dir = do
  eqs <- planeEqsFromFile (dir </> "planes.txt")
  forM (zip [0..] eqs) $ \(x :: Int, eq) -> do
    let name = "cloud_plane_hull" ++ show x ++ ".pcd"
        file = dir </> name
    putStrLn $ "Loading " ++ file

    points <- loadPCDFileXyzFloat file
    col <- getRandomColor
    i <- genID state

    return $ Plane i eq col points


loadPlanes :: State -> FilePath -> IO ()
loadPlanes state dir = do
  planes <- planesFromDir state dir
  forM_ planes (addPlane state)


planeCorner :: PlaneEq -> PlaneEq -> PlaneEq -> Vec3
planeCorner (PlaneEq n1 d1)
            (PlaneEq n2 d2)
            (PlaneEq n3 d3) = Vec3 (f x) (f y) (f z)
  where
    -- TODO Figure out how to detect when the system isn't solvable (parallel planes)
    Vec3 a1 b1 c1 = fromNormal n1
    Vec3 a2 b2 c2 = fromNormal n2
    Vec3 a3 b3 c3 = fromNormal n3
    f = realToFrac :: Double -> Float
    d = realToFrac :: Float -> Double
    [x,y,z] = HmatrixVec.toList . Matrix.flatten $ linearSolve lhs rhs
    lhs = (3><3)[ d a1, d b1, d c1
                , d a2, d b2, d c2
                , d a3, d b3, d c3 ]
    rhs = (3><1)[ d d1, d d2, d d3 ]


red :: Color3 GLfloat
red = Color3 1 0 0


addCornerPoint :: State -> IO ()
addCornerPoint state@State{ transient = TransientState{..}, ..} = do
  get sSelectedPlanes >>= \case
    [p1,p2,p3]-> do
      let corner = planeCorner (planeEq p1) (planeEq p2) (planeEq p3)

      -- First check if p1 is part of a room.
      rooms <- Map.elems <$> get sRooms
      case [ r | pid <- map planeID [p1, p2, p3]
               , Just r <- [findRoomContainingPlane rooms pid] ] of
        [r@Room{ roomID = i },r2,r3] | roomID r2 == i && roomID r3 == i -> do
          case roomCorners r of
            corners | length corners < 8 -> do
              putStrLn $ "Merging planes of room to corner " ++ show corner
              sRooms $~ Map.insert i r{ roomCorners = corner : roomCorners r }
            _ -> putStrLn $ "Room " ++ show i ++ " already has 8 corners"
        _ -> do
          putStrLn $ "Merged planes to corner " ++ show corner
          i <- genID state
          addPointCloud state $ Cloud i (OneColor red) (V.fromList [corner])

    ps -> putStrLn $ show (length ps) ++ " planes selected, need 3"

  sSelectedPlanes $= []


-- | Calculates the rotation matrix that will rotate plane1 into the same
-- direction as plane 2.
--
-- Note that if you actually want to rotate plane 2 onto plane 1, you have
-- to take the inverse or pass them the other way around!
rotationBetweenPlaneEqs :: PlaneEq -> PlaneEq -> Mat3
rotationBetweenPlaneEqs (PlaneEq n1 _) (PlaneEq n2 _) = o -- TODO change this to take Normal3 directly instead of planeEqs
  where
    -- TODO Use http://lolengine.net/blog/2013/09/18/beautiful-maths-quaternion-from-vectors
    o = rotMatrix3' axis theta
    axis = crossprod n1 n2
    costheta = dotprod n1 n2 / (norm n1 * norm n2)
    theta = acos costheta


rotatePlaneEq :: Mat3 -> PlaneEq -> PlaneEq
rotatePlaneEq rotMat (PlaneEq n d) = mkPlaneEq n' d'
  where
    n' = fromNormal n .* rotMat
    d' = d -- The distance from plane to origin does
           -- not change when rotating around origin.


rotatePlaneEqAround :: Vec3 -> Mat3 -> PlaneEq -> PlaneEq
rotatePlaneEqAround rotCenter rotMat (PlaneEq n d) = mkPlaneEq n' d'
  where
    -- See http://stackoverflow.com/questions/7685495
    n' = fromNormal n .* rotMat
    o = d *& fromNormal n
    o' = rotateAround rotCenter rotMat o
    d' = o' `dotprod` n' -- distance from origin along NEW normal vector


-- | Rotates a point around a rotation center.
rotateAround :: Vec3 -> Mat3 -> Vec3 -> Vec3
rotateAround rotCenter rotMat p = ((p &- rotCenter) .* rotMat) &+ rotCenter


rotatePlaneAround :: Vec3 -> Mat3 -> Plane -> Plane
rotatePlaneAround rotCenter rotMat p@Plane{ planeEq = oldEq, planeBounds = oldBounds }
  = p{ planeEq     = rotatePlaneEqAround rotCenter rotMat oldEq
     , planeBounds = V.map (rotateAround rotCenter rotMat) oldBounds }


rotatePlane :: Mat3 -> Plane -> Plane
rotatePlane rotMat p = rotatePlaneAround (planeMean p) rotMat p


pointMean :: Vector Vec3 -> Vec3
pointMean points = c
  where
    n = V.length points
    c = V.foldl' (&+) zero points &* (1 / fromIntegral n)  -- bound center


cloudMean :: Cloud -> Vec3
cloudMean Cloud{ cloudPoints } = pointMean cloudPoints


planeMean :: Plane -> Vec3
planeMean Plane{ planeBounds } = pointMean planeBounds


findRoomContainingPlane :: [Room] -> ID -> Maybe Room
findRoomContainingPlane rooms i = find (\r -> any ((i == ) . planeID) (roomPlanes r)) rooms


rotateSelectedPlanes :: State -> IO ()
rotateSelectedPlanes state@State{ transient = TransientState{..}, ..} = do
  get sSelectedPlanes >>= \case
    [p1,p2] -> do
      -- We want to rotate p1.
      let pid1 = planeID p1
      -- First check if p1 is part of a room.
      rooms <- Map.elems <$> get sRooms
      case findRoomContainingPlane rooms pid1 of
        Just oldRoom -> do
          let rot = rotationBetweenPlaneEqs (planeEq p1) (flipPlaneEq $ planeEq p2)

          putStrLn $ "Rotating room by " ++ show rot
          updateRoom state (rotateRoom rot oldRoom)

        Nothing -> do
          let p1' = rotatePlane rot p1
              rot = rotationBetweenPlaneEqs (planeEq p1) (planeEq p2)
          putStrLn $ "Rotating plane"
          addPlane state p1'

    ps -> putStrLn $ show (length ps) ++ " planes selected, need 2"

  -- Reset selected planes in any case
  sSelectedPlanes $= []


rotateCloudAround :: Vec3 -> Mat3 -> Cloud -> Cloud
rotateCloudAround rotCenter rotMat c@Cloud{ cloudPoints = oldPoints }
  = c { cloudPoints = V.map (rotateAround rotCenter rotMat) oldPoints }


roomMean :: Room -> Vec3
roomMean Room{ roomCloud } = cloudMean roomCloud


rotateRoomAround :: Vec3 -> Mat3 -> Room -> Room
rotateRoomAround rotCenter rotMat r@Room{ roomPlanes = oldPlanes, roomCloud = oldCloud, roomCorners = oldCorners }
  = r{ roomPlanes = map (rotatePlaneAround rotCenter rotMat) oldPlanes
     , roomCloud = rotateCloudAround rotCenter rotMat oldCloud
     , roomCorners = map (rotateAround rotCenter rotMat) oldCorners
     }

rotateRoom :: Mat3 -> Room -> Room
rotateRoom rotMat r = rotateRoomAround (roomMean r) rotMat r


translatePlaneEq :: Vec3 -> PlaneEq -> PlaneEq
-- translatePlaneEq off (PlaneEq n d) = PlaneEq n d' -- TODO this is ok but needs comment
translatePlaneEq off (PlaneEq n d) = mkPlaneEq (fromNormal n) d'
  where
    -- See http://stackoverflow.com/questions/7685495
    o = d *& fromNormal n
    o' = o &+ off
    d' = o' `dotprod` fromNormal n -- distance from origin along normal vector


translatePlane :: Vec3 -> Plane -> Plane
translatePlane off p@Plane{ planeEq = oldEq, planeBounds = oldBounds }
  = p{ planeEq     = translatePlaneEq off oldEq
     , planeBounds = V.map (off &+) oldBounds }


translateCloud :: Vec3 -> Cloud -> Cloud
translateCloud off c@Cloud{ cloudPoints = oldPoints }
  = c { cloudPoints = V.map (off &+) oldPoints }


translateRoom :: Vec3 -> Room -> Room
translateRoom off room@Room{ roomPlanes = oldPlanes, roomCloud = oldCloud, roomCorners = oldCorners  }
  = room{ roomPlanes = map (translatePlane off) oldPlanes
        , roomCloud = translateCloud off oldCloud
        , roomCorners = map (off &+) oldCorners
        }


-- Clouds recored with Kinfu clouds are always heads-up.
rotateKinfuRoom :: Room -> Room
rotateKinfuRoom = rotateRoom (rotMatrixX (toRad 180))


loadRoom :: State -> FilePath -> IO Room
loadRoom state@State{ transient = TransientState{ sRooms } } dir = do
  cloud <- cloudFromFile state (dir </> "cloud_downsampled.pcd")
  addPointCloud state cloud

  -- Make all plane normals inward facing
  let roomCenter = cloudMean cloud
      makeInwardFacing p@Plane{ planeEq = PlaneEq n d }
        = p{ planeEq = let inwardVec = roomCenter - planeMean p -- TODO use one point on plane instead of planeMean
                           pointsInward = inwardVec `dotprod` fromNormal n > 0
                        in if pointsInward then PlaneEq n d
                                           else PlaneEq (flipNormal n) (-d)
           }

  planes <- map makeInwardFacing <$> planesFromDir state dir

  i <- genID state
  let room = rotateKinfuRoom $ Room i planes cloud []
  sRooms $~ Map.insert i room
  putStrLn $ "Room " ++ show i ++ " loaded"
  return room


getRoom :: State -> ID -> IO (Maybe Room)
getRoom State{ transient = TransientState { sRooms } } i = Map.lookup i <$> get sRooms


changeRoom :: State -> ID -> (Room -> Room) -> IO ()
changeRoom state@State{ transient = TransientState { sRooms } } i f = do
  getRoom state i >>= \case
    Nothing -> putStrLn "no room loaded"
    Just r -> do
      let r' = f r
      sRooms $~ Map.insert i r'
      updatePointCloud state (roomCloud r')


updateRoom :: State -> Room -> IO ()
updateRoom state@State{ transient = TransientState{ sRooms } } room = do
  sRooms $~ Map.insert (roomID room) room
  updatePointCloud state (roomCloud room)


addPlane :: State -> Plane -> IO ()
addPlane State{ transient = TransientState{ sPlanes } } p@Plane{ planeID = i } = do
  sPlanes $~ (Map.insert i p)


fitCuboidToSelectedRoom :: State -> IO ()
fitCuboidToSelectedRoom state@State{ transient = TransientState{ sSelectedRoom } } = do
  get sSelectedRoom >>= \case
    Nothing -> putStrLn "no room selected"
    Just r  -> fitCuboidToRoom state r


fitCuboidToRoom :: State -> Room -> IO ()
fitCuboidToRoom state Room{ roomID, roomCorners } = do
  putStrLn $ "fitting cuboid to room " ++ show roomID

  if length roomCorners < 8
    then putStrLn "not enough room corners; need 8"
    else do
      putStrLn $ "Room corners: " ++ show roomCorners

      let points = map toDoubleVec roomCorners
          (params, steps, err, _) = fitCuboidFromCenterFirst points

      putStrLn $ "fit cuboid in " ++ show steps ++ " steps, RMSE: " ++ show (sqrt err)

      -- Replace room planes and corners by those of the cuboid

      let cuboidPoints = map toFloatVec $ cuboidFromParams params
          [x,y,z, a,b,c, q1,q2,q3,q4] = map toFloat params

      cuboidPlanes <- makePlanesFromCuboid state cuboidPoints
                                           (Vec3 x y z) (Vec3 a b c)
                                           (mkU (Vec4 q1 q2 q3 q4))

      changeRoom state roomID (\r -> r{ roomCorners = cuboidPoints
                                      , roomPlanes  = cuboidPlanes })


makePlanesFromCuboid :: State -> [Vec3] -> Vec3 -> Vec3 -> UnitQuaternion -> IO [Plane]
makePlanesFromCuboid state cuboidPoints cuboidCenter _cuboidDims@(Vec3 a b c) rotQuat = do

  px1 <- fromOriginPlane (mkPlaneEq (Vec3   1    0    0 ) (a/2))
  px2 <- fromOriginPlane (mkPlaneEq (Vec3 (-1)   0    0 ) (a/2))
  py1 <- fromOriginPlane (mkPlaneEq (Vec3   0    1    0 ) (b/2))
  py2 <- fromOriginPlane (mkPlaneEq (Vec3   0  (-1)   0 ) (b/2))
  pz1 <- fromOriginPlane (mkPlaneEq (Vec3   0    0    1 ) (c/2))
  pz2 <- fromOriginPlane (mkPlaneEq (Vec3   0    0  (-1)) (c/2))

  return [px1, px2, py1, py2, pz1, pz2]

  where
    rotMat = fromOrtho $ rightOrthoU rotQuat

    -- We first construct the planes as if the room was centered at
    -- the origin and the planes orthogonal to the axes, and then
    -- adjust the room to the center and rotation we got from the
    -- cuboid fitting.
    fromOriginPlane originEq = do
      i   <- genID state
      col <- getRandomColor
      let eq = translatePlaneEq cuboidCenter . rotatePlaneEqAround zero rotMat $ originEq

          reorderPolygon corners = let c1:rest = corners
                                       [c2,c3,c4] = sortBy (comparing (distance c1)) rest
                                    in [c1,c2,c4,c3]
          bounds =   V.fromList
                   . (\l -> assert (length l == 4) l)
                   . reorderPolygon
                   . filter ((< 1e-6) . abs . signedDistanceToPlaneEq eq)
                   $ cuboidPoints

      return $ Plane i eq col bounds


toFloatVec :: Vect.Double.Vec3 -> Vec3
toFloatVec (Vect.Double.Vec3 a b c) = Vec3 (realToFrac a) (realToFrac b) (realToFrac c)

toDoubleVec :: Vec3 -> Vect.Double.Vec3
toDoubleVec (Vec3 a b c) = Vect.Double.Vec3 (realToFrac a) (realToFrac b) (realToFrac c)


autoAlignFloor :: State -> Room -> IO ()
autoAlignFloor state room@Room{ roomID, roomPlanes } = do
  putStrLn $ "auto aligning floor of room " ++ show roomID

  case roomPlanes of
    [] -> putStrLn "room has no planes"
    ps -> do
      let floorPlane = maximumBy (comparing (dotprod vec3Y . planeNormal)) ps
          rot = rotationBetweenPlaneEqs (planeEq floorPlane) (mkPlaneEq vec3Y 1)

      updateRoom state (rotateRoom rot room)


newEmptyCloud :: State -> IO Cloud
newEmptyCloud state = do
  i <- genID state
  return $ Cloud i (OneColor red) (V.empty)


save :: State -> IO ()
save state = saveTo state "save.bin"


saveTo :: State -> FilePath -> IO ()
saveTo State{ transient = TransientState{ sRooms } } path = do
  putStrLn $ "Saving rooms to " ++ path
  get sRooms >>= Binary.encodeFile path
  putStrLn "saved"


load :: State -> IO ()
load state = loadFrom state "save.bin"


loadFrom :: State -> FilePath -> IO ()
loadFrom State{ transient = TransientState{ sRooms } } path = do
  putStrLn $ "Loading rooms from " ++ path
  (sRooms $=) =<< Binary.decodeFile path


clearRooms :: State -> IO ()
clearRooms State{ transient = TransientState{ sRooms } } = do
  putStrLn "Clearing"
  sRooms $= Map.empty


connectWalls :: State -> IO ()
connectWalls State{ transient = TransientState{..}, ..} = do
  get sSelectedPlanes >>= \case
    [p1,p2] -> do
      let pid1 = planeID p1
          pid2 = planeID p2
      rooms <- Map.elems <$> get sRooms
      case (findRoomContainingPlane rooms pid1, findRoomContainingPlane rooms pid2) of
        (Just room1, Just room2) -> do
          -- TODO make sure both rooms are axis aligned
          putStrLn $ "Connecting rooms " ++ show (roomID room1, roomID room2) ++ " via wall planes " ++ show (pid1, pid2)
          sConnectedWalls $~ \ws -> if (pid1, pid2) `elem` ws || (pid2, pid1) `elem` ws
                                      then ws
                                      else (pid1, pid2):ws
        _ -> do
          putStrLn $ "The planes " ++ show (pid1, pid2) ++ " are not walls of different rooms!"

    ps -> putStrLn $ show (length ps) ++ " walls selected, need 2"

  -- Reset selected planes in any case
  sSelectedPlanes $= []


devSetup :: State -> IO ()
devSetup state = do
  -- Coord planes
  pid1 <- genID state
  pid2 <- genID state
  pid3 <- genID state
  addPlane state (Plane pid1 (PlaneEq (mkNormal $ Vec3 1 0 0) 0) (Color3 1 0 0) (V.fromList [Vec3 0 0 0, Vec3 0 5 0, Vec3 0 5 5, Vec3 0 0 5]))
  addPlane state (Plane pid2 (PlaneEq (mkNormal $ Vec3 0 1 0) 0) (Color3 0 1 0) (V.fromList [Vec3 0 0 0, Vec3 5 0 0, Vec3 5 0 5, Vec3 0 0 5]))
  addPlane state (Plane pid3 (PlaneEq (mkNormal $ Vec3 0 0 1) 0) (Color3 0 0 1) (V.fromList [Vec3 0 0 0, Vec3 0 5 0, Vec3 5 5 0, Vec3 5 0 0]))

  r <- loadRoom state "/home/niklas/uni/individualproject/recordings/rec2/room4/walls-hulls"
  -- changeRoom state (roomID r) (translateRoom (Vec3 10 0 0))
  changeRoom state (roomID r) (\x -> x{ roomCorners =
    [ Vec3 4.136416 0.8529552 4.406994
    , Vec3 4.4101005 3.5105393 4.9701643
    , Vec3 0.73565805 3.9981246 4.6329975
    , Vec3 0.37607455 1.3853515 4.068618
    , Vec3 0.46311855 2.1294374 0.744112
    , Vec3 0.8107674 4.658209 1.2055379
    , Vec3 4.833117 4.139892 1.4955193
    , Vec3 4.5571914 1.541648 1.0290517
    ] })
  fitCuboidToRoom state =<< (\(Just x) -> x) <$> getRoom state (roomID r)
  autoAlignFloor state =<< (\(Just x) -> x) <$> getRoom state (roomID r)
  -- void $ loadRoom state "/home/niklas/uni/individualproject/recordings/rec2/room4/walls-hulls"


-- | For debugging / ghci only.
dfl :: [Vec3] -> IO ()
dfl ps = do
  state <- getState
  i <- genID state
  col <- getRandomColor
  addPointCloud state (Cloud i (OneColor col) (V.fromList ps))
