{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase, MultiWayIf, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 (parseOnly, sepBy1', double, endOfLine, skipSpace)
import           Data.Bits (unsafeShiftR)
import qualified Data.ByteString as BS
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Int (Int64)
import           Data.List (minimumBy, maximumBy)
import           Data.Ord (comparing)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Trees.KdTree as KdTree
import           Data.Trees.KdTree (KdTree(..))
import           Data.Vect.Float hiding (Vector)
import           Data.Vect.Float.Instances ()
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Data.Word
import           Foreign.C.Types (CInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (peek)
import           Foreign.Store (newStore, lookupStore, readStore)
import           Graphics.GLUtil
import           Graphics.UI.GLUT hiding (Plane)
import           Linear (V3(..))
import qualified PCD.Data as PCD
import           Safe
import           System.Endian (fromBE32)
import           System.Random (randomRIO)
import           System.SelfRestart (forkSelfRestartExePollWithAction)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Unsafe (unsafePerformIO)
import           HoniHelper (takeDepthSnapshot)


-- Orphan instance so that we can derive Ord
instance Ord Vec3 where


data Cloud = Cloud
  { cloudColor :: Color3 GLfloat
  , cloudPoints :: Vector Vec3
  } deriving (Eq, Ord, Show)

data Clouds = Clouds
  { allocatedClouds :: Map BufferObject Cloud
  } deriving (Eq, Ord, Show)

data DragMode = Rotate | Translate
  deriving (Eq, Ord, Show)

-- TODO make all State/TransientState fields strict so that we get an error if not initialized

-- |Application state
data State
  = State { sMouse :: IORef ( GLint, GLint )
          , sDragMode :: IORef (Maybe DragMode)
          , sSize :: IORef ( GLint, GLint )
          , sRotX :: IORef GLfloat
          , sRotY :: IORef GLfloat
          , sZoom :: IORef GLfloat
          , sPan :: IORef ( GLfloat, GLfloat, GLfloat )
          , queuedClouds :: IORef [Cloud]
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
          , sPickObjectAt :: IORef (Maybe (Int,Int))
          , sUnderCursor :: IORef (Maybe ID)
          , sDebugPickingDrawVisible :: IORef Bool
          , sDebugPickingTiming :: IORef Bool
          -- Correspondences
          , sKdDistance :: IORef Double
          , transient :: TransientState
          }

data TransientState
  = TransientState { sNextID :: IORef ID
                   , sPickingMode :: IORef Bool
                   , sClouds :: IORef Clouds
                   , sCorrespondenceLines :: IORef [(Vec3, Maybe Vec3)]
                   , sPolygons :: IORef [(ID, Vector Vec3, Color3 GLfloat)]
                   }


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


withDisabled :: [StateVar Capability] -> IO b -> IO b
withDisabled vars f = do
  befores <- mapM get vars
  mapM_ ($= Disabled) vars
  x <- f
  zipWithM_ ($=) vars befores
  return x


-- |Called when stuff needs to be drawn
display :: State -> DisplayCallback
display state@State{..} = do

  ( width, height ) <- get sSize
  rx                <- get sRotX
  ry                <- get sRotY
  z                 <- get sZoom
  ( tx, ty, tz )    <- get sPan

  let buffers = [ ColorBuffer, DepthBuffer, StencilBuffer ]

  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (fromIntegral width / fromIntegral height) 0.1 500.0


  matrixMode $= Modelview 0
  loadIdentity
  translate $ Vector3 0 0 (-z * 10.0)
  translate $ Vector3 (-tx) (-ty) (-tz)
  rotate rx $ Vector3 1 0 0
  rotate ry $ Vector3 0 1 0

  -- Do pick rendering (using color picking)
  pickingDisabled <- get sPickingDisabled
  get sPickObjectAt >>= \case
    Just (x,y) | not pickingDisabled -> do
      i <- colorPicking state (x,y)
      sPickObjectAt $= Nothing
      sUnderCursor $= if i == noID then Nothing else Just i
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
colorPicking :: State -> (Int, Int) -> IO ID
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

  on sDebugPickingDrawVisible $ do
    timeAfter <- getPOSIXTime
    putStrLn $ "Picking took " ++ show (timeAfter - timeBefore) ++ " s"

  return i


on :: HasGetter a => a Bool -> IO () -> IO ()
on var f = get var >>= \enabled -> when enabled f


i2c :: Int -> CInt
i2c = fromIntegral

c2i :: CInt -> Int
c2i = fromIntegral


-- |Draws the objects to show
drawObjects :: State -> IO ()
drawObjects state@State{ transient = TransientState{ sPickingMode } } = do
  picking <- get sPickingMode

  -- Objects must only be drawn in picking mode when they are colour picking
  -- aware, that is they query the picking mode and draw themselves only in
  -- colors generated by `idToColor <$> genID` if we are picking.

  when (not picking) $ drawReferenceSystem

  when (not picking) $ drawPointClouds state

  when (not picking) $ drawCorrespondenceLines state

  drawPolygons state


drawReferenceSystem :: IO ()
drawReferenceSystem = do

  displayQuad 1 1 1

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
drawPointClouds state@State{ transient = TransientState{ sClouds } } = do

  -- Allocate BufferObjects for all queued clouds
  processCloudQueue state

  Clouds{ allocatedClouds } <- get sClouds

  -- Render all clouds
  forM_ (Map.toList allocatedClouds) $ \(bufObj, Cloud{ cloudColor = col, cloudPoints }) -> do

    color col
    clientState VertexArray $= Enabled
    bindBuffer ArrayBuffer $= Just bufObj
    arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr
    drawArrays Points 0 (i2c $ V.length cloudPoints)
    bindBuffer ArrayBuffer $= Nothing
    clientState VertexArray $= Disabled


drawPolygons :: State -> IO ()
drawPolygons State{ sUnderCursor, transient = TransientState{ sPolygons, sPickingMode } } = do

  pols <- get sPolygons
  picking <- get sPickingMode
  underCursor <- get sUnderCursor

  let drawPolys = do
        forM_ pols $ \(i, points, Color3 r g b) -> do

          stencilFunc $= (Always, fromIntegral i, 0xff)

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
processCloudQueue State{ transient = TransientState{ sClouds }, queuedClouds } = do

  -- Get out queued clouds, set queued clouds to []
  queued <- atomicModifyIORef' queuedClouds (\cls -> ([], cls))

  forM_ queued $ \cloud@Cloud{ cloudPoints } -> do

    -- Allocate buffer object containing all these points
    bufObj <- fromVector ArrayBuffer cloudPoints

    modifyIORef sClouds $
      \p@Clouds{ allocatedClouds = cloudMap } ->
        p{ allocatedClouds = Map.insert bufObj cloud cloudMap }


addPointCloud :: State -> Cloud -> IO ()
addPointCloud State{ queuedClouds } cloud = do
  atomicModifyIORef' queuedClouds (\cls -> (cloud:cls, ()))


initializeObjects :: State -> IO ()
initializeObjects _state = do
  return ()


-- |Displays a quad
displayQuad :: GLfloat -> GLfloat -> GLfloat -> IO ()
displayQuad w h d = preservingMatrix $ do
  scale w h d

  stencilFunc $= (Always, 1, 0xff) -- allow picking the quad as ID 1

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
idle State{..} = do

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
  let diffX = fromIntegral $ posx - oldx
      diffY = fromIntegral $ posy - oldy

  sMouse $= ( posx, posy )

  get sDragMode >>= \case
    Just Rotate -> do
      sRotY $~! (+ diffX)
      sRotX $~! (+ diffY)
    Just Translate -> do
      zoom <- get sZoom
      sPan $~! (\(x,y,z) -> (x - (diffX * 0.03 * zoom), y + (diffY * 0.03 * zoom), z) )
    _ -> return ()


-- | Mouse motion (without buttons pressed)
passiveMotion :: State -> Position -> IO ()
passiveMotion State{..} (Position posx posy) = do

  sPickObjectAt $= Just (c2i posx, c2i posy)



changeFps :: State -> (Int -> Int) -> IO ()
changeFps State{ sFps } f = do
  sFps $~ f
  putStrLn . ("FPS: " ++) . show =<< get sFps


printStencilValue :: State -> CInt -> CInt -> IO ()
printStencilValue State{ sSize } x y = do
  ( _width, height ) <- get sSize -- TODO query freshly
  val <- alloca $ \(intPtr :: Ptr CInt) -> do
    readPixels (Position x (height-y-1)) (Size 1 1) (PixelData StencilIndex UnsignedInt intPtr)
    peek intPtr
  putStrLn $ "stencil value at " ++ show (x,height-y-1) ++ ": " ++ show val


-- |Button input
input :: State -> Key -> KeyState -> Modifiers -> Position -> IO ()
input state@State{..} (MouseButton LeftButton) Down _ (Position x y) = do
  printStencilValue state x y
  sPickObjectAt $= Just (c2i x, c2i y)
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
input state (Char 'p') Down _ _ = addRandomPoints state
input state (Char '\r') Down _ _ = addDevicePointCloud state
input state (Char 'c') Down _ _ = addCorrespondences state
input _state key Down _ _ = putStrLn $ "Unhandled key " ++ show key
input _state _ _ _ _ = return ()


-- |Mouse wheel movement (sZoom)
wheel :: State -> WheelNumber -> WheelDirection -> Position -> IO ()
wheel State{..} _num dir _pos
  | dir > 0   = get sZoom >>= (\x -> sZoom $= clamp (x + 0.5))
  | otherwise = get sZoom >>= (\x -> sZoom $= clamp (x - 0.5))
  where
    clamp x = 0.5 `max` (30.0 `min` x)


-- | Creates the default state
createState :: IO State
createState = do
  sMouse            <- newIORef ( 0, 0 )
  sDragMode         <- newIORef Nothing
  sSize             <- newIORef ( 0, 1 )
  sRotX             <- newIORef 0.0
  sRotY             <- newIORef 0.0
  sZoom             <- newIORef 5.0
  sPan              <- newIORef ( 0, 0, 0 )
  queuedClouds      <- newIORef []
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
  sKdDistance       <- newIORef 0.5
  transient         <- createTransientState

  return State{..} -- RecordWildCards for initialisation convenience


createTransientState :: IO TransientState
createTransientState = do
  sNextID <- newIORef 1
  sPickingMode <- newIORef False
  sClouds <- newIORef (Clouds Map.empty)
  sCorrespondenceLines <- newIORef []
  sPolygons <- newIORef []
  return TransientState{..}


-- |Main
main :: IO ()
main = do
  state <- createState
  mainState state


-- | Run `main` on a state.
mainState :: State -> IO ()
mainState state@State{..} = do

  -- Save the state globally
  globalStateRef $= Just state
  lookupStore 0 >>= \case -- to survive GHCI reloads
    Just _  -> return ()
    Nothing -> do
      -- Only store an empty transient state so that we can't access
      -- things that cannot survive a reload (like GPU buffers).
      emptytTransientState <- createTransientState
      void $ newStore state{ transient = emptytTransientState }

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
  pointSize   $= 1.0

  -- Enable stencil buffer for picking
  stencilTest  $= Enabled
  clearStencil $= 0
  stencilOp    $= (OpKeep, OpKeep, OpReplace)

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
    sRestartRequested $= False
    -- We can't just call `mainState state` here since that would (tail) call
    -- the original function instead of the freshly loaded one. That's why the
    -- function is put into the IORef to be updated by `restart`.
    f <- get sRestartFunction
    f



-- | Global state.
globalState :: State
globalState = unsafePerformIO $ do
  get globalStateRef >>= \case
    Nothing -> error "global state not set!"
    Just s  -> return s

{-# NOINLINE globalStateRef #-}
globalStateRef :: IORef (Maybe State)
globalStateRef = unsafePerformIO $ do
  -- putStrLn "setting globalState"
  newIORef Nothing

-- For restarting the program in GHCI while keeping the `State` intact.
restart :: IO ()
restart = do
  lookupStore 0 >>= \case
    Nothing -> putStrLn "restart: starting for first time" >> (void $ forkIO main)
    Just store -> do
      state@State{..} <- readStore store
      -- If OpenGL is (still or already) initialized, just ask it to
      -- shut down in the next `idle` loop.
      get sGlInitialized >>= \case
        True  -> do sRestartRequested $= True
                    sRestartFunction $= mainState state
        False -> void $ forkIO $ mainState state


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
  let points = map mkVec3 [(x+1,y+2,z+3),(x+4,y+5,z+6)]
      colour = Color3 (realToFrac $ x/10) (realToFrac $ y/10) (realToFrac $ z/10)
  addPointCloud state $ Cloud colour (V.fromList points)

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

      addPointCloud state $ Cloud (Color3 r g b) points

  where
    -- Scale the points from the camera so that they appear nicely in 3D space.
    -- TODO remove X/Y scaling by changing the camera in the viewer
    -- TODO Use camera intrinsics + error correction function
    scalePoints (Vec3 x y d) = Vec3 (x / 10.0)
                                    (y / 10.0)
                                    (d / 20.0 - 30.0)



instance KdTree.Point Vec3 where
    dimension _ = 3

    coord 0 (Vec3 a _ _) = realToFrac a
    coord 1 (Vec3 _ b _) = realToFrac b
    coord 2 (Vec3 _ _ c) = realToFrac c


vertexVec3 :: Vec3 -> IO ()
vertexVec3 (Vec3 x y z) = vertex (Vertex3 (realToFrac x) (realToFrac y) (realToFrac z) :: Vertex3 GLfloat)


addCorrespondences :: State -> IO ()
addCorrespondences State{ transient = TransientState { sClouds, sCorrespondenceLines }, .. } = do
  Clouds{ allocatedClouds } <- get sClouds
  case Map.elems allocatedClouds of
    c1:c2:_ -> do
                 let l1 = V.toList $ cloudPoints c1
                     l2 = V.toList $ cloudPoints c2
                     kd1 = KdTree.fromList l1
                     kd2 = KdTree.fromList l2
                     -- closest1 = take 100 [ (p1,p2) | p1 <- l1, let Just p2 = KdTree.nearestNeighbor kd2 p1 ]
                     -- closest1 = take 100 [ (p1,p2) | p1 <- l1, let Just p2 = KdTree.nearestNeighbor kd1 p1 ]
                     -- closest1 = take 100 [ (p1,p2) | p1 <- l1, let [_self, p2] = KdTree.kNearestNeighbors kd1 2 p1 ]
                     -- closest1 = [ (p1, atMay (KdTree.nearNeighbors kd1 2 p1) 1) | p1 <- l1 ]
                     -- Care: `nearNeighbors` returns closest last (`kNearestNeighbors` returns it first)

                    -- closest1 = [ (p1, secondSmallestBy (comparing (KdTree.dist2 p1)) $ KdTree.nearNeighbors kd1 200 p1) | p1 <- l1 ]
                    -- closest1 = [ (p1, secondSmallestBy (comparing (KdTree.dist2 p1)) $ KdTree.nearNeighbors kd1 0.5 p1) | p1 <- l1 ]

                 d <- get sKdDistance
                 let closest1 = [ (p1, secondSmallestBy (comparing (KdTree.dist2 p1)) $ KdTree.nearNeighbors kd2 d p1) | p1 <- l1 ]
                 putStrLn "closest1"
                 -- mapM_ print closest1
                 -- putStrLn "closestAll"
                 -- mapM_ print [ (p1, KdTree.kNearestNeighbors kd1 3 p1) | p1 <- l1 ]
                 -- putStrLn "closest200"
                 -- mapM_ print [ (p1, KdTree.nearNeighbors kd1 200 p1) | p1 <- l1 ]
                 putStrLn $ "drawing " ++ show (length closest1) ++ " correspondence lines"
                 sCorrespondenceLines $= closest1

    _       -> hPutStrLn stderr $ "WARNING: not not enough clouds for correspondences"


secondSmallest :: (Ord a) => [a] -> Maybe a
secondSmallest []      = Nothing
secondSmallest [_]     = Nothing
secondSmallest (a:b:l) = Just $ go l (min a b) (max a b)
  where
    go []     _  s2             = s2
    go (x:xs) s1 s2 | x < s1    = go xs x s1
                    | x < s2    = go xs s1 x
                    | otherwise = go xs s1 s2

secondSmallestBy :: (a -> a -> Ordering) -> [a] -> Maybe a
secondSmallestBy _ []      = Nothing
secondSmallestBy _ [_]     = Nothing
secondSmallestBy f (a:b:l) = Just $ go l (minimumBy f [a,b]) (maximumBy f [a,b])
  where
    go []     _  s2                = s2
    go (x:xs) s1 s2 | f x s1 == LT = go xs x s1
                    | f x s2 == LT = go xs s1 x
                    | otherwise    = go xs s1 s2


drawCorrespondenceLines :: State -> IO ()
drawCorrespondenceLines state@State{ transient = TransientState{ sCorrespondenceLines } } = do

  closest1 <- get sCorrespondenceLines
  lineWidth $= 1.0
  renderPrimitive Lines $ do
    forM_ (zip [1..] closest1) $ \(i, (p1, mp2)) -> do
      -- case mp2 of Just p2 | i `mod` 10 == 0 -> do
      case mp2 of Just p2 -> do
                    color3 1.0 0.0 0.0
                    vertexVec3 p1
                    vertexVec3 p2
                  _ -> return ()


loadPCDFileXyzFloat :: FilePath -> IO (Vector Vec3)
loadPCDFileXyzFloat file = V.map v3toVec3 <$> PCD.loadXyz file
  where
    v3toVec3 (V3 a b c) = Vec3 a b c


loadPCDFile :: State -> FilePath -> IO ()
loadPCDFile state file = do
  points <- loadPCDFileXyzFloat file
  addPointCloud state (Cloud (Color3 1 0 0) points)


data Plane = Plane Double Double Double Double -- parameters: a b c d
  deriving (Eq, Ord, Show)


loadPlanes :: State -> FilePath -> IO ()
loadPlanes _state file = do
  let doubleS = double <* skipSpace
      planesParser = (Plane <$> doubleS <*> doubleS <*> doubleS <*> double)
                     `sepBy1'` endOfLine
  planes <- parseOnly planesParser <$> BS.readFile file
  print planes


addBoundingHulls :: State -> IO ()
addBoundingHulls state@State{ transient = TransientState{ sPolygons } } = do
  forM_ [ "cloud_plane_hull0.pcd"
        , "cloud_plane_hull1.pcd"
        , "cloud_plane_hull2.pcd"
        , "cloud_plane_hull3.pcd"
        , "cloud_plane_hull4.pcd"
        ] $ \name -> do
    let file = "/home/niklas/uni/individualproject/recordings/rec2/room4/walls-hulls/" ++ name
    putStrLn $ "Loading " ++ file

    points <- loadPCDFileXyzFloat file
    col <- getRandomColor
    i <- genID state

    sPolygons $~ ((i, points, col):)
