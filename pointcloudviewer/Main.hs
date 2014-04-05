{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Int (Int64)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Vect.Float hiding (Vector)
import           Data.Vect.Float.Instances ()
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Foreign.Ptr (nullPtr)
import           Foreign.Store (newStore, lookupStore, readStore)
import           Graphics.GLUtil
import           Graphics.UI.GLUT
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
          , transient :: TransientState
          }

data TransientState
  = TransientState { sClouds :: IORef Clouds
                   }

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


-- |Called when stuff needs to be drawn
display :: State -> DisplayCallback
display state@State{..} = do

  ( width, height ) <- get sSize
  rx                <- get sRotX
  ry                <- get sRotY
  z                 <- get sZoom
  ( tx, ty, tz )    <- get sPan

  clear [ ColorBuffer, DepthBuffer ]


  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (fromIntegral width / fromIntegral height) 0.1 500.0


  matrixMode $= Modelview 0
  loadIdentity
  translate $ Vector3 0 0 (-z * 10.0)
  translate $ Vector3 (-tx) (-ty) (-tz)
  rotate rx $ Vector3 1 0 0
  rotate ry $ Vector3 0 1 0

  -- |Draw reference system
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

  preservingMatrix $ drawObjects state

  flush

  getTimeUs >>= \now -> sLastLoopTime $= Just now

-- |Draws the objects to show
drawObjects :: State -> IO ()
drawObjects state = do
  displayQuad 1 1 1

  drawPointClouds state


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
    drawArrays Points 0 (fromIntegral $ V.length cloudPoints)
    bindBuffer ArrayBuffer $= Nothing
    clientState VertexArray $= Disabled


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
initializeObjects state = do

  let points = map mkVec3 [(1,2,3),(4,5,6)]
  addPointCloud state $ Cloud (Color3 0 1 0) (V.fromList points)


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


-- |Mouse motion
motion :: State -> Position -> IO ()
motion State{..} (Position posx posy) = do
  ( mx, my ) <- get sMouse
  let diffX = fromIntegral $ posx - mx
      diffY = fromIntegral $ posy - my

  get sDragMode >>= \case
    Just Rotate -> do
      sRotY $~! (+ diffX)
      sRotX $~! (+ diffY)
    Just Translate -> do
      zoom <- get sZoom
      sPan $~! (\(x,y,z) -> (x - (diffX * 0.03 * zoom), y + (diffY * 0.03 * zoom), z) )
    _ -> return ()

  sMouse $= ( posx, posy )


changeFps :: State -> (Int -> Int) -> IO ()
changeFps State{ sFps } f = do
  sFps $~ f
  putStrLn . ("FPS: " ++) . show =<< get sFps

-- |Button input
input :: State -> Key -> KeyState -> Modifiers -> Position -> IO ()
input State{..} (MouseButton LeftButton) Down _ (Position x y) = do
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
  transient         <- createTransientState

  return State{..} -- RecordWildCards for initialisation convenience


createTransientState :: IO TransientState
createTransientState = do
  sClouds <- newIORef (Clouds Map.empty)
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

  void $ getArgsAndInitialize >> createWindow "3D cloud viewer"
  sGlInitialized $= True

  -- OpenGL
  clearColor  $= Color4 0 0 0 1
  shadeModel  $= Smooth
  depthMask   $= Enabled
  depthFunc   $= Just Lequal
  lineWidth   $= 3.0
  pointSize   $= 1.0

  -- Callbacks
  displayCallback       $= display state
  reshapeCallback       $= Just (reshape state)
  idleCallback          $= Just (idle state)
  mouseWheelCallback    $= Just (wheel state)
  motionCallback        $= Just (motion state)
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
  get sRestartRequested >>= \r -> when r $ do
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


-- Add some random points as one point cloud
addRandomPoints :: State -> IO ()
addRandomPoints state = do
  x <- randomRIO (0, 10)
  y <- randomRIO (0, 10)
  z <- randomRIO (0, 10)
  let points = map mkVec3 [(x+1,y+2,z+3),(x+4,y+5,z+6)]
      colour = Color3 (realToFrac $ x/10) (realToFrac $ y/10) (realToFrac $ z/10)
  addPointCloud state $ Cloud colour (V.fromList points)


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
                                    (d / 40.0 - 20.0)
