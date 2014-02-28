{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Vector.Storable (Vector, (!))
import           Foreign.Ptr (nullPtr)
import           Graphics.GLUtil
import           Graphics.UI.GLUT
import           System.Random (randomRIO)
import           System.SelfRestart (forkSelfRestartExePollWithAction)
import           System.IO (hPutStrLn, stderr)
import           Honi (Oni)
import qualified Honi as Honi
import           Honi.Types (SensorType (SensorDepth))
import qualified Honi.Types as Honi

import           HoniHelper (takeDepthSnapshot, withHoni)


data Cloud = Cloud
  { cloudColor :: Color3 GLfloat
  , cloudPoints :: [(Float, Float, Float)]
  , cloudNumPoints :: Int
  } deriving (Eq, Ord, Show)

data Clouds = Clouds
  { allocatedClouds :: Map BufferObject Cloud
  } deriving (Eq, Ord, Show)


-- |Application state
data State
  = State { sMouseX :: IORef ( GLint, GLint )
          , sSize :: IORef ( GLint, GLint )
          , sRotX :: IORef GLfloat
          , sRotY :: IORef GLfloat
          , sZoom :: IORef GLfloat
          , sPan :: IORef ( GLfloat, GLfloat, GLfloat )
          , sClouds :: IORef Clouds
          , queuedClouds :: IORef [Cloud]
          }

-- |Sets the vertex color
color3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3 x y z
  = color $ Color4 x y z 1.0


-- |Sets the vertex position
vertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3 x y z
  = vertex $ Vertex3 x y z


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
  rotate rx $ Vector3 1 0 0
  rotate ry $ Vector3 0 1 0
  translate $ Vector3 (-tx) (-ty) (-tz)

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

-- |Draws the objects to show
drawObjects :: State -> IO ()
drawObjects state = do
  displayQuad 1 1 1

  drawPointClouds state


drawPointClouds :: State -> IO ()
drawPointClouds state@State{ sClouds } = do

  -- Allocate BufferObjects for all queued clouds
  processCloudQueue state

  Clouds{ allocatedClouds } <- readIORef sClouds

  -- Render all clouds
  forM_ (Map.toList allocatedClouds) $ \(bufObj, Cloud{ cloudColor = col, cloudNumPoints = n }) -> do

    color col
    clientState VertexArray $= Enabled
    bindBuffer ArrayBuffer $= Just bufObj
    arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 nullPtr
    drawArrays Points 0 (fromIntegral n)
    bindBuffer ArrayBuffer $= Nothing
    clientState VertexArray $= Disabled


processCloudQueue :: State -> IO ()
processCloudQueue State{ sClouds, queuedClouds } = do

  -- Get out queued clouds, set queued clouds to []
  queued <- atomicModifyIORef' queuedClouds (\cls -> ([], cls))

  forM_ queued $ \cloud -> do

    let flat = concat [ [x,y,z] | (x,y,z) <- cloudPoints cloud ]

    -- Allocate buffer object containing all these points
    bufObj <- makeBuffer ArrayBuffer flat

    modifyIORef sClouds $
      \p@Clouds{ allocatedClouds = cloudMap } ->
        p{ allocatedClouds = Map.insert bufObj cloud cloudMap }


addPointCloud :: State -> Cloud -> IO ()
addPointCloud State{ queuedClouds } cloud = do
  atomicModifyIORef' queuedClouds (\cls -> (cloud:cls, ()))


initializeObjects :: State -> IO ()
initializeObjects state = do

  let ps = [(1,2,3),(4,5,6)]
  addPointCloud state $ Cloud (Color3 0 1 0) ps (length ps)


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
  postRedisplay Nothing


-- |Mouse motion
motion :: State -> Position -> IO ()
motion State{..} (Position x y) = do
  ( mx, my ) <- get sMouseX

  sRotY $~! (+ fromIntegral ( fromIntegral x - mx ) )
  sRotX $~! (+ fromIntegral ( fromIntegral y - my ) )

  sMouseX $= ( x, y )


-- |Button input
input :: State -> Key -> KeyState -> Modifiers -> Position -> IO ()
input State{..} (MouseButton LeftButton) Down _ (Position x y)
  = sMouseX $= ( x, y )
input state (MouseButton WheelDown) Down _ pos
  = wheel state 0 120 pos
input state (MouseButton WheelUp) Down _ pos
  = wheel state 0 (-120) pos
input _mxy _ _ _ _
  = return ()


-- |Mouse wheel movement (sZoom)
wheel :: State -> WheelNumber -> WheelDirection -> Position -> IO ()
wheel State{..} _num dir _pos
  | dir > 0   = get sZoom >>= (\x -> sZoom $= clamp (x + 0.1))
  | otherwise = get sZoom >>= (\x -> sZoom $= clamp (x - 0.1))
  where
    clamp x = 0.5 `max` (30.0 `min` x)


-- |Main
main :: IO ()
main = do
  forkSelfRestartExePollWithAction 1.0 $ do
    putStrLn "executable changed, restarting"
    threadDelay 1000000

  void $ getArgsAndInitialize >> createWindow "3D cloud viewer"

  -- Create a new state
  state <- State <$> newIORef ( 0, 0 )
                 <*> newIORef ( 0, 1 )
                 <*> newIORef 0.0
                 <*> newIORef 0.0
                 <*> newIORef 5.0
                 <*> newIORef ( 0, 0, 0 )
                 <*> newIORef (Clouds Map.empty)
                 <*> newIORef []

  -- OpenGL
  clearColor  $= Color4 0 0 0 1
  shadeModel  $= Smooth
  depthMask   $= Enabled
  depthFunc   $= Just Lequal
  lineWidth   $= 3.0
  pointSize   $= 2.0

  -- Callbacks
  displayCallback       $= display state
  reshapeCallback       $= Just (reshape state)
  idleCallback          $= Just (idle state)
  mouseWheelCallback    $= Just (wheel state)
  motionCallback        $= Just (motion state)
  keyboardMouseCallback $= Just (input state)

  initializeObjects state

  -- _ <- forkIO $ cloudAdderThread state

  _ <- forkIO $ honiThread state

  -- Let's get started
  mainLoop



-- Pressing Enter adds new points
cloudAdderThread :: State -> IO ()
cloudAdderThread state = do
  forever $ do
    _ <- getLine -- read newline
    x <- randomRIO (0, 10)
    y <- randomRIO (0, 10)
    z <- randomRIO (0, 10)
    let ps = [(x+1,y+2,z+3),(x+4,y+5,z+6)]
        colour = Color3 (realToFrac $ x/10) (realToFrac $ y/10) (realToFrac $ z/10)
    addPointCloud state $ Cloud colour ps (length ps)


-- Pressing Enter adds new points from a depth camera snapshot
honiThread :: State -> IO ()
honiThread state = withHoni $ forever $ do
  _ <- getLine

  putStrLn "Depth snapshot: start"
  s <- takeDepthSnapshot
  putStrLn "Depth snapshot: done"

  case s of
    Left err -> hPutStrLn stderr $ "WARNING: " ++ err
    Right (depthVec, width, height) -> do

      r <- randomRIO (0, 1)
      g <- randomRIO (0, 1)
      b <- randomRIO (0, 1)

      let points = [ ( fromIntegral x / 10.0
                     , fromIntegral y / 10.0
                     , fromIntegral depth / 40.0 - 20.0
                     ) | x <- [0..width-1]
                       , y <- [0..height-1]
                       , let depth = depthVec ! (y * width + x)
                       , depth /= 0
                   ]
          colour = Color3 r g b

      addPointCloud state $ Cloud colour points (length points)
