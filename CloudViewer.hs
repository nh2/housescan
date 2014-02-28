{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
--------------------------------------------------------------------------------
-- OpenGL quadrocopter viewer
--------------------------------------------------------------------------------
module Main where


import Data.IORef
import Control.Monad
import Control.Applicative
import Graphics.UI.GLUT

-- |Application state
data State
  = State { sMouseX :: IORef ( GLint, GLint )
          , sSize :: IORef ( GLint, GLint )
          , sRotX :: IORef GLfloat
          , sRotY :: IORef GLfloat
          , sZoom :: IORef GLfloat
          , sPan :: IORef ( GLfloat, GLfloat, GLfloat )
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
display State{..} = do

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

  preservingMatrix $ drawObjects

  flush

-- |Draws the objects to show
drawObjects :: IO ()
drawObjects = return ()


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
  void $ getArgsAndInitialize >> createWindow "3D cloud viewer"

  -- Create a new state
  state <- State <$> newIORef ( 0, 0 )
                 <*> newIORef ( 0, 1 )
                 <*> newIORef 0.0
                 <*> newIORef 0.0
                 <*> newIORef 5.0
                 <*> newIORef ( 0, 0, 0 )

  -- OpenGL
  clearColor  $= Color4 0 0 0 1
  shadeModel  $= Smooth
  depthMask   $= Enabled
  depthFunc   $= Just Lequal
  lineWidth   $= 3.0

  -- Callbacks
  displayCallback       $= display state
  reshapeCallback       $= Just (reshape state)
  idleCallback          $= Just (idle state)
  mouseWheelCallback    $= Just (wheel state)
  motionCallback        $= Just (motion state)
  keyboardMouseCallback $= Just (input state)


  -- Let's get started
  mainLoop


