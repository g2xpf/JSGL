module JSGL.Loop (
  execute 
) where

import Control.Concurrent
import JSGL.Drawer.Window (initWindow)
import JSGL.Utils.Debug
import JSGL.Types.Type 
import Control.Monad (when, join)
import Control.Monad.Trans.Class
import Control.Exception (bracket)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign.Ptr
import System.Random

import JSGL.Drawer.VBO
import JSGL.Drawer.Shader
import JSGL.Drawer.Shapes

wi = windowInfo {
  size = (3840, 2160),
  title = "Window",
  resizable = False,
  fullscreen = False
}

loop :: GLFW.Window -> IO () -> IO ()
loop window action = do
  shouldContinue <- GLFW.windowShouldClose window
  when (not shouldContinue) $ do
    action
    loop window action
    

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  print key
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

glfwBracket :: IO () -> IO ()
glfwBracket action = bracket GLFW.init (const GLFW.terminate) $ \initialized ->
  when initialized action

execute :: IO ()
execute = glfwBracket $ do
  window <- initWindow wi $ return ()
  GLFW.setKeyCallback window (Just callback)
  GLFW.makeContextCurrent (Just window)
  (x, y) <- GLFW.getFramebufferSize window
  glViewport 0 0 (fromIntegral x) (fromIntegral y)

  vert <- createShader GL_VERTEX_SHADER "src/JSGL/Shader/Main.vert"
  frag <- createShader GL_FRAGMENT_SHADER "src/JSGL/Shader/Main.frag"
  program <- linkProgram vert frag

  gen <- getStdGen
  let points = 6 
      pointsFloat = fromIntegral points
  shapeInfo <- createVAO points $ do
    createVBO attribInfo { 
      attribPointer = 0,
      dimension = 2,
      array = join [[cos (2.0*pi/pointsFloat*i), sin (2.0*pi/pointsFloat*i)] | i <- fromIntegral <$> [0..points-1]]
    }
    createVBO attribInfo {
      attribPointer = 1,
      dimension = 3,
      array = take (points * 3) $ randomRs (0.0, 1.0) gen :: [GLfloat]
    }
    createIBO [ 0, 1, 2,
                2, 0, 3,
                3, 0, 5,
                5, 3, 4]

  loop window $ do
    glClearColor 1.0 1.0 1.0 1.0
    glClear GL_COLOR_BUFFER_BIT
    glUseProgram program
    draw shapeInfo GL_TRIANGLES
    GLFW.swapBuffers window
    GLFW.pollEvents
