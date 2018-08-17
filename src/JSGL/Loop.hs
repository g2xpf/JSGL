module JSGL.Loop (
  execute 
) where

import JSGL.GL.Window (initWindow)
import JSGL.Utils.Debug
import JSGL.Types.Type 
import Control.Monad (when, join)
import Control.Exception (bracket)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign.Ptr

import JSGL.GL.VAO
import JSGL.GL.VBO
import JSGL.GL.Shader


wi = windowInfo {
  size = (1920, 1080),
  title = "Window",
  resizable = False
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

  vao <- createVAO $ do
    let theta = 2.0 * pi / 6.0;
    createVBO $ join [[cos (theta * i), sin (theta * i)] | i <- [1..6]]
    createEBO [0, 1, 2,
               2, 0, 5,
               5, 2, 4,
               2, 4, 3]
    bindAttribute 0 2

  loop window $ do
    glClearColor 1.0 1.0 1.0 1.0
    glClear GL_COLOR_BUFFER_BIT
    glUseProgram program
    glBindVertexArray vao
    glDrawElements GL_TRIANGLES 12 GL_UNSIGNED_INT nullPtr
    GLFW.swapBuffers window
    GLFW.pollEvents
