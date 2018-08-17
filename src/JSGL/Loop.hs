module JSGL.Loop (
  execute 
) where

import JSGL.GL.Window (initWindow)
import JSGL.Utils.Debug
import JSGL.Types.Type 
import Control.Monad (when)
import Control.Exception (bracket)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

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
    createVBO [0.5, 0.5, 0.0,
               0.5, -0.5, 0.0,
               -0.5, 0.5, 0.0,
               0.5, -0.5, 0.0,
               -0.5, -0.5, 0.0,
               -0.5, 0.5, 0.0]
    bindAttribute 0 3
  loop window $ do
    glClearColor 1.0 1.0 1.0 1.0
    glClear GL_COLOR_BUFFER_BIT
    glUseProgram program
    glBindVertexArray vao
    glDrawArrays GL_TRIANGLES 0 6
    GLFW.swapBuffers window
    GLFW.pollEvents
