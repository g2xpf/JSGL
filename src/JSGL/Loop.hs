module JSGL.Loop (
  execute 
) where

import JSGL.GL.Window (initWindow)
import JSGL.Utils.Debug
import JSGL.Types.Type 
import Control.Monad (when)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

wi = windowInfo {
  size = (1920, 1080),
  title = "Window",
  resizable = False
}

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  print key
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

execute :: IO ()
execute = do
  maybeWindow <- initWindow wi $ return undefined
  case maybeWindow of
    Nothing -> coloredLog Red $ print "Failed to create a GLFW window!"
    Just window -> do
      GLFW.setKeyCallback window (Just callback)
      GLFW.makeContextCurrent (Just window)
      (x, y) <- GLFW.getFramebufferSize window
      glViewport 0 0 (fromIntegral x) (fromIntegral y)
      let loop = do
            shouldContinue <- GLFW.windowShouldClose window
            when (not shouldContinue) $ do
              GLFW.pollEvents
              glClearColor 1.0 1.0 1.0 1.0
              glClear GL_COLOR_BUFFER_BIT
              GLFW.swapBuffers window
            loop
      loop
  GLFW.terminate
