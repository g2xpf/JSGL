module JSGL.Drawer.Window (
  initWindow,
) where

import JSGL.Types.Type
import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import System.Exit
import JSGL.Utils.Debug

initWindow :: WindowInfo -> IO () -> IO GLFW.Window
initWindow info initialzer = do
  let (windowWidth, windowHeight) = size info
      windowTitle = title info
      windowResizable = resizable info
      isFullscreen = fullscreen info
  primaryMonitor <- branch isFullscreen <$> GLFW.getPrimaryMonitor

  initialzer
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'Resizable $ resizable info
  maybeWindow <- GLFW.createWindow windowWidth windowHeight windowTitle primaryMonitor Nothing
  case maybeWindow of
    Just window -> return window
    Nothing -> do
      err $ "Failed to create a GLFW window!"
      exitFailure

branch :: Bool -> Maybe a -> Maybe a
branch is maybe = do
  if is
    then maybe
    else Nothing
