module JSGL.GL.Window (
  initWindow,
) where

import JSGL.Types.Type
import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

initWindow :: WindowInfo -> IO () -> IO (Maybe GLFW.Window)
initWindow info initialzer = do
  let (windowWidth, windowHeight) = size info
      windowTitle = title info
      windowResizable = resizable info
      
  print "po"
  initialzer
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'Resizable $ resizable info
  GLFW.createWindow windowWidth windowHeight windowTitle Nothing Nothing
