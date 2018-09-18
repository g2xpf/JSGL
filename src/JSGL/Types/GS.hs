module JSGL.Types.GS (
  GS,
  runGS, runGS_,
  -- monad reader
  reader, local, ask,
  -- monad state
  get, gets, put, modify, modify',
  -- monad trans
  lift,
  -- monad io
  liftIO
) where

import Control.Concurrent (forkIO, threadDelay)
import JSGL.Utils.Debug (warn, assert)
import JSGL.Drawer.Window (initWindow, WindowInfo(..))
import JSGL.Types.CState
import Control.Exception (bracket)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

type GS s a = CState s GLFW.Window IO a
runGS :: GS s a -> (WindowInfo, s) -> IO (a, s)
runGS gs (windowInfo, initState) = do
  let interval = 1000000 `div` fps windowInfo

  glfwBracket $ do
    window <- initWindow windowInfo
    GLFW.setKeyCallback window (Just callback)
    GLFW.makeContextCurrent (Just window)
    (x, y) <- GLFW.getFramebufferSize window
    glViewport 0 0 (fromIntegral x) (fromIntegral y)
    runCState gs (window, initState)

runGS_ :: GS s a -> (WindowInfo, s) -> IO ()
runGS_ gs init = do
  runGS gs init
  return ()

glfwBracket :: IO a -> IO a
glfwBracket action = do
  bracket GLFW.init (const GLFW.terminate) $ \initialized ->
    if initialized
      then action
      else return undefined

callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  print key
  when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

