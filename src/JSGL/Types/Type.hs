module JSGL.Types.Type (
  WindowInfo(..),
  windowInfo,
) where 

import JSGL.Utils.Debug

data WindowInfo = WindowInfo {
  size :: (Int, Int),
  title :: String,
  resizable :: Bool
}

windowInfo :: WindowInfo
windowInfo = WindowInfo {
  size = (800, 600),
  title = "Window",
  resizable = False
}


