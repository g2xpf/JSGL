module JSGL.Drawer.VBO (
  createVBO,
  createIBO,
) where

import Foreign
import Foreign.Ptr
import Foreign.C.String (newCAStringLen)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import JSGL.Types.Type
import Graphics.GL.Types
import Graphics.GL.Core33
import Graphics.UI.GLFW as GLFW

createVBO :: AttribInfo -> BufferObjectBuilder
createVBO info = do
  modify (\(ary, maybeIbo) -> (info : ary, maybeIbo))


createIBO :: Indices -> BufferObjectBuilder
createIBO indices = do
  (vbos, maybeIndices) <- get
  case maybeIndices of
    Just _ -> lift $ return ()
    Nothing -> put (vbos, Just indices)
