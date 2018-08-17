module JSGL.GL.VAO (
  createVAO,
) where

import Control.Monad.Trans.State
import JSGL.GL.VBO
import JSGL.Types.Type
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign
import Foreign.Ptr
import Foreign.C.String

createVAO :: BufferObjectBuilder -> IO VAO
createVAO vboBuilder = do
  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao
  evalStateT vboBuilder [] 
  return vao


  


