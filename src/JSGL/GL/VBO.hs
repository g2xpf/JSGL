module JSGL.GL.VBO (
  createVBO,
  createEBO,
  bindAttribute
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

createVBO :: Position -> BufferObjectBuilder
createVBO position = do
  bos <- get
  let size = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length position)
  vbo <- lift $ do 
    positionP <- newArray position
    vboP <- malloc
    glGenBuffers 1 vboP
    vbo <- peek vboP
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER size (castPtr positionP) GL_STATIC_DRAW
    return vbo
  put $ vbo : bos

createEBO :: Element -> BufferObjectBuilder
createEBO element = do
  bos <- get
  let size = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length element)
  ebo <- lift $ do
    elementP <- newArray element
    eboP <- malloc
    glGenBuffers 1 eboP
    ebo <- peek eboP
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
    glBufferData GL_ELEMENT_ARRAY_BUFFER size (castPtr elementP) GL_STATIC_DRAW
    return ebo
  put $ ebo : bos
    
bindAttribute :: AttribPointer -> Dimension -> BufferObjectBuilder
bindAttribute attr dim = do
  vbo <- head <$> get
  let size = fromIntegral $ sizeOf (0.0 :: GLfloat) * (fromIntegral dim)
  lift $ do
    glVertexAttribPointer 0 dim GL_FLOAT GL_FALSE size nullPtr
    glEnableVertexAttribArray attr
