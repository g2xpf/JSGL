module JSGL.Types.Type (
-- window info constructor
  WindowInfo(..),
  windowInfo,
-- shader source data tyeps
  ShaderType,
  ShaderObject,
-- alias of filename type
  FilePath,
-- vao, vbo typename
  VAO, BufferObject, VBO, IBO,
-- program object
  Program,
-- position
  Element,
-- buffer object builder
  BufferObjectBuilder,
-- attribute index
  AttribPointer,
-- vertex dimension
  Dimension,
-- AttribInfo
  AttribInfo(..),
  attribInfo,
  BufferData,
  BufferInfo,
  PointCount,
  Indices,
  DrawType,
-- shape info
  ShapeInfo(..),
  Drawable(..),
) where 

import JSGL.Utils.Debug
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign.Ptr
import System.IO(FilePath)
import Control.Monad.Trans.State

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

data BufferObjectInfo = BufferObjectInfo {
  vbo :: (Maybe AttribPointer, [GLfloat]),
  ibo :: IBO,
  program :: Program
} deriving Show

data AttribInfo = AttribInfo {
  attribPointer :: AttribPointer,
  dimension :: Dimension,
  array :: [GLfloat] 
} deriving (Show, Eq)

attribInfo = AttribInfo {
  attribPointer = 0,
  dimension = 0,
  array = []
}

class Drawable a where
  draw :: a -> DrawType -> IO ()

instance Drawable ShapeInfo where
  draw info drawType = do
    glBindVertexArray $ vao info
    let points' = fromIntegral $ points info
    if hasIndex info
    then glDrawElements drawType points' GL_UNSIGNED_INT nullPtr
    else glDrawArrays drawType 0 points'
    
data ShapeInfo = ShapeInfo {
  hasIndex :: Bool,
  vao :: VAO,
  points :: Int
}

type DrawType = GLenum
type Indices = [GLuint]
type BufferType = GLenum
type BufferInfo = ([AttribInfo], Maybe Indices)
type VAO = GLuint
type BufferObject = GLuint
type VBO = BufferObject
type IBO = BufferObject
type BufferData = [GLfloat]
type Program = GLuint
type Element = [GLenum]
type ShaderObject = GLuint
type ShaderType = GLenum
type BufferObjectBuilder = StateT BufferInfo IO ()
type AttribPointer = GLuint
type Dimension = GLint
type VBOBuilder = StateT [AttribInfo] IO Bool
type PointCount = Int
