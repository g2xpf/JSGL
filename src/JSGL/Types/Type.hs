module JSGL.Types.Type (
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
  ShapeObject(..),
  MetaInfo(..),
) where 

import JSGL.Utils.Debug
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign.Ptr
import Foreign
import System.IO (FilePath)
import Control.Monad.Trans.State

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

data ShapeInfo = ShapeInfo {
  hasIndex :: Bool,
  vao :: VAO,
  points :: Int
}

data ShapeObject = ShapeObject {
  program :: Program,
  position :: Position,
  shapeInfo :: ShapeInfo
}

data MetaInfo = MetaInfo {
  getWindow :: GLFW.Window
}

type Position = [GLfloat]
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
