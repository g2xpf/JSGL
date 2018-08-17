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
  VAO, BufferObject, VBO, EBO,
-- program object
  Program,
-- position
  Position, Element,
-- buffer object builder
  BufferObjectBuilder,
-- attribute index
  AttribPointer,
-- vertex dimension
  Dimension,
) where 

import JSGL.Utils.Debug
import Graphics.GL.Core33
import Graphics.GL.Types
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

type VAO = GLuint
type BufferObject = GLuint
type VBO = BufferObject
type EBO = BufferObject
type Position = [GLfloat]
type Program = GLuint
type Element = [GLenum]
type ShaderObject = GLuint
type ShaderType = GLenum
type BufferObjectBuilder = StateT [BufferObject] IO ()
type AttribPointer = GLuint
type Dimension = GLint
