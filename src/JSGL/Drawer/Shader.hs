module JSGL.Drawer.Shader (
  createShader,
  linkProgram
)where

import JSGL.Types.Type
import JSGL.Utils.Debug
import JSGL.Utils.File(basename, fetch)
import System.Exit
import Foreign
import Foreign.Ptr
import Foreign.C.String (newCAStringLen)
import Control.Exception
import System.Exit
import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Control.Monad (when)

createShader :: ShaderType -> FilePath ->  IO ShaderObject
createShader shaderType shaderPath = do
  shader <- glCreateShader shaderType
  (sourceP, len) <- newCAStringLen =<< fetch shaderPath
  lineP <- newArray [sourceP]
  lengthP <- newArray [fromIntegral len]
  glShaderSource shader 1 lineP lengthP
  glCompileShader shader
  successP <- malloc
  glGetShaderiv shader GL_COMPILE_STATUS successP
  success <- peek successP
  when (success == GL_FALSE) $ do
    err $ "Compile error in " ++ basename shaderPath ++ ":"
    let infoLength = 512
    resultP <- malloc
    infoLog <- mallocArray $ fromIntegral infoLength
    glGetShaderInfoLog shader (fromIntegral infoLength) resultP infoLog
    result <- fromIntegral <$> peek resultP
    logBytes <- peekArray result infoLog
    putStrLn $ map (toEnum.fromEnum) logBytes
    exitFailure
  return shader

linkProgram :: ShaderObject -> ShaderObject -> IO Program
linkProgram vs fs = do
  program <- glCreateProgram
  glAttachShader program vs
  glAttachShader program fs
  glLinkProgram program
  successP <- malloc
  glGetProgramiv program GL_LINK_STATUS successP
  success <- peek successP
  when (success == GL_FALSE) $ do
    err $ "Link error"
    let infoLength = 512
    resultP <- malloc
    infoLog <- mallocArray $ fromIntegral infoLength
    glGetProgramInfoLog program (fromIntegral infoLength) resultP infoLog
    result <- fromIntegral <$> peek resultP
    logBytes <- peekArray result infoLog
    putStrLn $ map (toEnum.fromEnum) logBytes
    exitFailure
  glDeleteShader vs
  glDeleteShader fs
  glUseProgram program
  return program
