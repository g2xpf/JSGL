module JSGL.Drawer.Shapes (
  createVAO,
) where

import Control.Monad.Trans.State
import Control.Monad (forM_, join)
import Data.IORef
import JSGL.Utils.Debug (assert, note, glErr)
import Data.List (splitAt, transpose)
import System.Exit
import JSGL.Drawer.VBO
import JSGL.Types.Type
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign
import Foreign.Ptr
import Foreign.C.String

createVAO :: PointCount -> BufferObjectBuilder -> IO ShapeInfo
createVAO cnt vboBuilder = do
  vaoP <- malloc
  glGenVertexArrays 1 vaoP
  vao <- peek vaoP
  glBindVertexArray vao
  (vbos, maybeIndices) <- execStateT vboBuilder ([], Nothing)
  points <- bindVBO cnt $ reverse vbos
  case maybeIndices of
    Just indices -> do
      bindIBO cnt indices
      return ShapeInfo {
        hasIndex = True,
        vao = vao,
        points = points * (length indices `div` 3)
      }
    Nothing -> return ShapeInfo {
      hasIndex = False,
      vao = vao,
      points = points
    }

bindVBO :: PointCount -> [AttribInfo] -> IO Int
bindVBO cnt attribInfoArray = do
  let dims = map dimension attribInfoArray
      bufferDataArray = map array attribInfoArray
      attribArray = map attribPointer attribInfoArray
  bufferDataArray' <- composeBufferData dims bufferDataArray
  assert (length bufferDataArray' == cnt) "The nunmber of vertices doesn't match"

  let size = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length . join $ bufferDataArray')
      points = length bufferDataArray'
  bufferDataArrayP <- newArray $ join bufferDataArray'
  vboP <- malloc
  glGenBuffers 1 vboP
  vbo <- peek vboP
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER size (castPtr bufferDataArrayP) GL_STATIC_DRAW

  offsetRef <- newIORef nullPtr
  forM_ attribInfoArray $ \info -> do
    let attrib = attribPointer info
        dim = dimension info
        arraySize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length (head bufferDataArray'))
    offsetP <- readIORef offsetRef
    glVertexAttribPointer attrib dim GL_FLOAT GL_FALSE arraySize offsetP
    glEnableVertexAttribArray attrib
    modifyIORef offsetRef (`plusPtr` (fromIntegral dim * fromIntegral (sizeOf (0.0 :: GLfloat))))
  return points

bindIBO :: PointCount -> Indices -> IO ()
bindIBO cnt indices = do
  let size = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
  indicesP <- newArray indices 
  assert (filter (\val -> (fromIntegral val) >= cnt) indices == []) "The number of vertices doesn't match"
  iboP <- malloc
  glGenBuffers 1 iboP
  ibo <- peek iboP
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
  glBufferData GL_ELEMENT_ARRAY_BUFFER size (castPtr indicesP) GL_STATIC_DRAW 

composeBufferData :: [Dimension] -> [BufferData] -> IO [BufferData]
composeBufferData dims buffs = do
  buffs' <- sequence $ zipWith split dims buffs
  let lengthArray = map length buffs'
  assert (allEq lengthArray) $ "The number of points isn't consistent: " ++ show lengthArray
  assert (not . elem 0 $ lengthArray) $ "The array contains empty array"
  return . map join . transpose $ buffs'

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq [x] = True
allEq (x:xs) = and $ map (x ==) xs

split :: Dimension -> BufferData -> IO [BufferData]
split n ary = do
  assert ((length ary `mod` (fromIntegral n)) == 0) $ "The array size or dimension might be invalid. given: " ++ (show $ length ary) ++ ", dimension: " ++ show n
  split' n ary []

split' :: Dimension -> BufferData -> [BufferData] -> IO [BufferData]
split' _ [] ret = return ret
split' n ary ret = do
  let (h, t) = splitAt (fromIntegral n) ary
  split' n t $ h : ret
