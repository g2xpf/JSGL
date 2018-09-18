module JSGL.Loop (
  execute 
) where

import Control.Concurrent
import JSGL.Drawer.Window (initWindow)
import JSGL.Utils.Debug
import JSGL.Types.Type 
import Control.Monad (when, join, replicateM_, forM_)
import Control.Monad.Trans.Class
import Control.Exception (bracket)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Types

import Foreign
import Foreign.Ptr
import Foreign.C.String

import System.Random

import JSGL.Drawer.VBO
import JSGL.Drawer.Shader
import JSGL.Drawer.Shapes
import JSGL.Drawer.Window
import Linear

import JSGL.Types.GS
import JSGL.Types.Drawable

wi = defaultWindowInfo {
  size = (2160, 2160)
}

transposeMatrix :: (Floating a, Epsilon a) => V3 a -> V3 a -> a -> V4 (V4 a)
transposeMatrix position axis angle =
  let rotQ = axisAngle axis angle
      rotM33 = fromQuaternion rotQ
      rotM33' = rotM33 !!* 0.5
  in transpose $ mkTransformationMat rotM33' position

execute :: IO ()
execute = (`runGS_` (wi, (0.0, 0.0))) $ do
  (program, shapeInfo, transP, cameraP) <- liftIO $ do
    vert <- createShader GL_VERTEX_SHADER "src/JSGL/Shader/Main.vert"
    frag <- createShader GL_FRAGMENT_SHADER "src/JSGL/Shader/Main.frag"
    program <- linkProgram vert frag

    gen <- getStdGen
    let pointsFloat = fromIntegral points
        row = 40 :: Int
        column = 40 :: Int
        irad = 0.2
        orad = 1.0
        points = (row + 1) * (column + 1)
    shapeInfo <- createVAO points $ do
      createVBO attribInfo {
        attribPointer = 0,
        dimension = 3,
        array = do
          i <- fromIntegral <$> [0..row] :: [GLfloat]
          let r = pi * 2.0 / (fromIntegral row) * i :: GLfloat
              rr = cos r
              ry = sin r
          j <- fromIntegral <$> [0..column] :: [GLfloat]
          let tr = pi * 2.0 / (fromIntegral column) * j
              tx = (rr * irad + orad) * cos tr
              ty = ry * irad
              tz = (rr * irad + orad) * sin tr
          [tx, ty, tz]
      }
      createVBO attribInfo {
        attribPointer = 1,
        dimension = 3,
        array = do
          i <- fromIntegral <$> [0..row] :: [GLfloat]
          let r = pi * 2.0 / (fromIntegral row) * i :: GLfloat
              rr = cos r
              ry = sin r
          j <- fromIntegral <$> [0..column] :: [GLfloat]
          let tr = pi * 2.0 / (fromIntegral column) * j
              rx = rr * cos tr
              rz = rr * sin tr
          [rx, ry, rz]
      }
      let indices = do
                    i <- [0..row-1]
                    j <- [0..column-1]
                    let r = (column + 1) * i + j
                    fromIntegral <$> [r, r + column + 1, r + 1, r + column + 1, r + column + 2, r + 1]
      createIBO indices

    transP <- malloc
    cameraP <- malloc

    glEnable GL_DEPTH_TEST
    glDepthFunc GL_LEQUAL
    return (program, shapeInfo, transP, cameraP)

  (transform, invTransform, camera) <- liftIO $ do
    transform <- newCString "transform"
    invTransform <- newCString "invTransform"
    camera <- newCString "camera"
    return (transform, invTransform, camera)
  window <- ask

  let gs = do
          shouldContinue <- liftIO $ do
            glClearColor 0.0 0.0 0.0 1.0
            glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
            glUseProgram program
            -- rotate

            timeValue <- maybe 0 realToFrac <$> GLFW.getTime

            forM_ [(0.0, 0.0, 0.0)] $ \(x, y, z) -> do
              let transformMatrix = transposeMatrix (V3 (x*0.5) (y*0.5) (z*0.5 :: GLfloat)) (V3 (0 :: GLfloat) (cos timeValue) (sin timeValue)) timeValue
                  cameraDirection = V3 x y z
              poke transP transformMatrix
              transformLoc <- glGetUniformLocation program transform
              glUniformMatrix4fv transformLoc 1 GL_FALSE (castPtr transP)

              poke transP . inv44 $ transformMatrix
              invTransformLoc <- glGetUniformLocation program invTransform
              glUniformMatrix4fv invTransformLoc 1 GL_FALSE (castPtr transP)

              poke cameraP cameraDirection
              cameraLoc <- glGetUniformLocation program camera
              glUniformMatrix4fv cameraLoc 1 GL_FALSE (castPtr transP)

              draw GL_TRIANGLES shapeInfo

            GLFW.swapBuffers window
            GLFW.pollEvents
            not <$> GLFW.windowShouldClose window
          when shouldContinue gs

  --loop window $ do
  gs
