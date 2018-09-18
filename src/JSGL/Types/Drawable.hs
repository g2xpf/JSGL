{-# LANGUAGE FlexibleInstances #-}

module JSGL.Types.Drawable (
  Drawable(..)
)where

import JSGL.Types.Type
import Graphics.GL.Core33
import Foreign.Ptr

class Drawable a where
  draw :: DrawType -> a -> IO ()

instance Drawable ShapeObject where
  draw drawType obj = do
    glUseProgram $ program obj
    draw drawType (shapeInfo obj)
    
instance Drawable ShapeInfo where
  draw drawType info = do
    glBindVertexArray $ vao info
    let points' = fromIntegral $ points info
    if hasIndex info
      then glDrawElements drawType points' GL_UNSIGNED_INT nullPtr
      else glDrawArrays drawType 0 points'
 
instance Drawable a => Drawable [a] where
  draw drawType = mapM_ $ draw drawType

instance (Drawable a, Drawable b) => Drawable (a, b) where
  draw drawType (a, b) = do
    draw drawType a
    draw drawType b

instance Drawable a => Drawable (Maybe a) where
  draw drawType maybeDrawable = case maybeDrawable of
                                  Just a -> draw drawType a
                                  Nothing -> return ()

instance Drawable a => Drawable (IO a) where
  draw drawType = (>>= draw drawType)

instance (Drawable a, Drawable b, Drawable c) => Drawable (a, b, c) where
  draw drawType (a, b, c) = do
    draw drawType a
    draw drawType b
    draw drawType c
  
instance (Drawable a, Drawable b, Drawable c, Drawable d) => Drawable (a, b, c, d) where
  draw drawType (a, b, c, d) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d

instance (Drawable a, Drawable b, Drawable c, Drawable d, Drawable e) => Drawable (a, b, c, d, e) where
  draw drawType (a, b, c, d, e) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d
    draw drawType e

instance (Drawable a, Drawable b, Drawable c, Drawable d, Drawable e, Drawable f) => Drawable (a, b, c, d, e, f) where
  draw drawType (a, b, c, d, e, f) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d
    draw drawType e
    draw drawType f

instance (Drawable a, Drawable b, Drawable c, Drawable d, Drawable e, Drawable f, Drawable g) => Drawable (a, b, c, d, e, f, g) where
  draw drawType (a, b, c, d, e, f, g) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d
    draw drawType e
    draw drawType f
    draw drawType g

instance (Drawable a, Drawable b, Drawable c, Drawable d, Drawable e, Drawable f, Drawable g, Drawable h) => Drawable (a, b, c, d, e, f, g, h) where
  draw drawType (a, b, c, d, e, f, g, h) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d
    draw drawType e
    draw drawType f
    draw drawType g
    draw drawType h

instance (Drawable a, Drawable b, Drawable c, Drawable d, Drawable e, Drawable f, Drawable g, Drawable h, Drawable i) => Drawable (a, b, c, d, e, f, g, h, i) where
  draw drawType (a, b, c, d, e, f, g, h, i) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d
    draw drawType e
    draw drawType f
    draw drawType g
    draw drawType h
    draw drawType i

instance (Drawable a, Drawable b, Drawable c, Drawable d, Drawable e, Drawable f, Drawable g, Drawable h, Drawable i, Drawable j) => Drawable (a, b, c, d, e, f, g, h, i, j) where
  draw drawType (a, b, c, d, e, f, g, h, i, j) = do
    draw drawType a
    draw drawType b
    draw drawType c
    draw drawType d
    draw drawType e
    draw drawType f
    draw drawType g
    draw drawType h
    draw drawType i
    draw drawType j

