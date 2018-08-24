{-# LANGUAGE TemplateHaskell #-}

module JSGL.Utils.Debug (
  coloredLog,
  errorLog,
  assert,
  note,
  Color(..),
  glErr,
) where

import Graphics.GL.Core33
import System.Console.ANSI
import System.Exit

coloredLog :: Color -> IO () -> IO ()
coloredLog color log = do
  setSGR [SetColor Foreground Vivid color]
  log
  setSGR [Reset]

errorLog :: String -> IO ()
errorLog log = coloredLog Red $ putStrLn log

assert :: Bool -> String -> IO ()
assert ok str = if ok 
              then return () 
              else do
                  errorLog str
                  exitFailure

note :: Show a => a -> IO ()
note s = coloredLog Cyan $ print s

glErr :: IO ()
glErr = do
  glGetError >>= errorLog . show
