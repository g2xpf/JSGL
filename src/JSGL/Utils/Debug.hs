{-# LANGUAGE TemplateHaskell #-}

module JSGL.Utils.Debug (
  -- logging utilities
  note,
  warn,
  err,
  glErr,

  -- log and exit
  assert,
) where

import Graphics.GL.Core33 (glGetError)
import System.Console.ANSI
import System.Exit

coloredLog :: Color -> IO () -> IO ()
coloredLog color log = do
  setSGR [SetColor Foreground Vivid color]
  log
  setSGR [Reset]

err :: Show a => a -> IO ()
err log = coloredLog Red $ print log

assert :: Show a => Bool -> a -> IO ()
assert ok s = if ok 
                then return () 
                else do
                  err s 
                  exitFailure

warn :: Show a => Bool -> a -> IO ()
warn ok s = if ok
              then return ()
              else coloredLog Yellow $ print s

note :: Show a => a -> IO ()
note s = coloredLog Cyan $ print s

glErr :: IO ()
glErr = glGetError >>= err . show
