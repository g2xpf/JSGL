{-# LANGUAGE TemplateHaskell #-}

module JSGL.Utils.Debug (
  coloredLog,
  Color(..),
) where

import System.Console.ANSI

coloredLog :: Color -> IO () -> IO ()
coloredLog color log = do
  setSGR [SetColor Foreground Vivid color]
  log
  setSGR [Reset]
