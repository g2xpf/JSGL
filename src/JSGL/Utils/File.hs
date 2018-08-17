module JSGL.Utils.File (
  fetch 
) where

import JSGL.Utils.Debug
import Control.Exception
import System.Exit

fetch :: String -> IO String
fetch filename = do
  mayErr <- try $ readFile filename :: IO (Either IOError String)
  case mayErr of
    Right contents -> return contents
    Left err -> do
      coloredLog Red $ print err
      exitFailure


