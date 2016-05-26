module Main where

import Control.Monad
import qualified Data.Map as M
import System.IO
import System.Exit
import qualified Rybu

main = getContents >>= processModelFile

processModelFile file =
  case Rybu.run file of

    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)

    Right output ->
        putStr output
