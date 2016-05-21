module Main where

import Control.Monad
import qualified Data.Map as M
import System.IO
import System.Exit
import Parser (parseModel)
import Codegen

main = getContents >>= processModelFile

processModelFile file =
  case parseModel "" file of

    Left err -> do
      hPutStrLn stderr $ "Syntax error: " ++ show err
      exitWith (ExitFailure 1)

    Right model ->

      case generateDedan model of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ show err
          exitWith (ExitFailure 1)

        Right source -> putStrLn source
