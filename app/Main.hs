module Main where

import Control.Monad
import qualified Data.Map as M
import System.Environment
import Parser (parseModel)
import Codegen

main = getContents >>= processModelFile

processModelFile file =
  case parseModel "" file of

    Left err ->
      putStrLn $ "Syntax error: " ++ show err

    Right model ->

      case generateDedan model of
        Left err ->
            putStrLn $ "Error: " ++ show err

        Right source -> putStrLn source
