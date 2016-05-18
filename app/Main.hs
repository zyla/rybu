module Main where

import Control.Monad
import qualified Data.Map as M
import Imp
import ImpParser (parseModel)
import CompileProc
import Codegen

main = getContents >>= processModelFile

processModelFile file =
  case parseModel "" file of

    Left err ->
      putStrLn $ "Syntax error: " ++ show err

    Right model@Model{model_servers=servers, model_serverInstances=instances, model_procs=procs} -> do

      putStrLn $ generateDedan model
