module Main where

import Control.Monad
import qualified Data.Map as M
import Imp
import ImpParser (parseModel)
import CompileProc

main = getContents >>= processModelFile

processModelFile file =
  case parseModel "" file of

    Left err ->
      putStrLn $ "Syntax error: " ++ show err

    Right Model{model_servers=servers, model_procs=procs} -> do

      forM_ servers $ \srv -> do
        putStrLn $ "server " ++ server_name srv
        case compileTransitions srv of
          Left err -> putStrLn $ "Error: " ++ show err
          Right transitions -> mapM_ print transitions

      forM_ procs $ \proc -> do
        putStrLn $ "process " ++ show proc
        putStrLn $ show (desugar $ process_stmt proc)
        let ((first, last), g) = compile $ desugar $ process_stmt proc
        forM_ (M.toList g) $ \(state, content) -> do
            putStr (if state == first then "*" else " ")
            print (state, content)
        putStrLn "---"
        case totallyCompileProcess (process_stmt proc) of
            Left err -> print err

            Right compiled -> do
                putStrLn $ "initial: {"
                    ++ message_server (cp_initialMessage compiled) ++ "."
                    ++ message_msg (cp_initialMessage compiled) ++ ", "
                    ++ cp_initialState compiled ++ "}"

                forM_ (cp_transitions compiled) $ \(state, in_msg, Message s m, outState) ->
                    putStrLn $ "{" ++ state ++ ", " ++ in_msg ++ "} -> {" ++ s ++ "." ++ m ++ ", " ++ outState ++ "}"
