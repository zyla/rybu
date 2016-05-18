{-# LANGUAGE RecordWildCards #-}
module Codegen where

import Control.Monad.Writer
import qualified Data.Map as M
import Data.List (intercalate, nub)

import Imp
import CompileProc

generateDedan :: Model -> String
generateDedan Model{..} = execWriter $ do
    let procNames = map process_name model_procs

    forM_ model_servers $ \server@Server{..} -> do
        tells ["server: ", server_name, " (servers "]
        tell $ intercalate ", " (map procServerName procNames)
        tell "; agents "
        tell $ intercalate ", " (map procAgentName procNames)
        tellLn "),"

        tell "services {"
        tell $ intercalate ", " (nub $ map t_name server_transitions)
        tellLn "},"

        tell "states {"
        tell $ intercalate ", " (map (encodeState . M.toList) $ allStates $ M.toList server_vars)
        tellLn "},"

        tellLn "actions {"

        let Right ts = compileTransitions server

        forM_ procNames $ \procName ->
            forM_ ts $ \(in_msg, in_state, out_msg, out_state) ->
                let agent = procAgentName procName
                    server = procServerName procName
                in tells ["  {", agent, ".", server_name, ".", in_msg, ", ", server_name, ".", in_state, "} -> {"
                         , agent, ".", server, ".", out_msg, ", ", server_name, ".", out_state, "},\n"]

        tellLn "};"

procServerName = ("S_"++)
procAgentName = ("A_"++)

tells :: [String] -> Writer String ()
tells = tell . concat

tellLn :: String -> Writer String ()
tellLn str = tell str >> tell "\n"
