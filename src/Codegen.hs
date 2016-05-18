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
        Right compiledProcs = mapM (\(Process name stmt) -> (,) name <$> totallyCompileProcess stmt) model_procs

    forM_ model_servers $ \server@Server{..} -> do
        serverHeader server_name
            (map procServerName procNames)
            (map procAgentName procNames)
            (nub $ map t_name server_transitions)
            (map (encodeState . M.toList) $ allStates $ M.toList server_vars)

        tellLn "actions {"

        let Right ts = compileTransitions server

        forM_ procNames $ \procName ->
            forM_ ts $ \(in_msg, in_state, out_msg, out_state) ->
                let agent = procAgentName procName
                    server = procServerName procName
                in tells ["  {", agent, ".", server_name, ".", in_msg, ", ", server_name, ".", in_state, "} -> {"
                         , agent, ".", server, ".", out_msg, ", ", server_name, ".", out_state, "},\n"]

        tellLn "};\n"

    forM_ compiledProcs $ \(name, CompiledProc{..}) -> do
        serverHeader (procServerName name)
            (map siFormalParam model_serverInstances)
            [procAgentName name]
            cp_services
            cp_states
        tellLn "actions {"

        forM_ cp_transitions $ \(in_state, in_msg, Message out_server out_msg, out_state) ->
            let agent = procAgentName name
                server = procServerName name
            in tells ["  {", agent, ".", server, ".", in_msg, ", ", server, ".", in_state, "} -> {"
                     , agent, ".", out_server, ".", out_msg, ", ", server, ".", out_state, "},\n"]

        tellLn "};\n"

siFormalParam ServerInstance{si_name=name, si_serverType=typ} = name ++ ": " ++ typ

serverHeader serverName servers agents services states = do
    tells ["server: ", serverName, " (servers "]
    tell $ intercalate ", " servers
    tell "; agents "
    tell $ intercalate ", " agents
    tellLn "),"

    tell "services {"
    tell $ intercalate ", " services
    tellLn "},"

    tell "states {"
    tell $ intercalate ", " states
    tellLn "},"

procServerName = ("S_"++)
procAgentName = ("A_"++)

tells :: [String] -> Writer String ()
tells = tell . concat

tellLn :: String -> Writer String ()
tellLn str = tell str >> tell "\n"
