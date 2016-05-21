module Codegen where

import Control.Monad.Writer
import qualified Data.Map as M
import Data.List (intercalate, nub)

import AST
import Err
import CompileProc
import CompileServer

generateDedan :: Model -> EM String
generateDedan Model{..} = execWriterT $ do
    let procNames = map process_name model_procs

    compiledProcs <- lift $ mapM (\(Process name stmt) -> (,) name <$> compileProcess stmt) model_procs

    forM_ model_servers $ \server@Server{..} -> do
        serverHeader server_name
            (map procServerName procNames)
            (map procAgentName procNames)
            (nub $ map t_name server_transitions)
            (map (encodeState . M.toList) $ allStates server_vars)

        tellLn "actions {"

        ts <- lift $ compileTransitions server

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


    tell "agents "
    tell $ intercalate ", " (map procAgentName procNames)
    tellLn ";\n"

    tell "servers "
    tell $ intercalate ", " (map procServerName procNames ++ map siFormalParam model_serverInstances)
    tellLn ";\n"

    tellLn "init -> {"
    forM_ model_serverInstances $ \(ServerInstance name _ initEnv) -> do
        tells ["  ", name, "("]
        tell $ intercalate "," (map procServerName procNames ++ map procAgentName procNames)
        tells [").", encodeState initEnv, ",\n"]

    forM_ compiledProcs $ \(name, CompiledProc{..}) -> do
        tells ["  ", procServerName name, "("]
        tell $ intercalate "," (map si_name model_serverInstances ++ [procAgentName name])
        tells [").", cp_initialState, ",\n"]

        tells ["  ", procAgentName name, ".", message_server cp_initialMessage, ".", message_msg cp_initialMessage, ",\n"]

    tellLn "}."

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

tells :: Monad m => [String] -> WriterT String m ()
tells = tell . concat

tellLn :: Monad m => String -> WriterT String m ()
tellLn str = tell str >> tell "\n"
