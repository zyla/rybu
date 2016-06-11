module Codegen where

import Control.Monad.Writer
import qualified Control.Monad.State as MS
import qualified Data.Map as M
import Data.List (intercalate, nub)

import AST
import Err
import CompileProc
import CompileServer
import Eval

generateDedan :: Model -> EM String
generateDedan Model{..} = execWriterT $ do
    let procNames = map process_name model_procs

    let evalConst env (name, expr) = do
            val <- evalExpr env expr
            pure (M.insert name val env)

    globalEnv <- lift $ foldM evalConst M.empty model_constants

    compiledProcs <- lift $ mapM (compileProcess globalEnv) model_procs
    let serversUsage = serversUsageFromProcs model_serverInstances compiledProcs
    compiledServers <- lift $ mapM (compileServer globalEnv serversUsage) model_servers

    forM_ compiledServers $ \server@CompiledServer{..} -> do
        serverHeader cs_name
            (map procServerName procNames)
            (map procAgentName procNames)
            cs_services
            cs_states

        tellLn "actions {"

        -- TODO(hator): warn if cs_usedBy is 0, means server is not used by any process
        forM_ cs_usedBy $ \procName ->
            forM_ cs_actions $ \ServerAction
                    {sa_inMessage=in_msg, sa_inState=in_state, sa_outMessage=out_msg, sa_outState=out_state} ->
                let agent = procAgentName procName
                    server = procServerName procName
                in tells ["  {", agent, ".", cs_name, ".", in_msg, ", ", cs_name, ".", in_state, "} -> {"
                         , agent, ".", server, ".", out_msg, ", ", cs_name, ".", out_state, "},\n"]

        tellLn "};\n"

    forM_ compiledProcs $ \CompiledProc{..} -> do
        let agent = procAgentName cp_name
            server = procServerName cp_name

        serverHeader server
            (map siFormalParam model_serverInstances)
            [agent]
            cp_services
            cp_states

        tellLn "actions {"

        forM_ cp_transitions $ \(in_state, in_msg, out_server, out_msg, out_state) ->
            tells ["  {", agent, ".", server, ".", in_msg, ", ", server, ".", in_state, "} -> {"
                  , agent, ".", out_server, ".", out_msg, ", ", server, ".", out_state, "},\n"]

        tellLn "};\n"


    tell "agents "
    tell $ intercalate ", " (map procAgentName procNames)
    tellLn ";\n"

    tell "servers "
    tell $ intercalate ", " (map procServerName procNames ++ map siFormalParam model_serverInstances)
    tellLn ";\n"

    tellLn "init -> {"
    forM_ model_serverInstances $ \(ServerInstance name _ initEnvExpr) -> do
        tells ["  ", name, "("]
        tell $ intercalate "," (map procServerName procNames ++ map procAgentName procNames)
        initEnv <- lift $ withContext ("in initializer of server instance " ++ show name) $
            (traverse . traverseSecond) (evalExpr globalEnv) initEnvExpr

        tells [").", encodeState initEnv, ",\n"]

    forM_ compiledProcs $ \CompiledProc{..} -> do
        tells ["  ", procServerName cp_name, "("]
        tell $ intercalate "," (map si_name model_serverInstances ++ [procAgentName cp_name])
        tells [").", cp_initialState, ",\n"]

        tells ["  ", procAgentName cp_name, ".", message_server cp_initialMessage, ".", message_msg cp_initialMessage, ",\n"]

    tellLn "}."

traverseSecond inj (a, b) = (,) a <$> inj b

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

serversUsageFromProcs :: [ServerInstance] -> [CompiledProc] -> ServersUsage
serversUsageFromProcs instances procs = MS.execState (forM_ procs collectServers) M.empty
    where
        collectServers :: CompiledProc -> MS.State ServersUsage ()
        collectServers CompiledProc{..} = forM_ cp_usedServersInstances $ addServer cp_name . serverNameFromInstance

        addServer :: ProcessName -> ServerName -> MS.State ServersUsage ()
        addServer procN serverN = MS.modify $ M.insertWith (++) serverN [procN]

        serverNameFromInstance :: ServerInstanceName -> ServerName
        serverNameFromInstance name =
            maybe "" id $ si_serverType <$> findServerInstance instances name

        findServerInstance :: [ServerInstance] -> ServerInstanceName -> Maybe ServerInstance
        findServerInstance [] name = Nothing
        findServerInstance (i:is) name = if (si_name i) == name
                                            then Just i
                                            else findServerInstance is name
