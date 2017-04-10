module Codegen where

import Control.Monad.Writer
import qualified Control.Monad.State as MS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate, nub, sortBy)

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
        compileServer' server = do
          let states =
                   map si_initialState
                 $ filter (\si -> si_serverType si == server_name server)
                 $ model_serverInstances
          initialStates <- (traverse . traverse . traverse) (evalExpr globalEnv) states
          compileServer globalEnv serversUsage server initialStates

    compiledServers <- lift $ mapM compileServer' model_servers
    lift $ checkInstancesInitialization globalEnv compiledServers model_serverInstances
    let serverInstances = sortInstancesInitializers model_serverInstances

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
            (map siFormalParam serverInstances)
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
    tell $ intercalate ", " (map procServerName procNames ++ map siFormalParam serverInstances)
    tellLn ";\n"

    tellLn "init -> {"
    forM_ serverInstances $ \(ServerInstance name _ initEnvExpr) -> do
        tells ["  ", name, "("]
        tell $ intercalate "," (map procServerName procNames ++ map procAgentName procNames)
        initEnv <- lift $ withContext ("in initializer of server instance " ++ show name) $
            (traverse . traverseSecond) (evalExpr globalEnv) initEnvExpr

        tells [").", encodeState initEnv, ",\n"]

    forM_ compiledProcs $ \CompiledProc{..} -> do
        tells ["  ", procServerName cp_name, "("]
        tell $ intercalate "," (map si_name serverInstances ++ [procAgentName cp_name])
        tells [").", cp_initialState, ",\n"]

        params <- lift $ mapM (evalExpr globalEnv) (message_params cp_initialMessage)
        let encodedMsg = encodeMessage (message_msg cp_initialMessage) params
        tells ["  ", procAgentName cp_name, ".", message_server cp_initialMessage, ".", encodedMsg, ",\n"]

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
serversUsageFromProcs instances procs = M.map S.toList $ MS.execState (forM_ procs collectServers) M.empty
    where
        collectServers :: CompiledProc -> MS.State (M.Map ServerName (S.Set ProcessName)) ()
        collectServers CompiledProc{..} =
            forM_ cp_usedServersInstances $ addServer cp_name . serverNameFromInstance

        addServer :: ProcessName -> ServerName -> MS.State (M.Map ServerName (S.Set ProcessName)) ()
        addServer procN serverN = MS.modify $ M.insertWith S.union serverN (S.singleton procN)

        serverNameFromInstance :: ServerInstanceName -> ServerName
        serverNameFromInstance name =
            maybe "" id $ si_serverType <$> findServerInstance instances name

        findServerInstance :: [ServerInstance] -> ServerInstanceName -> Maybe ServerInstance
        findServerInstance [] name = Nothing
        findServerInstance (i:is) name = if (si_name i) == name
                                            then Just i
                                            else findServerInstance is name

checkInstancesInitialization :: Env -> [CompiledServer] -> [ServerInstance] -> EM ()
checkInstancesInitialization env servers instances = forM_ instances $ checkInstance env servers
    where
        checkInstance :: Env -> [CompiledServer] -> ServerInstance -> EM ()
        checkInstance env servers ServerInstance{..} =
            withContext ("in initializer of server instance " ++ show si_name) $ do
                CompiledServer{..} <- lookupServer si_serverType servers
                withContext "in variable initializers" $ do
                    checkVars cs_vars si_initialState
                    checkTypes env cs_vars si_initialState

        lookupServer :: ServerName -> [CompiledServer] -> EM CompiledServer
        lookupServer name [] = err (UndefinedSymbol name)
        lookupServer name (cs:css) = if cs_name cs == name
                                    then pure cs
                                    else lookupServer name css

        checkVars :: [(Symbol, Type)] -> [(Symbol, Expr)] -> EM ()
        checkVars variables initializers =
            let vars = S.fromList $ map fst variables
                inits = S.fromList $ map fst initializers
                inits_vars = S.difference inits vars
                vars_inits = S.difference vars inits
            in if inits_vars /= S.empty
                then err $ UndefinedSymbol $ head $ S.toList inits_vars
                else if vars_inits /= S.empty
                    then err $ UninitializedVariable $ head $ S.toList vars_inits
                    else pure () -- check ok

        checkTypes :: Env -> [(Symbol, Type)] -> [(Symbol, Expr)] -> EM ()
        checkTypes env variables initializers =
            let vars = M.fromList variables
                inits = M.fromList initializers
            in void $ M.traverseWithKey (checkVarType env inits) vars

        checkVarType :: Env -> M.Map Symbol Expr -> Symbol -> Type -> EM ()
        checkVarType env inits s t =
            withContext ("in intilializer for variable " ++ show s) $ do
                let expr = inits M.! s
                value <- evalExpr env expr
                if inRange t value
                    then pure ()
                    else err $ TypeMismatch (show t) (show value)

sortInstancesInitializers :: [ServerInstance] -> [ServerInstance]
sortInstancesInitializers instances = sortInstanceInits <$> instances
    where
        sortInstanceInits :: ServerInstance -> ServerInstance
        sortInstanceInits si@(ServerInstance _ _ initialState) = si { si_initialState = sortBy (\(a, _) (b, _) -> compare a b) initialState }
