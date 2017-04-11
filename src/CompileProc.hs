module CompileProc where

import Control.Monad
import Control.Arrow (first, second)
import Data.Maybe
import qualified Control.Monad.State as MS
import qualified Data.Map as M
import qualified Data.Set as S
import AST hiding (Statement(..))
import qualified AST
import Err
import Eval

data CompiledProc = CompiledProc
    { cp_name :: ProcessName
    , cp_states :: [Symbol]
    , cp_services :: [Symbol]
    , cp_usedServersInstances :: [ServerInstanceName]
    , cp_initialState :: Symbol
    , cp_initialMessage :: Message
    , cp_transitions :: [(Symbol, Symbol, Maybe (Symbol, Symbol), Symbol)]
    } deriving (Show)

compileProcess :: Env -> AST.Process -> EM CompiledProc
compileProcess globalEnv (Process name stmt) = withContext ("in process " ++ show name) $ do
    let ((initial, final), cfg) = compile $ desugar stmt

        encodeT (T state input C_Terminate) =
            pure (encodeStateId cfg state, input, Nothing, encodeStateId cfg state)

        encodeT (T state input (C_Send (nextState, Message{..}))) = do
            paramValues <- mapM (evalExpr globalEnv) message_params
            pure (encodeStateId cfg state, input,
                  Just (message_server, encodeMessage message_msg paramValues),
                  encodeStateId cfg nextState)

    ts <- transitions cfg
    encodedTs <- mapM encodeT ts
    cont <- lookupCont cfg initial
    (initialState, initialMessage) <-
      case cont of
        C_Send msg -> pure msg
        C_Terminate -> err EmptyProcess

    return CompiledProc
        { cp_name = name
        , cp_states = dedup $ map (encodeStateId cfg . t_state) ts
        , cp_services = dedup $ map t_in_message ts
        , cp_usedServersInstances = dedup $ message_server initialMessage : mapMaybe (fmap message_server . contMessage . t_cont) ts
        , cp_initialState = encodeStateId cfg initialState
        , cp_initialMessage = initialMessage
        , cp_transitions = encodedTs
        }
  where
    dedup = S.toList . S.fromList

data SimpleStmt =
      Match Message [(Symbol, SimpleStmt)]
    | Loop SimpleStmt
    | Seq SimpleStmt SimpleStmt
    | Skip
      deriving (Show)

desugar :: AST.Statement -> SimpleStmt
desugar AST.Skip = Skip
desugar (AST.Loop body) = Loop (desugar body)
desugar (AST.Msg m) = desugar (AST.Match m [("ok", AST.Skip)])
desugar (AST.Block stmts) = foldr sseq Skip (map desugar stmts)
  where
    sseq Skip x = x
    sseq x Skip = x
    sseq x y    = Seq x y
desugar (AST.Match m a) = Match m (map (second desugar) a)


type CFG = M.Map StateId StateDesc

type StateId = Int
data StateDesc = Terminate | Goto StateId | Send Message [(Symbol, StateId)] deriving (Show)
type CompileM = MS.State (StateId, CFG)

newStateId :: CompileM StateId
newStateId = do
    id <- MS.gets fst
    MS.modify $ first (+1)
    return id

insertState :: StateId -> StateDesc -> CompileM ()
insertState k v = MS.modify $ second $ M.insert k v

compile :: SimpleStmt -> ((StateId, StateId), CFG)
compile stmt = second snd $ MS.runState go (0, M.empty)
  where
    go = do
      (initial, final) <- compile' stmt
      insertState final Terminate
      pure (initial, final)

compile' :: SimpleStmt -> CompileM (StateId, StateId)
compile' Skip = do
    id <- newStateId
    return (id, id)

compile' (Seq x y) = do
    (x1, x2) <- compile' x
    (y1, y2) <- compile' y
    insertState x2 (Goto y1)
    return (x1, y2)

compile' (Loop body) = do
    (start, end) <- compile' body
    insertState end (Goto start)

    dummy <- newStateId
    return (start, dummy)

compile' (Match msg actions) = do
    begin <- newStateId
    end <- newStateId

    compiledActions <- forM actions $ \(reply, stmt) -> do
        (x1, x2) <- compile' stmt
        insertState x2 (Goto end)

        return (reply, x1)
    
    insertState begin (Send msg compiledActions)

    return (begin, end)


data Cont = C_Send (StateId, Message) | C_Terminate deriving (Show)
data T = T { t_state :: StateId, t_in_message :: Symbol, t_cont :: Cont } deriving (Show)

contMessage :: Cont -> Maybe Message
contMessage (C_Send (_, msg)) = Just msg
contMessage C_Terminate = Nothing

lookupCont :: CFG -> StateId -> EM Cont
lookupCont g = go S.empty
  where
    go visited stateId
        | S.member stateId visited = Left ErrCycle
        | otherwise =
            case M.lookup stateId g of
                Nothing -> error ("panic: state not found: " ++ show stateId ++ " in " ++ show g)
                Just (Goto next) -> go (S.insert stateId visited) next
                Just Terminate -> Right C_Terminate
                Just (Send msg _) -> Right (C_Send (stateId, msg))

transitions :: CFG -> EM [T]
transitions g = concat <$> mapM toTransitions (M.toList g)
  where
    toTransitions (_, (Goto _)) = pure []
    toTransitions (_, Terminate) = pure []
    toTransitions (state, (Send _ actions)) =
        mapM (\(reply, nextState) -> T state reply <$> lookupCont g nextState) actions


encodeStateId :: CFG -> StateId -> Symbol
encodeStateId cfg stateId =
    case lookupCont cfg stateId of
        Right (C_Send (_, Message s m _)) -> "s" ++ show stateId ++ "_" ++ s ++ "_" ++ m
        Right C_Terminate -> "s" ++ show stateId ++ "_terminate"
        Left _ -> "s" ++ show stateId
