module CompileProc where

import Control.Monad
import Control.Arrow (first, second)
import qualified Control.Monad.State as MS
import qualified Data.Map as M
import qualified Data.Set as S
import AST hiding (Statement(..))
import qualified AST
import Err
import Eval

data CompiledProc = CompiledProc
    { cp_name :: Symbol
    , cp_states :: [Symbol]
    , cp_services :: [Symbol]
    , cp_usedServers :: [Symbol]
    , cp_initialState :: Symbol
    , cp_initialMessage :: Message
    , cp_transitions :: [(Symbol, Symbol, Symbol, Symbol, Symbol)]
    } deriving (Show)

compileProcess :: AST.Process -> EM CompiledProc
compileProcess (Process name stmt) = do
    let ((initial, final), cfg) = compile $ desugar stmt

        encodeT (T state input (nextState, Message{..})) = do
            paramValues <- mapM (evalExpr M.empty) message_params
            pure (encodeStateId cfg state, input,
                  message_server, encodeMessage message_msg paramValues,
                  encodeStateId cfg nextState)

    ts <- transitions cfg
    encodedTs <- mapM encodeT ts
    (initialState, initialMessage) <- lookupCont cfg initial

    return CompiledProc
        { cp_name = name
        , cp_states = dedup $ map (encodeStateId cfg . t_state) ts
        , cp_services = dedup $ map t_in_message ts
        , cp_usedServers = dedup $ map (message_server . snd . t_cont) ts
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
data StateDesc = Goto StateId | Send Message [(Symbol, StateId)] deriving (Show)
type CompileM = MS.State (StateId, CFG)

newStateId :: CompileM StateId
newStateId = do
    id <- MS.gets fst
    MS.modify $ first (+1)
    return id

insertState :: StateId -> StateDesc -> CompileM ()
insertState k v = MS.modify $ second $ M.insert k v

compile :: SimpleStmt -> ((StateId, StateId), CFG)
compile stmt = second snd $ MS.runState (compile' stmt) (0, M.empty)

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


type Cont = (StateId, Message)
data T = T { t_state :: StateId, t_in_message :: Symbol, t_cont :: Cont } deriving (Show)

lookupCont :: CFG -> StateId -> EM Cont
lookupCont g = go S.empty
  where
    go visited stateId
        | S.member stateId visited = Left ErrCycle
        | otherwise =
            case M.lookup stateId g of
                Nothing -> error ("panic: state not found: " ++ show stateId ++ " in " ++ show g)
                Just (Goto next) -> go (S.insert stateId visited) next
                Just (Send msg _) -> Right (stateId, msg)

transitions :: CFG -> EM [T]
transitions g = concat <$> mapM toTransitions (M.toList g)
  where
    toTransitions (_, (Goto _)) = pure []
    toTransitions (state, (Send _ actions)) =
        mapM (\(reply, nextState) -> T state reply <$> lookupCont g nextState) actions


encodeStateId :: CFG -> StateId -> Symbol
encodeStateId cfg stateId =
    case lookupCont cfg stateId of
        Right (_, Message s m _) -> "s" ++ show stateId ++ "_" ++ s ++ "_" ++ m
        Left _ -> "s" ++ show stateId
