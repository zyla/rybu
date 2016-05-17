module CompileProc where

import Control.Monad
import Control.Arrow (first, second)
import qualified Control.Monad.State as MS
import qualified Data.Map as M
import qualified Data.Set as S
import Imp

data SimpleStmt = SMatch Message [(Symbol, SimpleStmt)] | SLoop SimpleStmt | SSeq SimpleStmt SimpleStmt | SSkip
  deriving (Show)

desugar :: Statement -> SimpleStmt
desugar Skip = SSkip
desugar (Loop body) = SLoop (desugar body)
desugar (Msg m) = desugar (Match m [("ok", Skip)])
desugar (Block stmts) = foldr sseq SSkip (map desugar stmts)
  where
    sseq SSkip x = x
    sseq x SSkip = x
    sseq x y    = SSeq x y
desugar (Match m a) = SMatch m (map (second desugar) a)


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
compile' SSkip = do
    id <- newStateId
    return (id, id)

compile' (SSeq x y) = do
    (x1, x2) <- compile' x
    (y1, y2) <- compile' y
    insertState x2 (Goto y1)
    return (x1, y2)

compile' (SLoop body) = do
    (start, end) <- compile' body
    insertState end (Goto start)

    dummy <- newStateId
    return (start, dummy)

compile' (SMatch msg actions) = do
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

data CompileErr = ErrCycle (S.Set StateId) deriving (Show)

lookupCont :: CFG -> StateId -> Either CompileErr Cont
lookupCont g = go S.empty
  where
    go visited stateId
        | S.member stateId visited = Left (ErrCycle visited)
        | otherwise =
            case M.lookup stateId g of
                Nothing -> error ("panic: state not found: " ++ show stateId ++ " in " ++ show g)
                Just (Goto next) -> go (S.insert stateId visited) next
                Just (Send msg _) -> Right (stateId, msg)

transitions :: CFG -> Either CompileErr [T]
transitions g = concat <$> mapM toTransitions (M.toList g)
  where
    toTransitions (_, (Goto _)) = pure []
    toTransitions (state, (Send _ actions)) =
        mapM (\(reply, nextState) -> T state reply <$> lookupCont g nextState) actions


encodeStateId :: StateId -> Symbol
encodeStateId = ('s':) . show

data CompiledProc = CompiledProc
    { cp_states :: [Symbol]
    , cp_services :: [Symbol]
    , cp_usedServers :: [Symbol]
    , cp_initialState :: Symbol
    , cp_initialMessage :: Message
    , cp_transitions :: [(Symbol, Symbol, Message, Symbol)]
    } deriving (Show)


totallyCompileProcess :: Statement -> Either CompileErr CompiledProc
totallyCompileProcess stmt = do
    let ((initial, final), cfg) = compile $ desugar stmt
    ts <- transitions cfg
    (initialState, initialMessage) <- lookupCont cfg initial

    return CompiledProc
        { cp_states = dedup $ map (encodeStateId . t_state) ts
        , cp_services = dedup $ map t_in_message ts
        , cp_usedServers = dedup $ map (message_server . snd . t_cont) ts
        , cp_initialState = encodeStateId initialState
        , cp_initialMessage = initialMessage
        , cp_transitions = map encodeT ts
        }
  where
    dedup = S.toList . S.fromList
    encodeT (T state input (nextState, message)) = (encodeStateId state, input, message, encodeStateId nextState)
