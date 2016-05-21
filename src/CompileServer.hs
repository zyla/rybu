module CompileServer where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.List (intercalate, nub)
import qualified Data.Set as S
import qualified Data.Map as M

import AST
import Err
import Eval

data CompiledServer = CompiledServer
    { cs_name :: Symbol
    , cs_states :: [Symbol]
    , cs_services :: [Symbol]
    , cs_actions :: [ServerAction]
    } deriving (Eq, Show)

data ServerAction = ServerAction
    { sa_inMessage :: Symbol
    , sa_inState :: Symbol
    , sa_outMessage :: Symbol
    , sa_outState :: Symbol
    } deriving (Eq, Show)

compileServer :: Server -> EM CompiledServer
compileServer server@Server{..} = do
    actions <- compileTransitions server

    pure CompiledServer
        { cs_name = server_name
        , cs_states = map (encodeState . M.toList) (allStates server_vars)
        , cs_services = nub (map t_name server_transitions)
        , cs_actions = actions
        }

encodeValue :: Value -> String
encodeValue (Sym s) = s
encodeValue (Int i) = show i

typeValues :: Type -> [Value]
typeValues (Enum values) = map Sym values
typeValues (Range from to) = map Int [from..to]

encodeState :: [(Symbol, Value)] -> Symbol
encodeState = intercalate "_" . map (\(name, val) -> name ++ "_" ++ encodeValue val)

allAssignments :: [Type] -> [[Value]]
allAssignments = sequence . map typeValues

allStates :: [(Symbol, Type)] -> [Env]
allStates vars =
    let names = map fst vars
        types = map snd vars
    in map (M.fromList . zip names) (allAssignments types)

symbols :: TypeEnv -> Env
symbols = M.fromList . concatMap varSyms . M.toList
  where
    varSyms (name, Enum syms) = map (\sym -> (sym, Sym sym)) syms
    varSyms (name, Range _ _) = []

matchingStates :: Predicate -> TypeEnv -> EM [Env]
matchingStates pred types =
    let syms = symbols types
        matches env = evalPredicate (env `M.union` syms) pred
    in filterM matches (allStates $ M.toList types)


compileTransitions :: Server -> EM [ServerAction]
compileTransitions Server {server_vars=vars, server_transitions=transitions} =
    concat <$> mapM compileTransition transitions

  where
    encode = encodeState . M.toList
    syms = symbols (M.fromList vars)

    compileTransition :: Transition -> EM [ServerAction]
    compileTransition (Transition name pred maybeOutSignal assignment) = 
      let compileState state = do
            let env = M.union syms state
            updates <- (traverse . traverse) (evalExpr env) assignment
            pure ServerAction
                { sa_inMessage = name
                , sa_inState = encode state
                , sa_outMessage = outSignal
                , sa_outState = encode (M.union (M.fromList updates) state)
                }

          outSignal = fromMaybe "ok" maybeOutSignal
            

      in matchingStates pred (M.fromList vars) >>= mapM compileState
