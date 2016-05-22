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
    let env = M.empty
        
    typeEnv <- fmap M.fromList $ forM server_vars $ \(name, typeE) ->
        (,) name <$> evalType env typeE

    let encode = encodeState . M.toList
        syms = symbols typeEnv

        compileTransition :: Transition -> EM [ServerAction]
        compileTransition (Transition name pred maybeOutSignal assignment) = 
          let compileState state = do
                let env = M.union syms state

                updates <- forM assignment $ \(varName, expr) -> do
                    val <- evalExpr env expr
                    typ <- lookupType varName typeEnv

                    checkType typ val

                    pure (varName, val)

                pure ServerAction
                    { sa_inMessage = name
                    , sa_inState = encode state
                    , sa_outMessage = outSignal
                    , sa_outState = encode (M.union (M.fromList updates) state)
                    }

              outSignal = fromMaybe "ok" maybeOutSignal
                

          in matchingStates pred typeEnv >>= mapM compileState

    actions <- concat <$> mapM compileTransition server_transitions

    pure CompiledServer
        { cs_name = server_name
        , cs_states = map (encodeState . M.toList) (allStates $ M.toList typeEnv)
        , cs_services = nub (map t_name server_transitions)
        , cs_actions = actions
        }

typeValues :: Type -> [Value]
typeValues (Enum values) = map Sym values
typeValues (Range from to) = map Int [from..to]
typeValues (Array elemType size) = map Arr $ traverse typeValues (replicate (fromIntegral size) elemType)

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
symbols = M.fromList . concatMap varSyms . M.elems
  where
    varSyms (Enum syms) = map (\sym -> (sym, Sym sym)) syms
    varSyms (Range _ _) = []
    varSyms (Array typ _) = varSyms typ

matchingStates :: Predicate -> TypeEnv -> EM [Env]
matchingStates pred types =
    let syms = symbols types
        matches env = evalPredicate (env `M.union` syms) pred
    in filterM matches (allStates $ M.toList types)



checkType :: Type -> Value -> EM ()
checkType typ val
    | inRange typ val = pure ()
    | otherwise       = err $ TypeMismatch (ppType typ) (ppValue val)
 where
    ppType (Enum vals) = "{" ++ intercalate ", " vals ++ "}"
    ppType (Range from to) = show from ++ ".." ++ show to
    ppType (Array typ size) = "(" ++ ppType typ ++ ")[" ++ show size ++ "]"

lookupType :: Symbol -> TypeEnv -> EM Type
lookupType var env =
    case M.lookup var env of
        Just typ -> pure typ
        Nothing -> err (UndefinedSymbol var)
