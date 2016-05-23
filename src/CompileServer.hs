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

compileServer :: Env -> Server -> EM CompiledServer
compileServer env server@Server{..} = do
    typeEnv <- fmap M.fromList $ forM server_vars $ \(name, typeE) ->
        (,) name <$> evalType env typeE

    let encode = encodeState . M.toList
        serverEnv = symbols typeEnv `M.union` env

        compileTransition :: Transition -> EM [ServerAction]
        compileTransition (Transition msgSig pred ndParamsE maybeOutSignal assignment) = fmap (concat . concat) $ do
            params <- traverse (\(name, typeE) -> (,) name <$> evalType serverEnv typeE) (ms_params msgSig)

            forM (allStates params) $ \paramValues -> do
                let paramsEnv = M.fromList paramValues `M.union` serverEnv

                states <- matchingStates paramsEnv pred typeEnv

                forM states $ \state -> do
                    let stateEnv = state `M.union` paramsEnv

                    ndParams <- traverse (\(name, typeE) -> (,) name <$> evalType stateEnv typeE) ndParamsE

                    forM (allStates ndParams) $ \ndParamValues -> do
                        let env = M.fromList ndParamValues `M.union` stateEnv

                            outSignal = fromMaybe "ok" maybeOutSignal

                            applyUpdate updatedEnv (LHS var indexes, expr) = do
                                oldToplevelVal <- lookupVal state var

                                newVal <- evalExpr env expr

                                let updateArray indexE old f = do
                                        arr <- requireArray old
                                        index <- evalExpr env indexE >>= requireInt
                                        newVal <- f (arr !! index)
                                        pure (Arr $ listSet index newVal arr)

                                    updateIndexes [] old f = f old
                                    updateIndexes (i:is) old f = updateArray i old (\v -> updateIndexes is v f)

                                newToplevelVal <- updateIndexes indexes oldToplevelVal (\_ -> pure newVal)

                                toplevelType <- lookupType var typeEnv
                                checkType toplevelType newToplevelVal

                                pure (M.insert var newToplevelVal updatedEnv)

                        updates <- foldM applyUpdate state assignment

                        pure ServerAction
                            { sa_inMessage = encodeMessage (ms_name msgSig) (map snd paramValues)
                            , sa_inState = encode state
                            , sa_outMessage = outSignal
                            , sa_outState = encode (M.union updates state)
                            }
                    

    actions <- concat <$> mapM compileTransition server_transitions

    pure CompiledServer
        { cs_name = server_name
        , cs_states = map encodeState (allStates $ M.toList typeEnv)
        , cs_services = nub (map sa_inMessage actions)
        , cs_actions = actions
        }

listSet :: Int -> a -> [a] -> [a]
listSet 0 x (_:xs) = x:xs
listSet i newX (x:xs) = x : listSet (i - 1) newX xs

typeValues :: Type -> [Value]
typeValues (Enum values) = map Sym values
typeValues (Range from to) = map Int [from..to]
typeValues (Array elemType size) = map Arr $ traverse typeValues (replicate (fromIntegral size) elemType)

encodeState :: [(Symbol, Value)] -> Symbol
encodeState = intercalate "_" . map (\(name, val) -> name ++ "_" ++ encodeValue val)

allStates :: [(Symbol, Type)] -> [[(Symbol, Value)]]
allStates vars =
    let names = map fst vars
        types = map snd vars
    in map (zip names) . sequence . map typeValues $ types


symbols :: TypeEnv -> Env
symbols = M.fromList . concatMap varSyms . M.elems
  where
    varSyms (Enum syms) = map (\sym -> (sym, Sym sym)) syms
    varSyms (Range _ _) = []
    varSyms (Array typ _) = varSyms typ

matchingStates :: Env -> Predicate -> TypeEnv -> EM [Env]
matchingStates upperEnv pred types =
    let matches env = evalPredicate (env `M.union` upperEnv) pred
    in filterM matches (map M.fromList $ allStates $ M.toList types)

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
