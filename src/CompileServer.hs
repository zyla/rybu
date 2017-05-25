module CompileServer (
    compileServer
  , CompiledServer(..)
  , ServerAction(..)
  , ServersUsage
  , encodeState
) where

import Control.Monad
import qualified Control.Monad.State as MS
import Data.Maybe (fromMaybe)
import Data.List (intercalate, nub)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.List

import AST
import Err
import Eval

data CompiledServer = CompiledServer
    { cs_name :: ServerName
    , cs_states :: [Symbol]
    , cs_services :: [Symbol]
    , cs_actions :: [ServerAction]
    , cs_usedBy :: [ProcessName]
    , cs_vars :: [(Symbol, Type)]
    } deriving (Eq, Show)

data ServerAction = ServerAction
    { sa_inMessage :: Symbol
    , sa_inState :: Symbol
    , sa_outMessage :: Symbol
    , sa_outState :: Symbol
    } deriving (Eq, Show)

type ServersUsage = M.Map ServerName [ProcessName]

compileServer :: Env -> ServersUsage -> Server -> EM CompiledServer
compileServer env serversUsage server@Server{..} =
  withContext ("in server " ++ server_name) $ do
    typeEnv <- fmap M.fromList $ forM server_vars $ \(name, typeE) ->
        withContext ("in type of var " ++ name) $
            (,) name <$> evalType env typeE

    actions <- concat <$> mapM (compileTransition typeEnv env) server_transitions
    compiled_vars <- traverse (\(s, _) -> lookupType s typeEnv >>= \t -> return (s, t)) server_vars

    pure CompiledServer
        { cs_name = server_name
        , cs_states = map encodeState (allStates $ M.toList typeEnv)
        , cs_services = nub (map sa_inMessage actions)
        , cs_actions = actions
        , cs_usedBy = fromMaybe [] $ M.lookup server_name serversUsage
        , cs_vars = compiled_vars
        }


compileTransition :: TypeEnv -> Env -> Transition -> EM [ServerAction]
compileTransition typeEnv env = \case
  Transition msgSig pred ndParamsE maybeOutSignal assignment -> runListT $ do
    let
      encode = encodeState . M.toList
      serverEnv = symbols typeEnv `M.union` env

    withContext' ("in action " ++ ms_name msgSig) $ do
      paramValues <- liftList $ fmap allStates $ traverseEnv (evalType serverEnv) (ms_params msgSig)
      let paramsEnv = M.fromList paramValues `M.union` serverEnv

      state <- liftList $ return $ allStates' typeEnv

      let stateEnv = state `M.union` paramsEnv

      stateMatches <- lift $
        withContext ("when evaluating predicate for state " ++ ppEnv state) $
          evalPredicate stateEnv pred
      guard stateMatches

      withContext' ("for state " ++ ppEnv state) $ do
        ndParamValues <- liftList $ fmap allStates $ traverseEnv (evalType stateEnv) ndParamsE
        let env = M.fromList ndParamValues `M.union` stateEnv

        updates <- withContext' "when evaluating updates" $ lift $
          foldM (applyUpdate typeEnv env state) state assignment

        pure ServerAction
          { sa_inMessage = encodeMessage (ms_name msgSig) (map snd paramValues)
          , sa_inState = encode state
          , sa_outMessage = fromMaybe "ok" maybeOutSignal
          , sa_outState = encode (M.union updates state)
          }

  Yield _ _ -> pure []  -- NOTE(hator): Yield signal does not generate any IMDS action

applyUpdate :: TypeEnv -> Env -> Env -> Env -> (LHS, Expr) -> EM Env
applyUpdate typeEnv env state updatedEnv (LHS var indexes, expr) = do
  oldToplevelVal <- lookupVal state var

  withContext ("in assignment of variable " ++ show var) $ do
    newVal <- evalExpr env expr

    newToplevelVal <- updateIndexes env indexes (\_ -> pure newVal) oldToplevelVal

    toplevelType <- lookupType var typeEnv
    checkType toplevelType newToplevelVal

    pure (M.insert var newToplevelVal updatedEnv)

updateArray :: Env -> Expr -> (Value -> EM Value) -> Value -> EM Value
updateArray env indexE f old = do
  arr <- requireArray old
  index <- evalExpr env indexE >>= requireValidIndex (length arr)
  Arr <$> traverseOf (ix index) f arr

updateIndexes :: Env -> [Expr] -> (Value -> EM Value) -> Value -> EM Value
updateIndexes env = foldr (.) id . map (updateArray env)

typeValues :: Type -> [Value]
typeValues (Enum values) = map Sym values
typeValues (Range from to) = map Int [from..to]
typeValues (Array elemType size) = map Arr $ traverse typeValues (replicate (fromIntegral size) elemType)

-- | Encode a server state as a valid Dedan symbol.
--
-- The input must be sorted.
encodeState :: [(Symbol, Value)] -> Symbol
encodeState = intercalate "_" . map (\(name, val) -> name ++ "_" ++ encodeValue val)

-- | All possible environments resulting from all possible assignments of type values in the given type env.
--
-- >>>  allStates [("a", Range 1 2), ("b", Range 5 6)]
-- [[("a",Int 1),("b",Int 5)],[("a",Int 1),("b",Int 6)],[("a",Int 2),("b",Int 5)],[("a",Int 2),("b",Int 6)]]
allStates :: [(Symbol, Type)] -> [[(Symbol, Value)]]
allStates = traverseEnv typeValues

allStates' :: TypeEnv -> [Env]
allStates' = map M.fromList . allStates . M.toList

-- | An environment containing symbol values for types in given TypeEnv.
--
-- TODO: this is legacy and should be removed.
symbols :: TypeEnv -> Env
symbols = M.fromList . concatMap varSyms . M.elems
  where
    varSyms (Enum syms) = map (\sym -> (sym, Sym sym)) syms
    varSyms (Range _ _) = []
    varSyms (Array typ _) = varSyms typ

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

-- ListT utilities

liftList :: m [a] -> ListT m a
liftList = ListT

withContext' :: String -> ListT EM a -> ListT EM a
withContext' = mapListT . withContext

-- Lens utilities

-- | A Traversal over values in a list of key-value pairs.
traverseEnv :: Applicative f => (a -> f b) -> [(k, a)] -> f [(k, b)]
traverseEnv = traverse . traverse

-- | A Traversal over the element of a list with given index.
ix :: Applicative f => Int -> (a -> f a) -> [a] -> f [a]
ix _ _ [] = pure []
ix 0 inj (x:xs) = (:) <$> inj x <*> pure xs
ix i inj (x:xs) = (:) x <$> ix (i - 1) inj xs

-- | Use a Traversal as a 'traverse'-like function.
--
-- This is just the identity function, but aids readability.
traverseOf :: Applicative f => ((s -> f t) -> a -> f b) -> (s -> f t) -> a -> f b
traverseOf = id
