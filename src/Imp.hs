module Imp where

import Control.Monad (join, filterM)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map as M

type Symbol = String

data Type = Enum (S.Set Symbol) | Range Integer Integer deriving (Show)

data Value = Sym Symbol | Int Integer deriving (Eq, Show)

type Env = M.Map Symbol Value
type TypeEnv = M.Map Symbol Type

data Transition = Transition Symbol Predicate (Maybe Symbol) [(Symbol, Expr)] deriving (Show)

data Server = Server
    { server_name :: Symbol
    , server_vars :: TypeEnv
    , server_transitions :: [Transition]
    } deriving (Show)

data Statement = Skip | Loop Statement | Block [Statement] | Msg Message | Match Message [(Symbol, Statement)]
    deriving (Show)

data Message = Message { message_server :: Symbol, message_msg :: Symbol } deriving (Show)

data Process = Process
    { process_name :: Symbol
    , process_stmt :: Statement
    } deriving (Show)

data Model = Model
    { model_servers :: [Server]
    , model_procs :: [Process]
    } deriving (Show)

encodeValue :: Value -> String
encodeValue (Sym s) = s
encodeValue (Int i) = show i

typeValues :: Type -> [Value]
typeValues (Enum values) = map Sym (S.toList values)
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

data Predicate = Cmp Expr CmpOp Expr | And Predicate Predicate | Or Predicate Predicate | BoolLit Bool deriving (Show)
data CmpOp = Equal | LessThan | GreaterThan deriving (Show)

data Expr = Var Symbol | Lit Value | BinOp Expr BinOp Expr
  deriving (Show)

data BinOp = Plus | Minus deriving (Show)

data EvalErr = UndefinedSymbol Symbol | TypeMismatch deriving (Show)

err = Left

evalPredicate :: Env -> Predicate -> Either EvalErr Bool
evalPredicate env = eval
  where
    eval (And p1 p2) = (&&) <$> eval p1 <*> eval p2
    eval (Or p1 p2) = (||) <$> eval p1 <*> eval p2
    eval (Cmp e1 op e2) = join $ evalCmpOp op <$> evalExpr env e1 <*> evalExpr env e2
    eval (BoolLit b) = pure b

    evalCmpOp Equal            v1       v2  = pure (v1 == v2)
    evalCmpOp LessThan    (Int v1) (Int v2) = pure (v1 < v2)
    evalCmpOp GreaterThan (Int v1) (Int v2) = pure (v1 > v2)
    evalCmpOp _                 _        _  = err TypeMismatch
    

evalExpr env (Var var) =
    case M.lookup var env of
        Just val -> pure val
        Nothing -> err (UndefinedSymbol var)
evalExpr env (Lit val) = pure val
evalExpr env (BinOp e1 op e2) = join $ evalBinOp op <$> evalExpr env e1 <*> evalExpr env e2
  where
    evalBinOp Plus (Int i1) (Int i2) = pure $ Int (i1 + i2)
    evalBinOp Minus (Int i1) (Int i2) = pure $ Int (i1 - i2)
    evalBinOp _ _ _ = err TypeMismatch

symbols :: TypeEnv -> Env
symbols = M.fromList . concatMap varSyms . M.toList
  where
    varSyms (name, Enum syms) = map (\sym -> (sym, Sym sym)) (S.toList syms)
    varSyms (name, Range _ _) = []

matchingStates :: Predicate -> TypeEnv -> Either EvalErr [Env]
matchingStates pred types =
    let syms = symbols types
        matches env = evalPredicate (env `M.union` syms) pred
    in filterM matches (allStates $ M.toList types)


compileTransitions :: Server -> Either EvalErr [(Symbol, String, Symbol, String)]
compileTransitions Server {server_vars=vars, server_transitions=transitions} =
    concat <$> mapM compileTransition transitions

  where
    encode = encodeState . M.toList
    syms = symbols vars

    compileTransition :: Transition -> Either EvalErr [(Symbol, String, Symbol, String)]
    compileTransition (Transition name pred maybeOutSignal assignment) = 
      let compileState state = do
            let env = M.union syms state
            updates <- (traverse . traverse) (evalExpr env) assignment
            pure (name, encode state, outSignal, encode (M.union (M.fromList updates) state))

          outSignal = fromMaybe "ok" maybeOutSignal
            

      in matchingStates pred vars >>= mapM compileState
