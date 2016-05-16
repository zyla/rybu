module Imp where

import Control.Monad (join, filterM)
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map as M

type Symbol = String

data Type = Enum (S.Set Symbol) | Range Integer Integer deriving (Show)

data Value = Sym Symbol | Int Integer deriving (Eq, Show)

type Env = M.Map Symbol Value
type TypeEnv = M.Map Symbol Type

data Transition = Transition Symbol Predicate [(Symbol, Expr)] deriving (Show)

data Server = Server
    { server_vars :: TypeEnv
    , server_transitions :: [Transition]
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


compileTransitions :: Server -> Either EvalErr [(Symbol, String, String)]
compileTransitions (Server vars transitions) = concat <$> mapM compileTransition transitions
  where
    encode = encodeState . M.toList
    syms = symbols vars

    compileTransition :: Transition -> Either EvalErr [(Symbol, String, String)]
    compileTransition (Transition name pred assignment) = 
      let compileState state = do
            let env = M.union syms state
            updates <- (traverse . traverse) (evalExpr env) assignment
            pure (name, encode state, encode (M.union (M.fromList updates) state))
            

      in matchingStates pred vars >>= mapM compileState
