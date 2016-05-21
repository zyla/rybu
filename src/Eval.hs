module Eval where

import Control.Monad (join)
import qualified Data.Map as M

import AST
import Err

type Env = M.Map Symbol Value
type TypeEnv = M.Map Symbol Type

evalPredicate :: Env -> Predicate -> EM Bool
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
evalExpr env (LitInt val) = pure (Int val)
evalExpr env (BinOp e1 op e2) = join $ evalBinOp op <$> evalExpr env e1 <*> evalExpr env e2
  where
    evalBinOp Plus (Int i1) (Int i2) = pure $ Int (i1 + i2)
    evalBinOp Minus (Int i1) (Int i2) = pure $ Int (i1 - i2)
    evalBinOp _ _ _ = err TypeMismatch
