module Eval where

import Control.Monad (join, when)
import Data.List (intercalate)
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

    evalCmpOp Equal       (Int v1) (Int v2) = pure (v1 == v2)
    evalCmpOp Equal       (Sym v1) (Sym v2) = pure (v1 == v2)
    evalCmpOp Equal       (Arr v1) (Arr v2) = pure (v1 == v2)

    evalCmpOp NotEqual         v1       v2  = not <$> evalCmpOp Equal v1 v2

    evalCmpOp LessThan    (Int v1) (Int v2) = pure (v1 < v2)
    evalCmpOp GreaterThan (Int v1) (Int v2) = pure (v1 > v2)
    evalCmpOp LessThanEqual  (Int v1) (Int v2) = pure (v1 <= v2)
    evalCmpOp GreaterThanEqual (Int v1) (Int v2) = pure (v1 >= v2)
    evalCmpOp _                 _        _  = err OpTypeMismatch
    
evalExpr :: Env -> Expr -> EM Value
evalExpr env (Var var) = lookupVal env var

evalExpr env (LitInt val) = pure (Int val)

evalExpr env (LitSym val) = pure (Sym val)

evalExpr env (LitArr exprs) = Arr <$> traverse (evalExpr env) exprs

evalExpr env (LitArrFill sizeE valueE) = do
    size <- requireInt =<< evalExpr env sizeE
    when (size < 0) $ err (ArraySizeNegative size)

    val <- evalExpr env valueE
    return $ Arr (replicate size val)

evalExpr env (ArrayIndex arrayE indexE) = do
    array <- requireArray =<< evalExpr env arrayE
    let size = length array

    index <- requireValidIndex size =<< evalExpr env indexE

    return $ array !! index

evalExpr env (ArraySlice arrayE index1E index2E) = do
    array <- requireArray =<< evalExpr env arrayE
    let size = length array

    index1 <- requireValidIndex size =<< evalExpr env index1E
    index2 <- requireValidIndex size =<< evalExpr env index2E

    return $ Arr $ take (index2 - index1 + 1) $ drop index1 array

evalExpr env (BinOp e1 op e2) = join $ evalBinOp op <$> evalExpr env e1 <*> evalExpr env e2
  where
    evalBinOp Plus (Int i1) (Int i2) = pure $ Int (i1 + i2)
    evalBinOp Minus (Int i1) (Int i2) = pure $ Int (i1 - i2)
    evalBinOp Modulo (Int i1) (Int i2) = pure $ Int (i1 `mod` i2)

    evalBinOp Plus (Arr a1) (Arr a2) = pure $ Arr (a1 ++ a2)

    evalBinOp _ _ _ = err OpTypeMismatch


inRange :: Type -> Value -> Bool

inRange (Range from to) (Int v) = v >= from && v <= to
inRange (Range from to) _ = False

inRange (Enum vals) (Sym v) = v `elem` vals
inRange (Enum vals) _ = False

inRange (Array typ size) (Arr xs) =
    length xs == fromIntegral size && all (inRange typ) xs
inRange (Array typ size) _ = False

requireValidIndex size val = do
    index <- requireInt val
    when (index < 0 || index >= size) $ err (IndexOutOfBounds index size)
    pure index

requireInt (Int v) = pure (fromIntegral v)
requireInt v = err $ TypeMismatch "Int" (ppValue v)

requireArray (Arr v) = pure v
requireArray v = err $ TypeMismatch "Array" (ppValue v)

ppValue :: Value -> String
ppValue (Sym s) = s
ppValue (Int i) = show i
ppValue (Arr xs) = "[" ++ intercalate ", " (map ppValue xs) ++ "]"

ppEnv :: Env -> String
ppEnv env = "{" ++ intercalate "," (map ppVar $ M.toList env) ++ " }"
  where
    ppVar (key, value) = " " ++ key ++ " = " ++ ppValue value

encodeValue :: Value -> String
encodeValue (Sym s) = s
encodeValue (Int i) = show i
encodeValue (Arr xs) = intercalate "_" (map encodeValue xs)

evalType :: Env -> TypeExpr -> EM Type

evalType env (EnumE syms) = pure (Enum syms)

evalType env (RangeE from to) = Range
    <$> (evalExpr env from >>= requireInt)
    <*> (evalExpr env to >>= requireInt)

evalType env (ArrayE typ size) = Array
    <$> evalType env typ
    <*> (evalExpr env size >>= requireInt)

encodeMessage name paramValues = name ++ concatMap (('_':) . encodeValue) paramValues

lookupVal env var =
    case M.lookup var env of
        Just val -> pure val
        Nothing -> err (UndefinedSymbol var)
