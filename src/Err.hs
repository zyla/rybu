module Err(Err(..), err, EM, withContext, ppError) where

import Control.Arrow

data Err =
      UndefinedSymbol String
    | OpTypeMismatch -- ^ TODO: more info
    | TypeMismatch String String -- ^ type, value
    | ArraySizeNegative Int
    | IndexOutOfBounds Int Int -- ^ index, size
    | ErrCycle
    | UninitializedVariable String
    | Context String Err
      deriving (Eq, Show)

err = Left

type EM = Either Err

withContext :: String -> EM a -> EM a
withContext context = Control.Arrow.left (Context context)

ppError :: Err -> String
ppError (UndefinedSymbol sym) = "Undefined symbol " ++ show sym
ppError (OpTypeMismatch) = "Operator type mismatch"
ppError (TypeMismatch type_ val) = "Type mismatch: value " ++ val ++ " not in type " ++ type_
ppError (ArraySizeNegative size) = "Bad array size: " ++ show size
ppError (IndexOutOfBounds index size) = "Index " ++ show index ++ " out of array bounds (array size = " ++ show size ++ ")"
ppError (ErrCycle) = "Cycle detected in CFG"
ppError (UninitializedVariable var) = "Uninitialized variable " ++ show var
ppError (Context context err) = ppError err ++ "\n  " ++ context
