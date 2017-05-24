module Err(Err(..), err, EM, withContext, ppError) where

import Control.Arrow (left)

-- | A Rybu computation. May produce an error of type 'Err'.
type EM = Either Err

-- | Possible semantic errors.
data Err =
    UndefinedSymbol String
  | OpTypeMismatch -- ^ TODO: more info
  | TypeMismatch String String -- ^ type, value
  | ArraySizeNegative Int
  | IndexOutOfBounds Int Int -- ^ index, size
  | ErrCycle
  | UninitializedVariable String
  | EmptyProcess
  | Context String Err
  deriving (Eq, Show)

-- | Throw an error and abort the computation.
err :: Err -> EM a
err = Left

-- | Map over the error in a given computation, if there's one.
-- Equivalent to @over _Left@ from lens.
mapError :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapError = left

-- | Augment the possible error message in a given computation.
--
-- >>> err OpTypeMismatch
-- Left OpTypeMismatch
--
-- >>> withContext "in server foo" $ err OpTypeMismatch
-- Left (Context "in server foo" OpTypeMismatch)
--
-- >>> withContext "more context" $ withContext "in server foo" $ err OpTypeMismatch
-- Left (Context "more context" (Context "in server foo" OpTypeMismatch))
withContext :: String -> EM a -> EM a
withContext context = mapError (Context context)

-- | Human-readable form of an error, with context.
--
-- >>> putStrLn $ ppError EmptyProcess
-- Empty process
--
-- >>> putStrLn $ ppError $ Context "in process foo" EmptyProcess
-- Empty process
--   in process foo
ppError :: Err -> String
ppError = \case
  UndefinedSymbol sym         -> "Undefined symbol " ++ show sym
  OpTypeMismatch              -> "Operator type mismatch"
  TypeMismatch type_ val      -> "Type mismatch: value " ++ val ++ " not in type " ++ type_
  ArraySizeNegative size      -> "Bad array size: " ++ show size
  IndexOutOfBounds index size -> "Index " ++ show index ++ " out of array bounds (array size = " ++ show size ++ ")"
  ErrCycle                    -> "Cycle detected in CFG"
  UninitializedVariable var   -> "Uninitialized variable " ++ show var
  EmptyProcess                -> "Empty process"
  Context context err         -> ppError err ++ "\n  " ++ context
