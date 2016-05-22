module Err(Err(..), err, EM) where

data Err =
      UndefinedSymbol String
    | OpTypeMismatch -- ^ TODO: more info
    | TypeMismatch String String -- ^ type, value
    | ErrCycle
      deriving (Eq, Show)

err = Left

type EM = Either Err
