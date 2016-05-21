module Err(Err(..), err, EM) where

data Err =
      UndefinedSymbol String
    | TypeMismatch
    | ErrCycle
      deriving (Show)

err = Left

type EM = Either Err
