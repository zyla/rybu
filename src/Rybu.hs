-- | Main interface to the library.
module Rybu where

import Control.Arrow (left)

import Parser (parseModel)
import Codegen (generateDedan)
import Err (ppError)

-- | Compile the given program.
-- Returns either error message or model.
run :: String -> Either String String
run input = do
  model <- left syntaxError $ parseModel "" input
  output <- left semanticError $ generateDedan model
  pure (output ++ "\n")

syntaxError err = "Syntax Error: " ++ show err
semanticError err = "Error: " ++ ppError err
