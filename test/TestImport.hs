module TestImport (
    module TestImport
  , module X
) where

import Test.Hspec as X
import Test.HUnit as X

import Parser
import Text.Parsec (eof)

testF :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> Assertion
testF name f input expected =
    assertEqual (name ++ " " ++ show input) expected (f input)

unsafeParse parser source = 
    case Parser.parse (Parser.whiteSpace *> parser <* eof) "" source of
        Right server -> server
        Left err -> error ("input:\n" ++ source ++ "\nParse Error: " ++ show err)
