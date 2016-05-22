module EvalSpec where

import Test.Hspec
import Test.HUnit

import qualified Data.Map as M

import AST
import Eval (inRange)

spec :: Spec
spec = do
    describe "inRange" $ do
        let test = testF "inRange" (\(typ, val) -> inRange typ val)

        it "integers" $ do
            test (Range 1 3, Int 0) False
            test (Range 1 3, Int 1) True
            test (Range 1 3, Int 2) True
            test (Range 1 3, Int 3) True
            test (Range 1 3, Int 4) False
            test (Range 1 3, Sym "foo") False

        it "enums" $ do
            test (Enum ["up", "down"], Sym "up") True
            test (Enum ["up", "down"], Sym "down") True
            test (Enum ["up", "down"], Sym "left") False
            test (Enum ["up", "down"], Int 1) False

testF :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> Assertion
testF name f input expected =
    assertEqual (name ++ " " ++ show input) expected (f input)
