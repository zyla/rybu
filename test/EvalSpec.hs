module EvalSpec where

import TestImport

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
