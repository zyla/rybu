module EvalSpec where

import TestImport

import qualified Data.Map as M

import AST
import qualified Parser
import Eval (inRange, evalExpr, evalType, evalPredicate)
import Err

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
            test (Range 1 3, Arr [Int 2]) False

        it "enums" $ do
            test (Enum ["up", "down"], Sym "up") True
            test (Enum ["up", "down"], Sym "down") True
            test (Enum ["up", "down"], Sym "left") False
            test (Enum ["up", "down"], Int 1) False

        it "arrays" $ do
            test (Array (Range 1 3) 3, iarray [1, 2, 3]) True
            test (Array (Range 1 3) 3, iarray [1, 5, 3]) False
            test (Array (Range 1 3) 3, iarray [1, 1, 1, 1]) False
            test (Array (Range 1 3) 3, iarray [1, 1]) False
            test (Array (Range 1 3) 3, Sym "foo") False
            test (Array (Range 1 3) 3, Int 1) False
            

    describe "eval" $ do
        let env = M.fromList
                [ ("up", Sym "up")
                , ("size", Int 5)
                , ("arr", Arr [Int 1, Int 3, Int 5, Int 7, Int 9])
                ]
            test expr expected =
              testF "evalExpr" (evalExpr env)
                (unsafeParse Parser.expr expr)
                expected

        it "basic literals" $ do
            test "1" $ Right (Int 1)
            test "up" $ Right (Sym "up")
            test ":true" $ Right (Sym "true")

        it "arithmetic" $ do
            test "1 + 2 + 3" $ Right (Int 6)
            test "size - 1" $ Right (Int 4)
            test "5 % 4" $ Right (Int 1)

        it "array literals" $ do
            test "[1, 3, 5]" $ Right (iarray [1, 3, 5])
            test "[size; 0]" $ Right (iarray [0, 0, 0, 0, 0])
            test "[size-10; 0]" $ Left (ArraySizeNegative (-5))

        it "array indexing" $ do
            test "arr[1]" $ Right (Int 3)
            test "arr[0-1]" $ Left (IndexOutOfBounds (-1) 5)
            test "arr[size]" $ Left (IndexOutOfBounds 5 5)
            test "arr[size+1]" $ Left (IndexOutOfBounds 6 5)

        it "array slicing" $ do
            test "arr[1..3]" $ Right (iarray [3, 5, 7])
            test "arr[0-1..1]" $ Left (IndexOutOfBounds (-1) 5)
            test "arr[0..size]" $ Left (IndexOutOfBounds 5 5)

        it "array concatenation" $ do
            test "[1,2] + [3,4,5]" $ Right (iarray [1, 2, 3, 4, 5])

        it "misc" $ do
            test "arr[0] + arr[1]" $ Right (Int 4)
            test "(arr[1..4] + [ arr[ arr[0] ] ])" $ Right (iarray [3, 5, 7, 9, 3])


    describe "predicates" $ do
        let env = M.empty
            test str expected =
              testF "evalPredicate" (evalPredicate env)
                (unsafeParse Parser.predicate str)
                expected

        it "supports both syntaxes for equality" $ do
            test "1 = 1" $ Right True
            test "1 == 1" $ Right True


    describe "evalType" $ do
        let env = M.fromList [ ("up", Sym "up"), ("size", Int 5) ]
            test expr expected =
              testF "evalType" (evalType env)
                (unsafeParse Parser.typ expr)
                expected

        it "basic types" $ do
            test "{up, down}" $ Right (Enum ["up", "down"])
            test "1..15" $ Right (Range 1 15)
            test "up..15" $ Left (TypeMismatch "Int" "up")
            test "1..up" $ Left (TypeMismatch "Int" "up")

        it "arrays" $ do
            test "{up, down}[3]" $ Right (Array (Enum ["up", "down"]) 3)
            test "(1..10)[3]" $ Right (Array (Range 1 10) 3)
            test "(1..10)[up]" $ Left (TypeMismatch "Int" "up")

iarray = Arr . map Int
