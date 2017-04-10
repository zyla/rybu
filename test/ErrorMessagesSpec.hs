module ErrorMessagesSpec where

import TestImport

import Data.List
import qualified Data.Map as M

import qualified Parser
import Codegen
import Err

import Text.RawString.QQ (r)

spec :: Spec
spec = describe "generateDedan" $ context "should show context in errors" $ do
    it "in servers" $ do
        testErrorMessageContains [r|
            server buf {
                var count : 1..N;
            }
        |] ["server buf", "var count", "N"]

        testErrorMessageContains [r|
            server buf {
                var count : (0..1)[:banana];
            }
        |] ["server buf", "var count", "banana not in type Int"]

        testErrorMessageContains [r|
            server sem {
                { v | value = :up } -> { ok }
            }
        |] ["server sem", "action v", "predicate", "value"]

        testErrorMessageContains [r|
            server sem {
                var value : {up, down};
                { v | value = foo } -> { ok }
            }
        |] ["server sem", "action v", "predicate", "state { value = :up }", "foo"]

        testErrorMessageContains [r|
            server sem {
                var value : {up, down};
                { v } -> { value = 7 }
            }
        |] ["server sem", "action v",
            "updates",
            "state { value = :up }",
            "assignment of variable \"value\"",
            "value 7 not in type {up, down}"]

        testErrorMessageContains [r|
            server sem {
                var value : {up, down};
                { v } -> { bleh = 7 }
            }
        |] ["server sem",
            "action v",
            "updates",
            "state { value = :up }",
            "Undefined symbol \"bleh\""]

    it "in initializers" $ do

        testErrorMessageContains [r|
            server sem { var state : {up, down} }
            var mutex = sem() { state = bleh };
        |] ["initializer of server instance \"mutex\"",
            "Undefined symbol \"bleh\""]

    it "in processes" $ do
        testErrorMessageContains [r|
            process foo() { loop skip; }
        |] ["process \"foo\"",
            "Cycle detected in CFG"]
        

testErrorMessageContains :: String -> [String] -> Assertion
testErrorMessageContains source chunks =
    let model = unsafeParse Parser.model source
    in case generateDedan model of
        Right _ -> error $ "Expected error message with " ++ show chunks ++ ", got success!"
        Left err ->
            let msg = ppError err
            in forM_ chunks $ \chunk ->
                unless (chunk `isInfixOf` msg) $
                    error $ "Error message:\n" ++ msg ++ "\ndoes not contain " ++ show chunk
