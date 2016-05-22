module CompileServerSpec where

import TestImport

import qualified Parser
import CompileServer
import Err

import Text.RawString.QQ (r)

spec :: Spec
spec = describe "compileServer" $ do
    context "should detect undefined symbols" $ do
        it "in predicate" $ shouldFail [r|
            server sem {
                var state : {up, down};
                {p | state = left} -> {ok}
            }
        |] (UndefinedSymbol "left")

        it "in state update" $ shouldFail [r|
            server sem {
                var state : {up, down};
                {p} -> {state = left}
            }
        |] (UndefinedSymbol "left")

        it "in state LHS" $ shouldFail [r|
            server sem {
                var state : {up, down};
                {p} -> {counter = 1}
            }
        |] (UndefinedSymbol "counter")

    it "should detect badly typed arithmetic" $ do
        shouldFail [r|
            server sem {
                var state : {up, down};
                {p | state = up} -> {state = state + 1}
            }
        |] OpTypeMismatch

    it "should detect badly typed comparison" $ do
        shouldFail [r|
            server sem {
                var state : {up, down};
                {p | state = 1} -> {ok}
            }
        |] OpTypeMismatch

    it "should detect badly typed updates" $ do
        shouldFail [r|
            server buf {
                var count : 0..3;
                {put} -> {count = count + 1}
            }
        |] (TypeMismatch "0..3" "4")

    it "should handle parameterized messages" $ do
        let CompiledServer{..} = compileServer' [r|
            server counter {
                var val : 1..3;
                { set(new_val : 1..3, a : {b}) | val = 1 } -> { val = new_val }
            }
        |]

        let expected = for [1..3] $ \index ->
                ServerAction ("set_" ++ show index ++ "_b") "val_1" "ok" ("val_" ++ show index) 

        assertEqual "" expected cs_actions

for = flip map

compileServer' source =
    let server = unsafeParse Parser.server source
    in case compileServer server of
        Right server -> server
        Left err -> error $ "compileServer error: " ++ show err

shouldFail source err =
    let server = unsafeParse Parser.server source
    in assertEqual source (Left err) (compileServer server)
