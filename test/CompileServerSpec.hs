module CompileServerSpec where

import TestImport
import Control.Arrow (left)

import qualified Data.Map as M

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

    it "should detect out-of-bounds array updates" $ do
        shouldFail [r|
            server test {
                var a : {unit}[3];
                {go} -> {a[3] = :unit}
            }
        |] (IndexOutOfBounds 3 3)

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

    it "should handle nondeterministic params" $ do
        let CompiledServer{..} = compileServer' [r|
            server counter {
                var val : 1..3;
                { set | val = 1 } for (new_val : 1..3) -> { val = new_val }
            }
        |]

        let expected = for [1..3] $ \index ->
                ServerAction "set" "val_1" "ok" ("val_" ++ show index)

        assertEqual "" expected cs_actions

    it "should handle array updates" $ do
        let CompiledServer{..} = compileServer' [r|
            server counter {
                var buf : ((1..5)[2])[2];
                { foo | buf = [[1, 2], [3, 4]] } -> { buf[0][1] = 5 }
            }
        |]

        let expected = [ServerAction "foo" "buf_1_2_3_4" "ok" "buf_1_5_3_4"]

        assertEqual "" expected cs_actions

    it "should handle array example" $ do
        shouldCompile [r|
            server buf {
                var data : (0..2)[5];
                var count : 0..5;

                { put(elem : 0..2) | count < 5 } -> {
                    data[count] = elem,
                    count = count + 1
                }
            }
        |]

    it "should handle new action syntax" $ do
        shouldCompile [r|
            server buf {
                var data : (0..2)[5];
                var count : 0..5;

                { put(elem : 0..2) | count < 5 } -> {
                    data[count] = elem;
                    count = count + 1;
                    return :ok;
                }
            }
        |]

    it "should handle negative integers" $ do
        let CompiledServer{..} = compileServer' [r|
            server counter {
                var count : -1..0;
                { decrement | count == 0 } -> { count = count - 1; return :ok; }
            }
        |]

        let expected = [ServerAction "decrement" "count_0" "ok" "count_m1"]

        assertEqual "" expected cs_actions

    it "should accept `yield` statement" $ do
        let CompiledServer{..} = compileServer' [r|
            server counter {
                var count : 0..1;
                { decrement | count == 1 } -> { count = count - 1; return :ok; }
                { decrement | count == 0 } -> { yield; }     -- Yield should compile to no IMDS action
            }
        |]

        let expected = [ServerAction "decrement" "count_1" "ok" "count_0"]
        assertEqual "" expected cs_actions

    it "should handle logic operator precedence" $ do
        let CompiledServer{..} = compileServer' [r|
            server logic {
                var x: {A, B};
                var y: {C, D};
                { action | x == :A && y == :D || x == :B && y == :C } ->
                    { x = :B }
            }
        |]

        let expected = [ ServerAction "action" "x_A_y_D" "ok" "x_B_y_D"
                       , ServerAction "action" "x_B_y_C" "ok" "x_B_y_C"
                       ]
        assertEqual "" expected cs_actions

for = flip map

compileServer' source =
    let server = unsafeParse Parser.server source
    in case compileServer M.empty M.empty server of
        Right server -> server
        Left err -> error $ "compileServer error: " ++ show err

shouldFail source err =
    let server = unsafeParse Parser.server source
    in assertEqual source (Left err) $ left stripContext $ compileServer M.empty M.empty server

shouldCompile source = compileServer' source `seq` (pure () :: Assertion)
