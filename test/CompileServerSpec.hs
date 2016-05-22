module CompileServerSpec where

import Test.Hspec
import Test.HUnit

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


shouldFail source err =
    let server = unsafeParseServer source
    in assertEqual source (Left err) (compileServer server)

unsafeParseServer source = 
    case Parser.parse (Parser.whiteSpace *> Parser.server) "" source of
        Right server -> server
        Left err -> error ("Parse Error: " ++ show err)
