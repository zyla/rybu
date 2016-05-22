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


shouldFail source err =
    let server = unsafeParseServer source
    in assertEqual source (Left err) (compileServer server)

unsafeParseServer source = 
    case Parser.parse (Parser.whiteSpace *> Parser.server) "" source of
        Right server -> server
        Left err -> error ("Parse Error: " ++ show err)
