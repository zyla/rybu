module CodegenSpec where

import TestImport
import Control.Arrow (left)

import qualified Parser
import Codegen
import Err

import Text.RawString.QQ (r)

spec :: Spec
spec = do
    describe "generateDedan" $ do
        context "should check instance variable initialization" $ do
            it "detects if instatiates existing server" $
                shouldFail [r|
                    server A { var a : 0..1; }
                    var t = B() { };
                |] (UndefinedSymbol "B")

            it "detects if all variables are initialized" $
                shouldFail [r|
                    server test {
                        var a : 0..1;
                        var b : { zero, one };
                    }
                    var t = test() { a = 0 };
                |] (UninitializedVariable "b")

            it "detects if only proper variables are initialized" $
                shouldFail [r|
                    server test {
                        var a : 0..1;
                    }
                    var t = test() { a = 0, b = 0 };
                |] (UndefinedSymbol "b")

            it "detects type mismatch" $ do
                shouldFail [r|
                    server test {
                        var a : 0..1;
                        var b : { zero, one };
                    }
                    var t = test() { a = :zero, b = 1 };
                |] (TypeMismatch "Range 0 1" "Sym \"zero\"")

shouldFail source err =
    let model = unsafeParse Parser.model source
    in assertEqual source (Left err) $ left stripContext $ generateDedan model

stripContext :: Err -> Err
stripContext (Context _ e) = stripContext e
stripContext e = e
