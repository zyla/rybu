module CodegenSpec where

import TestImport
import Control.Arrow (left)

import qualified Parser
import Codegen
import Err

import Text.RawString.QQ (r)
import Data.List (isPrefixOf)

spec :: Spec
spec =
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

            it "detects type mismatch" $
                shouldFail [r|
                    server test {
                        var a : 0..1;
                        var b : { zero, one };
                    }
                    var t = test() { a = :zero, b = 1 };
                |] (TypeMismatch "Range 0 1" "Sym \"zero\"")

        context "should sort variable initializers in instance initializers" $ do
            it "doesn't mess if they are sorted already" $
                shouldWork
                    [r|
                        server test {
                            var a : {unit};
                            var b : {empty};
                        }
                        var t = test() { a = :unit, b = :empty };
                    |]
                    [r|
                        server: test (servers ; agents ),
                        services {},
                        states {a_unit_b_empty},
                        actions {
                        };

                        agents ;

                        servers t: test;

                        init -> {
                          t().a_unit_b_empty,
                        }.
                    |]

            it "sorts if not sorted" $
                shouldWork
                    [r|
                        server test {
                            var a : {unit};
                            var b : {empty};
                        }
                        var t = test() { b = :empty, a = :unit };
                    |]
                    [r|
                        server: test (servers ; agents ),
                        services {},
                        states {a_unit_b_empty},
                        actions {
                        };

                        agents ;

                        servers t: test;

                        init -> {
                          t().a_unit_b_empty,
                        }.
                    |]


shouldFail source err =
    let model = unsafeParse Parser.model source
    in assertEqual source (Left err) $ left stripContext $ generateDedan model
    where
        stripContext :: Err -> Err
        stripContext (Context _ e) = stripContext e
        stripContext e = e

shouldWork source output =
    let model = unsafeParse Parser.model source
        strippedOutput = stripNewlines $ stripTabs output
    in assertEqual source (Right strippedOutput) $ generateDedan model
    where
        stripTabs :: String -> String
        stripTabs = unlines . fmap stripTabsLine . lines
        stripTabsLine :: String -> String
        stripTabsLine str = if "    " `isPrefixOf` str
                            then stripTabsLine $ drop 4 str
                            else str
        stripNewlines :: String -> String
        stripNewlines = stripBeginNewlines . stripEndNewlines
        stripBeginNewlines = dropWhile (=='\n')
        stripEndNewlines = (++"\n") . reverse . dropWhile (=='\n') . reverse
