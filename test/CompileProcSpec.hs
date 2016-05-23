module CompileProcSpec where

import TestImport

import qualified Data.Map as M

import qualified Parser
import CompileProc
import AST

import Text.RawString.QQ (r)

spec :: Spec
spec = describe "compileProc" $ do
    it "should handle parameterized messages" $ do
        let CompiledProc{..} = compileProc' [r|
            process test() {
                loop {
                    counter.set(0, 5);
                }
            }
        |]

        let encodedMsg = "set_0_5"
            stateName = "s0_counter_set"

        assertEqual ""
            [(stateName, "ok", "counter", encodedMsg, stateName)]
            cp_transitions

compileProc' source =
    let process = unsafeParse Parser.process source
    in case compileProcess M.empty process of
        Right process -> process
        Left err -> error $ "compileProcess error: " ++ show err
