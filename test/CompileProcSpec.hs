module CompileProcSpec where

import TestImport

import qualified Data.Map as M

import qualified Parser
import CompileProc
import AST
import Err

import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "compileProc" $ do
    it "should handle parameterized messages" $ do
        let CompiledProc{..} = compileProcRight' [r|
            process test() {
                loop {
                    counter.set(0, 5);
                }
            }
        |]

        let encodedMsg = "set_0_5"
            stateName = "s0_counter_set"

        assertEqual ""
            [(stateName, "ok", Just ("counter", encodedMsg), stateName)]
            cp_transitions

    it "should handle terminating processes" $ do
        let CompiledProc{..} = compileProcRight' [r|
            process test() {
                sem.p();
            }
        |]

        let stateName = "s0_sem_p"

        assertEqual ""
            [(stateName, "ok", Nothing, stateName)]
            cp_transitions

    it "should return error on empty process" $ do
        let Left error = compileProc' [r|
            process test() {}
        |]
        assertEqual "" (Context "in process \"test\"" EmptyProcess) error

  describe "process parser" $ do
    it "should support the 'thread' keyword" $ do
      Parser.parse Parser.process "" "thread foo() { skip; }"
        `shouldSatisfy` isRight


compileProcRight' source =
    let process = unsafeParse Parser.process source
    in case compileProcess M.empty process of
        Right process -> process
        Left err -> error $ "compileProcess error: " ++ show err

compileProc' source = compileProcess M.empty $ unsafeParse Parser.process source
