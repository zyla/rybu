-- | Test that examples still work by comparing them with reference.
module Main where

import Control.Monad (forM_)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), takeExtension, replaceExtension)
import System.Exit (exitWith, ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Rybu

data Example = Example
    { example_name :: String
    , example_input_name :: String
    , example_input :: String
    , example_output_name :: String
    , example_output :: String
    } deriving (Show)

getExamples = getDirectoryContents "examples" >>= mapM getExample . filter isExampleFile
  where
    isExampleFile name = takeExtension name == ".txt"

    getExample name = Example name
        <$> pure inputName
        <*> readFile inputName
        <*> pure outputName
        <*> readFile outputName
      where
        inputName = "examples" </> name
        outputName = "test/example_results/" </> replaceExtension name ".dedan.txt"

failed msg = do
    putStrLn $ "FAILED\n" ++ msg

main = do
    examples <- getExamples

    forM_ examples $ \Example{..} -> do
        putStr $ example_name ++ ": "
        case Rybu.run example_input of
            Left err -> failed err >> exitWith (ExitFailure 1)
            Right output
                | output == example_output -> putStrLn "OK"
                | otherwise                -> do
                    failed "Output differs from reference"
                    (_, diffOut, diffErr) <- readProcessWithExitCode
                        "diff" ["-u", example_output_name, "-"] output

                    putStr $ diffOut ++ diffErr
                    exitWith (ExitFailure 1)
