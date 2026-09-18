module Main (main) where

import System.Environment
import System.IO
import GHC.IO.Encoding
import qualified Data.Text as T

import Parser
import Executor ( runExecutor, runREPL, runProgram )

main :: IO ()
main = do
  -- hSetBuffering stdout LineBuffering
  setLocaleEncoding utf8
  getArgs >>= \case
    [filename] -> parseFileWith sourceCode filename >>= \case
      Left err     -> putStrLn ("Error parsing file:\n" <> T.unpack err)
      Right stmts  -> runExecutor False (runProgram stmts) >> pure () -- >>= \case
        -- Left err     -> putStrLn ("Error executing file: " <> show err)
        -- Right result -> putStrLn ("Execution finished with result " <> show result)
    []         -> runREPL >> pure ()
    _          -> putStrLn "Syntax: uwu <filename>"