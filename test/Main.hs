module Main (main) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.IORef
import Data.Time.Clock
import Text.Megaparsec
import System.IO
import System.Console.Haskeline
import qualified Data.Text as T
import qualified Data.Map as M

import Types.Executor
import Parser
import Executor

tryParse :: Show a => Parser a -> T.Text -> String
tryParse p txt = case parse p "" txt of
  Left err -> errorBundlePretty err
  Right x  -> show x

main :: IO ()
main = undefined
  -- testEval

testParse :: IO ()
testParse = do
  -- putStrLn $ tryParse (expr 0 Nothing) "false ^ 0.000532 + true > -2.543 % n00b[not -2] * 2 + 3"
  -- putStrLn $ tryParse (expr 0 Nothing) "false[3] + 0.002^3[2+arr[2][3]] % xyz"
  -- putStrLn $ tryParse (expr 0 Nothing) "fn(t0m, d1ck, 1+2, 3-4^harry)"
  -- putStrLn $ tryParse varDeclStmt "let abc0123_: array[int] = 1 + 5 + f(99)[2];"
  -- putStrLn $ tryParse varDeclStmt "var bird = \"bird\";"
  -- putStrLn $ tryParse varDeclStmt "let foo: array[array[array[float]]];"
  -- putStrLn $ tryParse expression "1 + (2 < 3 < 4) * 5"
  -- putStrLn $ tryParse expression "1+(2^3)^((4 - 3) * 2)"
  -- putStrLn $ tryParse expression "1+(2^(3))^(((4 - 3)) * 2)"
  -- putStrLn $ tryParse expression "2 < 3 < 4"
  -- putStrLn $ tryParse expression "3 < 4 + 5 >= 6"
  -- putStrLn $ tryParse expression "bird * 5 < -a < 4"
  -- putStrLn $ tryParse expression "add[3<4] + f(5) == 6^7^8 + 9 * 5 % (foo0[1] - bar) >= 31 - 6"
  -- putStrLn $ tryParse expression "a += b %= c /= d + e -= f ^= 4 ^ 5"
  -- putStrLn $ tryParse (singleStmt WithoutBreak)
  --   "if 3 < 5 { let a = 3; var b: array[int] = true; } else while arr[i] + 2 >= bar { let a = 4; let b = 3; }"
  -- putStrLn $ tryParse (ifStmt WithoutBreak)
  --   "if 3 < 5 { while 3 >= 5 { break; } break; }"
  -- putStrLn $ tryParse (singleStmt WithoutBreak) "if 3 < 5 { let a = 5; continue; }"
  -- putStrLn $ tryParse (singleStmt WithoutBreak) "if 3 < 5 { let a = 5; break; }"
  -- putStrLn $ tryParse (singleStmt WithoutBreak) "if { oh dear }"
  -- putStrLn $ tryParse (singleStmt WithoutBreak) "if 3 < { let a = 5; break; }"
  -- putStrLn $ tryParse (singleStmt WithoutBreak) "if 3 <= 5 { let a = 5; } else 123"
  -- putStrLn $ tryParse (singleStmt WithoutBreak) "if 3 < 5 { let a = 5; break; } else { if 3 < 5 { \"this should be nested\"; while 1 + 2 == 3 {} }};"
  -- putStrLn $ tryParse expression "2 + (3 + 4)"
  -- putStrLn $ tryParse expression "((2) + 3) + ((((4)))))"
  -- putStrLn $ tryParse expression "not (a == b)"
  -- putStrLn $ tryParse expression "(not a) == b"
  -- putStrLn $ tryParse expression "-(a + b)"
  -- putStrLn $ tryParse expression "(-a) + b"
  -- putStrLn $ tryParse expression "ab[3] += [abc,1+2*5^(6*p),\"abc\"][3]"
  -- putStrLn $ tryParse ident "hel0"
  -- putStrLn $ tryParse ident "0hel"
  -- putStrLn $ tryParse ident ""
  putStrLn $ tryParse fnDecl "fn solution(n: int) -> array[str] {var result: array[str] = []; var i = 1; while i <= n { if i % 5 == 0 and i % 3 == 0 { result.push(\"fizzbuzz\"); } else if i % 3 == 0 { result.push(\"fizz\"); } else if i % 5 == 0 { result.push(\"buzz\"); } else { result.push(format(\"{}\", i)); } i += 1; } return result; }"
  putStrLn $ tryParse fnDecl "fn solution(n: int) -> {}"
  putStrLn $ tryParse fnDecl "fn solution(n: int) {}"
  -- code <- readFile "fizzbuzz.im"
  -- putStrLn $ tryParse sourceCode (T.pack code)
  -- code2 <- readFile "insertionSort.im"
  -- putStrLn $ tryParse sourceCode (T.pack code2)

-- runREPL :: InputT Executor a -> IO (Either Error a)
-- runREPL f = runExecutor (runInputT defaultSettings f)

toRun :: [T.Text]
toRun = 
  [ "let a = [1,2,3];"
  , "let b = [4.0,5.0,6.0];"
  , "[a, b];"
  , "[a, b][1][0] = 3;"
  ]

-- testEval :: IO ()
-- testEval = do
--   -- runReaderT (runStateT (runExceptT (coerce FloatT five)) M.empty) M.empty >>= print
--   -- runExecutor (coerce (ArrayT FloatT) (ValArray IntT [ValInt 3, ValInt 5])) >>= print . (== Right (ValArray FloatT [ValFloat 3.0,ValFloat 5.0]))
--   -- runExecutor (coerce FloatT (ValInt 3)) >>= print . (== Right (ValFloat 3.0))
--   -- runExecutor (coerce (ArrayT (ArrayT (ArrayT FloatT))) a) >>= print . (== Right b)
--   hSetBuffering stdout NoBuffering
--   result <- runREPL $ do
--     lift (evalAndPrint toRun)
--     forever $ do
--       getInputLine "> " >>= \case
--         Just a  -> lift (evalAndPrint [T.pack a] `catchError` (liftIO . print))
--         Nothing -> lift (throwError (MiscError "Quit"))
--   case result of
--     Left err -> print err
--     _        -> pure ()

evalAndPrint :: [T.Text] -> Executor ()
evalAndPrint xs = forM_ xs $ \t -> do
  let v = parse (singleStmt WithBreak) "" t
  case v of
    Left err -> liftIO (putStrLn (errorBundlePretty err))
    Right x  -> do
      liftIO (putStrLn ("Parsed: " <> show x))
      _ <- runStatement x
      pure ()
-- type Executor = ReaderT FuncMap (StateT (NE.NonEmpty StackFrame) (ExceptT Error IO))
