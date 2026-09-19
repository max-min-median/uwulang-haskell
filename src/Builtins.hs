module Builtins where

import Control.Applicative (asum)
import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import System.IO.Error
import Text.Read (readMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV

import Types.Values
import Types.Executor
import Values
import Control.Exception (try)

builtins :: M.Map T.Text ([Value] -> Executor Value)
builtins = M.fromList 
  [ ("print", fnPrint)
  , ("println", fnPrintln)
  , ("format", fnFormat)
  , ("len", fnLen)
  , ("strslice", fnStrslice)
  , ("sleep", fnSleep)
  , ("type", fnType)
  , ("elapsed", fnElapsed)
  , ("input", fnInput)
  , ("parse", fnParse)
  , ("exit", fnExit)
  , ("fread", fnFread)
  , ("fwrite", fnFwrite) ]

methods :: M.Map T.Text (Value -> [Value] -> Executor Value)
methods = M.fromList 
  [ ("push", methodPush)
  , ("pop", methodPop)
  , ("remove", methodRemove) ]

fnPrint :: [Value] -> Executor Value
fnPrint args = fnFormat args >>= liftIO . TIO.putStr . valToText >> pure ValVoid

fnPrintln :: [Value] -> Executor Value
fnPrintln args = fnPrint args >> liftIO (TIO.putStrLn "") >> pure ValVoid

fnFormat :: [Value] -> Executor Value
fnFormat = \case
  (ValStr fmtStr:args) -> go (T.unpack fmtStr) args >>= pure . ValStr . T.pack . concat
  --   let chunks = T.splitOn "{}" fmtStr
  --   when (length chunks /= length args + 1) $ throwError $ ArityMismatch ("format: found " <> T.show (length chunks - 1) <> " placeholders but received " <> T.show (length args) <> " arguments")
  --   pure $ ValStr (T.concat (zipWith (<>) chunks (map valToText args ++ [""])))
  (val:_)              -> throwError $ ArityMismatch ("format: expected 1st argument to be of type 'str', not '" <> T.show (valType val) <> "'")
  _                    -> throwError $ ArityMismatch ("format: expected at least 1 argument (format string)")
  where
    go :: String -> [Value] -> Executor [String]
    go = \cases
      ('{':'{':xs) args'       -> ("{":) <$> go xs args'
      ('}':'}':xs) args'       -> ("}":) <$> go xs args'
      ('{':'}':xs) (arg:args') -> (T.unpack (valToText arg):) <$> go xs args'
      ('{':'}':_) []           -> throwError $ ArityMismatch ("format: more placeholders than arguments")
      (x:xs) args'             -> ([x]:) <$> go xs args'
      [] (_:_)                 -> throwError $ ArityMismatch ("format: more arguments than placeholders")
      [] []                    -> pure []

fnLen :: [Value] -> Executor Value
fnLen = \case
  [val] -> case val of
    ValArray _ _ lenRef _ -> liftIO (readIORef lenRef) >>= pure . ValInt
    ValStr txt            -> pure (ValInt (T.length txt))
    _                     -> throwError $ TypeError ("len: expected type 'str' or 'array', not '" <> T.show (valType val) <> "'")
  args  -> throwError $ ArityMismatch ("len: expected 1 argument but received " <> T.show (length args))

fnSleep :: [Value] -> Executor Value
fnSleep = \case
  [val] -> do
    delay <- case val of
      ValInt x   -> pure $ x * 1000000
      ValFloat x -> pure $ round (x * 1000000)
      _          -> throwError $ TypeError ("sleep: expected type 'int' or 'float', not '" <> T.show (valType val) <> "'")
    when (delay < 0) $ throwError (MiscError ("sleep: argument must not be negative"))
    liftIO (threadDelay delay)
    pure ValVoid
  args  -> throwError $ ArityMismatch ("sleep: expected 1 argument but received " <> T.show (length args))
  
fnType :: [Value] -> Executor Value
fnType = \case
  [val] -> pure $ ValStr (T.show (valType val))
  args  -> throwError $ ArityMismatch ("type: expected 1 argument but received " <> T.show (length args))

fnElapsed :: [Value] -> Executor Value
fnElapsed = \case
  []   -> do
    now <- liftIO getCurrentTime
    start <- asks startTime
    let diff = now `diffUTCTime` start
    pure $ ValFloat (realToFrac diff)
  args -> throwError $ ArityMismatch ("elapsed: expected 0 arguments but received " <> T.show (length args))

fnInput :: [Value] -> Executor Value
fnInput = \case
  [val] -> case val of
    ValStr prompt -> do
      liftIO (TIO.putStr prompt)
      liftIO TIO.getLine >>= pure . ValStr
    _             -> throwError $ TypeError ("input: expected 1st argument (prompt) to be of type 'str', not '" <> T.show (valType val) <> "'")
  args  -> throwError $ ArityMismatch ("input: expected 1 argument but received " <> T.show (length args))
  
fnParse :: [Value] -> Executor Value
fnParse = \case
  [val] -> case val of
    ValStr txt -> do
      let str = case T.unpack (T.strip txt) of
            ('+':xs) -> xs
            xs       -> xs
      if str == "true" then pure $ ValBool True
      else if str == "false" then pure $ ValBool False
      else pure $ fromMaybe val (asum (map ($ str) [ (ValInt <$>) . (readMaybe :: String -> Maybe Int)
                                                   , (ValFloat <$>) . (readMaybe :: String -> Maybe Double) ]
                                      )
                                )
    _          -> throwError $ TypeError ("input: expected 1st argument to be of type 'str', not '" <> T.show (valType val) <> "'")
  args  -> throwError $ ArityMismatch ("input: expected 1 argument but received " <> T.show (length args))

fnExit :: [Value] -> Executor Value
fnExit = \case
  [val] -> case val of
    ValInt x   -> throwError $ ExitError x
    _          -> throwError $ TypeError ("sleep: expected type 'int' or 'float', not '" <> T.show (valType val) <> "'")
  args  -> throwError $ ArityMismatch ("sleep: expected 1 argument but received " <> T.show (length args))

fnStrslice :: [Value] -> Executor Value
fnStrslice = \case
  [a,b,c] -> case (a,b,c) of
    (ValStr txt, ValInt start, ValInt end) -> do
      let vec = V.fromList (T.unpack txt)
      when (start < 0) $ throwError (IndexError ("strslice: 'start' argument cannot be negative."))
      when (start > end) $ throwError (IndexError ("strslice: 'start' argument cannot be larger than 'end'."))
      when (end > V.length vec) $ throwError (IndexError ("strslice: 'end' (" <> T.show end <> ") cannot be larger than length of text (" <> T.show (V.length vec) <> ")"))
      pure $ ValStr . T.pack . V.toList . V.slice start (end-start) $ vec
    _                                    -> throwError $ TypeError "strslice: arguments should be of type 'str', 'int', 'int'"
  args         -> throwError $ ArityMismatch ("strslice: expected 3 arguments but received " <> T.show (length args))

fnFread :: [Value] -> Executor Value
fnFread = \case
  [val] -> case val of
    ValStr path -> do
      liftIO (try (TIO.readFile (T.unpack path))) >>= \case
        Left err       -> throwError $ FileError ("fread: unable to open '" <> path <> "': " <> T.pack (ioeGetErrorString err))
        Right contents -> pure (ValStr contents)
    _          -> throwError $ TypeError ("fread: expected 1st argument to be of type 'str', not '" <> T.show (valType val) <> "'")
  args  -> throwError $ ArityMismatch ("fread: expected 1 argument but received " <> T.show (length args))

fnFwrite :: [Value] -> Executor Value
fnFwrite = \case
  [val1, val2] -> case (val1, val2) of
    (ValStr path, ValStr content) -> do
      liftIO (try (TIO.writeFile (T.unpack path) content)) >>= \case
        Left err -> throwError $ FileError ("Unable to open '" <> path <> "': " <> T.pack (ioeGetErrorString err))
        Right _  -> pure (ValVoid)
    _          -> throwError $ TypeError ("fwrite: expected arguments to be of type 'str' and 'str, not '" <> T.show (valType val1) <> "' and '" <> T.show (valType val2) <> "'")
  args  -> throwError $ ArityMismatch ("fwrite: expected 2 arguments but received " <> T.show (length args))

methodPush :: Value -> [Value] -> Executor Value
methodPush obj args = case obj of
  ValArray mut t lenRef vecRef -> do
    when (not mut) $ throwError (TypeError "push: cannot grow immutable array")
    val <- case args of
      [val] -> coerce (Just True) t val
      _     -> throwError (ArityMismatch ("push: expected 1 argument but received " <> T.show (length args)))
    vec <- liftIO (readIORef vecRef)
    idx <- liftIO (readIORef lenRef)
    newVec <- if idx >= MV.length vec then
                liftIO (MV.unsafeGrow vec (MV.length vec + 1) >>= \grownVec -> writeIORef vecRef grownVec >> pure grownVec)
              else pure vec
    liftIO (MV.write newVec idx val >> modifyIORef lenRef (+1))
    pure ValVoid
  val                -> throwError $ TypeError ("push: expected an array, not '" <> T.show (valType val) <> "'")

methodPop :: Value -> [Value] -> Executor Value
methodPop obj args = case obj of
  ValArray mut _ lenRef vecRef -> do
    when (not mut) $ throwError (TypeError "pop: cannot pop from immutable array")
    case args of
      [] -> pure ()
      _  -> throwError (ArityMismatch ("pop: expected 0 arguments but received " <> T.show (length args)))
    len <- liftIO (readIORef lenRef)
    when (len == 0) $ throwError (IndexError "pop: cannot pop from empty array")
    let idx = len - 1
    liftIO (modifyIORef' lenRef (subtract 1))
    vec <- liftIO (readIORef vecRef)
    result <- MV.read vec idx
    liftIO (MV.write vec idx ValVoid)
    pure result
  val                -> throwError $ TypeError ("pop: expected an array, not '" <> T.show (valType val) <> "'")

methodRemove :: Value -> [Value] -> Executor Value
methodRemove obj args = case obj of
  ValArray mut _ lenRef vecRef -> do
    when (not mut) $ throwError (TypeError "remove: cannot remove from immutable array")
    idx <- case args of
      [val] -> toArrayIndex val `catchError` const (throwError (TypeError ("remove: index should be of type 'int', not " <> T.show (valType val))))
      _     -> throwError (ArityMismatch ("remove: expected 1 argument but received " <> T.show (length args)))
    len <- liftIO (readIORef lenRef)
    when (idx < 0 || idx >= len) $ throwError (IndexError ("remove: cannot remove index " <> T.show idx <> " from array of length " <> T.show len))    
    liftIO (modifyIORef' lenRef (subtract 1))
    vec <- liftIO (readIORef vecRef)
    result <- MV.read vec idx
    let sliceLen = len - idx - 1
        from = MV.unsafeSlice (idx+1) sliceLen vec
        to = MV.unsafeSlice idx sliceLen vec
    MV.unsafeMove from to
    MV.write vec (len-1) ValVoid
    liftIO (MV.write vec idx ValVoid)
    pure result
  val                -> throwError $ TypeError ("pop: expected an array, not '" <> T.show (valType val) <> "'")