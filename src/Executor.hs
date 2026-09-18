module Executor where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Fixed
import Data.IORef
import Data.Maybe
import System.Console.Haskeline
import Text.Megaparsec
import qualified Data.Map as M
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Types.Executor
import Types.Statements
import Types.Types
import Types.Operators
import Types.Expressions
import Types.Values

import Values
import Env
import Parser
import Builtins

runExecutor :: Bool -> Executor a -> IO (Either Error a)
runExecutor replMode' f = freshEnv replMode' >>= runExceptT . runReaderT f >>= \case
  left@(Left err) -> case err of
    ExitError x   -> putStrLn ("Exited with code " <> show x) >> pure left
    _             -> putStrLn (show err) >> pure left
  right@(Right _) -> pure right

runREPL :: IO (Either Error a)
runREPL = do
  putStrLn "Welcome to 'uwulang' v0.1.0.0 --- (c) max-min-median 2026"
  runExecutor True (runInputT defaultSettings repl)
  where
    repl = forever $ do
      getInputLine "> " >>= \case
        Just txt -> lift (parseAndRun (T.pack txt) `catchError` (liftIO . print))
        Nothing  -> lift (throwError (MiscError "Quit"))

parseAndRun :: T.Text -> Executor ()
parseAndRun = parse sourceCode "" >>> \case
  Left err    -> liftIO (putStrLn (errorBundlePretty err))
  Right stmts -> do
      liftIO (putStr "Parsed: ")
      forM_ stmts $ liftIO . putStrLn . show
      forM_ stmts runStatement

runProgram :: [Statement] -> Executor ()
runProgram stmts = foldM runEach ExecOK stmts >>= processResult
  where
    runEach = \cases
      ExecOK stmt -> runStatement stmt
      otherRet _  -> pure otherRet
    processResult = \case
      ExecOK   -> liftIO $ putStrLn "Execution OK (TODO: add time taken)"
      Continue -> liftIO $ putStrLn "Unexpected 'continue' reached... Terminating"
      Break    -> liftIO $ putStrLn "Unexpected 'break' reached... Terminating"
      Return _ -> liftIO $ putStrLn "Unexpected 'return' reached... Terminating"

runStatement :: Statement -> Executor ExecResult
runStatement = \case
  VarDeclStmt mut id' maybeType maybeExpr -> do  -- | Parser guarantees either type or expr will be provided
    ensureNotInLocalsWith frameVars id'
    when (isNothing maybeType && isNothing maybeExpr) $ throwError (TypeError "'let'/'var' must supply either type or value")
    val <- case maybeExpr of
      Just expr' -> evalFull expr' >>= \case
        ValVoid -> throwError $ TypeError ("<void> cannot be assigned to variables")
        val     -> pure val
      Nothing    -> pure ValVoid
    let t = fromMaybe (valType val) maybeType
    when (innerType t == UndefinedT) $ throwError (TypeError "type must be supplied for empty arrays (or empty nested arrays)")
    ref <- coerce (Just mut) t val >>= liftIO . newIORef
    newLocal id' (Variable id' mut t ref)
    pure ExecOK

  fn@(FnDeclStmt id' _ _ _) -> do
    when (id' `M.member` builtins) $ throwError (IdNotFoundError ("Function '" <> id' <> "' is a built-in and cannot be redefined"))
    newFn id' fn >> pure ExecOK  -- | Register a new function in the local scope. Redefinition is allowed only in REPL mode.
    
  ExprStmt expr' -> do
    Env _ replMode' stackRef' <- ask
    stack <- liftIO (readIORef stackRef')
    result <- evalFull expr'
    when (replMode' && case stack of [_] -> True; _ -> False) $ liftIO (putStrLn (show result <> ": " <> show (valType result))) `catchError` eHandler
    pure ExecOK

  IfStmt cond blockStmt maybeElseStmt -> evalFull cond >>= checkValTrue >>= \p ->
    if p then runStatement blockStmt
    else case maybeElseStmt of
      Nothing       -> pure ExecOK
      Just elseStmt -> runStatement elseStmt

  WhileStmt cond blockStmt ->
    let loop = evalFull cond >>= checkValTrue >>= \p ->
          if p then runStatement blockStmt >>= \case
            ExecOK   -> loop
            Continue -> loop
            result   -> pure result
          else pure ExecOK
    in loop

  BlockStmt stmts -> do
    newFrame
    let stepBlock = \cases
          ExecOK stmt -> runStatement stmt
          result _    -> pure result
    result <- foldM stepBlock ExecOK stmts
    popFrame
    pure result

  BreakStmt -> pure Break
  ContinueStmt -> pure Continue
  ReturnStmt ret -> case ret of
    Nothing    -> pure $ Return ValVoid
    Just expr' -> evalFull expr' >>= pure . Return

  where
    -- only for debugging / REPL
    eHandler err = liftIO (print err)

eval :: Expr -> Executor Value
eval = \case
  ValExpr val -> pure val
  IdentExpr id' -> searchStackWith frameVars id' >>= pure . ValRef
  ArrayExpr exprList -> do  -- literal array
    (t,arr) <- foldM evalArrayStep (UndefinedT,[]) exprList
    vec <- liftIO (V.thaw (V.fromList (reverse arr)))
    vecRef <- liftIO (newIORef vec)
    lenRef <- liftIO (newIORef (MV.length vec))
    let val = ValArray True t lenRef vecRef
    setMutable True val
  InfixExpr expr1 op expr2 -> case op of
    AssignWith maybeSubOp -> case expr1 of
      IndexExpr arrExpr idxExpr -> do
        arr <- evalFull arrExpr
        idx <- evalFull idxExpr
        val <- case maybeSubOp of
          Nothing    -> evalFull expr2
          Just subOp -> evalInfix Index arr idx >>= \v1 -> evalFull expr2 >>= evalInfix subOp v1
        modifyArr arr idx val
      IdentExpr id' -> do
        var <- searchStackWith frameVars id'
        val <- case maybeSubOp of
          Nothing    -> evalFull expr2
          Just subOp -> evalFull expr1 >>= \v1 -> evalFull expr2 >>= evalInfix subOp v1
        assignVar var val
      _           -> throwError $ TypeError "expected l-value for assignment"
    Logical logicOp -> do  -- special-cased due to short-circuiting
      v1 <- evalFull expr1
      case (v1, logicOp) of
        (ValBool True, Or)   -> pure v1
        (ValBool False, And) -> pure v1
        (val, _) | valType val /= BoolT -> throwError (TypeError ("cannot perform '" <> T.show logicOp <> "' on operand of type '" <> T.show (valType val) <> "'"))
        _                    -> do
          v2 <- evalFull expr2
          case (v1, v2, logicOp) of
            (ValBool x, ValBool y, And) -> pure $ ValBool (x && y)
            (ValBool x, ValBool y, Or)  -> pure $ ValBool (x || y)
            _                           -> throwError $ TypeError ("cannot perform '" <> T.show logicOp <> "' between types '" <> T.show (valType v1)  <> "' and '" <> T.show (valType v2) <> "'")
    _                     -> evalFull expr1 >>= \v1 -> evalFull expr2 >>= \v2 -> evalInfix op v1 v2
  PrefixExpr Neg expr1 -> evalFull expr1 >>= \case
    ValFloat x -> pure $ ValFloat (-x)
    ValInt x   -> pure $ ValInt (-x)
    val        -> throwError $ TypeError ("cannot negate operand of type '" <> T.show (valType val) <> "'")
  PrefixExpr Not expr1 -> evalFull expr1 >>= \case
    ValBool x -> pure $ ValBool (not x)
    val       -> throwError $ TypeError ("cannot perform logical 'not' on operand of type '" <> T.show (valType val) <> "'")
  PrefixExpr _ _ -> error "Unreachable"
  IndexExpr arrExpr idxExpr -> do
    idx <- evalFull idxExpr >>= toArrayIndex
    evalFull arrExpr >>= \case
      ValArray _ _ lenRef vecRef -> do
        len <- liftIO (readIORef lenRef)
        vec <- liftIO (readIORef vecRef)
        when (idx < 0 || idx >= len) $ throwError (IndexError ("array of length " <> T.show len <> " has no index " <> T.show idx))
        liftIO (MV.read vec idx)
      ValStr s              -> case T.take 1 . T.drop idx $ s of
        T.Empty -> throwError (IndexError ("string of length " <> T.show (T.length s) <> " has no index " <> T.show idx))
        txt     -> pure $ ValStr txt
      val                   -> throwError (TypeError ("cannot index into '" <> T.show val <> "': not an array"))
  FnExpr (InfixExpr objExpr MemberAccess (IdentExpr method)) args
    | method `M.member` methods -> evalFull objExpr >>= \obj -> mapM evalFull args >>= (methods M.! method) obj
  FnExpr (IdentExpr id') args
    | id' `M.member` builtins -> mapM evalFull args >>= builtins M.! id'
  FnExpr funcIdExpr args -> do  -- expects IdentExpr as first operand, but looks it up in `fns` rather than `getVarFromIdent`
    funcId <- case funcIdExpr of
      IdentExpr id' -> pure id'
      _             -> throwError $ TypeError ("'" <> T.show funcIdExpr <> "' is not a callable function")
    searchStackWith frameFns funcId >>= \case
      FnDeclStmt _ params returnType fnBody -> do
        when (length params /= length args) $ throwError $ ArityMismatch (funcId <> ": expected " <> T.show (length params) <> " arguments but received " <> T.show (length args))
        newFrame
        let makeArg (paramId, paramType) argExpr = do
              ref <- evalFull argExpr >>= coerce Nothing paramType >>= liftIO . newIORef
              newLocal paramId (Variable paramId True paramType ref)
        zipWithM_ makeArg params args
        val <- runStatement fnBody >>= \case
          ExecOK     -> pure ValVoid
          Return val -> pure val
          Break      -> throwError $ MiscError (funcId <> ": illegal 'break' in function body")
          Continue   -> throwError $ MiscError (funcId <> ": illegal 'continue' in function body")
        popFrame
        coerce Nothing returnType val `catchError` const (throwError $ TypeError (funcId <> ": unable to coerce " <> T.show val <> " to return type '" <> T.show returnType <> "'"))
      _                                            -> throwError $ IdNotFoundError ("function '" <> funcId <> "' does not exist")

evalVariable :: Variable -> Executor Value
evalVariable (Variable {varName = name, varRef = ref}) = do
  liftIO (readIORef ref) >>= \case
    ValVoid -> throwError (VarError ("'" <> name <> "' has not been initialized"))
    val     -> pure val

assignVar :: Variable -> Value -> Executor Value
assignVar (Variable name mut t ref) val = do
  liftIO (readIORef ref) >>= \case
    ValVoid -> pure ()
    _       -> when (not mut) $ throwError (TypeError ("immutable variable '" <> name <> "' has already been assigned"))
  newVal <- coerce (Just mut) t val
  liftIO (writeIORef ref newVal)
  pure newVal

evalFull :: Expr -> Executor Value
evalFull expr' = eval expr' >>= \case
  ValRef var -> evalVariable var
  val        -> pure val

-- | Builds the array an element at a time. Keep track of the current inner type. If a stricter inner type is found, coerce all
-- accumulated values to this stricter type.
-- Literal arrays are considered mutable.
evalArrayStep :: (TypeT, [Value]) -> Expr -> Executor (TypeT, [Value])
evalArrayStep (t, valList) expr' = do
  val <- evalFull expr'
  case (t, valType val) of
    (t1, t2) | t1 == t2                 -> pure (t1, val:valList)
             | t2 `coercibleTo` t1      -> coerce (Just True) t1 val >>= \newVal -> pure (t1, newVal:valList)
             | t1 `coercibleTo` t2      -> mapM (coerce (Just True) t2) valList >>= \newList -> pure (t2, val:newList)
             | otherwise                -> throwError $ TypeError ("array cannot contain both '" <> T.show t1 <> "' and '" <> T.show t2 <> "'")

evalInfix :: Operator -> Value -> Value -> Executor Value
evalInfix op v1 v2 = case op of
  Add -> case (v1, v2) of
    (ValStr s1, ValStr s2) -> pure $ ValStr (s1 <> s2)
    _                      -> ensureNums >> arithmetic (+) (+)
  Sub -> ensureNums >> arithmetic (-) (-)
  Mul -> ensureNums >> arithmetic (*) (*)
  Div -> do
    ensureNums
    when (valIsZero v2) $ throwError (DivisionByZero ("cannot divide " <> T.show v1 <> " by zero"))
    arithmetic div (/)
  Mod -> do
    ensureNums
    when (valIsZero v2) $ throwError (DivisionByZero ("cannot modulo " <> T.show v1 <> " by zero"))
    case coerceNumPair v1 v2 of
      IntPair x y   -> pure $ ValInt (x `mod` y)
      FloatPair x y -> pure $ ValFloat (x `mod'` y)
  Pow -> do
    ensureNums
    case (v1, v2) of
      (ValInt x, ValInt y)     -> pure $ ValInt (x ^ y)
      (ValInt x, ValFloat y)   -> pure $ ValFloat (fromIntegral x ** y)
      (ValFloat x, ValInt y)   -> pure $ ValFloat (x ^^ y)
      (ValFloat x, ValFloat y) -> pure $ ValFloat (x ** y)
      _                        -> error "Unreachable"
  Equals    -> comparison v1 v2 >>= pure . ValBool . (== EQ)
  NotEquals -> comparison v1 v2 >>= pure . ValBool . (/= EQ)
  GT'       -> comparison v1 v2 >>= pure . ValBool . (> EQ)
  GTE       -> comparison v1 v2 >>= pure . ValBool . (>= EQ)
  LT'       -> comparison v1 v2 >>= pure . ValBool . (< EQ)
  LTE       -> comparison v1 v2 >>= pure . ValBool . (<= EQ)
  _   -> error "Unreachable"
  where
    genericTypeError = throwError $ TypeError ("cannot perform '" <> T.strip (T.show op) <> "' between types '" <> T.show (valType v1)  <> "' and '" <> T.show (valType v2) <> "'")
    ensureNums = unless (valIsNum v1 && valIsNum v2) genericTypeError

    arithmetic :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Executor Value
    arithmetic intFn floatFn = case coerceNumPair v1 v2 of
        IntPair x y   -> pure $ ValInt (x `intFn` y)
        FloatPair x y -> pure $ ValFloat (x `floatFn` y)

    comparison :: Value -> Value -> Executor Ordering
    comparison = \cases
      (ValStr x) (ValStr y)                 -> pure $ compare x y
      (ValBool x) (ValBool y)               -> pure $ compare x y
      (ValArray _ _ _ x) (ValArray _ _ _ y) -> do
        vec1 <- liftIO (readIORef x)
        vec2 <- liftIO (readIORef y)
        compareVectors vec1 vec2
      val1 val2 | valIsNum val1 && valIsNum val2 -> case coerceNumPair val1 val2 of
        IntPair x y   -> pure $ compare x y
        FloatPair x y -> pure $ compare x y
                | otherwise                      -> genericTypeError
      where
        compareVectors vec1 vec2 = do
          foldM step EQ [0 .. min (MV.length vec1) (MV.length vec2) - 1] >>= \case
            EQ -> pure $ compare (MV.length vec1) (MV.length vec2)
            LT -> pure LT
            GT -> pure GT
          where
            step EQ i = MV.read vec1 i >>= \elem1 -> MV.read vec2 i >>= \elem2 -> comparison elem1 elem2
            step x _ = pure x

printStack :: Executor ()
printStack = do
  frameRefs <- getStack
  forM_ frameRefs $ \frameRef -> do
    frame <- liftIO (readIORef frameRef)
    let vars = M.toList $ frameVars frame
        fns  = M.toList $ frameFns frame
    liftIO $ putStrLn "Variables:"
    mapM (\(id',var) -> evalVariable var >>= \val -> pure (id' <> " = " <> T.show val)) vars >>= liftIO . TIO.putStrLn . T.intercalate ", "
    liftIO $ putStrLn "Functions:"
    forM_ fns (\(_, fn) -> liftIO (putStrLn (show fn)))
    liftIO $ putStrLn ""
