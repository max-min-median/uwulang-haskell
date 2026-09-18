module Env where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock
import qualified Data.Map as M

import Types.Statements
import Types.Types
import Types.Values
import Types.Executor
import Control.Applicative (asum)
import Control.Monad (when)

-- data StackFrame = StackFrame { frameVariables :: M.Map Ident Variable, frameFns :: M.Map Ident Function }
-- type Stack = [IORef StackFrame]
-- data Env = Env { startTime :: UTCTime, replMode :: Bool, stackRef :: IORef Stack }

freshEnv :: Bool -> IO Env
freshEnv replMode' = Env <$> getCurrentTime <*> pure replMode' <*> (newIORef emptyFrame >>= \frame -> newIORef [frame])

emptyFrame :: StackFrame
emptyFrame = StackFrame M.empty M.empty

getStack :: Executor Stack
getStack = asks stackRef >>= liftIO . readIORef

putStack :: Stack -> Executor ()
putStack stk = asks stackRef >>= \ref -> liftIO (writeIORef ref stk)

newFrame :: Executor ()
newFrame = asks stackRef >>= \ref -> liftIO (newIORef emptyFrame >>= modifyIORef' ref . (:))

popFrame :: Executor ()
popFrame = getStack >>= \case
  [_]        -> throwError $ MiscError ("cannot pop last remaining stack frame!")
  (_:frames) -> putStack frames
  []         -> error "Unreachable"

searchFrameWith :: (StackFrame -> M.Map Ident a) -> Ident -> StackFrame -> Maybe a
searchFrameWith selector id' = M.lookup id' . selector

getLocalFrameRef :: Executor (IORef StackFrame)
getLocalFrameRef = getStack >>= \case
    []                -> throwError $ MiscError ("stack is empty!")
    (localFrameRef:_) -> pure localFrameRef

getLocalFrame :: Executor StackFrame
getLocalFrame = getLocalFrameRef >>= liftIO . readIORef

ensureNotInLocalsWith :: (StackFrame -> M.Map Ident a) -> Ident -> Executor ()
ensureNotInLocalsWith selector id' = getLocalFrame >>= \frame -> case searchFrameWith selector id' frame of
  Nothing -> pure ()
  Just _  -> throwError $ VarError ("'" <> id' <> "' is already defined in this scope")

newLocal :: Ident -> Variable -> Executor ()
newLocal id' var = do
  localRef <- getLocalFrameRef
  frame <- liftIO (readIORef localRef)
  let updatedFrame = frame { frameVars = M.insert id' var (frameVars frame) }
  liftIO (writeIORef localRef updatedFrame)

newFn :: Ident -> Statement -> Executor ()
newFn id' stmt = do
  replMode' <- asks replMode
  when (not replMode') $ ensureNotInLocalsWith frameFns id'
  localRef <- getLocalFrameRef
  frame <- liftIO (readIORef localRef)
  let updatedFrame = frame { frameFns = M.insert id' stmt (frameFns frame) }
  liftIO (writeIORef localRef updatedFrame)

searchStackWith :: (StackFrame -> M.Map Ident a) -> Ident -> Executor a
searchStackWith selector id' = do
  frameRefs <- getStack
  hits <- mapM (\ref -> liftIO (readIORef ref) >>= pure . searchFrameWith selector id') frameRefs
  case asum hits of
    Nothing  -> throwError $ IdNotFoundError ("'" <> id' <> "' not found in stack frame")
    Just var -> pure var