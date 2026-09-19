module Types.Executor where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.Text as T

import Types.Types
import Types.Values
import Types.Statements

data StackFrame = StackFrame { frameVars :: M.Map Ident Variable, frameFns :: M.Map Ident Statement }
type Stack = [IORef StackFrame]

data ExecResult = ExecOK | Continue | Break | Return Value deriving Show
data Error = TypeError T.Text | DivisionByZero T.Text | VarError T.Text | IndexError T.Text | ArityMismatch T.Text
           | IdNotFoundError T.Text | MiscError T.Text | ParseError T.Text | FileError T.Text
           | ExitError !Int deriving (Eq, Show)

data Env = Env { startTime :: UTCTime, replMode :: Bool, stackRef :: IORef Stack }

type Executor = ReaderT Env (ExceptT Error IO)