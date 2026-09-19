module Types.Values where

import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import System.IO.Unsafe (unsafePerformIO)

import Types.Types
import Control.Monad.Reader (MonadIO(liftIO))
import Data.List (intercalate)

data Value = ValInt !Int | ValFloat !Double | ValStr !T.Text | ValBool !Bool | ValArray !Bool !TypeT (IORef Int) (IORef (MV.IOVector Value)) | ValRef Variable | ValVoid

data Variable = Variable { varName :: !Ident, varMutable :: !Bool, varType :: !TypeT, varRef :: IORef Value }

instance Show Value where
  show (ValInt x) = show x
  show (ValFloat x) = show x
  show (ValStr x) = show x
  show (ValBool True) = "true"
  show (ValBool False) = "false"
  show (ValArray mut _ lenRef vecRef) = unsafePerformIO (liftIO showVec)
    where
      showVec :: IO String
      showVec = do
        let mutChar = if mut then 'm' else 'i'
        len <- readIORef lenRef
        vec <- readIORef vecRef
        v' <-  V.freeze . MV.unsafeSlice 0 len $ vec
        pure $ mutChar:"[" <> intercalate ", " (V.toList (V.map show v')) <> "]"
  show (ValRef (Variable { varRef = ref })) = show . unsafePerformIO . liftIO $ readIORef ref
  show ValVoid = "<void>"

valType :: Value -> TypeT
valType = \case
  ValInt _       -> IntT
  ValFloat _     -> FloatT
  ValStr _       -> StrT
  ValBool _      -> BoolT
  ValArray _ t _ _ -> ArrayT t
  ValRef (Variable { varType = t }) -> t
  ValVoid        -> VoidT

valIsZero :: Value -> Bool
valIsZero = \case
  ValInt 0     -> True
  ValFloat 0.0 -> True
  _            -> False

valIsNum :: Value -> Bool
valIsNum = \case
  ValInt _   -> True
  ValFloat _ -> True
  _          -> False

valIsArray :: Value -> Bool
valIsArray = \case
  ValArray _ _ _ _ -> True
  ValRef (Variable { varType = t }) -> case t of
    ArrayT _ -> True
    _        -> False
  _              -> False

valToText :: Value -> T.Text
valToText = \case
  ValStr s -> s
  val      -> T.show val