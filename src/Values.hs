module Values where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector.Strict.Mutable as MV

import Types.Types
import Types.Values
import Types.Executor
import Data.Maybe (fromMaybe)

toArrayIndex :: Value -> Executor Int
toArrayIndex = \case
  ValInt x -> pure x
  val      -> throwError $ TypeError ("index should be of type 'int', not '" <> T.show (valType val) <> "'")

modifyArr :: Value -> Value -> Value -> Executor Value
modifyArr arr idx val = do
  let vt = valType val
  case arr of
    ValArray mut t lenRef vecRef
      | not mut                  -> throwError $ TypeError "modifyArr: cannot mutate immutable array"
      | not (vt `coercibleTo` t) -> throwError $ TypeError ("modifyArr: cannot place value of type '" <> T.show vt <> "' into array containing type '" <> T.show t <> "'")
      | otherwise                -> do
        i <- toArrayIndex idx
        len <- liftIO (readIORef lenRef)
        vec <- liftIO (readIORef vecRef)
        when (i >= len) $ throwError (IndexError ("modifyArr: cannot modify index " <> T.show i <> " for array of length " <> T.show len))
        coerce (Just mut) t val >>= \newVal -> MV.write vec i newVal >> pure newVal -- TODO
    _ -> throwError $ TypeError ("modifyArr: cannot modify '" <> T.show val <> "': not an array")

-- cloneValue :: (Maybe Bool) -> Value -> Executor Value
-- cloneValue maybeMut = \case
--   ValArray mut t lenRef vecRef -> do
--     clone <- liftIO (readIORef vecRef) >>= MV.clone
--     len <- liftIO (readIORef lenRef)
--     newLenRef <- liftIO (readIORef lenRef >>= newIORef)
--     newVecRef <- liftIO (newIORef clone)
--     forM_ [0 .. len - 1] $ MV.modifyM clone (cloneValue maybeMut)
--     pure $ ValArray (fromMaybe mut maybeMut) t newLenRef newVecRef
--   val                      -> pure val

-- | Mainly for coercing 'int' to 'float', as well as arrays of type 'UndefinedT' (because they are empty) to a more
-- concrete type. This function never mutates the provided argument.
coerce :: Maybe Bool -> TypeT -> Value -> Executor Value
coerce maybeMut = \cases
  UndefinedT _             -> throwError $ TypeError ("cannot coerce to '" <> T.show UndefinedT <> "'")
  VoidT ValVoid            -> pure ValVoid
  t     ValVoid            -> throwError $ TypeError ("could not coerce 'void' to '" <> T.show t <> "'")
  -- t (ValRef (Variable _ _ _ ref)) -> liftIO (readIORef ref) >>= coerce t
  FloatT (ValInt x)        -> pure $ ValFloat (fromIntegral x)
  (ArrayT innerT) val@(ValArray mut elemT lenRef vecRef) -> do
    let finalMut = fromMaybe mut maybeMut
    if (innerT == UndefinedT || elemT == innerT) && (mut == finalMut) then pure val
    else do
      len <- liftIO (readIORef lenRef)
      newLenRef <- liftIO (newIORef len)
      newVec <- liftIO (readIORef vecRef >>= MV.clone)
      forM_ [0 .. len - 1] (MV.modifyM newVec (coerce maybeMut innerT))
      newVecRef <- liftIO (newIORef newVec)
      pure (ValArray finalMut innerT newLenRef newVecRef)
  t val | valType val == t -> pure val
        | otherwise        -> throwError $ TypeError ("could not coerce '" <> T.show val {- T.show (valType val) -} <> "' to '" <> T.show t <> "'")

data NumPair = IntPair Int Int | FloatPair Double Double deriving Show

-- | Assumes arguments are either ValInt or ValFloat
coerceNumPair :: Value -> Value -> NumPair
coerceNumPair v1 v2 = case (v1, v2) of
  (ValInt x, ValInt y)     -> IntPair x y
  (ValInt x, ValFloat y)   -> FloatPair (fromIntegral x) y
  (ValFloat x, ValInt y)   -> FloatPair x (fromIntegral y)
  (ValFloat x, ValFloat y) -> FloatPair x y
  _                        -> error "Unreachable"

checkValTrue :: Value -> Executor Bool
checkValTrue = \case
  ValBool True  -> pure True
  ValBool False -> pure False
  val           -> throwError $ TypeError ("expected condition to be a boolean, not '" <> T.show val <> "'")

setMutable :: Bool -> Value -> Executor Value
setMutable mut = \case
  ValArray _ t lenRef vecRef -> do
    vec <- liftIO (readIORef vecRef)
    len <- liftIO (readIORef lenRef)
    forM_ [0 .. len-1] (MV.modifyM vec (setMutable mut))
    pure $ ValArray mut t lenRef vecRef
  val                        -> pure val