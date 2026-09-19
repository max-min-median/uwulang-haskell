module Types.Types where

import qualified Data.Text as T

type Ident = T.Text

data TypeT = IntT | FloatT | StrT | BoolT | ArrayT TypeT | VoidT | UndefinedT deriving Eq

instance Show TypeT where
  show IntT = "int" 
  show FloatT = "float" 
  show StrT = "str" 
  show BoolT = "bool" 
  show (ArrayT t) = "array[" <> show t <> "]"
  show VoidT = "void"
  show UndefinedT = "<T?>"

innerType :: TypeT -> TypeT
innerType = \case
  (ArrayT t) -> innerType t
  t          -> t

coercibleTo :: TypeT -> TypeT -> Bool
coercibleTo = \cases
  _ UndefinedT            -> False
  UndefinedT _            -> True
  IntT FloatT             -> True
  (ArrayT t1) (ArrayT t2) -> t1 `coercibleTo` t2
  t1 t2 | t1 == t2        -> True
  _ _                     -> False