module Types.Operators where

import Data.Maybe (fromMaybe)
import Data.List (dropWhileEnd)

data LogicalOperator = And | Or deriving (Eq, Show)

data Operator = Neg
              | Add
              | Sub
              | Mul
              | Div
              | Mod
              | Pow
              | AssignWith (Maybe Operator)
              | Not
              | Logical LogicalOperator
              | Equals    | NotEquals
              | GT' | LT' | GTE | LTE
              | MemberAccess
              | Index
              | FnCall
  deriving Eq

instance Show Operator where
  show Neg = "-"
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Mod = " % "
  show Pow = "^"
  show (AssignWith op) = " " <> fromMaybe "" (strip . show <$> op) <> "= "
    where strip = dropWhile (== ' ') . dropWhileEnd (== ' ')
  show (Logical And) = " and "
  show (Logical Or) = " or "
  show Not = "not "
  show Equals = " == "
  show NotEquals = " != "
  show GT' = " > "
  show GTE = " >= "
  show LT' = " < "
  show LTE = " <= "
  show MemberAccess = "."
  show Index = error "Unreachable"
  show FnCall = error "Unreachable"

precedenceOf :: Operator -> (Int, Int)
precedenceOf op = case op of
  Index     -> (12, 0)
  FnCall    -> (12, 0)
  MemberAccess -> (12, 0)
  Pow       -> (11, 10)
  Neg       -> (10, 10)
  Div       -> (8, 8)
  Mul       -> (8, 8)
  Mod       -> (8, 8)
  Sub       -> (7, 7)
  Add       -> (7, 7)
  Equals    -> (6, 6)
  NotEquals -> (6, 6)
  GT'       -> (6, 6)
  GTE       -> (6, 6)
  LT'       -> (6, 6)
  LTE       -> (6, 6)
  Not       -> (5, 5)
  Logical And -> (4, 4)
  Logical Or  -> (3, 3)
  AssignWith _ -> (2, 1)