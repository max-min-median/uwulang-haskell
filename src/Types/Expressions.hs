module Types.Expressions where

import Data.List (intercalate)
import qualified Data.Text as T

import Types.Types
import Types.Values
import Types.Operators

data Expr = ValExpr Value
          | IdentExpr Ident
          | ArrayExpr [Expr]
          | InfixExpr Expr Operator Expr
          | PrefixExpr Operator Expr
          | IndexExpr Expr Expr
          | FnExpr Expr [Expr]

instance Show Expr where
  show (ValExpr x) = show x
  show (IdentExpr x) = T.unpack x
  show (ArrayExpr x) = "[" <> intercalate ", " (map show x) <> "]"
  show (InfixExpr lf op rt) = left <> show op <> right
    where
      (precLf, precOp, precRt) = (exprPrecedence lf, precedenceOf op, exprPrecedence rt)
      left = (if snd precLf < fst precOp then pad '(' ')' else id) $ show lf
      right = (if snd precOp >= fst precRt then pad '(' ')' else id) $ show rt
  show (PrefixExpr op rt) = show op <> right
    where
      (precOp, precRt) = (precedenceOf op, exprPrecedence rt)
      right = (if snd precOp >= fst precRt then pad '(' ')' else id) $ show rt
  show (IndexExpr name idx) = show name <> pad '[' ']' (show idx)
  show (FnExpr name args) = show name <> pad '(' ')' (intercalate ", " (map show args))

exprPrecedence :: Expr -> (Int, Int)
exprPrecedence x = case x of
  InfixExpr _ op _ -> precedenceOf op
  PrefixExpr op _ -> precedenceOf op
  _               -> (maxBound, maxBound)

pad :: Char -> Char -> String -> String
pad x y = (x:) . (++ [y])
