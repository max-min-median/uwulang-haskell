module Types.Statements where

import Data.List (intercalate)
import qualified Data.Text as T

import Types.Types
import Types.Expressions

data Statement = VarDeclStmt Bool Ident (Maybe TypeT) (Maybe Expr)
               | FnDeclStmt Ident [(Ident, TypeT)] TypeT Statement
               | IfStmt Expr Statement (Maybe Statement)
               | WhileStmt Expr Statement 
               | ExprStmt Expr
               | BlockStmt [Statement]
               | ReturnStmt (Maybe Expr)
               | BreakStmt
               | ContinueStmt

indent :: Int -> String -> String
indent n s = T.unpack $ spaces <> T.replace "\n" replacement (T.pack s)
  where
    spaces = T.replicate n " "
    replacement = "\n" <> spaces

instance Show Statement where
  show (VarDeclStmt mut id' maybeType maybeExpr) = letOrVar <> T.unpack id' <> typeStr <> exprStr <> ";"
    where
      letOrVar = if mut then "var " else "let "
      typeStr = case maybeType of Just t -> ": " <> show t; _ -> ""
      exprStr = case maybeExpr of Just x -> " = " <> show x; _ -> ""
  show (FnDeclStmt id' args retType body) = "fn " <> T.unpack id' <> "(" <> intercalate ", " (map showArg args) <> ")" <> returnType <> " " <> show body
    where
      returnType = case retType of VoidT -> ""; t -> " -> " <> show t
      showArg (id'',t) = T.unpack id'' <> ": " <> show t
  show (IfStmt cond block maybeElseBody) = "if " <> show cond <> " " <> show block <> elseBody
    where
      elseBody = case maybeElseBody of Just b@(BlockStmt _) -> " else " <> show b; Just stmt -> " else\n    " <> show stmt; _ -> ""
  show (WhileStmt cond block) = "while " <> show cond <> " " <> show block
  show BreakStmt = "break;"
  show ContinueStmt = "continue;"
  show (ReturnStmt maybeExpr) = "return" <> case maybeExpr of Just x -> " " <> show x <> ";"; Nothing -> ";"
  show (BlockStmt stmts) = "{\n" <> unlines (map (indent 4 . show) stmts) <> "}"
  show (ExprStmt x) = show x <> ";"