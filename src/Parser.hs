module Parser where

import Control.Monad (when)
import Data.List (partition)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Types.Operators
import Types.Types
import Types.Values
import Types.Expressions
import Types.Statements

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

isIdentChar :: Char -> Bool
isIdentChar ch = ch >= 'A' && ch <= 'Z'
              || ch >= 'a' && ch <= 'z'
              || ch >= '0' && ch <= '9'
              || ch == '_'

keywords :: S.Set T.Text
keywords = S.fromList
  [ "let", "var", "fn", "return", "if", "else", "while", "continue", "break"
  , "and", "or", "not", "true", "false", "array", "int", "float", "str", "bool" ]

keyword :: T.Text -> Parser T.Text
keyword kw = lexeme $ try (string kw <* notFollowedBy (satisfy isIdentChar))

semicolon :: Parser T.Text
semicolon = lexeme (string ";") <|> fail "expected semicolon after statement"

ident :: Parser Ident
ident = do
  name <- lookAhead (T.cons <$> letterChar <*> takeWhileP (Just "ident") isIdentChar)
  when (name `S.member` keywords) $ fail (T.unpack ("keyword \"" <> name <> "\" cannot be used as identifier"))
  _ <- lexeme $ T.cons <$> letterChar <*> takeWhileP (Just "ident") isIdentChar
  pure name

type' :: Parser TypeT
type' = lexeme $ choice
  [ IntT    <$ keyword "int"
  , FloatT  <$ keyword "float"
  , StrT    <$ keyword "str"
  , BoolT   <$ keyword "bool"
  , (ArrayT <$ lexeme (keyword "array")) <* lexeme (char '[') <*> lexeme (type') <* char ']' ]

atomicValue :: Parser Value
atomicValue = lexeme $ choice
  [ ValFloat <$> try L.float
  , ValInt <$> L.decimal
  , ValStr . T.pack <$> (char '"' *> literalString <* lexeme (char '"')) 
  , ValBool True <$ keyword "true"
  , ValBool False <$ keyword "false"
  ]
  where
    literalString = many (escBS <|> escDQ <|> escLF <|> escCR <|> escTab <|> escNull <|> normalChar)
    escBS = '\\' <$ string "\\\\"
    escLF = '\n' <$ string "\\n"
    escCR = '\r' <$ string "\\r"
    escDQ = '\"' <$ string "\\\""
    escTab = '\t' <$ string "\\t"
    escNull = '\0' <$ string "\\0"
    normalChar = satisfy (not . (`elem` ("\\\"" :: String)))

operator :: Parser Operator
operator = lexeme $ choice
  [ AssignWith (Just Add) <$ string "+="
  , AssignWith (Just Sub) <$ string "-="
  , AssignWith (Just Mul) <$ string "*="
  , AssignWith (Just Div) <$ string "/="
  , AssignWith (Just Mod) <$ string "%="
  , AssignWith (Just Pow) <$ string "^="
  , Add <$ string "+"
  , Sub <$ string "-"
  , Mul <$ string "*"
  , Div <$ string "/"
  , Mod <$ string "%"
  , Pow <$ string "^"
  , Equals <$ string "=="
  , NotEquals <$ string "!="
  , GTE <$ string ">="
  , LTE <$ string "<="
  , GT' <$ string ">"
  , LT' <$ string "<"
  , AssignWith Nothing <$ string "="
  , Logical And <$ keyword "and"
  , Logical Or <$ keyword "or"
  , Not <$ keyword "not"
  , MemberAccess <$ string "."
  , FnCall <$ string "("
  , Index <$ string "[" ]

-- convenience function for parsing full expressions
expression :: Parser Expr
expression = expr 0 Nothing

expr :: Int -> Maybe Expr -> Parser Expr
expr precedence = \case
  Nothing -> do  -- not holding any value, so get one
    left <- lexeme $ choice
      [ PrefixExpr Neg <$> lexeme (char '-' *> expr (snd (precedenceOf Neg)) Nothing)
      , PrefixExpr Not <$> lexeme (keyword "not" *> expr (snd (precedenceOf Not)) Nothing)
      , lexeme (string "(") *> expression <* lexeme (string ")")  -- parenthesized expression
      , ArrayExpr <$> (lexeme (string "[") *> expression `sepBy` lexeme (string ",") <* lexeme (string "]"))  -- array
      , ValExpr <$> atomicValue
      , IdentExpr <$> ident
      , fail "expected literal / identifier for expression"]
    expr precedence (Just left)
  Just left -> do  -- already holding a value, so get an operator
    optional (lookAhead operator) >>= \case
      Just op -> do
        let leftIsComparison = case left of InfixExpr _ op' _ -> isComparison op'; _ -> False
        when (leftIsComparison && isComparison op) $ fail "comparisons cannot be chained"
        let (precEnter, precAmbient) = precedenceOf op
        if precEnter > precedence then do
          _ <- operator  -- consume the operator
          case op of
            FnCall       -> do
              args <- expr precAmbient Nothing `sepBy` lexeme (string ",") <* lexeme (string ")")
              expr precedence (Just (FnExpr left args))
            Index        -> do
              idx <- expr precAmbient Nothing <* lexeme (string "]")
              expr precedence (Just (IndexExpr left idx))
            MemberAccess -> do
              member <- IdentExpr <$> ident
              expr precedence (Just (InfixExpr left MemberAccess member))
            _            -> do
              right <- expr precAmbient Nothing
              expr precedence (Just (InfixExpr left op right))
        else
          pure left
      Nothing -> pure left
  where
    isComparison = \case Equals -> True; NotEquals -> True; GTE -> True; LTE -> True; GT' -> True; LT' -> True; _ -> False


data AllowBreak = WithBreak | WithoutBreak

-- let <ident>[:<type>] [= <expr>];
varDeclStmt :: Parser Statement
varDeclStmt = lexeme $ VarDeclStmt <$>
  choice [ False <$ keyword "let", True <$ keyword "var" ]
  <*> ident
  <*> optional (lexeme (string ":") *> type')
  <*> optional (lexeme (string "=") *> expression)
  <* semicolon

singleStmt :: AllowBreak -> Parser Statement
singleStmt breakFlag = choice
  [ (blockOf (singleStmt breakFlag))
  , fnDecl
  , ifStmt breakFlag
  , whileStmt
  , varDeclStmt
  , exprStmt
  , ReturnStmt <$ keyword "return" <*> optional expression <* semicolon
  , keyword "break" *> semicolon *> breakOrFail BreakStmt "break"
  , keyword "continue" *> semicolon *> breakOrFail ContinueStmt "continue" ]
  where
    breakAllowed = case breakFlag of WithBreak -> True; WithoutBreak -> False
    breakOrFail stmt name = if breakAllowed then pure stmt else fail (name <> " not allowed outside a loop")

-- { <statement> [<statement>]* }
blockOf :: Parser Statement -> Parser Statement
blockOf p = BlockStmt <$> 
  (lexeme (string "{") *> (hoistFunctions <$> many p) <* lexeme (string "}"))

-- if <cond> <blockOfStmt> [else <statement>]
ifStmt :: AllowBreak -> Parser Statement
ifStmt breakFlag = IfStmt <$> (keyword "if" *> (expression <|> fail "expected 'if' condition"))
                          <*> (blockOf (singleStmt breakFlag) <|> fail "expected block after condition")
                          <*> optional (keyword "else" *> (singleStmt breakFlag <|> fail "expected statement after 'else'"))

-- while <cond> <blockOfStmt>
whileStmt :: Parser Statement
whileStmt = WhileStmt <$> (keyword "while" *> (expression <|> fail "expected 'while' condition"))
                      <*> blockOf (singleStmt WithBreak <|> fail "expected block after condition")

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> expression <* semicolon

fnDecl :: Parser Statement
fnDecl = FnDeclStmt <$> (keyword "fn" *> (ident <|> fail "expected function identifier"))
                    <*> (argList <|> fail "expected argument list")
                    <*> (optional (lexeme (string "->") *> (type' <|> fail "expected function return type")) >>= \case
                          Nothing -> pure VoidT
                          Just t  -> pure t)
                    <*> (blockOf (singleStmt WithoutBreak) <|> fail "expected function body")
  where
    argList = lexeme (string "(") *> ((,) <$> ident <* lexeme (string ":") <*> type') `sepBy` lexeme (string ",") <* lexeme (string ")")

hoistFunctions :: [Statement] -> [Statement]
hoistFunctions stmts = let (fns, code) = partition isFn stmts in fns <> code
  where
    isFn (FnDeclStmt _ _ _ _) = True
    isFn _                    = False

sourceCode :: Parser [Statement]
sourceCode = space *> (hoistFunctions <$> many (singleStmt WithoutBreak)) <* (eof <|> fail "syntax error in statement / function declaration")

parseFileWith :: Parser a -> FilePath -> IO (Either T.Text a)
parseFileWith p file = do
  txt <- T.IO.readFile file
  case parse p "" txt of
    Left err  -> pure $ Left (T.pack (errorBundlePretty err))
    Right x   -> pure $ Right x