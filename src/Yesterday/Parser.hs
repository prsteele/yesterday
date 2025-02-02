module Yesterday.Parser where

import Data.Char (isSpace)
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import System.Exit
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer as L
import Yesterday.Types

type Parser = Parsec Void T.Text

parse :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text Void) a
parse p = runParser p "<string>"

parseIO :: Parser a -> T.Text -> IO a
parseIO p x = case parse p x of
  Left err -> putStrLn (errorBundlePretty err) >> exitFailure
  Right result -> pure result

integer :: Parser Integer
integer = L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (pure ()) L.decimal

double :: Parser Double
double = L.float

word :: Parser T.Text
word = T.pack <$> many alphaNumChar

linesOf :: Parser a -> Parser [a]
linesOf p = do
  inits <- many (try (p <* eol))
  final <- (eof $> []) <|> fmap pure (try p) <|> pure []
  pure (inits ++ final)

notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

identifier :: Parser T.Text
identifier = T.append <$> (T.singleton <$> letterChar) <*> (T.pack <$> many alphaNumChar)

clause :: Parser Clause
clause = do
  predicate <- expr
  acts <- many action
  pure $ Clause predicate acts

expr :: Parser Expr
expr = exprCall <|> exprLit
  -- TODO: exprGroup is `(` expr `)` to reset precedence

exprLit :: Parser Expr
exprLit = ELit <$> choice
  [ exprAnyLit
  , exprBoolLit
  , exprIntegerLit
  , exprStringLit
  , exprVar -- Last because I don't want to really implement keywords
  ]

exprCall :: Parser Expr
exprCall = do
  callee <- expr
  args <- many (space1 *> expr)
  pure $ ECall callee args

exprVar :: Parser Value
exprVar = do
  Var . Variable <$> identifier

exprAnyLit :: Parser Value
exprAnyLit = chunk "_" <* space1 $> AnyLit

exprBoolLit :: Parser Value
exprBoolLit =
  choice [
    chunk "true" $> BoolLit True,
    chunk "false" $> BoolLit False
  ]

exprIntegerLit :: Parser Value
exprIntegerLit = IntegerLit <$> integer

exprStringLit :: Parser Value
exprStringLit = do
  StringLit <$> (quote *> innerChars <* quote)

quote :: Parser T.Text
quote = T.singleton <$> char '"'

innerChars :: Parser T.Text
innerChars = T.pack <$> many (satisfy (not . \c -> c == '"'))

action :: Parser Action
action = undefined

parseFunction :: T.Text -> Parser Function
parseFunction name = do
  inputs <- many (try (identifier <* space1))
  _ <- MC.space
  _ <- chunk "->"
  _ <- space1
  output <- identifier
  _ <- MC.space
  _ <- eol

  arms <- linesOf clause

  pure Function {
    funName = name,
    parameters = map (\n -> Variable { varName = n }) inputs,
    result = Variable { varName = output },
    clauses = arms
  }

parseFile :: FilePath -> Parser a -> IO a
parseFile fname parser = do
  contents <- TIO.readFile fname
  let result = runParser parser fname contents
  case result of
    Left err -> putStrLn (errorBundlePretty err) >> exitFailure
    Right answer -> pure answer
