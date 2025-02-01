module Yesterday.Parser where

import Data.Char (isSpace)
import Data.Functor
import qualified Data.Text as T
import Data.Void
import System.Exit
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer as L

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
