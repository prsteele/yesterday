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

parseFunction :: Parser Function
parseFunction = undefined

parseFile :: FilePath -> Parser a -> IO a
parseFile fname parser = do
  contents <- TIO.readFile fname
  let result = runParser parser fname contents
  case result of
    Left err -> putStrLn (errorBundlePretty err) >> exitFailure
    Right answer -> pure answer

f :: IO Function
f = parseFile "Whatever.time" parseFunction
