module Yesterday.Parser where

import Data.Bifunctor
import Data.Char (isSpace)
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L
import Yesterday.Types

type Parser = Parsec Void T.Text

parse :: FilePath -> Parser a -> T.Text -> Either T.Text a
parse fname p = first (T.pack . errorBundlePretty) . runParser p fname

parseFile :: FilePath -> Parser a -> IO (Either T.Text a)
parseFile fname parser = parse fname parser <$> TIO.readFile fname

integer :: Parser Integer
integer = L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (pure ()) L.decimal

double :: Parser Double
double = L.float

word :: Parser T.Text
word = T.pack <$> many MC.alphaNumChar

linesOf :: Parser a -> Parser [a]
linesOf p = do
  inits <- many (try (p <* MC.eol))
  final <- (eof $> []) <|> fmap pure (try p) <|> pure []
  pure (inits ++ final)

notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

identifier :: Parser T.Text
identifier = T.append <$> (T.singleton <$> MC.letterChar) <*> (T.pack <$> many MC.alphaNumChar)

clause :: Parser Clause
clause = Clause <$> expr <*> many expr

clause' :: Parser Clause
clause' = Clause <$> expr <*> (chunk "::" *> sepBy expr (chunk ","))

expr :: Parser Expr
expr =
  choice
    [ -- matchParen,
      -- andExpr,
      -- orExpr,
      -- multExpr,
      -- divExpr,
      -- addExpr,
      -- subExpr,
      -- eqExpr,
      -- notEqExpr,
      -- ltExpr,
      -- leExpr,
      -- gtExpr,
      -- geExpr,
      -- getsExpr,
      -- plusGetsExpr,
      -- callExpr,
      histOpExpr,
      derefExpr,
      bangExpr,
      nullExpr,
      interrobangExpr,
      litExpr
    ]

matchParen :: Parser Expr
matchParen = chunk "(" *> expr <* chunk ")"

whitespaceChunk :: T.Text -> Parser T.Text
whitespaceChunk c = MC.hspace *> chunk c <* MC.hspace

andExpr :: Parser Expr
andExpr = EAnd <$> try expr <* whitespaceChunk "&" <*> expr

orExpr :: Parser Expr
orExpr = EOr <$> try expr <* whitespaceChunk "|" <*> expr

multExpr :: Parser Expr
multExpr = EMul <$> try expr <* whitespaceChunk "*" <*> expr

divExpr :: Parser Expr
divExpr = EDiv <$> try expr <* whitespaceChunk "/" <*> expr

addExpr :: Parser Expr
addExpr = EAdd <$> try expr <* whitespaceChunk "+" <*> expr

subExpr :: Parser Expr
subExpr = ESub <$> try expr <* whitespaceChunk "-" <*> expr

eqExpr :: Parser Expr
eqExpr = EEq <$> try expr <* whitespaceChunk "=" <*> expr

notEqExpr :: Parser Expr
notEqExpr = EEq <$> try expr <* whitespaceChunk "!=" <*> expr

ltExpr :: Parser Expr
ltExpr = ELT <$> try expr <* whitespaceChunk "<" <*> expr

leExpr :: Parser Expr
leExpr = ELE <$> try expr <* whitespaceChunk "<=" <*> expr

gtExpr :: Parser Expr
gtExpr = EGT <$> try expr <* whitespaceChunk ">" <*> expr

geExpr :: Parser Expr
geExpr = EGE <$> try expr <* whitespaceChunk ">=" <*> expr

getsExpr :: Parser Expr
getsExpr = EGets <$> try expr <* whitespaceChunk "<-" <*> expr

plusGetsExpr :: Parser Expr
plusGetsExpr = EPlusGets <$> try expr <* whitespaceChunk "<+" <*> expr

callExpr :: Parser Expr
callExpr = do
  callee <- expr
  args <- some (MC.space1 *> expr)
  pure $ ECall callee args

parentSuffix :: Parser HistOp
parentSuffix = do
  _ <- chunk "~"
  ex <- optional expr
  case ex of
    Nothing -> pure (HistParent (ELit (IntegerLit 1)))
    Just x -> pure (HistParent x)

childSuffix :: Parser HistOp
childSuffix = do
  _ <- chunk "^"
  ex <- optional expr
  case ex of
    Nothing -> pure (HistChild (ELit (IntegerLit 1)))
    Just x -> pure (HistChild x)

histOpExpr :: Parser Expr
histOpExpr = EHist <$> try expr <*> some (parentSuffix <|> childSuffix)

derefExpr :: Parser Expr
derefExpr = chunk "@" *> MC.space *> (EDeref <$> expr)

bangExpr :: Parser Expr
bangExpr = try (chunk "!" *> MC.space *> (EHistLit <$> expr))

nullExpr :: Parser Expr
nullExpr = try (chunk "null") *> MC.space1 *> (ENull <$> varExpr)

interrobangExpr :: Parser Expr
interrobangExpr = chunk "!?" *> MC.space *> (EInterrobang <$> expr)

-- parentExpr :: Parser Expr
-- parentExpr = EHist <$> try expr <* chunk "~" <*> many histOp
--   where
--     histOp =

-- TODO: exprGroup is `(` expr `)` to reset precedence

litExpr :: Parser Expr
litExpr =
  ELit
    <$> choice
      [ exprBoolLit,
        exprIntegerLit,
        exprStringLit,
        fmap Var varExpr -- Last because I don't want to really implement keywords
      ]

varExpr :: Parser Variable
varExpr = do
  Variable <$> identifier

exprBoolLit :: Parser Value
exprBoolLit =
  choice
    [ chunk "true" $> BoolLit True,
      chunk "false" $> BoolLit False
    ]

exprIntegerLit :: Parser Value
exprIntegerLit = IntegerLit <$> integer

exprStringLit :: Parser Value
exprStringLit = do
  StringLit <$> (quote *> innerChars <* quote)

quote :: Parser T.Text
quote = T.singleton <$> MC.char '"'

innerChars :: Parser T.Text
innerChars = T.pack <$> many (satisfy (not . \c -> c == '"'))

action :: Parser Action
action = undefined

yesterdayFunction :: Parser Function
yesterdayFunction = do
  inputs <- many (try (identifier <* MC.space))
  _ <- MC.space *> chunk "->" *> MC.space1
  output <- identifier <* MC.hspace <* MC.eol
  arms <- linesOf clause

  eof -- Consume the result of input
  pure
    Function
      { funParameters = map (\n -> Variable {varName = n}) inputs,
        funResult = Variable {varName = output},
        funClauses = arms
      }

parseModule :: FilePath -> IO (Either T.Text Function)
parseModule fname = parseFile fname yesterdayFunction
