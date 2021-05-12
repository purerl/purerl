module Language.PureScript.Erl.Parser (parseFile) where

import Prelude.Compat

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import qualified Text.Parsec.Char as PC

parseFile :: P.SourceName -> Text -> Either P.ParseError  ( [(Text, Int)], [(Text, Int)] )
parseFile = P.parse parseLines

parseLines :: P.Parsec Text u  ( [(Text, Int)], [(Text, Int)] )
parseLines = do
  l <- parseLine
  lns <- P.many $ do
    _ <- P.endOfLine
    parseLine
  P.eof
  pure (mconcat $ l : lns)

parseLine :: P.Parsec Text u ( [(Text, Int)], [(Text, Int)] )
parseLine = ((,[]) <$> P.try (parseAttribute "export")) <|>
            (([],) <$> P.try (parseAttribute "purs_ignore_exports")) <|>
  do
    P.skipMany (PC.noneOf ['\n', '\r'])
    pure ([],[])

parseAttribute :: String -> P.Parsec Text u [(Text, Int)]
parseAttribute text = attributeParser text
  (P.between (PC.char '[') (PC.char ']') (atomArityParser `P.sepBy` PC.char ','))

-- P.Parsec String u Token
--
attributeParser :: String -> P.Parsec Text u a -> P.Parsec Text u a
attributeParser name valueParser =
  -- PC.char '-' *> PC.string name *> P.between (PC.char '(') (PC.char ')') valueParser
  do
    PC.spaces
    _ <- PC.char '-'
    PC.spaces
    _ <- PC.string name
    PC.spaces
    res <- P.between (PC.char '(' *> PC.spaces) (PC.spaces *> PC.char ')') valueParser
    PC.spaces
    _ <- PC.char '.'
    P.skipMany (PC.noneOf ['\n', '\r'])
    pure res

atomArityParser :: P.Parsec Text u (Text, Int)
atomArityParser = do
  PC.spaces
  a <- atomParser
  PC.spaces
  _ <- PC.char '/'
  PC.spaces
  n <- read <$> P.many1 PC.digit
  PC.spaces
  pure (a, n)

atomParser :: P.Parsec Text u Text
atomParser = quotedAtomParser <|> identifierParser

identifierParser :: P.Parsec Text u Text
identifierParser = do
  h <- PC.lower
  t <- T.pack <$> P.many (PC.alphaNum <|> PC.char '_' <|> PC.char '@')
  pure $ T.cons h t

quotedAtomParser :: P.Parsec Text u Text
quotedAtomParser = P.between (PC.char '\'') (PC.char '\'')
  (T.pack <$> P.many1 (PC.noneOf ['\'', '\\'] <|> atomEscapedCharParser))

atomEscapedCharParser :: P.Parsec Text u Char
atomEscapedCharParser = do
  _ <- PC.char '\\'
  PC.char '\'' <|> PC.char '\\'
