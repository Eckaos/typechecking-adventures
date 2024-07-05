{-# LANGUAGE LambdaCase #-}

module Parser where

import Common
import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Char (isAlpha, isAlphaNum)
import Data.Void (Void)
import RawSyntax
import System.Exit (exitSuccess)
import Text.Megaparsec (MonadParsec (eof, takeWhile1P, try), ParseErrorBundle, Parsec, SourcePos, choice, empty, errorBundlePretty, getSourcePos, many, optional, parse, some, (<|>))
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

withPos :: Parser RTm -> Parser RTm
withPos p = RSrcPos <$> getSourcePos <*> p

lexeme = L.lexeme ws

symbol s = lexeme (C.string s)

char c = lexeme (C.char c)

parens p = char '(' *> p <* char ')'

pArrow = symbol "->"

keyword :: String -> Bool
keyword x = x == "let" || x == "in" || x == "\\" || x == "Type"

pIdent :: Parser Name
pIdent = try $ do
  x <- takeWhile1P Nothing isAlphaNum
  guard (not (keyword x))
  x <$ ws

pKeyword :: String -> Parser ()
pKeyword kw = do
  C.string kw
  (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

pAtom :: Parser RTm
pAtom =
  withPos ((RVar <$> pIdent) <|> (RType <$ symbol "Type"))
    <|> parens pRaw

pBinder = pIdent <|> symbol "_"

pSpine = foldl1 RApp <$> some pAtom

pLam = do
  char '\\'
  xs <- some pBinder
  char '.'
  t <- pRaw
  pure (foldr RLam t xs)

pPi = do
  dom <- some (parens ((,) <$> some pBinder <*> (char ':' *> pRaw)))
  pArrow
  cod <- pRaw
  pure $ foldr (\(xs, a) t -> foldr (\x -> RPi x a) t xs) cod dom

funOrSpine = do
  sp <- pSpine
  optional pArrow >>= \case
    Nothing -> pure sp
    Just _ -> RPi "_" sp <$> pRaw

pLet = do
  pKeyword "let"
  x <- pBinder
  symbol ":"
  a <- pRaw
  symbol "="
  t <- pRaw
  symbol "in"
  u <- pRaw
  pure $ RLet x a t u

pRaw = withPos (pLam <|> pLet <|> try pPi <|> funOrSpine)

pSrc = ws *> pRaw <* eof

parseString :: String -> IO RTm
parseString src =
  case parse pSrc "(stdin)" src of
    Left e -> do
      putStrLn $ errorBundlePretty e
      exitSuccess
    Right t ->
      pure t

parseStdin :: IO (RTm, String)
parseStdin = do
  file <- getContents
  tm <- parseString file
  pure (tm, file)
