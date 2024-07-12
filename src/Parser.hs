{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad (guard)
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Char (isAlpha, isAlphaNum)
import Data.Void (Void)
import Primitive
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
keyword x = x == "let" || x == "in" || x == "\\" || x == "Type" || x == "Int" || x == "Double" || x == "Nat"

pIdent :: Parser String
pIdent = try $ do
  x <- takeWhile1P Nothing isAlpha
  guard (not (keyword x))
  x <$ ws

pKeyword :: String -> Parser ()
pKeyword kw = do
  C.string kw
  (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

pIntType = do
  symbol "Int"
  pure $ RLit $ PrimTy IntType

pDoubleType = do
  symbol "Double"
  pure $ RLit $ PrimTy DoubleType

pNatType = do
  symbol "Nat"
  pure $ RLit $ PrimTy NatType

pAtom :: Parser RTm
pAtom =
  ((RVar <$> pIdent) <|> (RType <$ symbol "Type") <|> pIntType <|> pDoubleType <|> pNatType)
    <|> parens pRaw

pBinder = pIdent <|> symbol "_"

pSpine = foldl1 RApp <$> some pAtom

pInt = RLit . I <$> lexeme L.decimal

pDouble = RLit . D <$> lexeme L.float

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

binary name f = InfixL (f <$ symbol name)

unary name f = Prefix (f <$ symbol name)

pBinOp =
  makeExprParser
    (pSpine <|> pAtom <|> try pDouble <|> pInt <|> pRaw)
    [ [unary "-" (RUnOpPat Sub)],
      [binary "*" (RBinOpPat Mul), binary "/" (RBinOpPat Div)],
      [binary "+" (RBinOpPat Add), binary "-" (RBinOpPat Sub)],
      [binary "->" (RPi "_")]
    ]

pRaw = (pLam <|> pLet <|> try pPi <|> pBinOp)

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
