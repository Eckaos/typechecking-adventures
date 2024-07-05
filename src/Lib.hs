{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Lib (main') where

import Evaluation (nf, quote)
import Parser (parseStdin)
import RawSyntax
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Printf (printf)
import Typecheck (emptyCxt, infer)

displayError :: String -> (String, SourcePos) -> IO ()
displayError file (msg, SourcePos path (unPos -> linum) (unPos -> colnum)) = do
  let lnum = show linum
      lpad = map (const ' ') lnum
  printf "%s:%d:%d:\n" path linum colnum
  printf "%s |\n" lpad
  printf "%s | %s\n" lnum (lines file !! (linum - 1))
  printf "%s | %s\n" lpad (replicate (colnum - 1) ' ' ++ "^")
  printf "%s\n" msg

helpMsg =
  unlines
    [ "usage: typechecking-adventures [--help|nf|type]",
      "  --help : display this message",
      "  nf     : read & typecheck expression from stdin, print its normal form and type",
      "  type   : read & typecheck expression from stdin, print its type"
    ]

mainWith :: IO [String] -> IO (RTm, String) -> IO ()
mainWith getOpt getRaw = do
  getOpt >>= \case
    ["--help"] -> putStrLn helpMsg
    ["nf"] -> do
      (t, file) <- getRaw
      case infer (emptyCxt (initialPos file)) t of
        Left err -> displayError file err
        Right (t, a) -> do
          print $ nf [] t
          putStrLn "  :"
          print $ quote 0 a
    ["type"] -> do
      (t, file) <- getRaw
      case infer (emptyCxt (initialPos file)) t of
        Left err -> displayError file err
        Right (t, a) -> print $ quote 0 a
    _ -> putStrLn helpMsg

main' :: IO ()
main' = mainWith getArgs parseStdin
