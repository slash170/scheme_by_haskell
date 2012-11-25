{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Main where

import System.IO()
import System.Environment
import Numeric
import Control.Monad()
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))

-- Parser 本体
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match:" ++ show err
                   Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool

-- Parser 部品
spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do
  char '\\'          -- バックスラッシュ
  x <- oneOf "\\\""  -- バックスラッシュまたは二重引用符
  return $ case x of -- エスケープされた文字を返す
             'n' -> '\n'
             'r' -> '\r'
             't' -> '\t'
             _   -> x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
  string "#"
  x <- oneOf "tf"
  return $ case x of
             't' -> Bool True
             _   -> Bool False

parseNumber :: Parser LispVal
parseNumber = do
  num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
  return $ num

parseDigital1 :: Parser LispVal
parseDigital1 = do
  x <- many1 digit
  return $ Number $ read x

parseDigital2 :: Parser LispVal
parseDigital2 = do
  try $ string "#d"
  x <- many1 digit
  return $ Number $ read x

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number $ hex2dig x

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number $ oct2dig x

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number $ bin2dig x

-- Utility
oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ readOct x !! 0

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ readHex x !! 0

bin2dig :: [Char] -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num a => a -> [Char] -> a
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs
