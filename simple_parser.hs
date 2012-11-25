{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Main where

import System.IO()
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Ratio

data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Number     Integer
             | Float      Double
             | Ratio      Rational
             | String     String
             | Bool       Bool
             | Character  Char

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
parseExpr = parseAtom
            <|> parseString
            <|> try parseCharacter
            <|> try parseBool
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseNumber
            <|> try parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

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

-- () List の Parser
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- dot '.' 対の Parser
parseDottedList :: Parser LispVal
parseDottedList = do
  hdToken <- endBy parseExpr spaces
  tlToken <- char '.' >> spaces >> parseExpr
  return $ DottedList hdToken tlToken

-- SingleQuote の Parser
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote",x]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
           <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
  return $ Character $ case value of
                         "space"   -> ' '
                         "newline" -> '\n'
                         _         -> (value!!0)

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

-- reads を使用し、残った文字列があったらエラーにするよう変更すべき。
parseDigital1 :: Parser LispVal
parseDigital1 = do
  x <- many1 digit
  return $ Number $ read x

-- reads を使用し、残った文字列があったらエラーにするよう変更すべき。
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

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . head $ readFloat(x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio ((read x)%(read y))


-- Utility
-- reads での読み込み時、残った文字列があったらエラーにするよう変更すべき。
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
