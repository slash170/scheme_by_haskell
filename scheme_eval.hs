{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

module Main where

import System.IO()
import System.Environment
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Ratio

-- データ定義
data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Number     Integer
             | Float      Double
             | Ratio      Rational
             | String     String
             | Bool       Bool
             | Character  Char

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispVal where show = showVal
instance Show LispError where show = showError
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

-- Main 処理
main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args!!0) >>= eval
  putStrLn $ extractValue $ trapError evaled

-- 表示
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = show contents
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio contents) = show contents
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList hdToken tlToken) = "(" ++ unwordsList hdToken ++ "." ++ showVal tlToken ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- エラー処理
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialForm message form) = message ++ ":" ++ show form
showError (NotFunction message func) = message ++ ":" ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- 評価
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- ($ args) の意味が分からん
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args " func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String,[LispVal] -> ThrowsError LispVal)]
primitives = [("+",numericBinop (+)),
              ("-",numericBinop (-)),
              ("*",numericBinop (*)),
              ("/",numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                       if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- 構文解析
-- Parser 本体
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
