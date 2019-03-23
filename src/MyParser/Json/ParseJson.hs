module MyParser.Json.ParseJson (parseJson) where

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.String
import Control.Monad
--
import MyParser.Json.Types
  ( JValue (..)
  , JsonParser
  )

-- | Helper function to ignore surrounding spaces
trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

oneOfStr :: String -> Parser String
oneOfStr = fmap return . oneOf

noneOfStr :: String -> Parser String
noneOfStr = fmap return . noneOf

-----------------------
-- | Individual parsers
-----------------------

-- | Null parser
parseNull :: JsonParser
parseNull = do
  string "null"
  return JNull <?> "null"

-- | Boolean parser
parseFalse :: JsonParser
parseFalse = do
  string "false"
  return (JBool False) <?> "false"

parseTrue :: JsonParser
parseTrue = do
  string "true"
  return (JBool True) <?> "true"

parseBool = parseFalse <|> parseTrue

-- | String parser
parseEscapedChar :: Parser Char
parseEscapedChar =
  let charMaps =
       [ ('\\', '\\')
       , ('"', '"')
       , ('/', '/')
       , ('b', '\b')
       , ('f', '\f')
       , ('n', '\n')
       , ('r', '\r')
       , ('t', '\t')
       ] in
  choice (map (\(x, y) -> char x $> y) charMaps)

parseUnicode :: Parser Char
parseUnicode = do
  char 'u'
  fourHexDigits <- count 4 hexDigit
  let hexInInt = read $ "0x" ++ fourHexDigits :: Int
  return (toEnum hexInInt :: Char)

parseEscapedAndUnicode :: Parser Char
parseEscapedAndUnicode = char '\\' *> (parseEscapedChar <|> parseUnicode)

parseLiteralString :: Parser Char
parseLiteralString = noneOf "\"\\"

parseString :: JsonParser
parseString = do
  char '"'
  str <- many $ parseEscapedAndUnicode <|> parseLiteralString
  char '"'
  return (JString str) <?> "string"

-- | Number parser
parseNumber :: JsonParser
parseNumber = do
  let minus = string "-" :: Parser String
      num = string "0" <|> (oneOfStr "123456789" <> many digit) :: Parser String
      frac = string "." <> many digit :: Parser String
      expo = oneOfStr "eE" <> oneOfStr "+-" <> many1 digit :: Parser String
  result <- option "" minus <> num <> option "" frac <> option "" expo
  return (JNumber (read result :: Double)) <?> "number"

-- | Array parser
parseArray :: JsonParser
parseArray = do
  char '['
  arr <- trim $ parseJson' `sepBy` (string "," <> many space)
  char ']'
  return (JArray arr) <?> "array"

-- | Object parser
parseObject :: JsonParser
parseObject = do
  char '{'
  result <- parseKeyValue `sepBy` char ','
  char '}'
  return (JObject $ map (\(JString k, v) -> (k, v)) result) <?> "object"
  where parseKeyValue = do
          key <- trim parseString
          char ':'
          value <- trim parseJson'
          return (key, value)

-----------------------
-- | Parse JSON
-----------------------

parseJson' :: JsonParser
parseJson' = parseNull
  <|> parseBool
  <|> parseString
  <|> parseNumber
  <|> parseArray
  <|> parseObject

parseJson :: JsonParser
parseJson = trim parseJson' <* eof
