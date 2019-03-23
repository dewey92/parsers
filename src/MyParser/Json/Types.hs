module MyParser.Json.Types
 ( JValue (..)
 , JsonParser
 ) where

import Data.Text
import Text.Parsec
import Text.Parsec.String

data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Show, Eq)

type JsonParser = Parser JValue

