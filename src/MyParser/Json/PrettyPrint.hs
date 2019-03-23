module MyParser.Json.PrettyPrint
  ( compactPrint
  , prettyPrint
  ) where

import MyParser.Json.Types (JValue (..))
import Data.List (intercalate)

toJson :: Bool -> Int -> JValue -> String
toJson isCompact indentCount json = eval json 1 where
  spaceGap = if isCompact then "" else " "
  lineGap = if isCompact then "" else "\n"
  eval jvalue level = case jvalue of
    JString str -> "\"" ++ str ++ "\""
    JNumber num -> show num
    JBool bool -> show bool
    JArray xs -> "[" ++ intercalate ("," ++ spaceGap) (map (`eval` level) xs) ++ "]"
    JObject obj ->
      "{" ++ lineGap ++ openingIndent
      ++ intercalate ("," ++ lineGap ++ openingIndent)
          ( map (\(_1, _2) -> _1 ++ ":" ++ spaceGap ++ eval _2 (level + 1)) obj )
      ++ lineGap ++ closingIndent ++ "}"
      where
        openingIndent = replicate (level * indentCount) ' '
        closingIndent = replicate ((level - 1) * indentCount) ' '

compactPrint = toJson True 0
prettyPrint = toJson False 2
