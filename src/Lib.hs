{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Lib where
--   ( someFunc
--   , JSONValue (..)
--   , someJson
--   , evalJson
--   , toJSONNumber
--   ) where

-- import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data JSONValue a
--   = JSONString String
--   | JSONNumber Int
--   | JSONBool Bool
--   | JSONArray [JSONValue a]
--   | JSONObject [(String, JSONValue a)]
--   deriving (Show, Eq)

-- data Exists :: (k -> *) -> * where
--   This :: p a -> Exists p

-- data JSONValue a where
--   JSONString :: String -> JSONValue String
--   JSONNumber :: (Num a, Show a) => a -> JSONValue a
--   JSONBool :: Bool -> JSONValue Bool
--   JSONArray :: [Exists JSONValue] -> JSONValue [Exists JSONValue]
--   JSONObject :: [(String, Exists JSONValue)] -> JSONValue [(String, Exists JSONValue)]

-- -- deriving instance Show (JSONValue a)

-- someJson = JSONObject
--   [ ("name", This $ JSONString "Jihad")
--   , ("age", This $ JSONNumber 25)
--   , ("isMarried", This $ JSONBool True)
--   , ("wives", This $ JSONArray [This $ JSONString "Puspita", This $ JSONString "Awalina"])
--   , ("children", This $ JSONArray
--     [ This $ JSONObject
--       [ ("firstName", This $ JSONString "Razan")
--       , ("lastName", This $ JSONString "Waspada")]
--     , This $ JSONObject
--       [ ("firstName", This $ JSONString "Naila")
--       , ("lastName", This $ JSONString "Waspada")]
--     ])
--   ]

-- evalJson :: JSONValue a -> String
-- evalJson json = eval' json 1 where
--   eval' json' level = case json' of
--     JSONString str -> "\"" ++ str ++ "\""
--     JSONNumber num -> show num
--     JSONBool bool -> show bool
--     JSONArray xs -> "[" ++ intercalate ", " (map (`eval'` level) xs) ++ "]"
--     JSONObject obj ->
--       "{\n" ++ openingIndent
--       ++ intercalate (",\n" ++ openingIndent)
--           ( map (\(_1, _2) -> _1 ++ ": " ++ eval' _2 (level + 1)) obj )
--       ++ "\n" ++ closingIndent ++ "}"
--       where
--         openingIndent = replicate (level * 2) ' '
--         closingIndent = replicate ((level - 1) * 2) ' '

-- toJSONNumber :: String -> Maybe (JSONValue Bool)
-- toJSONNumber str = Just (JSONBool True)

-- data Test a where
--   TInt :: Int -> Test Int
--   TString :: String -> Test String
--   TList :: [Exists Test] -> Test [Exists Test]

-- someTest :: Test [Exists Test]
-- someTest = TList [This (TInt 5), This (TInt 6), This (TString "Haha")]
