module MyParser.Json.ParseJsonSpec (spec) where

import Test.Hspec
import Text.Parsec
import MyParser.Json.ParseJson (parseJson)
import MyParser.Json.Types (JValue (..))

runJsonParser = parse parseJson ""

spec :: Spec
spec = do
  describe "Null parser" $ do
    it "returns JNull when it's a valid null" $ do
      runJsonParser "null" `shouldBe` Right JNull
      runJsonParser "  null  " `shouldBe` Right JNull
    it "fails the JNull when not null" $ do
      runJsonParser "nullzz" `shouldNotBe` Right JNull
      runJsonParser "zznull" `shouldNotBe` Right JNull

  describe "Boolean parser" $ do
    it "returns JBool True or False" $ do
      runJsonParser "true" `shouldBe` Right (JBool True)
      runJsonParser "  true  " `shouldBe` Right (JBool True)
      runJsonParser "false" `shouldBe` Right (JBool False)
      runJsonParser "  false  " `shouldBe` Right (JBool False)
    it "fails JBool when not exact match with true or false" $ do
      runJsonParser "falsezz" `shouldNotBe` Right (JBool False)
      runJsonParser "zztrue" `shouldNotBe` Right (JBool True)

  describe "Number parser" $ do
    it "returns positive number" $
      runJsonParser "123" `shouldBe` Right (JNumber 123)
    it "returns negative number" $
      runJsonParser "-123" `shouldBe` Right (JNumber (-123))
    it "returns float number" $
      runJsonParser "123.45" `shouldBe` Right (JNumber 123.45)
    it "returns exponential number" $ do
      runJsonParser "123.456e+2" `shouldBe` Right (JNumber 12345.6)
      runJsonParser "123.456E+2" `shouldBe` Right (JNumber 12345.6)
      runJsonParser "123.456e-2" `shouldBe` Right (JNumber 1.23456)
      runJsonParser "123.456E-2" `shouldBe` Right (JNumber 1.23456)

  describe "String parser" $ do
    it "returns en empty string" $
      runJsonParser "\"\"" `shouldBe` Right (JString "")
    it "returns escaped char" $ do
      -- A string containing \ and \
      runJsonParser "\"\\\\\"" `shouldBe` Right (JString "\\")
      -- A string containing \ and "
      runJsonParser "\"\\\"\"" `shouldBe` Right (JString "\"")
      -- A string containing \ and /
      runJsonParser "\"\\/\"" `shouldBe` Right (JString "/")
      runJsonParser "\"\\b\"" `shouldBe` Right (JString "\b")
      runJsonParser "\"\\f\"" `shouldBe` Right (JString "\f")
      runJsonParser "\"\\n\"" `shouldBe` Right (JString "\n")
      runJsonParser "\"\\r\"" `shouldBe` Right (JString "\r")
      runJsonParser "\"\\t\"" `shouldBe` Right (JString "\t")
      runJsonParser "\"\\u0025\"" `shouldBe` Right (JString "%")
    it "returns plain string" $ do
      let someText = "123!@#$%^&*()-_=+Jihad Ngganteng"
      runJsonParser ("\"" ++ someText ++ "\"") `shouldBe` Right (JString someText)
    it "returns the escaped and unescaped chars combination" $ do
      let inputStr = "\"ABC\\n DEF\\\\\\\" G\\/\""
      let expectedStr = "ABC\n DEF\\\" G/"
      runJsonParser inputStr `shouldBe` Right (JString expectedStr)

  describe "Array parser" $ do
    it "returns an empty array" $
      runJsonParser "[]" `shouldBe` Right (JArray [])
    it "returns a nested array" $
      runJsonParser "[[]]" `shouldBe` Right (JArray [JArray []])
    it "parses JSON value recursively through array" $
      runJsonParser "[ null, 1, \"haha\", true ]" `shouldBe` Right (JArray [JNull, JNumber 1, JString "haha", JBool True])

  describe "Object parser" $ do
    it "returns an empty object" $
      runJsonParser "{}" `shouldBe` Right (JObject [])
    it "returns a singleton object" $
      runJsonParser "{ \"key\": null }" `shouldBe` Right (JObject [( "key", JNull)])
    it "parses JSON value recursively through object" $
      runJsonParser "{ \"key\": { \"nestedKey\": 1 } }" `shouldBe` Right (JObject [( "key", JObject [( "nestedKey", JNumber 1 )])])
    it "parses JSON object with more than one key" $
      runJsonParser "{\"key1\": 1, \"key2\": 2 }" `shouldBe` Right (JObject [("key1", JNumber 1), ("key2", JNumber 2)])
