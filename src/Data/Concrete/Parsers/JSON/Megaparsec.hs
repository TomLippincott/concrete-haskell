{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.JSON.Megaparsec
       ( fromText
       ) where

import Data.Concrete.Parsers.JSON (JSON(..))
import Data.Text.Lazy (pack)
import Data.Functor (($>))
import Data.Map (fromList)
import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
import Text.Megaparsec ( parseErrorPretty
                       , (<|>)
                       , space
                       , hexDigitChar
                       , count
                       , manyTill
                       , anyChar
                       , runParser
                       , some
                       , char
                       , choice
                       , sepBy
                       , between
                       )

import Text.Megaparsec.Text.Lazy (Parser)

fromText t = case runParser arrayP "JSON data" t of
               Right x -> Right x
               Left e -> Left (parseErrorPretty e)

jsonP :: Parser JSON
jsonP = lexeme' $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]

nullP = symbol' "null" >> return JNull
  
boolP = JBool <$> (((==) "true") <$> (symbol' "true" <|> symbol' "false"))
  
numberP = JNumber <$> signed space number

escapedChar = do
  char '\\'
  choice [ char '\"' $> '\"'
         , char '\\' $> '\\'
         , char '/'  $> '/'
         , char 'n'  $> '\n'
         , char 'r'  $> '\r'
         , char 'f'  $> '\f'
         , char 't'  $> '\t'
         , char 'b'  $> '\b'
         , unicodeEscape
         ]

unicodeEscape = char 'u' >> count 4 hexDigitChar >>= (\code -> return $ toEnum (read ("0x" ++ code)))

stringLiteral = lexeme' $ do 
  char '\"'
  (escapedChar <|> anyChar) `manyTill` char '\"'
  
stringP = do
  c <- pack <$> stringLiteral
  return $ JText c

arrayP = JArray <$> brackets (jsonP `sepBy` comma)
  
pairP = do
  key <- stringLiteral
  symbol' ":"
  value <- jsonP
  return (key, value)

objectP = JObject <$> fromList <$> braces (pairP `sepBy` comma)

lexeme' = lexeme space
symbol' = symbol space
brackets = between (symbol' "[") (symbol' "]")
braces = between (symbol' "{") (symbol' "}")
comma = symbol' ","
