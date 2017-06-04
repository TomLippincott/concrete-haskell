{-# LANGUAGE OverloadedStrings #-}
module Data.Concrete.Parsers.CONLL.Attoparsec
  ( fromText
  ) where

import Control.Applicative ((<|>))
import Data.Text.Lazy (pack)
import Data.Functor (($>))
import Data.Map (fromList)
import Data.Attoparsec.Text.Lazy ( eitherResult
                                 , manyTill
                                 , sepBy
                                 , parse
                                 , Parser(..)
                                 , choice
                                 , skipSpace
                                 , string
                                 , scientific
                                 , char
                                 , anyChar
                                 , count
                                 , digit
                                 , letter
                                 )

fromText t = error "test"
-- case eitherResult $ parse arrayOfObjectsP t of
--                Right r -> Right r
--                Left e -> Left (show e)

-- arrayOfObjectsP = JArray <$> brackets (topLevelObjectP `sepBy` comma)

-- topLevelObjectP = objectP

-- jsonP = skipSpace >> choice [stringP, nullP, numberP, boolP, arrayP, objectP] >>= (\x -> skipSpace >> return x)

-- nullP = string "null" >> return JNull

-- boolP = JBool <$> (((==) "true") <$> (string "true" <|> string "false"))

-- numberP = JNumber <$> scientific

-- escapedChar = do
--   char '\\'
--   choice [ char '\"' $> '\"'    
--          , char '\\' $> '\\'
--          , char '/'  $> '/'
--          , char 'n'  $> '\n'
--          , char 'r'  $> '\r'
--          , char 'f'  $> '\f'
--          , char 't'  $> '\t'
--          , char 'b'  $> '\b'
--          , unicodeEscape
--          ]

-- unicodeEscape = char 'u' >> count 4 (choice [digit, letter]) >>= (\code -> return $ toEnum (read ("0x" ++ code)))

-- stringLiteral = lexeme' $ do 
--   char '\"'
--   (escapedChar <|> anyChar) `manyTill` char '\"'
  
-- stringP = JText <$> pack <$> stringLiteral

-- arrayP = JArray <$> brackets (jsonP `sepBy` comma)

-- pairP = do
--   key <- stringLiteral
--   symbol' ":"
--   value <- jsonP
--   return (key, value)

-- objectP = JObject <$> fromList <$> braces (pairP `sepBy` comma)

-- lexeme' m = m >>= (\x -> skipSpace >> return x)
-- symbol' m = string m >>= (\x -> skipSpace >> return x)
-- brackets = between (symbol' "[") (symbol' "]")
-- braces = between (symbol' "{") (symbol' "}")
-- comma = symbol' ","
-- between o c m = o >> m >>= (\x -> c >> return x)

