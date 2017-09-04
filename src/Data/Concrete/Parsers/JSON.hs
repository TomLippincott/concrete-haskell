{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.JSON
       ( parser
       , arrayParser
       , lineParser
       ) where

import Control.Monad.State (State, get, put, modify, modify')
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Text.Lazy (pack, Text)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty (fromList)
import Text.Megaparsec.Char.Lexer (symbol, lexeme, signed, scientific, charLiteral)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
import Data.Void (Void)
import Text.Megaparsec.Char (space, eol, noneOf, anyChar, char, hexDigitChar, notChar)
import Text.Megaparsec ( parseErrorPretty
                       , (<|>)
                       , count
                       , manyTill
                       , runParser
                       , try
                       , some
                       , choice
                       , sepBy
                       , between
                       , match
                       , ParsecT
                       , runParserT'
                       , State(..)
                       , getParserState
                       , eof
                       , many
                       , takeWhileP
                       )
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
import Data.Concrete.Parsers.Utils ( communicationRule
                                   , sectionRule
                                   , pathArrayRule
                                   , pathArrayEntryRule
                                   , pathDictionaryRule
                                   , pathDictionaryKeyRule
                                   , Located(..)
                                   , pushPathComponent
                                   , popPathComponent
                                   , modifyPathComponent
                                   , incrementPathComponent
                                   )

parser :: CommunicationParser Communication
parser = communicationRule id objectP

-- | Parses an array of JSON objects, turning each into a Communication
arrayParser :: CommunicationParser [Communication]
arrayParser = brackets ((communicationRule id objectP) `sepBy` comma)

-- | Parses a sequence of JSON objects (i.e. not a valid JSON object overall), like one object per line
lineParser :: CommunicationParser [Communication]
lineParser = many parser

jsonP = lexeme' $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]

nullP = sectionRule id $ symbol' "null" >> return ()

boolP = sectionRule id $ (symbol' "true" <|> symbol' "false") >> return ()

numberP = sectionRule id $ signed space scientific >> return ()

stringP = sectionRule (adjustTextSpan 1 (-1)) stringLiteral >> return ()
  --stringPLiteral >> return ()

--stringPLiteral = lexeme' $ do 
  --char '\"'
--  sectionRule (adjustTextSpan 1 (-1)) stringLiteral -- ((escapedChar <|> anyChar) `manyTill` char '\"')
--escapedChar :: CommunicationParser Char
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

stringLiteral = lexeme' $ between (char '"') (char '"') (many (escapedChar <|> notChar '"'))

arrayEntryP = pathArrayEntryRule $ do
  jsonP

arrayP =  pathArrayRule $ do  
  (try (brackets space)) <|> ((brackets (arrayEntryP `sepBy` comma)) >> return ())

pairP = do
  key <- pack <$> stringLiteral
  pushPathComponent key
  symbol' ":"
  value <- jsonP
  c <- popPathComponent
  return (key, value)

objectP = sectionRule id $ Map.fromList <$> braces (pairP `sepBy` comma) >> return ()

lexeme' = lexeme space
symbol' = symbol space
brackets = between (symbol' "[") (symbol' "]")
braces = between (symbol' "{") (symbol' "}")
comma = symbol' ","

{-# INLINE stringLiteral #-}
