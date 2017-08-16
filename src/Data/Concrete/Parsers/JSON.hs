{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.JSON
       ( parser
       , lineParser
       ) where

import Control.Monad.State (State, get, put, modify, modify')
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Scientific (scientific, Scientific(..))
import Data.Text.Lazy (pack, Text)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty (fromList)
import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
import Text.Megaparsec.Error (Dec)
import Text.Megaparsec ( parseErrorPretty
                       , (<|>)
                       , eol
                       , space
                       , hexDigitChar
                       , count
                       , manyTill
                       , anyChar
                       , runParser
                       , try
                       , some
                       , char
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
                       )
import Control.Monad.IO.Class (liftIO)
import Text.Megaparsec.Text.Lazy (Parser)
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

-- | Parses an array of JSON objects, turning each into a Communication
parser :: CommunicationParser ()
parser = brackets ((communicationRule id objectP) `sepBy` comma) >> return ()

-- | Parses a sequence of JSON objects (i.e. not a valid JSON object overall), like one object per line
lineParser :: CommunicationParser ()
lineParser = (many (communicationRule id objectP)) >> return ()

jsonP = lexeme' $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]

nullP = sectionRule id $ symbol' "null" >> return ()

boolP = sectionRule id $ (symbol' "true" <|> symbol' "false") >> return ()

numberP = sectionRule id $ signed space number >> return ()

stringP = stringPLiteral >> return ()

stringPLiteral = lexeme' $ do 
  char '\"'
  sectionRule (adjustTextSpan 0 (-1)) ((escapedChar <|> anyChar) `manyTill` char '\"')

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

arrayEntryP = pathArrayEntryRule $ do
  jsonP

arrayP =  pathArrayRule $ do  
  (try (brackets space)) <|> ((brackets (arrayEntryP `sepBy` comma)) >> return ())

pairP = do
  key <- stringLiteral
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
