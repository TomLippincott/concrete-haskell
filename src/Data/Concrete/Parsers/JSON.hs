{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.JSON
       ( parseCommunication
       , sequenceSource
       , arraySource
       ) where

import Control.Monad.State (State, get, put, modify, modify')
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Text.Lazy (pack, Text)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty (fromList)
import Text.Megaparsec.Char.Lexer (symbol, lexeme, signed, scientific)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
import Text.Megaparsec.Char ( char
                            , space
                            , anyChar
                            , hexDigitChar
                            , satisfy
                            )
import Text.Megaparsec ( parseErrorPretty
                       , (<|>)
                       , count
                       , manyTill
                       , runParser
                       , some
                       , choice
                       , sepBy
                       , between
                       , match
                       , ParsecT
                       , runParserT'
                       , State(..)
                       , getParserState
                       , many
                       , eof
                       , try
                       , mkPos
                       )
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser, IngestStream)
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
import Data.Concrete.Parsers.Utils (unfoldParse, unfoldParseArray)
import Conduit
import qualified Data.List.NonEmpty as NE

-- | Parses a sequence of JSON objects into a stream
sequenceSource :: Text -> ConduitM () Communication IO ()
sequenceSource = unfoldParse parseCommunication

-- | Parses an array of JSON objects into a stream
arraySource :: Text -> ConduitM () Communication IO ()
arraySource = unfoldParseArray parseCommunication

-- | Parser for turning a single JSON object into a Communication
parseCommunication :: CommunicationParser Communication
parseCommunication = communicationRule id objectP -- >> return default_Communication

jsonP = lexeme' $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]

nullP = sectionRule id $ symbol' "null" >> return ()

boolP = sectionRule id $ (symbol' "true" <|> symbol' "false") >> return ()

numberP = sectionRule id $ signed space scientific >> return ()

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
