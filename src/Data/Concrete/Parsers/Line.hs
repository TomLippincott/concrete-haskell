{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.Line
       ( parser
       ) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
import Data.Concrete.Parsers.Utils (communicationRule)
import Data.Scientific (scientific, Scientific(..))
import Data.Text.Lazy (pack, Text)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty (fromList)
import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
import Text.Megaparsec.Error (Dec)
import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
import Text.Megaparsec ( parseErrorPretty
                       , (<|>)
                       , satisfy
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
                       , match
                       , ParsecT
                       , runParserT'
                       , State(..)
                       , getParserState
                       , spaceChar
                       , eof
                       , noneOf
                       , try
                       )

import Control.Monad.IO.Class (liftIO)       
import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)

parser :: CommunicationParser ()
parser = do
  space
  some (lexeme' document)
  return ()

type CS = CommunicationParser String
type CSS = CommunicationParser [String]
type CC = CommunicationParser Char

document :: CommunicationParser ()
document = communicationRule id (parens (some sentence) >> eof)

sentence :: CSS
sentence = between (symbol' "(S") (symbol' ")") (some (lexeme' phrase))

phrase :: CS
phrase = lexeme' $ parens ((lexeme' tag) >> (tag <|> phrase))

tag :: CS
tag = some nonSpace

lexicalItem :: CS
lexicalItem = some $ nonSpace

nonSpace :: CC
nonSpace = satisfy (not . isSpace)

lexeme' = lexeme space
symbol' = symbol space
parens = between (char '(') (char ')')
