{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.XML
       ( sequenceSource
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
--import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)

import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
--import Text.Megaparsec.Error (Dec)
import Text.Megaparsec.Char.Lexer (symbol, lexeme, signed, scientific)
import Text.Megaparsec.Char ( eol
                            , noneOf
                            , newline
                            , char
                            , anyChar
                            , space
                            , hexDigitChar
                            , tab
                            , separatorChar
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
                       , sepBy1
                       , many
                       , eof
                       , someTill
                       )
-- import Text.Megaparsec ( parseErrorPretty
--                        , (<|>)
--                        , satisfy
--                        , space
--                        , hexDigitChar
--                        , count
--                        , manyTill
--                        , anyChar
--                        , runParser
--                        , some
--                        , char
--                        , choice
--                        , sepBy
--                        , between
--                        , match
--                        , ParsecT
--                        , runParserT'
--                        , State(..)
--                        , getParserState
--                        , spaceChar
--                        , eof
--                        , noneOf
--                        , try
--                        )

import Control.Monad.IO.Class (liftIO)       
--import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
--import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)

sequenceSource = undefined

parser :: CommunicationParser ()
parser = do
  space
  some document
  space
  eof
  return ()

-- type CS = CommunicationParser String
-- type CSS = CommunicationParser [String]
-- type CC = CommunicationParser Char

document :: CommunicationParser ()
document = lexeme' $ communicationRule id (parens (some sentence)) >> return ()

sentence = lexeme' $ between (symbol' "(S") (symbol' ")") (some phrase)

phrase = lexeme' $ parens (tag >> some (tag <|> phrase)) >> return []

tag = lexicalItem

lexicalItem = lexeme' $ some notSpaceOrParen

notSpaceOrParen = satisfy (\c -> and [(not . isSpace) c, ('(' /= c), (')' /= c)])

lexeme' = lexeme space
symbol' = symbol space
parens = between (symbol' "(") (symbol' ")")
