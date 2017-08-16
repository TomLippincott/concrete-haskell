{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CSV
       ( parser
       ) where

import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
import Data.Concrete.Parsers.Utils (communicationRule)
import Data.Scientific (scientific, Scientific(..))
import Data.Text.Lazy (pack, unpack, Text)
import qualified Data.Text.Lazy as T
import Data.Functor (($>))
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty (fromList)
import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
import Text.Megaparsec.Error (Dec)
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
                       , sepBy1
                       , sepEndBy
                       , between
                       , match
                       , newline
                       , ParsecT
                       , runParserT'
                       , State(..)
                       , getParserState
                       , eol
                       , noneOf
                       , eof
                       , many
                       )
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Text.Megaparsec.Text.Lazy (Parser)
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule, pushPathComponent, popPathComponent)

-- | Parser for CSV files
parser :: Maybe [Text] -> Char -> CommunicationParser ()
parser h d = do
  fs <- if h == Nothing then header d else return $ fromJust h
  space
  withFields fs d
  space
  eof
  return ()

withFields :: [Text] -> Char -> CommunicationParser ()
withFields fs d = (communicationRule id (row d fs)) `sepEndBy` (newline) >> return ()
  
header :: Char -> CommunicationParser [Text]
header d = (entry d) `sepBy1` (char d)

entry :: Char -> CommunicationParser Text
entry d = pack <$> (many (noneOf [d, '\n']))

namedEntry :: Text -> Char -> CommunicationParser Text
namedEntry f d = do
  pushPathComponent (unpack f)
  t <- sectionRule id $ pack <$> (many (noneOf [d, '\n']))
  popPathComponent
  return t

row :: Char -> [Text] -> CommunicationParser ()
row d fs = do
  ls <- mapM (\n -> namedEntry n d >> char d) (init fs)
  s <- namedEntry (last fs) d
  return ()
