{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CONLL
       ( parser
       ) where

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
                       , match
                       , ParsecT
                       , runParserT'
                       , State(..)
                       , getParserState
                       , eol
                       , newline
                       , sepBy1
                       , many
                       , noneOf
                       )

import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)

-- CONLLX CONLLU
ufields = ["ID", "FORM", "LEMMA", "UPOSTAG", "XPOSTAG", "FEATS", "HEAD", "DEPREL", "DEPS", "MISC"]
xfields = ["ID", "FORM", "LEMMA", "PLEMMA", "POS", "PPOS", "FEAT", "PFEAT", "HEAD", "PHEAD", "DEPREL", "PDEPREL"]

parser :: CommunicationParser ()
parser = (communicationRule id sentence) `sepBy1` eol >> return ()

sentence = (commentLine <|> wordLine) `sepBy1` eol

commentLine = (char '#') >> (many $ noneOf ['\n']) >> return ()

wordLine = row '\t' ufields

row :: Char -> [Text] -> CommunicationParser ()
row d fs = do
  es <- (entry d) `sepBy1` (char d)
  return ()
  where
    entry' n = entry d >>= (\x -> char d >> return x)
    entry'' n = entry d

entry :: Char -> CommunicationParser Text
entry d = pack <$> (many (noneOf [d, '\n']))
