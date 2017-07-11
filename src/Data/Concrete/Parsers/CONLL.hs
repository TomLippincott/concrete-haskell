{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CONLL
       ( parser
       , xfields
       , ufields
       ) where

import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
import Data.Concrete.Parsers.Utils (communicationRule)
import Data.Scientific (scientific, Scientific(..))
import Data.Text.Lazy (pack, unpack, Text)
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
                       , tab
                       , newline
                       , sepBy1
                       , many
                       , noneOf
                       , eof
                       , separatorChar
                       , someTill
                       )

import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule, pushPathComponent, popPathComponent)

ufields = ["ID", "FORM", "LEMMA", "UPOSTAG", "XPOSTAG", "FEATS", "HEAD", "DEPREL", "DEPS", "MISC"] :: [Text]
xfields = ["ID", "FORM", "LEMMA", "PLEMMA", "POS", "PPOS", "FEAT", "PFEAT", "HEAD", "PHEAD", "DEPREL", "PDEPREL"] :: [Text]

parser :: [Text] -> CommunicationParser ()
parser fs = (communicationRule id (sentence fs)) `sepBy1` sentenceBreak >> return ()

sentence fs = (some (commentLine <|> wordLine fs)) >> return ()

commentLine = (char '#') >> (manyTill anyChar newline)
  
wordLine fs = (row fs) >> return []

row fs = do
  ls <- mapM (\n -> namedEntry n >> char '\t') (init fs)
  s <- namedEntry (last fs)
  newline
  return ()

namedEntry f = do
  pushPathComponent (unpack f)
  t <- sectionRule id $ pack <$> (some (noneOf ['\t', '\n']))
  popPathComponent
  return t

sentenceBreak = newline
