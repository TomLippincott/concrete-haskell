{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CONLL
       ( sequenceSource
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
import Text.Megaparsec.Char.Lexer (symbol, lexeme, signed, scientific)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
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
import Text.Megaparsec.Char ( eol
                            , noneOf
                            , separatorChar
                            , tab
                            , newline
                            , char
                            , anyChar
                            , space
                            , hexDigitChar
                            )
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule, pushPathComponent, popPathComponent)
import Data.Concrete.Parsers.Utils (unfoldParseNewline)

sequenceSource fs = unfoldParseNewline (communicationRule id (sentence fs))

ufields = ["ID", "FORM", "LEMMA", "UPOSTAG", "XPOSTAG", "FEATS", "HEAD", "DEPREL", "DEPS", "MISC"] :: [Text]
xfields = ["ID", "FORM", "LEMMA", "PLEMMA", "POS", "PPOS", "FEAT", "PFEAT", "HEAD", "PHEAD", "DEPREL", "PDEPREL"] :: [Text]

--parserSentence :: [Text] -> CommunicationParser ()
--parser fs = (communicationRule id (sentence fs)) `sepBy1` sentenceBreak >> return ()

sentence fs = (some (commentLine <|> wordLine fs))

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

--sentenceBreak = newline
