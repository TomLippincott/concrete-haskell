{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CONLL
       ( arrayOfObjectsP
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
                       )

import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)

-- CONLLX CONLLU
  
arrayOfObjectsP :: CommunicationParser ()
arrayOfObjectsP = sentence `sepBy` (count 2 eol) >> return ()

--sentence :: CommunicationParser ()
sentence = error "unimplemented"
