{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CSV
       ( sequenceSource
       ) where

import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
import Data.Concrete.Parsers.Utils (communicationRule)
import Data.Scientific (scientific, Scientific(..))
import Data.Text.Lazy (pack, unpack, Text)
import qualified Data.Text.Lazy as T
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List.NonEmpty (fromList)
import Text.Megaparsec.Char.Lexer (symbol, lexeme, signed, scientific)
import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
import Text.Megaparsec.Char ( eol
                            , noneOf
                            , newline
                            , char
                            , anyChar
                            , space
                            , hexDigitChar
                            )
import Text.Megaparsec ( parseErrorPretty
                       , (<|>)
                       , count
                       , manyTill
                       , runParser
                       , some
                       , choice
                       , sepBy
                       , sepBy1
                       , sepEndBy
                       , between
                       , match
                       , ParsecT
                       , runParserT'
                       , State(..)
                       , getParserState
                       , eof
                       , many
                       )
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
--import Text.Megaparsec.Text.Lazy (Parser)
--import Data.Concrete (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
--import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (unfoldParse, unfoldParseArray)
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule, pushPathComponent, popPathComponent)
import Conduit

--sequenceSource = undefined
sequenceSource :: Bool -> Char -> [String] -> Text -> Text -> Text -> ConduitM () Communication IO ()
sequenceSource h d = undefined -- do
--  fs <- if h == True then header d else return []
--  unfoldParse (communicationRule id (row d fs))

-- parser :: Bool -> Char -> CommunicationParser ()
-- parser h d = do
--   fs <- if h == True then header d else return []
--   space
--   withFields fs d
--   space
--   eof
--   return ()

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
