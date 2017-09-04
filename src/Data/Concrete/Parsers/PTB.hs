{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.PTB
       ( treeParser
       , fileParser
       ) where

treeParser = error ""
fileParser = error ""
-- import Data.Char (isSpace)
-- import Data.List (intercalate)
-- import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
-- import Data.Concrete.Parsers.Utils (communicationRule)
-- import Data.Scientific (scientific, Scientific(..))
-- import Data.Text.Lazy (pack, Text)
-- import Data.Functor (($>))
-- import qualified Data.Map as Map
-- import Data.Map (Map)
-- import Data.List.NonEmpty (fromList)
-- import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
-- import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
-- import Text.Megaparsec.Error (Dec)
-- import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
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

-- import Control.Monad.IO.Class (liftIO)       
-- import Text.Megaparsec.Text.Lazy (Parser)
-- import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
-- import qualified Control.Monad.State as S
-- import qualified Control.Monad.Identity as I
-- import Data.Concrete.Parsers.Utils (communicationRule, sectionRule, sentenceRule, tokenRule, pushPathComponent, popPathComponent)

-- -- | Parser for PENN Treebank format
-- --   NOTE: currently, doesn't capture tags/parses
-- fileParser :: CommunicationParser [Communication]
-- fileParser = do
--   space
--   cs <- some treeParser
--   space
--   eof
--   return cs

-- treeParser :: CommunicationParser Communication
-- treeParser = lexeme' $ communicationRule id (parens (some sentence))

-- sentence = do
--   pushPathComponent "sentence"
--   (sectionRule id . sentenceRule id) $ lexeme' $ between (symbol' "(S") (symbol' ")") (some phrase)
--   popPathComponent  
  
-- phrase = lexeme' $ parens (tag >> some (token <|> phrase)) >> return []

-- tag = lexicalItem

-- token = tokenRule id lexicalItem

-- lexicalItem = lexeme' $ some notSpaceOrParen

-- notSpaceOrParen = satisfy (\c -> and [(not . isSpace) c, ('(' /= c), (')' /= c)])

-- lexeme' = lexeme space
-- symbol' = symbol space
-- parens = between (symbol' "(") (symbol' ")")
