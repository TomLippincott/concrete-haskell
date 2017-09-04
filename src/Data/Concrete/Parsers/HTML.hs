{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}
module Data.Concrete.Parsers.HTML
       ( parser
       ) where
parser = error ""

-- import Data.Maybe (fromJust)
-- import Data.List (intercalate)
-- import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
-- import Data.Concrete.Parsers.Utils (communicationRule, Located(..))
-- import Data.Scientific (scientific, Scientific(..))
-- import Data.Text.Lazy (pack, Text)
-- import Data.Functor (($>))
-- import qualified Data.Map as Map
-- import Data.Map (Map)
-- import Data.List.NonEmpty (fromList)
-- import Text.Megaparsec.Lexer (symbol, lexeme, signed, number)
-- import Text.Megaparsec.Pos (initialPos, defaultTabWidth)
-- import Text.Megaparsec.Error (Dec)
-- import Text.Megaparsec ( parseErrorPretty
--                        , (<|>)
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
--                        , many
--                        )

-- import Text.Megaparsec.Text.Lazy (Parser)
-- import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
-- --import Data.Concrete (default_Communication, Communication(..), Section(..), TextSpan(..))
-- import qualified Control.Monad.State as S
-- import qualified Control.Monad.Identity as I
-- --import Data.Concrete.Types
-- import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)

-- parser :: CommunicationParser ()
-- parser = html >> return ()

-- html = many anyChar
