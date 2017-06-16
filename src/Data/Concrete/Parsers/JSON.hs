{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}
module Data.Concrete.Parsers.JSON
       ( parser
       ) where

import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser)
import Data.Concrete.Parsers.Utils (communicationRule, Located(..))
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
                       )

import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete (default_Communication, Communication(..), Section(..), TextSpan(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)

parser :: CommunicationParser ()
parser = brackets ((communicationRule id objectP) `sepBy` comma) >> return ()

jsonP = lexeme' $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]
  
nullP = sectionRule id $ symbol' "null" >> return ()
  
boolP = sectionRule id $ (symbol' "true" <|> symbol' "false") >> return ()
  
numberP = sectionRule id $ signed space number >> return ()

stringP = sectionRule (adjustTextSpan 1 (-1)) $ pack <$> stringLiteral >> return ()

escapedChar = do
  char '\\'
  choice [ char '\"' $> '\"'
         , char '\\' $> '\\'
         , char '/'  $> '/'
         , char 'n'  $> '\n'
         , char 'r'  $> '\r'
         , char 'f'  $> '\f'
         , char 't'  $> '\t'
         , char 'b'  $> '\b'
         , unicodeEscape
         ]

unicodeEscape = char 'u' >> count 4 hexDigitChar >>= (\code -> return $ toEnum (read ("0x" ++ code)))

stringLiteral = lexeme' $ do 
  char '\"'
  (escapedChar <|> anyChar) `manyTill` char '\"'

arrayEntryP = do
  bs@(Bookkeeper{..}) <- S.get
  let p = (read $ head path) :: Int
  S.modify' (\ bs -> bs { path=(show (p + 1)):(tail path) } )
  jsonP
  return ()

arrayP = do
  S.modify' (\ bs@(Bookkeeper{..}) -> bs { path=(show (-1)):path })
  c <- brackets (arrayEntryP `sepBy` comma)
  S.modify' (\ bs@(Bookkeeper{..}) -> bs { path=tail path})
  return ()

pairP = do
  key <- stringLiteral
  S.modify' (\ bs@(Bookkeeper{..}) -> bs { path=key:path })
  symbol' ":"
  value <- jsonP
  S.modify' (\ bs@(Bookkeeper{..}) -> bs { path=tail path})
  return (key, value)

objectP = sectionRule id $ Map.fromList <$> braces (pairP `sepBy` comma) >> return ()

lexeme' = lexeme space
symbol' = symbol space
brackets = between (symbol' "[") (symbol' "]")
braces = between (symbol' "{") (symbol' "}")
comma = symbol' ","
