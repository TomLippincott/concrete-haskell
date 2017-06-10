{-# LANGUAGE DeriveGeneric, OverloadedStrings, ApplicativeDo #-}
module Data.Concrete.Parsers.CSV
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
                       )

import Text.Megaparsec.Text.Lazy (Parser)
import Data.Concrete (default_Communication, Communication(..))
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import Data.Concrete.Types
import Data.Concrete.Parsers.Utils (communicationRule, sectionRule)
  
arrayOfObjectsP :: ParsecT Dec Text (S.StateT Bookkeeper IO) ()
arrayOfObjectsP = brackets ((communicationRule id objectP) `sepBy` comma) >> return ()

jsonP = sectionRule id jsonP'

jsonP' = lexeme' $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]
  
nullP = symbol' "null" >> return ()
  
boolP = (symbol' "true" <|> symbol' "false") >> return ()
  
numberP = signed space number >> return ()

stringP = pack <$> stringLiteral >> return ()

objectP = Map.fromList <$> braces (pairP `sepBy` comma) >> return ()

arrayEntryP = do
  bs@(Bookkeeper{..}) <- S.get
  let p = (read $ head path) :: Int
  S.put $ bs { path=(show (p + 1)):(tail path) } 
  jsonP
  return ()
  
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

lexeme' = lexeme space
symbol' = symbol space
brackets = between (symbol' "[") (symbol' "]")
braces = between (symbol' "{") (symbol' "}")
comma = symbol' ","
