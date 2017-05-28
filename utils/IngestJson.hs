{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, GADTs, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Functor (($>))
import Data.List (intercalate)
import Control.Applicative (empty)
import Data.Monoid ((<>))
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack, take)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Vector (Vector, fromList, snoc, empty)
import Data.List (isSuffixOf, reverse, drop)
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Data.List.NonEmpty as NE

import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip

import qualified Options.Applicative as O
import Options.Applicative (option, auto, short, long, metavar, strOption, help, execParser, info, helper, fullDesc, progDesc, many)

import Text.Megaparsec.Char (spaceChar, char)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Combinator (choice, sepBy, sepBy1, between)
import Text.Megaparsec.Error (Dec)
import Text.Megaparsec.Pos (defaultTabWidth)
import Text.Megaparsec.Prim (Parsec, ParsecT, setTokensProcessed)
import Text.Megaparsec ( parseErrorPretty
                       , many
                       , eof
                       , (<|>)
                       , space
                       , try
                       , hexDigitChar
                       , count
                       , manyTill
                       , anyChar
                       , getParserState
                       , State(..)
                       , ParsecT
                       , SourcePos(..)
                       , Pos(..)
                       , initialPos
                       , runParserT
                       , runParserT'
                       , some
                       )

import Data.Concrete ( read_Communication
                     , write_Communication
                     , default_Communication
                     , Communication(..)
                     , default_Section
                     , Section(..)
                     , default_AnnotationMetadata
                     , AnnotationMetadata(..)
                     , default_CommunicationMetadata
                     , CommunicationMetadata(..)
                     , default_Sentence
                     , Sentence(..)
                     , default_TextSpan
                     , TextSpan(..)
                     )

import qualified Data.Concrete.Utils as CU
import qualified Data.Concrete as C

type Parser = Parsec Dec Text

data JSON = JNumber Double
          | JString String
          | JArray [JSON]
          | JObject (M.Map String JSON)
          | JBool Bool
          | JNull
            deriving (Show)

data BuildState = BuildState { path :: [String]
                             , sections :: [Section]
                             , comms :: [Communication]
                             , offset :: Int
                             , id :: Maybe String
                             }

type CommunicationParser a = ParsecT Dec Text (S.StateT BuildState IO) a 

jsonCP :: CommunicationParser JSON
jsonCP = do
  s <- (fromIntegral . stateTokensProcessed) <$> getParserState  
  j <- lexeme $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]
  e <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs@(BuildState {..}) <- S.get
  uuid <- liftIO CU.getUUID
  let path' = T.pack (intercalate "." (reverse path))
      section = default_Section { section_uuid=uuid
                                , section_label=Just path'
                                , section_textSpan=Just $ TextSpan (s - (fromIntegral offset)) (e - (fromIntegral offset))
                                , section_kind="metadata"
                                }
  S.put $ bs { sections=section:sections }
  return j

nullP = do
  symbol "null"
  return $ JNull
  
boolP = do
  c <- symbol "true"  <|> symbol "false"
  return $ JBool (c == "true")
  
numberP = do
  c <- L.signed (pure ()) (try L.float <|> (fromInteger <$> L.integer))
  return $ JNumber c

escapedChar = do
  char '\\'
  choice [
    char '\"' $> '\"',   -- A boring list of hardcoded values from the spec.
    char '\\' $> '\\',
    char '/'  $> '/' ,
    char 'n'  $> '\n',
    char 'r'  $> '\r',
    char 'f'  $> '\f',
    char 't'  $> '\t',
    char 'b'  $> '\b',
    unicodeEscape ]

unicodeEscape = do
  char 'u'
  code <- count 4 hexDigitChar
  return $ toEnum (read ("0x" ++ code))

stringLiteral = lexeme $ do 
  char '\"'
  (escapedChar <|> anyChar) `manyTill` char '\"'

stringP = do
  c <- stringLiteral
  return $ JString c

arrayEntryP = do
  bs@(BuildState{..}) <- S.get
  let p = (read $ head path) :: Int
  S.put $ bs { path=(show (p + 1)):(tail path) }  
  jsonCP
  
arrayP = do
  S.modify' (\ bs@(BuildState{..}) -> bs { path=(show (-1)):path })
  c <- brackets (arrayEntryP `sepBy` comma)
  S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
  return $ JArray c
  
pairP = do
  key <- stringLiteral
  symbol ":"
  S.modify' (\ bs@(BuildState{..}) -> bs { path=key:path })
  value <- jsonCP
  S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
  return (key, value)

objectP = do
  c <- M.fromList <$> braces (pairP `sepBy` comma)
  return $ JObject c

topLevelObjectP t = do
  offset <- (fromIntegral . stateTokensProcessed) <$> getParserState
  S.modify (\bs -> bs { offset=offset })
  objectP
  end <- (fromIntegral . stateTokensProcessed) <$> getParserState
  let t' = substr t offset end
  uuid <- liftIO CU.getUUID
  S.modify (\ bs@(BuildState{..}) -> bs { sections=[]
                                        , comms=(default_Communication { communication_id=C.uUID_uuidString uuid
                                                                       , communication_uuid=uuid
                                                                       , communication_text=Just $ T.pack t'
                                                                       , communication_sectionList=(Just . fromList . reverse) sections
                                                                       }):comms
                                        })

symbol = L.symbol space
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")
lexeme = L.lexeme space
comma = symbol ","

substr :: Text -> Int -> Int -> String
substr t s e = unpack res
  where
    (_, start) = T.splitAt (fromIntegral s) t    
    res = T.take (fromIntegral $ e - s) start

data Parameters = Parameters { inputFile :: String
                             , outputFile :: String
                             , commType :: String
                             , commId :: String
                             , contentSectionTypes :: [String]
                             } deriving (Show)

parameters :: O.Parser Parameters
parameters = Parameters
             <$> strOption (short 'i'
                            <> long "input"
                            <> metavar "INPUT_FILE"
                            <> help "Text file of JSON objects"
                           )
             <*> strOption (short 'o'
                            <> long "output"
                            <> metavar "OUTPUT_FILE"
                            <> help "Tar archive of Concrete Communications"
                           )
             <*> strOption (short 't'
                            <> long "commType"
                            <> metavar "COMMUNICATION_TYPE"
                            <> help "String describing the type of Communication(s)"
                           )
             <*> strOption (short 'I'
                            <> long "commId"
                            <> metavar "COMMUNICATION_ID"
                            <> help "String describing the Communication ID"
                           )
             <*> many (strOption (short 's'
                            <> long "contentSectionTypes"
                            <> metavar "CONTENT_SECTION_TYPES"
                            <> help "The names of sections to be considered \"content\" rather than \"metadata\""
                           ))

parseComms :: Text -> IO [Communication]
parseComms t = do
  let s = State { stateInput=t
                , statePos=NE.fromList $ [initialPos "JSON"]
                , stateTokensProcessed=0
                , stateTabWidth=defaultTabWidth
                }
      unwrapped = between space eof (some (topLevelObjectP t))
      wrapped = between space eof (brackets ((topLevelObjectP t) `sepBy` comma))
  (r, BuildState _ _ cs _ _) <- (S.runStateT (runParserT' (wrapped <|> unwrapped) s) (BuildState [] [] [] 0 Nothing))
  return cs

main = do
  ps <- execParser opts
  let f = inputFile ps
  t <- decodeUtf8 <$> case takeExtension f of
    ".gz" -> (liftM GZip.decompress . BS.readFile) f
    _ -> BS.readFile f
  cs <- parseComms t
  CU.writeCommunications (outputFile ps) cs
  where 
    opts = info (helper <*> parameters)
           ( fullDesc
             <> progDesc "Ingest JSON into Concrete Communications"
           )
