{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, GADTs, MultiParamTypeClasses, FlexibleInstances, RecordWildCards, BangPatterns #-}
module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class (liftIO)
--import Control.DeepSeq
import Data.Map (toList)
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
import System.IO (stdin, stdout, openFile, Handle, IOMode(..))
import Data.Maybe (fromJust)
import Data.List (foldl', foldr, foldl)
--import Data.Either (fromRight)
import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip

import qualified Options.Applicative as O
import Options.Applicative (option, auto, short, long, metavar, strOption, help, execParser, info, helper, fullDesc, progDesc, many, switch)

-- import Text.Megaparsec.Char (spaceChar, char)
-- import qualified Text.Megaparsec.Lexer as L
-- import Text.Megaparsec.Combinator (choice, sepBy, sepBy1, between)
-- import Text.Megaparsec.Error (Dec)
-- import Text.Megaparsec.Pos (defaultTabWidth)
-- import Text.Megaparsec.Prim (Parsec, ParsecT, setTokensProcessed)
-- import Text.Megaparsec ( parseErrorPretty
--                        , many
--                        , eof
--                        , (<|>)
--                        , space
--                        , try
--                        , hexDigitChar
--                        , count
--                        , manyTill
--                        , anyChar
--                        , getParserState
--                        , State(..)
--                        , ParsecT
--                        , SourcePos(..)
--                        , Pos(..)
--                        , initialPos
--                        , runParserT
--                        , runParserT'
--                        , runParser
--                        , runParser'                       
--                        , some
--                        , match
--                        )

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
import qualified Data.Concrete.Parsers.JSON.Attoparsec as AP --(fromText) --attoFromText, megaFromText, topLevelObjectP, JSON(..))
import qualified Data.Concrete.Parsers.JSON.Megaparsec as MP --(fromText) --attoFromText, megaFromText, topLevelObjectP, JSON(..))
import Data.Concrete.Parsers.JSON (JSON(..))
--type Input = Text -- ByteString
--type CommunicationParser a = ParsecT Dec Input (S.StateT BuildState IO) a 

--data JSON = JNumber Double
--          | JText Text
--          | JArray [JSON]
--          | JObject (M.Map String JSON)
--          | JBool Bool
--          | JNull
--            deriving (Show)


-- jsonCP :: CommunicationParser JSON
-- jsonCP = do
--   s <- (fromIntegral . stateTokensProcessed) <$> getParserState  
--   j <- lexeme $ choice [nullP, numberP, stringP, boolP, objectP, arrayP]
--   e <- (fromIntegral . stateTokensProcessed) <$> getParserState
--   bs@(BuildState {..}) <- S.get
--   uuid <- liftIO CU.getUUID
--   let path' = (intercalate "." (reverse path))
--       section = default_Section { section_uuid=uuid
--                                 , section_label=Just $ T.pack path'
--                                 , section_textSpan=Just $ TextSpan (s - (fromIntegral offset)) (e - (fromIntegral offset))
--                                 , section_kind=if path' `elem` (contentSectionTypes params) then "content" else "metadata"
--                                 }
--   S.put $ bs { sections=section:sections }
--   return j

-- nullP = do
--   symbol "null"
--   S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), T.pack "null"):idVals })    
--   return $ JNull
  
-- boolP = do
--   c <- symbol "true"  <|> symbol "false"
--   S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), T.pack c):idVals })  
--   return $ JBool (c == "true")
  
-- numberP = do
--   (s, c) <- match $ L.signed (pure ()) (try L.float <|> (fromInteger <$> L.integer))
--   S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), T.pack s):idVals })  
--   return $ JNumber c

-- escapedChar = do
--   char '\\'
--   choice [
--     char '\"' $> '\"',   -- A boring list of hardcoded values from the spec.
--     char '\\' $> '\\',
--     char '/'  $> '/' ,
--     char 'n'  $> '\n',
--     char 'r'  $> '\r',
--     char 'f'  $> '\f',
--     char 't'  $> '\t',
--     char 'b'  $> '\b',
--     unicodeEscape ]

-- unicodeEscape = do
--   char 'u'
--   code <- count 4 hexDigitChar
--   return $ toEnum (read ("0x" ++ code))

-- stringLiteral = lexeme $ do 
--   char '\"'
--   (escapedChar <|> anyChar) `manyTill` char '\"'
  
-- stringP = do
--   c <- T.pack <$> stringLiteral
--   S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), c):idVals })
--   return $ JText c

-- arrayEntryP = do
--   bs@(BuildState{..}) <- S.get
--   let p = (read $ head path) :: Int
--   S.put $ bs { path=(show (p + 1)):(tail path) }  
--   jsonCP
  
-- arrayP = do
--   S.modify' (\ bs@(BuildState{..}) -> bs { path=(show (-1)):path })
--   c <- brackets (arrayEntryP `sepBy` comma)
--   S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
--   return $ JArray c
  
-- pairP = do
--   key <- stringLiteral
--   symbol ":"
--   S.modify' (\ bs@(BuildState{..}) -> bs { path=key:path })
--   value <- jsonCP
--   S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
--   return (key, value)

-- objectP = do
--   c <- M.fromList <$> braces (pairP `sepBy` comma)
--   return $ JObject c

-- makeId :: [(Text, Text)] -> Text -> Text
-- makeId ss i = foldr (\ (a, b) x -> T.replace (T.concat ["${", a, "}"]) b x) i ss

-- topLevelObjectP = do
--   offset <- (fromIntegral . stateTokensProcessed) <$> getParserState
--   S.modify (\bs -> bs { offset=offset })
--   (ts, _) <- match objectP
--   end <- (fromIntegral . stateTokensProcessed) <$> getParserState
--   uuid <- liftIO CU.getUUID
--   BuildState{..} <- S.get
--   let comm = default_Communication { communication_id=makeId idVals ((T.pack . commId) params)
--                                    , communication_type=T.pack $ commType params
--                                    , communication_uuid=uuid
--                                    , communication_text=Just $ T.pack ts
--                                    , communication_sectionList=(Just . fromList . reverse) sections
--                                    } 
--   comms' <- if length comms >= 100
--             then
--               (liftIO $ CU.writeCommunications out comms) >> return [comm]
              
--             else
--               return $ comm:comms
--   S.modify (\ bs@(BuildState{..}) -> bs { sections=[]
--                                         , comms=comms'
--                                         , idVals=[]
--                                         })

-- symbol = L.symbol space
-- brackets = between (symbol "[") (symbol "]")
-- braces = between (symbol "{") (symbol "}")
-- lexeme = L.lexeme space
-- comma = symbol ","

data Parameters = Parameters { inputFile :: Maybe String
                             , outputFile :: Maybe String
                             , commType :: String
                             , commId :: String
                             , contentSectionTypes :: [String]
                             , attoparsec :: Bool
                             } deriving (Show)

parameters :: O.Parser Parameters
parameters = Parameters
             <$> (O.optional $ strOption (short 'i'
                                          <> long "input"
                                          <> metavar "INPUT_FILE"
                                          <> help "Text file of JSON objects"
                                         )
                 )
             <*> (O.optional $ strOption (short 'o'
                                          <> long "output"
                                          <> metavar "OUTPUT_FILE"
                                          <> help "Tar archive of Concrete Communications"
                                         )
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
             <*> switch (short 'a'
                          <> long "attoparsec"
                          <> help "Whether to use Attoparsec instead of Megaparsec"
                        )
                      
                      

--parseComms :: Parameters -> Input -> Handle -> I
-- parseComms ps t h = do
--   let s = State { stateInput=t
--                 , statePos=NE.fromList $ [initialPos "JSON"]
--                 , stateTokensProcessed=0
--                 , stateTabWidth=defaultTabWidth
--                 }
--       unwrapped = between space eof (some topLevelObjectP)
--   print $ (\(x, y) -> length <$> y) (runParser' unwrapped s)
  
  --print x
      --wrapped = between space eof (brackets (topLevelObjectP `sepBy` comma))
  --(r, BuildState {..}) <- (S.runStateT (runParserT' (wrapped <|> unwrapped) s) (BuildState [] [] [] 0 ps [] h))
  --(r, BuildState {..}) <- (S.runStateT (runParserT' unwrapped s) (BuildState [] [] [] 0 ps [] h))
  --liftIO $ CU.writeCommunications out comms
  --return comms

main = do
  ps <- execParser opts
  ofd <- case outputFile ps of
           Just f -> openFile f WriteMode
           Nothing -> return stdout
  -- t <- decodeUtf8 <$> case inputFile ps of
  --                       Just f -> case f of
  --                         ".gz" -> (liftM GZip.decompress . BS.readFile) f      
  --                         _ -> BS.readFile f
  --                       Nothing -> BS.hGetContents stdin
  -- parseComms ps t ofd

  h <- case inputFile ps of
    Just f -> case takeExtension f of
      ".gz" -> (liftM GZip.decompress . BS.readFile) f
      _ -> BS.readFile f
    Nothing -> BS.hGetContents stdin
  let cs = (if attoparsec ps == True then AP.fromText else MP.fromText) (decodeUtf8 h)
  case cs of
    Right (JArray x) -> print $ length x
    Right (JObject x) -> print $ (length . toList) x
    Right x -> print x
    Left e -> putStr e
  where 
    opts = info (helper <*> parameters)
           ( fullDesc
             <> progDesc "Ingest JSON into Concrete Communications"
           )
