{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map, toList, (!), keys)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.List (intercalate)
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack, take)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn, hClose)
import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip
import Data.Concrete.Utils (writeCommunication)
import qualified Data.Concrete.Utils as CU
import Data.Concrete.Parsers.Types (CommunicationParser)
import Data.Concrete.Parsers (communicationParsers, ingest)
import Options.Applicative ( Parser(..)
                           , option
                           , auto
                           , short
                           , long
                           , metavar
                           , strOption
                           , help
                           , execParser
                           , info
                           , helper
                           , fullDesc
                           , progDesc
                           , many
                           , switch
                           , optional
                           )

data Parameters = Parameters { inputFile :: Maybe String
                             , outputFile :: Maybe String
                             , commType :: String
                             , commId :: String
                             , contentSectionTypes :: [String]
                             , format :: String
                             } deriving (Show)

parameters :: Parser Parameters
parameters = Parameters
             <$> (optional $ strOption (short 'i'
                                         <> long "input"
                                         <> metavar "INPUT_FILE"
                                         <> help "Text file of JSON objects"
                                       )
                 )
             <*> (optional $ strOption (short 'o'
                                         <> long "output"
                                         <> metavar "OUTPUT_FILE"
                                         <> help "Where to write the Concrete Communications"
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
             <*> strOption (short 'f'
                            <> long "format"
                            <> help ("Input format: (" ++ ((intercalate "|" . keys) (Map.fromList communicationParsers)) ++ ")")
                           )
                      
main = do
  ps <- execParser opts
  
  ih <- case inputFile ps of
    Just f -> case takeExtension f of
      ".gz" -> (liftM GZip.decompress . BS.readFile) f
      _ -> BS.readFile f
    Nothing -> BS.hGetContents stdin
  let (_, p, _, _) = (Map.fromList communicationParsers) ! (format ps)

  oh <- case outputFile ps of
          Just f -> openFile f WriteMode
          Nothing -> return stdout
  let cb = case outputFile ps of
             Just f -> case takeExtension f of
               _ -> writeCommunication oh
             Nothing -> writeCommunication oh
  cs <- ingest cb p (decodeUtf8 ih) (contentSectionTypes ps) (commId ps) (commType ps)
  hClose oh
  where
    opts = info (helper <*> parameters)
           ( fullDesc
             <> progDesc "Ingest Concrete Communications from various formats"
           )
