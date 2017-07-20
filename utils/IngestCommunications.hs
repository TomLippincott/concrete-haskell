{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Network as Net
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
import Data.Concrete.Utils (writeCommunication, sendCommunication, connectToService)
import Data.Concrete (default_Communication)
import qualified Data.Concrete.Utils as CU
import Data.Concrete.Parsers.Types (CommunicationParser)
import Data.Concrete.Parsers (communicationParsers, ingest)
import qualified Data.Concrete.Services.StoreCommunicationService_Client as StoreService
import qualified Data.Concrete.Services.Service_Client as Service
import Options.Generic

data Parameters w = Parameters { inputFile :: w ::: Maybe String <?> "Input file, possibly compressed (.bz2 or .gz)"
                               , commType :: w ::: String <?> "Value for the 'type' field of each Communication"
                               , commId :: w ::: String <?> "Template for the 'id' field of each Communication"
                               , contentSectionTypes :: w ::: [String] <?> "Section types that should count as 'content' rather than 'metadata'"
                               , format :: w ::: String <?> "Input format: (JSON, CSV, PTB)"
                               , outputFile :: w ::: Maybe String <?> "An output file (.txt, .gz, .bz2, .tgz, .tbz2, .zip)"
                               , host :: w ::: Maybe String <?> "Host name for a StoreCommunicationService"
                               , port :: w ::: Maybe Int <?> "Port for a StoreCommunicationService"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)
                      
main = do
  ps <- unwrapRecord "Ingest Concrete Communications from various formats"
  ih <- case inputFile ps of
    Just f -> case takeExtension f of
      ".gz" -> (liftM GZip.decompress . BS.readFile) f
      _ -> BS.readFile f
    Nothing -> BS.hGetContents stdin
  let (_, cp, _, _) = (Map.fromList communicationParsers) ! (format ps)
  cb <- case (outputFile ps, host ps, port ps) of
          (Just f, Nothing, Nothing) -> case takeExtension f of
            "gz" -> writeCommunication <$> openFile f WriteMode
            "bz2" -> writeCommunication <$> openFile f WriteMode
            _ -> writeCommunication <$> openFile f WriteMode
          (Nothing, Just h, Just p) -> do
            con <- connectToService h p
            return $ StoreService.store con
          (Nothing, Nothing, Nothing) -> return $ writeCommunication stdout
          _ -> error "Specify either an output file,  a host and port, or nothing (for stdout)"
  cs <- ingest (putStrLn . unpack . CU.showCommunication) cp (decodeUtf8 ih) (contentSectionTypes ps) (commId ps) (commType ps)          
  --cs <- ingest print cp (decodeUtf8 ih) (contentSectionTypes ps) (commId ps) (commType ps)
  return ()
