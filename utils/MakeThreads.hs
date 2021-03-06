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
import Data.Map.Strict (Map, toList, (!), keys)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack, take)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn, hClose)
import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip
import Data.Concrete.Utils (writeCommunication, getCompressor, getDecompressor)
import Data.Concrete.Services (connectToService)
import Data.Concrete.Services.Store (makeTarStore, storeDirect)
import Data.Concrete.Services.Fetch (makeTarFetch, fetchDirect)
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import qualified Data.Concrete.Utils as CU
import Data.Concrete.Parsers.Types (CommunicationParser)
import Data.Concrete.Parsers (communicationParsers, ingest)
import qualified Data.Concrete.Autogen.StoreCommunicationService_Client as StoreService
import qualified Data.Concrete.Autogen.Service_Client as Service
import Options.Generic

data Parameters w = Parameters { inputFile :: w ::: String <?> "Input tar file"
                               , outputFile :: w ::: String <?> "Output file"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)
                      
main = do
  ps <- unwrapRecord "Transform a file of Communications into a MultiGraph"
  ih <- makeTarFetch $ inputFile ps
  cs <- fetchDirect ih
  print $ length cs
  oh <- openFile (outputFile ps) WriteMode
  print $ head cs
  --storeDirect oh cs
  return ()
