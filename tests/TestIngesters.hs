{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (toList, (!), keys)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack, take)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip
import Data.Concrete.Utils (writeCommunication, showCommunication)
import Data.Concrete.Parsers.Types (CommunicationParser)
import Data.Concrete.Parsers (communicationParsers, ingest)
import Data.Concrete.Parsers.Utils (finalizeCommunication)
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Text.Printf (printf)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Data.Concrete.Services.Store as Store
import qualified Data.Concrete.Services.Fetch as Fetch
import Data.Concrete.Services.Store (ZipStore(..), TarStore(..), HandleStore(..), makeTarStore, makeZipStore, makeHandleStore)
import Data.Concrete.Services.Fetch (ZipFetch(..), TarFetch(..), HandleFetch(..), makeTarFetch, makeZipFetch, makeHandleFetch)
import Data.Concrete.Services (runConcreteService, Compression(..), connectToService)
import qualified Data.Concrete.Autogen.FetchCommunicationService_Client as FetchService
import qualified Data.Concrete.Autogen.StoreCommunicationService_Client as StoreService
import qualified Data.Concrete.Autogen.Service_Client as Service
import Data.Concrete.Autogen.Access_Types (FetchRequest(..), default_FetchRequest, FetchResult(..))
import System.IO.Unsafe
import qualified Codec.Compression.GZip as GZip
import qualified Data.Vector as V
import Conduit
import System.Exit (exitFailure)
 
testFormat (name, (desc, src, contentSects, idStr)) = do
  putStrLn $ "\nTesting parser for " ++ name
  let inputFile = printf "tests/data/example.%s.gz" name :: String
  txt <- (liftM GZip.decompress . BS.readFile) inputFile
  ingest src (\c -> print $ communication_id c) (\_ -> True) (decodeUtf8 txt) contentSects idStr "test comms"

main = do  
  mapM testFormat communicationParsers
  exitFailure
