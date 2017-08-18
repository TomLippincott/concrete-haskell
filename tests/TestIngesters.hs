{-# LANGUAGE OverloadedStrings #-}
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
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Text.Printf (printf)
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import Data.Concrete.Services.Store (ZipStore(..), TarStore(..), process, makeTarStore, makeZipStore)
import Data.Concrete.Services (runConcreteService, Compression(..), connectToService)
import qualified Data.Concrete.Autogen.FetchCommunicationService_Client as FetchService
import qualified Data.Concrete.Autogen.StoreCommunicationService_Client as StoreService
import qualified Data.Concrete.Autogen.Service_Client as Service
import System.IO.Unsafe
import qualified Codec.Compression.GZip as GZip


testFormat :: (String, (desc, CommunicationParser (), [String], String)) -> IO ()
testFormat (f, (d, p, c, i)) = do
  let inputFile = printf "tests/data/example.%s.gz" f :: String
  putStrLn (printf "\t%s" f :: String)
  con <- connectToService "localhost" 9090
  ih <- (liftM GZip.decompress . BS.readFile) inputFile
  let (_, cp, _, _) = (Map.fromList communicationParsers) ! f
  ingest (\c -> do
             putStrLn $ printf "\t\t%s" (communication_id c)
             StoreService.store con c) p (decodeUtf8 ih) c i "concrete-haskell unit test data"
  return ()


main = do
  putStrLn "\nTesting parsers:"
  let outputFile = "test.zip"
  store <- C.forkIO $ do
    case takeExtension outputFile of
      ".zip" -> do
        h <- makeZipStore outputFile
        runConcreteService 9090 process h
      ".tar" -> do
        h <- makeTarStore outputFile
        runConcreteService 9090 process h
  C.threadDelay 1000000
  sequence $ map testFormat (communicationParsers)
  C.killThread store
