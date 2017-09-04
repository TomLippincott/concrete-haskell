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
import System.Exit (exitWith, ExitCode(..))

testFormat :: (String, (desc, CommunicationParser [Communication], [String], String)) -> IO ()
testFormat (f, (d, p, c, i)) = do
  let inputFile = printf "tests/data/example.%s.gz" f :: String
  putStrLn (printf "\t%s" f :: String)
  con <- connectToService "localhost" 9090
  ih <- (liftM GZip.decompress . BS.readFile) inputFile
  let (_, cp, _, _) = (Map.fromList communicationParsers) ! f
  cs <- ingest p (decodeUtf8 ih) c i "concrete-haskell unit test data"
  print $ length cs
  -- (\c -> do
  --                  putStrLn $ printf "\t\t%s" (communication_id c)
  --                  StoreService.store con c) 
  return ()


testFetch :: IO ()
testFetch = do
  putStrLn "Testing fetch service:"
  con <- connectToService "localhost" 9091
  c <- FetchService.getCommunicationCount con
  putStrLn $ printf "\tFetch service reports %d Communications" c
  ids <- FetchService.getCommunicationIDs con 0 c
  putStrLn $ printf "\tReceived %d Communication IDs" (length ids)
  FetchResult {..} <- FetchService.fetch con $ default_FetchRequest { fetchRequest_communicationIds=ids }
  sequence $ map (putStrLn . printf "\t\tFetched Communication with ID == %s" . T.unpack . communication_id) (V.toList fetchResult_communications)
  return ()
  

main = do
  putStrLn "\nTesting parsers + fetch and store services:"
  let outputFile = "test.gz"
  store <- C.forkIO $ do
    case takeExtension outputFile of
      ".zip" -> do
        h <- makeZipStore outputFile
        runConcreteService 9090 Store.process h
      ".tar" -> do
        h <- makeTarStore outputFile
        runConcreteService 9090 Store.process h
      _ -> do
        h <- makeHandleStore outputFile
        runConcreteService 9090 Store.process h
  C.threadDelay 1000000
  sequence $ map testFormat communicationParsers
  C.killThread store
  exitWith (ExitFailure 1)
  -- C.threadDelay 1000000
  -- fetch <- C.forkIO $ do
  --   case takeExtension outputFile of
  --     ".zip" -> do
  --       h <- makeZipFetch outputFile
  --       runConcreteService 9091 Fetch.process h
  --     ".tar" -> do
  --       h <- makeTarFetch outputFile
  --       runConcreteService 9091 Fetch.process h
  --     _ -> do
  --       h <- makeHandleFetch outputFile
  --       runConcreteService 9091 Fetch.process h
  -- C.threadDelay 1000000
  -- testFetch
  -- C.killThread fetch
