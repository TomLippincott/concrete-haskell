{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (toList, (!), keys)
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

testFormat :: (String, (desc, CommunicationParser (), [String], String)) -> IO ()
testFormat (f, (d, p, c, i)) = do
  let fname = printf "tests/data/example.%s.gz" f :: String
  ih <- (liftM GZip.decompress . BS.readFile) fname
  putStrLn fname
  putStrLn "***********************\n"
  ingest (\x -> putStrLn $ unpack $ T.concat [showCommunication x, "\n"]) p (decodeUtf8 ih) c i "concrete-haskell unit test data"
  return ()

main = putStrLn "\nTesting parsers:" >> (sequence $ map testFormat (communicationParsers))
