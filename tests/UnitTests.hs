{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (toList, (!), keys)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack, take)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip
import Data.Concrete.Utils (writeCommunication, showCommunication)
import Data.Concrete.Parsers.Types (CommunicationParser)
import Data.Concrete.Parsers (communicationParsers, ingest)
import Data.Concrete (Communication(..))
import Text.Printf (printf)

testFormat :: (String, (desc, CommunicationParser (), [String], String)) -> IO ()
testFormat (f, (d, p, c, i)) = do
  let fname = printf "tests/data/example.%s.gz" f :: String
  ih <- (liftM GZip.decompress . BS.readFile) fname
  putStrLn fname
  ingest (\x -> putStrLn $ unpack $ showCommunication x) p (decodeUtf8 ih) c i "concrete-haskell unit test data"
  --["catchphrase", "name", "age", "relatives.0.name", "favorites.friend.name"] "test_id_${name}_${}" "test_comm"  
  return ()

main = putStrLn "\nTesting parsers:" >> (sequence $ map testFormat (toList communicationParsers))
