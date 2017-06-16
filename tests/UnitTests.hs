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

testFormat :: (String, (desc, CommunicationParser ())) -> IO ()
testFormat (f, (_, p)) = do
  let fname = printf "tests/data/example.%s.gz" f :: String
  ih <- (liftM GZip.decompress . BS.readFile) fname
  putStrLn fname
  --ingest (\x -> putStrLn $ (unpack . communication_id) x) p (decodeUtf8 ih) ["catchphrase"] "test_id_${}" "test_comm"
  ingest (\x -> putStr $ unpack $ showCommunication x) p (decodeUtf8 ih) ["catchphrase"] "test_id_${}" "test_comm"  
  return ()

main = putStrLn "\nTesting parsers:" >> (sequence $ map testFormat (toList communicationParsers))
