{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Utils
       (
         getUUID
       , createAnnotationMetadata
       , readCommunications
       , writeCommunications       
       ) where

import GHC.Generics
import qualified Data.Concrete as C
import Data.Concrete (Communication(..), UUID(..), default_Communication)
import Data.Text
import Data.ByteString.Lazy
import Data.Map
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import qualified Data.Text.Lazy as T
import Thrift
import Thrift.Transport.Handle
import Thrift.Transport.Framed
import Thrift.Transport.Empty
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON
import Thrift.Transport.IOBuffer
import Thrift.Transport
import qualified Data.List as L
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Data.Time
import Data.Time.Clock.POSIX
import System.FilePath (takeFileName, (</>), (<.>))
import Data.Either (rights)
import Control.Monad (liftM)
import Data.Foldable (foldr)
import System.IO (Handle)
  
getUUID :: IO UUID
getUUID = do
  uuid <- (T.pack . toString) <$> nextRandom
  return $ UUID uuid

writeCommunications :: Handle -> [Communication] -> IO ()
writeCommunications out cs = do
  let tarPath = "comms" -- (dropTarSuffix . takeFileName) out
  texts <- sequence [commToString c | c <- cs]
  let names = rights [Tar.toTarPath False (tarPath </> ((T.unpack . C.communication_id) c) <.> "comm") | c <- cs]
      entries = [Tar.fileEntry n t|(n, t) <- L.zip names texts]
      t = Tar.write entries
  (BS.hPutStr out . GZip.compress) t

readCommunications :: String -> IO [Communication]
readCommunications f = do
  t <- (liftM GZip.decompress . BS.readFile) f
  let es = Tar.read t
      Right cs = Tar.foldlEntries (\x y -> ((stringToComm . entryToString . Tar.entryContent) y):x) ([] :: [IO Communication]) es
  sequence cs

entryToString :: Tar.EntryContent -> BS.ByteString
entryToString (Tar.NormalFile s _) = s

data TString = TString ReadBuffer WriteBuffer

getWrite :: TString -> WriteBuffer
getWrite (TString r w) = w

newTString = do
  w <- newWriteBuffer
  r <- newReadBuffer
  return $ TString r w

instance Transport TString where
    tIsOpen = const $ return False
    tClose  = const $ return ()
    tRead (TString r w) i = readBuf r i --return ""
    tPeek (TString r w) = peekBuf r --const $ return Nothing
    tWrite (TString r w) bs = writeBuf w bs --return ()
    tFlush (TString r w) = flushBuf w >> return () --const$ return ()
  
commToString :: Communication -> IO BS.ByteString
commToString c = do
  otransport <- newTString
  let oproto = CompactProtocol otransport
  C.write_Communication oproto c
  flushBuf (getWrite otransport)

stringToComm :: BS.ByteString -> IO Communication
stringToComm s = do
  otransport@(TString r w) <- newTString
  fillBuf r s
  let oproto = CompactProtocol otransport
  C.read_Communication oproto

dropTarSuffix :: String -> String
dropTarSuffix f = (L.reverse . L.drop n . L.reverse) f
  where
    n = if ".tgz" `L.isSuffixOf` f then 4 else if ".tar.gz" `L.isSuffixOf` f then 7 else 0

createAnnotationMetadata :: String -> IO C.AnnotationMetadata
createAnnotationMetadata s = do
  time <- round `fmap` getPOSIXTime
  return C.default_AnnotationMetadata { C.annotationMetadata_tool=T.pack s
                                      , C.annotationMetadata_timestamp=time
                                      }
