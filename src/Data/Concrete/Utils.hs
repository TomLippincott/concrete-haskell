{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}
module Data.Concrete.Utils
       (
         getUUID
       , createAnnotationMetadata
       , writeCommunication
       , showCommunication
       , commToString
       , incrementUUID
       , stringToComm
       ) where

import GHC.Generics
import Path.IO (resolveFile')
import Data.Concrete.Autogen.Communication_Types (Communication(..), default_Communication, read_Communication, write_Communication)
import Data.Concrete.Autogen.Structure_Types (Section(..))
import Data.Concrete.Autogen.Uuid_Types (UUID(..))
import Data.Concrete.Autogen.Metadata_Types (AnnotationMetadata(..), default_AnnotationMetadata)
import Data.Concrete.Autogen.Spans_Types (TextSpan(..))
import Data.Concrete.Autogen.Service_Iface (Service_Iface(about, alive))
import Data.Concrete.Autogen.Services_Types (ServiceInfo(..))
import Data.Concrete.Autogen.Access_Types (FetchResult(..), default_FetchResult)
import Data.Concrete.Autogen.StoreCommunicationService_Iface (StoreCommunicationService_Iface(store))
import Data.Concrete.Autogen.FetchCommunicationService_Iface (FetchCommunicationService_Iface(fetch))
import qualified Data.Concrete.Autogen.StoreCommunicationService as StoreCommunicationService
import Data.Text
import Data.Either (rights)
import Data.Maybe (fromJust, maybeToList, fromMaybe)
import Data.ByteString.Lazy
import Data.Map
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString, toWords, fromWords, fromString, nil)
import qualified Data.UUID as U
import qualified Data.Text.Lazy as T
import Thrift
import Thrift.Transport.Handle hiding (HandleSource)
import Thrift.Transport.Framed
import Thrift.Transport.Empty
import Thrift.Protocol.Compact
import Thrift.Protocol
import Thrift.Transport.IOBuffer
import Thrift.Transport
import Thrift.Server (runThreadedServer)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Zip       as Zip
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import Data.Time
import Data.Time.Clock.POSIX
import System.FilePath (takeFileName, (</>), (<.>))
import Data.Either (rights)
import Control.Monad (liftM, join)
import Data.Foldable (foldr)
import System.IO (Handle)
import qualified Data.Vector as V
import qualified Data.Binary.Get as G
import qualified Control.Monad.Extra as E
    
getUUID :: IO UUID
getUUID = do
  uuid <- (T.pack . toString) <$> nextRandom
  return $ UUID uuid

incrementUUID :: UUID -> UUID
incrementUUID (UUID u) = case toWords (fromMaybe nil $ fromString (T.unpack u)) of
  (a, b, c, d) -> UUID $ (T.pack . toString) (fromWords a b c (d + 1))

writeCommunication :: Handle -> Communication -> IO ()
writeCommunication out c = do
  t <- commToString c
  BS.hPutStr out t

entryToString :: Tar.EntryContent -> BS.ByteString
entryToString (Tar.NormalFile s _) = s

data TString = TString ReadBuffer WriteBuffer

getWrite :: TString -> WriteBuffer
getWrite (TString r w) = w

getRead :: TString -> ReadBuffer
getRead (TString r w) = r          

newTString = do
  w <- newWriteBuffer
  r <- newReadBuffer
  return $ TString r w

instance Transport TString where
    tIsOpen (TString r w) = do
      p <- peekBuf r
      case p of
        Nothing -> return False
        _ -> return True
    tClose (TString r w) = case peekBuf r of
                             _ -> return ()
    tRead (TString r w) i = readBuf r i
    tPeek (TString r w) = peekBuf r
    tWrite (TString r w) bs = writeBuf w bs
    tFlush (TString r w) = flushBuf w >> return ()
  
commToString :: Communication -> IO BS.ByteString
commToString c = do  
  otransport <- newTString
  let oproto = CompactProtocol otransport
  write_Communication oproto c
  flushBuf (getWrite otransport)

stringToComm :: BS.ByteString -> IO Communication
stringToComm s = do
  otransport@(TString r w) <- newTString
  fillBuf r s
  let oproto = CompactProtocol otransport
  read_Communication oproto

createAnnotationMetadata :: String -> IO AnnotationMetadata
createAnnotationMetadata s = do
  time <- round `fmap` getPOSIXTime
  return default_AnnotationMetadata { annotationMetadata_tool=T.pack s
                                    , annotationMetadata_timestamp=time
                                    }

showSection :: T.Text -> Section -> T.Text
showSection t s = T.concat ["    ", ((fromMaybe "*NO LABEL*" . section_label) s), " == ", t']
  where
    TextSpan s' e' = (fromJust . section_textSpan) s
    k = section_kind s
    t' = substr t (fromIntegral s') (fromIntegral e')

substr :: T.Text -> Int -> Int -> T.Text
substr t s e = res
  where
    (_, start) = T.splitAt (fromIntegral s) t    
    res = T.take (fromIntegral $ e - s) start

showCommunication :: Communication -> T.Text
showCommunication c = T.concat [communication_id c, " ", communication_type c, "\n  Content sections:\n", T.intercalate "\n" contentSects, "\n  ", metadataText]
  where
    ss = L.concat $ L.map V.toList (maybeToList (communication_sectionList c))
    t = (fromJust . communication_text) c
    contentSects = L.map (showSection t) ((L.filter (\x -> section_kind x == "content")) ss)
    metadataSects = L.map (fromMaybe "?" . section_label) ((L.filter (\x -> section_kind x /= "content")) ss)
    --metadataText = T.intercalate ", " metadataSects
    metadataText = T.concat [(T.pack . show) (L.length metadataSects), " metadata sections"]
