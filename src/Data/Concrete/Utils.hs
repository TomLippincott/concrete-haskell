{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Utils
       (
         getUUID
       , createAnnotationMetadata
       , readCommunicationsFromBytes
       , readCommunicationsFromTar
       , readCommunicationsFromZip
       , writeCommunications
       , writeCommunication
       , writeCommunicationToZip
       , showCommunication
       , sendCommunication
       , connectToService
       ) where

import GHC.Generics
import qualified Data.Concrete as C
import Path.IO (resolveFile')
import Data.Concrete (Communication(..), Section(..), UUID(..), default_Communication, read_Communication)
--import Data.Concrete (StoreCommunicationService_Client)
import Data.Text
import Data.Maybe (fromJust, maybeToList, fromMaybe)
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
import Thrift.Protocol
import Thrift.Transport.IOBuffer
import Thrift.Transport
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Zip       as Zip
import qualified Codec.Archive.Tar.Entry as Tar
import Data.Time
import Data.Time.Clock.POSIX
import System.FilePath (takeFileName, (</>), (<.>))
import Data.Either (rights)
import Control.Monad (liftM, join)
import Data.Foldable (foldr)
import System.IO (Handle)
import qualified Data.Vector as V
import qualified Network as Net

getUUID :: IO UUID
getUUID = do
  uuid <- (T.pack . toString) <$> nextRandom
  return $ UUID uuid

readCommunicationsFromTar :: ByteString -> IO [Communication]
readCommunicationsFromTar bs = do
  let es = Tar.read bs
      cs = Tar.foldEntries (\e x -> (Tar.entryContent e):x) [] (\e -> []) es
  sequence $ L.map (\ (Tar.NormalFile bs _) -> stringToComm bs) cs

writeCommunicationsToTar :: [Communication] -> ByteString
writeCommunicationsToTar cs = error "unimplemented"

readCommunicationsFromZip :: String -> IO [Communication]
readCommunicationsFromZip f = do
  f' <- resolveFile' f
  es <- Zip.withArchive f' ((Map.keys <$> Zip.getEntries))
  bss <- Zip.withArchive f' ((sequence . (L.map Zip.getEntry)) es)
  sequence $ L.map (stringToComm . fromStrict) bss
  
writeCommunicationsToZip :: String -> [Communication] -> IO ()
writeCommunicationsToZip f cs = return ()

writeCommunicationToZip :: String -> [Communication] -> IO ()
writeCommunicationToZip f cs = return ()

writeCommunications :: Handle -> [Communication] -> IO ()
writeCommunications out cs = do
  let tarPath = "comms"
  texts <- sequence [commToString c | c <- cs]
  let names = rights [Tar.toTarPath False (tarPath </> ((T.unpack . C.communication_id) c) <.> "comm") | c <- cs]
      entries = [Tar.fileEntry n t|(n, t) <- L.zip names texts]
      t = Tar.write entries
  (BS.hPutStr out . GZip.compress) t

writeCommunication :: Handle -> Communication -> IO ()
writeCommunication out c = do
  t <- commToString c
  BS.hPutStr out t
 -- 
connectToService :: String -> Int -> IO (CompactProtocol (FramedTransport Handle), CompactProtocol (FramedTransport Handle))
connectToService h p = do
  transport <- hOpen (h, Net.PortNumber $ fromIntegral p)
  transport' <- openFramedTransport transport
  let protocol = CompactProtocol transport'
  return (protocol, protocol)

sendCommunication :: String -> Int -> Communication -> IO ()
sendCommunication h p c = do  
  return ()

readCommunicationsFromBytes :: BS.ByteString -> IO [Communication]
readCommunicationsFromBytes t = do
  transport <- newTString
  fillBuf (getRead transport) t
  let iproto = CompactProtocol transport
  c <- read_Communication iproto
  c' <- read_Communication iproto
  return [c']
  where
    readCommunication' pr cs = do
      o <- tIsOpen $ getTransport pr      
      case o of
        True -> do
          c <- read_Communication pr
          readCommunication' pr $ c:cs
        False -> return cs

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

showSection :: T.Text -> C.Section -> T.Text
showSection t s = T.concat ["    ", ((fromMaybe "*NO LABEL*" . C.section_label) s), " == ", t']
  where
    C.TextSpan s' e' = (fromJust . C.section_textSpan) s
    k = C.section_kind s
    t' = substr t (fromIntegral s') (fromIntegral e')

substr :: T.Text -> Int -> Int -> T.Text
substr t s e = res
  where
    (_, start) = T.splitAt (fromIntegral s) t    
    res = T.take (fromIntegral $ e - s) start

showCommunication :: Communication -> T.Text
showCommunication c = T.concat [C.communication_id c, " ", C.communication_type c, "\n  Content sections:", "\n", T.intercalate "\n" contentSects, "\n  Metadata sections: ", metadataText, "\n"]
  where
    ss = L.concat $ L.map V.toList (maybeToList (C.communication_sectionList c))
    t = (fromJust . C.communication_text) c
    contentSects = L.map (showSection t) ((L.filter (\x -> section_kind x == "content")) ss)
    metadataSects = L.map (fromMaybe "?" . C.section_label) ((L.filter (\x -> section_kind x /= "content")) ss)
    metadataText = T.intercalate ", " metadataSects
