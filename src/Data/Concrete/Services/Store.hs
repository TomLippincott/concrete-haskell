{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Data.Concrete.Services.Store ( HandleStore(..)
                                    , ZipStore(..)
                                    , TarStore(..)
                                    , process
                                    , makeTarStore
                                    ) where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Zip       as Zip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import System.IO (Handle, hSeek, hTell, hFlush, SeekMode(..))
import qualified Data.Text.Lazy as T
import qualified Data.List as L
import Path.IO (resolveFile')
import Data.Either (rights)
import Path (Path, Dir, File, Abs, filename)
import Data.Concrete.Services (Compression(..))
import Data.Concrete.Autogen.Communication_Types (Communication(..), default_Communication, read_Communication, write_Communication)
import Data.Concrete.Autogen.Service_Iface (Service_Iface(about, alive))
import Data.Concrete.Autogen.Services_Types (ServiceInfo(..))
import Data.Concrete.Autogen.StoreCommunicationService_Iface (StoreCommunicationService_Iface(store))
import Data.Concrete.Autogen.StoreCommunicationService (process)
import Data.Concrete.Utils (commToString)
import System.IO (openFile, IOMode(..))
import Control.Monad (liftM)
import System.FilePath (takeExtension)

lift1st :: Monad m => (m a, b) -> m (a, b)
lift1st (f, s) = do
  f' <- f
  return (f', s)


-- Handle-backed

newtype HandleStore = HandleStore (Handle, Maybe Compression)

instance Service_Iface HandleStore where
  about _ = return $ ServiceInfo "Flat-file-backed StoreCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True  

instance StoreCommunicationService_Iface HandleStore where
  store (HandleStore (h, c)) comm = do
    t <- commToString comm
    let c' = case c of
               Nothing -> id
               Just GZip -> GZip.compress
               Just BZip -> BZip.compress
    LBS.hPutStr h (c' t)

-- Zip-backed

newtype ZipStore = ZipStore (Handle, Maybe Compression)

instance Service_Iface (Zip.ZipArchive ()) where
  about _ = return $ ServiceInfo "Zip-backed StoreCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance StoreCommunicationService_Iface (Zip.ZipArchive ()) where
  store _ c = do
    bs <- commToString c
    f <- filename <$> resolveFile' ((T.unpack . communication_id) c)
    es <- Zip.mkEntrySelector f
    return $ Zip.addEntry Zip.BZip2 (LBS.toStrict bs) es
    print $ T.unpack (communication_id c)
    return ()

-- Tar-backed

newtype TarStore = TarStore (Handle, (LBS.ByteString -> LBS.ByteString), SBS.ByteString, Integer)

makeTarStore :: String -> IO TarStore
makeTarStore f = do
  (h, c) <- case takeExtension f of
              ".tgz" -> lift1st (openFile f WriteMode, GZip.compress)
              ".tbz2" -> lift1st (openFile f WriteMode, BZip.compress)
              ".tar" -> lift1st (openFile f WriteMode, id)
  let pad =  LBS.toStrict (c (LBS.replicate (1024) 0))
  return $ TarStore (h, c, pad, fromIntegral $ - (SBS.length pad))

instance Service_Iface TarStore where
  about _ = return $ ServiceInfo "Tar-backed StoreCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance StoreCommunicationService_Iface TarStore where
  store (TarStore (h, c, pad, o)) comm = do
    cur <- hTell h
    if cur == 0 then return () else hSeek h RelativeSeek o
    t <- commToString comm
    let e = Tar.fileEntry ((L.head . rights) [Tar.toTarPath False ((T.unpack . communication_id) comm)]) t
        bs' = Tar.write [e]
        bs'' = LBS.toStrict bs'
        l = SBS.length bs''
        bs''' = SBS.take (l - 1024) bs''
    LBS.hPutStr h (c (LBS.fromStrict bs'''))
    SBS.hPutStr h pad
    hFlush h
