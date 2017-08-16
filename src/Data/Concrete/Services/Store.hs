{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}
{-|
Description: Implementations of StoreCommunicationService
-}

module Data.Concrete.Services.Store ( ZipStore(..)
                                    , TarStore(..)
                                    , process
                                    , makeTarStore
                                    , makeZipStore
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
import Control.Monad.IO.Class (liftIO)

lift1st :: Monad m => (m a, b) -> m (a, b)
lift1st (f, s) = do
  f' <- f
  return (f', s)

-- | Handle-based Store backend
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

-- | Zip-based Store backend
newtype ZipStore = ZipStore String

instance Service_Iface ZipStore where
  about _ = return $ ServiceInfo "Zip-backed StoreCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance StoreCommunicationService_Iface ZipStore where
  store (ZipStore ff) c = do
    f' <- resolveFile' ff
    bs <- commToString c
    f <- filename <$> resolveFile' ((T.unpack . communication_id) c)
    es <- Zip.mkEntrySelector f
    liftIO $ Zip.withArchive f' $ Zip.addEntry Zip.BZip2 (LBS.toStrict bs) es
    return ()

-- | Create a Zip-backed Store handler based on the given file
makeZipStore :: String -> IO ZipStore
makeZipStore f = return $ ZipStore f

-- | Tar-based Store backend
newtype TarStore = TarStore (Handle, (LBS.ByteString -> LBS.ByteString), SBS.ByteString, Integer)

-- | Create a Tar-backed Store handler based on the given file
makeTarStore :: String -> IO TarStore
makeTarStore f = do
  (h, c) <- case takeExtension f of
              --".tgz" -> lift1st (openFile f WriteMode, GZip.compress)
              --".tbz2" -> lift1st (openFile f WriteMode, BZip.compress)
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
