{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-|
Description: Implementations of FetchCommunicationService for various backends
-}

module Data.Concrete.Services.Fetch ( HandleFetch(..)
                                    , makeHandleFetch
                                    , ZipFetch(..)
                                    , makeZipFetch
                                    , TarFetch(..)
                                    , makeTarFetch                                    
                                    , process
                                    ) where

import System.IO (Handle)
import qualified Data.Vector as V
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Zip       as Zip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import System.IO (openFile, IOMode(..), hTell)
import System.FilePath (takeExtension)
import Data.Either (rights)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Concrete.Utils (stringToComm)
import Data.Concrete.Services (Compression(..))
import Data.Concrete.Autogen.Communication_Types (Communication(..), default_Communication, read_Communication, write_Communication)
import Data.Concrete.Autogen.Service_Iface (Service_Iface(about, alive))
import Data.Concrete.Autogen.Services_Types (ServiceInfo(..))
import Data.Concrete.Autogen.Access_Types (FetchResult(..), default_FetchResult, FetchRequest(..))
import Data.Concrete.Autogen.FetchCommunicationService_Iface (FetchCommunicationService_Iface(fetch, getCommunicationIDs, getCommunicationCount))
import Data.Concrete.Autogen.FetchCommunicationService (process)
import Data.List (genericDrop, genericTake, genericLength)
import Data.Text.Lazy (Text, pack, unpack)
import Path.IO (resolveFile')
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import Control.Monad (liftM)

-- | Handle-based Fetch backend
newtype HandleFetch = HandleFetch Handle

instance Service_Iface HandleFetch where
  about _ = return $ ServiceInfo "Handle-backed FetchCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance FetchCommunicationService_Iface HandleFetch where
  fetch _ (FetchRequest ii _) = error "unimplemented"
  getCommunicationIDs _ offset count = error "unimplemented"
  getCommunicationCount _ = error "unimplemented"

makeHandleFetch :: String -> IO HandleFetch
makeHandleFetch f = error "unimplemented"

-- | Zip-based Fetch backend
newtype ZipFetch = ZipFetch ((Map String Zip.EntrySelector), String)

instance Service_Iface ZipFetch where
  about _ = return $ ServiceInfo "Zip-backed FetchCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance FetchCommunicationService_Iface ZipFetch where
  fetch (ZipFetch (ms, f)) (FetchRequest ii _) = do
    f' <- resolveFile' f
    let ids = map T.unpack (V.toList ii)
        es = map (\i -> ms Map.! i) ids
    ss <- liftIO $ Zip.withArchive f' (sequence $ map (liftM LBS.fromStrict . Zip.getEntry) es)
    cs <- sequence $ map stringToComm ss
    return $ default_FetchResult { fetchResult_communications=V.fromList cs }
  getCommunicationIDs (ZipFetch (ms, f)) offset count = return $ V.fromList $ ((map (pack . fst)) . genericTake count . genericDrop offset . Map.toList) ms
  getCommunicationCount (ZipFetch (ms, f)) = return ((genericLength . Map.toList) ms)

-- | Create a Zip-based Fetch handler based on the given file
makeZipFetch :: String -> IO ZipFetch
makeZipFetch f = do
  f' <- resolveFile' f  
  es <- liftIO $ Zip.withArchive f' Zip.getEntries
  ms <- Map.fromList <$> mapM (\e -> do
                                  s <- liftIO $ Zip.withArchive f' (Zip.getEntry e)
                                  c <- stringToComm (LBS.fromStrict s)
                                  return (T.unpack $ communication_id c, e)) (Map.keys es)
  return $ ZipFetch (ms, f)

-- | Tar-based Fetch backend
newtype TarFetch = TarFetch (Handle, (LBS.ByteString -> LBS.ByteString), Tar.TarIndex, Map String FilePath)

instance Service_Iface TarFetch where
  about _ = return $ ServiceInfo "Tar-backed FetchCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance FetchCommunicationService_Iface TarFetch where
  fetch (TarFetch (h, c, i, l)) ii = do
    cc <- sequence $ map fetchOne ((map unpack . V.toList . fetchRequest_communicationIds) ii)
    return $ default_FetchResult { fetchResult_communications=V.fromList cc }
    where
      fetchOne :: String -> IO Communication
      fetchOne p = do
        let (Just (Tar.TarFileEntry o)) = Tar.lookup i (l Map.! p)
        e <- Tar.hReadEntry h o
        stringToComm ((((\ (Tar.NormalFile bs _) -> bs) . Tar.entryContent) ) e)
  getCommunicationIDs (TarFetch (_, _, _, l)) offset count = return $ V.fromList $ ((map (pack . fst)) . genericTake count . genericDrop offset . Map.toList) l
  getCommunicationCount (TarFetch (_, _, _, l)) = return ((genericLength . Map.toList) l)

-- | Create a Tar-based Fetch handler based on the given file
makeTarFetch :: String -> IO TarFetch
makeTarFetch f = do
  let c = case takeExtension f of
            --".tgz" -> GZip.decompress
            --".tbz2" -> BZip.decompress
            ".tar" -> id
  h <- openFile f ReadMode
  bs <- c <$> LBS.readFile f
  let e = Tar.read bs
  (l, i) <- build e
  h' <- openFile f ReadMode
  return $ TarFetch (h, c, i, Map.fromList l)

commFromEntry :: Tar.Entry -> IO Communication
commFromEntry e = stringToComm (((\ (Tar.NormalFile bs _) -> bs) . Tar.entryContent) e)

build :: Tar.Entries e -> IO ([(String, FilePath)], Tar.TarIndex)
build = go ([], Tar.empty)
  where
    go :: ([(String, FilePath)], Tar.IndexBuilder) -> Tar.Entries e -> IO ([(String, FilePath)], Tar.TarIndex)
    go (l, !builder) (Tar.Next e es) = do
      c <- commFromEntry e
      go (((unpack . communication_id) c, Tar.entryPath e):l, Tar.addNextEntry e builder) es
    go (l, !builder) (Tar.Done) = do
      return (l, Tar.finalise builder)
