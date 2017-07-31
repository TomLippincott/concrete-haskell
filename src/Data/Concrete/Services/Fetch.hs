{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances, BangPatterns #-}

module Data.Concrete.Services.Fetch ( HandleFetch(..)
                                    , ZipFetch(..)
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

-- Handle-backed

newtype HandleFetch = HandleFetch (Handle, Maybe Compression)

-- Zip-backed

newtype ZipFetch = ZipFetch (Handle, Maybe Compression)

-- Tar-backed

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
