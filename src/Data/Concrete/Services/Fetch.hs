{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
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
                                    , fetchDirect
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
import qualified "zip-conduit" Codec.Archive.Zip       as ZipC
import qualified "zip" Codec.Archive.Zip       as Zip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as TarIndex
import Codec.Archive.Tar.Index (TarIndex)
--import Data.Either (fromRight)
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
import Control.Monad (liftM, when)
import Conduit
import Data.Conduit
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.BZlib (bunzip2)
import qualified Crypto.Hash.Conduit as CH
import qualified Data.Conduit.Tar    as CT
import Control.DeepSeq

fetchDirect :: FetchCommunicationService_Iface a => a -> IO [Communication]
fetchDirect f = do
  n <- getCommunicationCount f
  is <- getCommunicationIDs f 0 n
  FetchResult cs <- fetch f (FetchRequest is Nothing)
  return $ V.toList cs

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
newtype ZipFetch = ZipFetch (String, Map Text String)

instance Service_Iface ZipFetch where
  about _ = return $ ServiceInfo "Zip-backed FetchCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance FetchCommunicationService_Iface ZipFetch where
  fetch (ZipFetch (f, ms)) (FetchRequest ii _) = error "" --do
    -- f' <- resolveFile' f
    -- let ids = map T.unpack (V.toList ii)
    --     es = map (\i -> ms Map.! i) ids
    -- ss <- liftIO $ Zip.withArchive f' (sequence $ map (liftM LBS.fromStrict . Zip.getEntry) es)
    -- cs <- sequence $ map stringToComm ss
    -- return $ default_FetchResult { fetchResult_communications=V.fromList cs }
  getCommunicationIDs (ZipFetch (f, ms)) offset count = error "" -- return $ V.fromList $ ((map (pack . fst)) . genericTake count . genericDrop offset . Map.toList) ms
  getCommunicationCount (ZipFetch (f, ms)) = error "" --return ((genericLength . Map.toList) ms)

-- | Create a Zip-based Fetch handler based on the given file
makeZipFetch :: String -> IO ZipFetch
makeZipFetch f = do
  f' <- resolveFile' f  
  es <- liftIO $ ZipC.withArchive f ZipC.entryNames -- Zip.getEntries
  -- ms <- Map.fromList <$> mapM (\e -> do
  --                                 s <- liftIO $ Zip.withArchive f' (Zip.getEntry e)
  --                                 c <- stringToComm (LBS.fromStrict s)
  --                                 return (T.unpack $ communication_id c, e)) (Map.keys es)
  return $ ZipFetch (f, Map.fromList [])

-- | Tar-based Fetch backend
newtype TarFetch = TarFetch (Handle, Map Text TarIndex.TarEntryOffset) -- [(String, Int, Int)])
  --(String, Int, Int)]
  deriving Show
--(Handle, (LBS.ByteString -> LBS.ByteString), Tar.TarIndex, Map String FilePath)

instance Service_Iface TarFetch where
  about _ = return $ ServiceInfo "Tar-backed FetchCommunicationService" "0.0.1" (Just "Haskell implementation")
  alive _ = return True

instance FetchCommunicationService_Iface TarFetch where
  fetch (TarFetch (h, m)) ii = do
    let i = [m Map.! n | n <- ((V.toList . fetchRequest_communicationIds) ii)]
    i' <- mapM (\ o -> do
                   e <- TarIndex.hReadEntry h o
                   case Tar.entryContent e of
                     Tar.NormalFile bs _ -> do
                       stringToComm bs
                       --return $!! (communication_id c, o)
               ) i
    return $ default_FetchResult { fetchResult_communications=V.fromList i' }

  --fetch (TarFetch (h, c, i, l)) ii = error "te"
    -- do
    -- cc <- sequence $ map fetchOne ((map unpack . V.toList . fetchRequest_communicationIds) ii)
    -- return $ default_FetchResult { fetchResult_communications=V.fromList cc }
    -- where
    --   fetchOne :: String -> IO Communication
    --   fetchOne p = do
    --     let (Just (Tar.TarFileEntry o)) = Tar.lookup i (l Map.! p)
    --     e <- Tar.hReadEntry h o
    --     stringToComm ((((\ (Tar.NormalFile bs _) -> bs) . Tar.entryContent) ) e)
  --getCommunicationIDs (TarFetch (_, _, _, l)) offset count = error "dd"
  getCommunicationIDs (TarFetch (_, m))  offset count = return $ (V.fromList . take count' . drop offset' . Map.keys) m
    where
      count' = fromIntegral count
      offset' = fromIntegral offset
    -- return $ V.fromList $ ((map (pack . fst)) . genericTake count . genericDrop offset . Map.toList) l
  --getCommunicationCount (TarFetch (_, _, _, l)) = error "as"
  getCommunicationCount (TarFetch (_, m)) = return $ fromIntegral $ Map.size m
  -- return ((genericLength . Map.toList) l)

-- | Create a Tar-based Fetch handler based on the given file
makeTarFetch :: String -> IO TarFetch
makeTarFetch f = do
  let c = case takeExtension f of
            ".tgz" -> GZip.decompress
            ".tbz2" -> BZip.decompress
            ".tar" -> id
  h <- openFile f ReadMode
  t <- LBS.hGetContents h
  h' <- openFile f ReadMode

  let es = Tar.read t
      cs = []
      Right i = (liftM TarIndex.toList . TarIndex.build) es

  --print i
  --print $ length i
  i' <- mapM (\(_, o) -> do
                 e <- TarIndex.hReadEntry h' o
                 case Tar.entryContent e of
                   Tar.NormalFile bs _ -> do
                     c <- stringToComm bs
                     return $!! (communication_id c, o)
             ) i
  --let cs = Tar.foldEntries (\e l -> ((T.pack . Tar.entryPath) e):l) [] (\e -> []) (Tar.read t)
  -- cs <- Tar.foldEntries (\e !l -> do
  --                           l' <- l
  --                           let con = Tar.entryContent e
  --                           case con of
  --                             Tar.NormalFile bs _ -> do
  --                               comm <- stringToComm bs
  --                               return $!! (communication_id comm):l'
  --                             _ -> return $!! l'
  --                       ) (return []) (\e -> return []) (Tar.read t)
  
  --is <- runConduitRes $ sourceFileBS f .| c .| CT.untar .| CT.withEntries commIdFromEntry .| sinkList
  --bs <- c <$> LBS.hGetContents h
  -- let e = Tar.read bs
  -- (l, i) <- build e
  -- h' <- openFile f ReadMode
  return $ TarFetch (h', Map.fromList i') --(fromRight (TarIndex.finalise Tar.empty). TarIndex.build . Tar.read) bs) --Tar.finalise Tar.empty)
  --is -- (h, c, i, is)

--entryName :: Monad m => CT.Header -> Conduit SBS.ByteString m (String, Int, Int)
--entryName c = yield $ (CT.headerFilePath c, CT.headerPayloadOffset c, CT.headerPayloadSize c)

--commFromEntry :: Tar.Entry -> IO Communication
commIdFromEntry e = when (CT.headerFileType e == CT.FTNormal) $ do
  --yield (communication_id default_Communication)
  c <- await
  case c of
    Just t -> do
      c' <- liftIO $ stringToComm (LBS.fromStrict t) -- default_Communication
      yield (communication_id c')
    _ -> yield ""
  --liftIO $ print c
  
  --let c' = default_Communication
  --
  --yield "" -- (communication_id c')
  --stringToComm (((\ (Tar.NormalFile bs _) -> bs) . Tar.entryContent) e)

-- build :: Tar.Entries e -> IO ([(String, FilePath)], Tar.TarIndex)
-- build = go ([], Tar.empty)
--   where
--     go :: ([(String, FilePath)], Tar.IndexBuilder) -> Tar.Entries e -> IO ([(String, FilePath)], Tar.TarIndex)
--     go (l, !builder) (Tar.Next e es) = do
--       c <- commFromEntry e
--       go (((unpack . communication_id) c, Tar.entryPath e):l, Tar.addNextEntry e builder) es
--     go (l, !builder) (Tar.Done) = do
--       return (l, Tar.finalise builder)
