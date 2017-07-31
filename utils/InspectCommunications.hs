{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Data.Concrete.Utils as CU
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import qualified Data.Concrete.Autogen.FetchCommunicationService_Client as FetchService
import Data.Concrete.Autogen.Access_Types (FetchRequest(..))
import Data.Concrete.Services (connectToService)
import qualified Data.Vector as V
import Control.Monad (liftM, mapM, join)
import Data.List (intercalate, concat)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromJust, catMaybes, maybeToList, fromMaybe)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import System.FilePath (takeExtension)
import Options.Generic

data Parameters w = Parameters { inputFile :: w ::: Maybe String <?> "Input file, possibly compressed (.bz2 or .gz)"
                               , start :: w ::: Maybe Int <?> "Index of first Communication to show"
                               , end :: w ::: Maybe Int <?> "Index of last Communication to show"
                               , host :: w ::: Maybe String <?> "Host name for FetchCommunicationService"
                               , port :: w ::: Maybe Int <?> "Port number for FetchCommunicationService"                               
                               }
                  deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

main = do
  ps <- unwrapRecord "Inspect Concrete Communications"
  let s = start ps
      e = end ps
      cb = (\x -> (putStrLn . T.unpack) (CU.showCommunication x))
  cs <- case inputFile ps of
    Nothing -> do
      con <- connectToService ((fromMaybe "0.0.0.0" . host) ps) ((fromMaybe 9090 . port) ps)
      c <- FetchService.getCommunicationCount con
      print c
      l <- FetchService.getCommunicationIDs con 0 c
      --print l
      c' <- FetchService.fetch con (FetchRequest (V.drop 50 l) Nothing)
      print c'
      return ()
    Just f -> case takeExtension f of
      --".tar" -> CU.readCommunicationsFromTar <$> (BS.readFile f)
      --".tgz" -> CU.readCommunicationsFromTar <$> ((liftM GZip.decompress . BS.readFile) f)
      --".tbz2" -> CU.readCommunicationsFromTar <$> ((liftM BZip.decompress . BS.readFile) f)
      --".zip" -> return $ CU.readCommunicationsFromZip f
      --".bz2" -> CU.mapCommunicationsFromBytes cb s e <$> ((liftM BZip.decompress . BS.readFile) f)
      --".gz" -> CU.mapCommunicationsFromBytes cb s e <$> ((liftM GZip.decompress . BS.readFile) f)
      _ -> error "unimplemented" --CU.mapCommunicationsFromBytes cb s e <$> (BS.readFile f)
    --Nothing -> CU.readCommunicationsFromBytes <$> (BS.hGetContents stdin)
  return ()

