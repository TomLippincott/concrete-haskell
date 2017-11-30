{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Data.Map.Strict (Map, toList, (!), keys)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack, take)
import Data.Text.Lazy.Encoding (decodeUtf8, decodeLatin1)
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn, hClose)
import System.FilePath (takeExtension)
import Data.Concrete.Utils (writeCommunication, getCompressor, getDecompressor)
import Data.Concrete.Services (connectToService)
import Data.Concrete.Services.Store (makeTarStore, makeHandleStore, makeZipStore, ZipStore(..), TarStore(..), HandleStore(..), storeDirect)
import Data.Concrete.Parsers (communicationParsers, ingest)
import Data.Concrete.Parsers.Utils (unfoldParse, finalizeCommunication)
import qualified Data.Concrete.Autogen.StoreCommunicationService_Client as StoreService
import Data.Concrete.Autogen.StoreCommunicationService_Iface (StoreCommunicationService_Iface(store))
import Options.Generic hiding (Text)
import Conduit

data Parameters w = Parameters { inputFile :: w ::: Maybe String <?> "Input text file, possibly compressed (.bz2 or .gz)"
                               , commType :: w ::: Text <?> "Value for the 'type' field of each Communication"
                               , commId :: w ::: Text <?> "Template for the 'id' field of each Communication"
                               , contentSectionTypes :: w ::: [Text] <?> "Section types that should count as 'content' rather than 'metadata'"
                               , format :: w ::: String <?> "Input format: (JSON-SEQUENCE, JSON-ARRAY, CSV, PTB, CONLL-U)"
                               , outputFile :: w ::: Maybe String <?> "An output file (.tar)"                               
                               , host :: w ::: Maybe String <?> "Host name for a StoreCommunicationService"
                               , port :: w ::: Maybe Int <?> "Port for a StoreCommunicationService"
                               , latin1 :: w ::: Maybe Bool <?> "Input is Latin-1 (8859) rather than UTF-8"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)
                      
main = do

  ps <- unwrapRecord "Ingest Concrete Communications from various formats"
  let decompress = getDecompressor ((fromMaybe "" . inputFile) ps)
      compress = getCompressor ((fromMaybe "" . outputFile) ps)
      content = contentSectionTypes ps
      cid = commId ps
      ctype = commType ps
      (_, cp, _, _) = (Map.fromList communicationParsers) ! (format ps)
      inFile = inputFile ps
      outFile = fromMaybe "" (outputFile ps)
      decode = if fromMaybe False (latin1 ps) == True then decodeLatin1 else decodeUtf8
      
  t <- case inputFile ps of
         Just f -> (liftM decompress . BS.readFile) f
         Nothing -> (liftM decompress . BS.hGetContents) stdin
  
  cb <- case (outputFile ps, host ps, port ps) of
          (Just f, Nothing, Nothing) -> case takeExtension f of
            ".tar" -> store <$> makeTarStore f
            ".zip" -> store <$> makeZipStore f
            _ -> store <$> makeHandleStore f
          (Nothing, Just h, Just p) -> do
            con <- connectToService h p
            return $ StoreService.store con
          (Nothing, Nothing, Nothing) -> return $ writeCommunication stdout
          _ -> error "Specify either an output file, a host and port, or nothing (for flat serialization to stdout)"

  ingest cp cb (\_ -> False) (decode t) content cid ctype
