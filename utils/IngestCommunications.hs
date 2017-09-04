{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where


import qualified Network as Net
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map.Strict (Map, toList, (!), keys)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)
import Control.Monad (void, join, liftM)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn, hClose)
import System.FilePath (takeExtension)
import qualified Codec.Compression.GZip as GZip
import Data.Concrete.Utils (writeCommunication, getCompressor, getDecompressor)
import Data.Concrete.Services (connectToService)
import Data.Concrete.Services.Store (makeTarStore, storeDirect)
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import qualified Data.Concrete.Utils as CU
import Data.Concrete.Parsers.Types (CommunicationParser)
import Data.Concrete.Parsers (communicationParsers, ingest)
import qualified Data.Concrete.Autogen.StoreCommunicationService_Client as StoreService
import qualified Data.Concrete.Autogen.Service_Client as Service
import Options.Generic hiding (Text)

data Parameters w = Parameters { inputFile :: w ::: Maybe String <?> "Input file, possibly compressed (.bz2 or .gz)"
                               , commType :: w ::: Text <?> "Value for the 'type' field of each Communication"
                               , commId :: w ::: Text <?> "Template for the 'id' field of each Communication"
                               , contentSectionTypes :: w ::: [Text] <?> "Section types that should count as 'content' rather than 'metadata'"
                               , format :: w ::: String <?> "Input format: (JSON, JSON-LINE, CSV)"
                               , outputFile :: w ::: Maybe String <?> "An output file (.txt, .gz, .bz2, .tgz, .tbz2, .zip)"
                               , host :: w ::: Maybe String <?> "Host name for a StoreCommunicationService"
                               , port :: w ::: Maybe Int <?> "Port for a StoreCommunicationService"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)
                      
main = do
  ps <- unwrapRecord "Ingest Concrete Communications from various formats"
  let compress = getCompressor ((fromMaybe "" . inputFile) ps)
      decompress = getDecompressor ((fromMaybe "" . outputFile) ps)
  ih <- case inputFile ps of
    --Just f -> (liftM decompress . BS.readFile) f
    Just f ->  BS.readFile f    
    Nothing -> (liftM decompress . BS.hGetContents) stdin
  let (_, cp, _, _) = (Map.fromList communicationParsers) ! (format ps)

  cs <- take 40 <$> ingest cp (decodeUtf8 ih) (contentSectionTypes ps) (commId ps) (commType ps)

  --print $ length cs
  oh <- makeTarStore (fromJust $ outputFile ps)
  storeDirect oh cs


  -- case (outputFile ps, host ps, port ps) of
  --   (Just f, Nothing, Nothing) ->

  --     case takeExtension f of
  --                                   _ -> storeDirect <$> makeTarStore f <*> 

  
