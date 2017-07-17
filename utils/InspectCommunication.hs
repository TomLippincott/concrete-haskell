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
import qualified Data.Concrete as C
import qualified Data.Vector as V
import Control.Monad (liftM, mapM, join)
import Data.List (intercalate, concat)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromJust, catMaybes, maybeToList)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import System.FilePath (takeExtension)
import Options.Generic

data Parameters w = Parameters { inputFile :: w ::: Maybe String <?> "Input file, possibly compressed (.bz2 or .gz)"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

main = do
  ps <- unwrapRecord "Inspect Concrete Communications"
  cs <- join $ case inputFile ps of
    Just f -> case takeExtension f of
      ".tar" -> CU.readCommunicationsFromTar <$> (BS.readFile f)
      ".tgz" -> CU.readCommunicationsFromTar <$> ((liftM GZip.decompress . BS.readFile) f)
      ".tbz2" -> CU.readCommunicationsFromTar <$> ((liftM BZip.decompress . BS.readFile) f)
      ".zip" -> return $ CU.readCommunicationsFromZip f
      ".bz2" -> CU.readCommunicationsFromBytes <$> ((liftM BZip.decompress . BS.readFile) f)
      ".gz" -> CU.readCommunicationsFromBytes <$> ((liftM GZip.decompress . BS.readFile) f)
      _ -> CU.readCommunicationsFromBytes <$> (BS.readFile f)
    Nothing -> CU.readCommunicationsFromBytes <$> (BS.hGetContents stdin)
  putStr $ ((T.unpack . T.concat) $ [CU.showCommunication (head cs)])
