{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import System.FilePath (takeExtension)
import Data.Concrete.Services.Fetch (ZipFetch(..), TarFetch(..), process, makeTarFetch, makeZipFetch)
import Data.Concrete.Services (runConcreteService, Compression(..))
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import System.IO (openFile, IOMode(..))
import Control.Monad (liftM)

data Parameters w = Parameters { file :: w ::: String <?> "File to fetch Communications from"
                               , port :: w ::: Int <?> "Port for a FetchCommunicationService"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

main = do
  ps <- unwrapRecord "Run a file-backed Fetch service: supports .tar and .zip"
  let f = file ps
  case takeExtension f of
    ".zip" -> do
      h <- makeZipFetch f
      runConcreteService (port ps) process h
    ".tar" -> do
      h <- makeTarFetch f
      runConcreteService (port ps) process h
