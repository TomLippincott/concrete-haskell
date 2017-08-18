{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import System.FilePath (takeExtension)
import Data.Concrete.Services.Store (ZipStore(..), TarStore(..), process, makeTarStore, makeZipStore)
import Data.Concrete.Services (runConcreteService, Compression(..))
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import System.IO (openFile, IOMode(..))
import Control.Monad (liftM)

data Parameters w = Parameters { file :: w ::: String <?> "File to store Communications in"
                               , port :: w ::: Int <?> "Port for a StoreCommunicationService"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

main = do
  ps <- unwrapRecord "Run a file-backed Store service: supports .tar and .tgz"
  let f = file ps
  case takeExtension f of
    --".zip" -> do
    --  h <- makeZipStore f
    --  runConcreteService (port ps) process h
    ".tar" -> do
      h <- makeTarStore f
      runConcreteService (port ps) process h
