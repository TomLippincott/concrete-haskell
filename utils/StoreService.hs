{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import System.FilePath (takeExtension)
import Data.Concrete.Services.Store (ZipStore(..), TarStore(..), HandleStore(..), process, makeTarStore)
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
  h <- makeTarStore f
  runConcreteService (port ps) h process
