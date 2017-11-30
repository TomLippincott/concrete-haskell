{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}


module Main (main) where


import Control.Monad (liftM, foldM)
import Data.Concrete.Services (connectToService)
import Data.Concrete.Services.Store (makeTarStore, storeDirect)
import Data.Concrete.Services.Fetch (makeTarFetch, fetchDirect)
import Data.Concrete.Autogen.AnnotateCommunicationService_Client (annotate)
import Options.Generic


data Parameters w = Parameters { inputFile :: w ::: String <?> "Input tar file"
                               , outputFile :: w ::: String <?> "Output tar file"
                               , host :: w ::: [String] <?> "Annotator host (may be specified multiple times)"
                               , port :: w ::: [Int] <?> "Annotator port (may be specified multiple times)"
                               } deriving (Generic)


instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)


main = do
  opts <- unwrapRecord "Run a tar file of Communications through a sequence of AnnotateCommunicationServices"
  source <- makeTarFetch $ inputFile opts
  comms <- fetchDirect source
  services <- mapM (uncurry connectToService) (zip (host opts) (port opts))
  comms' <- foldM (\cs s -> mapM (annotate s) cs) comms services
  oh <- makeTarStore $ outputFile opts
  storeDirect oh comms'
