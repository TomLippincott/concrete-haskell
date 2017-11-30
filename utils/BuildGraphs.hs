{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}


module Main (main) where


import Control.Monad (void, join, liftM)
import Data.Concrete.Services.Store (makeTarStore, storeDirect)
import Data.Concrete.Services.Fetch (makeTarFetch, makeZipFetch, fetchDirect)
import Data.Concrete.Autogen.Communication_Types (Communication(..))
import Data.Concrete.Autogen.Graph_Types
import Data.Concrete.Autogen.AnnotateCommunicationService_Client (annotate)
import Options.Generic
import Conduit
import System.FilePath (takeExtension)

data Parameters w = Parameters { inputFile :: w ::: String <?> "Input tar file"
                               , outputFile :: w ::: String <?> "Output tar file"
                               , identifier :: w ::: String <?> "Section that is used to identify a node"
                               , child :: w ::: Maybe String <?> "Section that identifies a node's child"
                               , parent :: w ::: Maybe String <?> "Section that identifies a node's parent"                               
                               } deriving (Generic)


instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)


main = do
  ps <- unwrapRecord "Transform a file of Communications into a MultiGraph based on a particular link"
  --ih <- makeTarFetch $ inputFile ps
  cs <- join $ case takeExtension (inputFile ps) of
          ".zip" -> (liftM fetchDirect . makeZipFetch) (inputFile ps)
          ".tar" -> (liftM fetchDirect . makeTarFetch) (inputFile ps)          
--          ".tar" -> fetchDirect <$> makeTarFetch $ inputFile ps --(liftM decompress . BS.hGetContents) stdin
  --foldOver (\x -> print x >> return ()) ih
  --cs <- fetchDirect ih
  --cs' <- runConduit $ yieldMany cs .| takeC 10 .| sinkList
  print $ length cs
