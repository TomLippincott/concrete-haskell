{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (foldM)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn, hClose)
import Data.Concrete.Services (connectToService)
import Data.Concrete.Services.Store (makeTarStore, storeDirect)
import Data.Concrete.Services.Fetch (makeTarFetch, fetchDirect)
import Data.Concrete.Autogen.Communication_Types (Communication(..))
import Data.Concrete.Autogen.AnnotateCommunicationService_Client (annotate)
import Options.Generic

data Parameters w = Parameters { inputFile :: w ::: String <?> "Input tar file"
                               , outputFile :: w ::: String <?> "Output tar file"
                               , hosts :: w ::: [String] <?> "Annotator hosts"
                               , ports :: w ::: [Int] <?> "Annotator ports"
                               } deriving (Generic)

instance ParseRecord (Parameters Wrapped)
deriving instance Show (Parameters Unwrapped)

applyOneAnnotator :: [Communication] -> (String, Int) -> IO [Communication]
applyOneAnnotator comms (host, port) = do
  a <- connectToService host port
  sequence $ map (annotate a) comms

main = do
  ps <- unwrapRecord "Apply a sequence of AnnotateCommunicationServices to Communications"
  ih <- makeTarFetch $ inputFile ps
  cs <- fetchDirect ih
  let anns = zip (hosts ps) (ports ps)
  cs' <- foldM applyOneAnnotator cs anns
  oh <- makeTarStore (outputFile ps)
  storeDirect oh cs'
