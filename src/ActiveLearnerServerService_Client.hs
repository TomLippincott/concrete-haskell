{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.9.3)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module ActiveLearnerServerService_Client(start,stop,addAnnotations) where
import Service_Client
import qualified Data.IORef as R
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T

import qualified Services_Types
import qualified Uuid_Types
import qualified Communication_Types


import Learn_Types
import ActiveLearnerServerService
seqid = R.newIORef 0
start (ip,op) arg_sessionId arg_task arg_contact = do
  send_start op arg_sessionId arg_task arg_contact
  recv_start ip
send_start op arg_sessionId arg_task arg_contact = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("start", T.M_CALL, seqn)
  write_Start_args op (Start_args{start_args_sessionId=arg_sessionId,start_args_task=arg_task,start_args_contact=arg_contact})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_start ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_Start_result ip
  T.readMessageEnd ip
  P.return $ start_result_success res
stop (ip,op) arg_sessionId = do
  send_stop op arg_sessionId
  recv_stop ip
send_stop op arg_sessionId = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("stop", T.M_CALL, seqn)
  write_Stop_args op (Stop_args{stop_args_sessionId=arg_sessionId})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_stop ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_Stop_result ip
  T.readMessageEnd ip
  P.return ()
addAnnotations (ip,op) arg_sessionId arg_annotations = do
  send_addAnnotations op arg_sessionId arg_annotations
  recv_addAnnotations ip
send_addAnnotations op arg_sessionId arg_annotations = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("addAnnotations", T.M_CALL, seqn)
  write_AddAnnotations_args op (AddAnnotations_args{addAnnotations_args_sessionId=arg_sessionId,addAnnotations_args_annotations=arg_annotations})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_addAnnotations ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_AddAnnotations_result ip
  T.readMessageEnd ip
  P.return ()
