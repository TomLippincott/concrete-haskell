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

module FeedbackService_Client(startFeedback,addCommunicationFeedback,addSentenceFeedback) where
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
import qualified Structure_Types
import qualified Uuid_Types
import qualified Metadata_Types


import Search_Types
import FeedbackService
seqid = R.newIORef 0
startFeedback (ip,op) arg_results = do
  send_startFeedback op arg_results
  recv_startFeedback ip
send_startFeedback op arg_results = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("startFeedback", T.M_CALL, seqn)
  write_StartFeedback_args op (StartFeedback_args{startFeedback_args_results=arg_results})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_startFeedback ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_StartFeedback_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (startFeedback_result_ex res)
  P.return ()
addCommunicationFeedback (ip,op) arg_searchResultsId arg_communicationId arg_feedback = do
  send_addCommunicationFeedback op arg_searchResultsId arg_communicationId arg_feedback
  recv_addCommunicationFeedback ip
send_addCommunicationFeedback op arg_searchResultsId arg_communicationId arg_feedback = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("addCommunicationFeedback", T.M_CALL, seqn)
  write_AddCommunicationFeedback_args op (AddCommunicationFeedback_args{addCommunicationFeedback_args_searchResultsId=arg_searchResultsId,addCommunicationFeedback_args_communicationId=arg_communicationId,addCommunicationFeedback_args_feedback=arg_feedback})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_addCommunicationFeedback ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_AddCommunicationFeedback_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (addCommunicationFeedback_result_ex res)
  P.return ()
addSentenceFeedback (ip,op) arg_searchResultsId arg_communicationId arg_sentenceId arg_feedback = do
  send_addSentenceFeedback op arg_searchResultsId arg_communicationId arg_sentenceId arg_feedback
  recv_addSentenceFeedback ip
send_addSentenceFeedback op arg_searchResultsId arg_communicationId arg_sentenceId arg_feedback = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("addSentenceFeedback", T.M_CALL, seqn)
  write_AddSentenceFeedback_args op (AddSentenceFeedback_args{addSentenceFeedback_args_searchResultsId=arg_searchResultsId,addSentenceFeedback_args_communicationId=arg_communicationId,addSentenceFeedback_args_sentenceId=arg_sentenceId,addSentenceFeedback_args_feedback=arg_feedback})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_addSentenceFeedback ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_AddSentenceFeedback_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (addSentenceFeedback_result_ex res)
  P.return ()
