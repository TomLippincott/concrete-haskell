{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.10.0)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module ResultsServerService_Client(registerSearchResult,getSearchResults,getSearchResultsByUser,getLatestSearchResult,getSearchResult,startSession,stopSession,getNextChunk,submitAnnotation) where
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
import qualified Search_Types
import qualified Communication_Types


import Results_Types
import ResultsServerService
seqid = R.newIORef 0
registerSearchResult (ip,op) arg_result arg_taskType = do
  send_registerSearchResult op arg_result arg_taskType
  recv_registerSearchResult ip
send_registerSearchResult op arg_result arg_taskType = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("registerSearchResult", T.M_CALL, seqn)
  write_RegisterSearchResult_args op (RegisterSearchResult_args{registerSearchResult_args_result=arg_result,registerSearchResult_args_taskType=arg_taskType})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_registerSearchResult ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_RegisterSearchResult_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (registerSearchResult_result_ex res)
  P.return ()
getSearchResults (ip,op) arg_taskType arg_limit = do
  send_getSearchResults op arg_taskType arg_limit
  recv_getSearchResults ip
send_getSearchResults op arg_taskType arg_limit = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("getSearchResults", T.M_CALL, seqn)
  write_GetSearchResults_args op (GetSearchResults_args{getSearchResults_args_taskType=arg_taskType,getSearchResults_args_limit=arg_limit})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_getSearchResults ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_GetSearchResults_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (getSearchResults_result_ex res)
  P.return $ getSearchResults_result_success res
getSearchResultsByUser (ip,op) arg_taskType arg_userId arg_limit = do
  send_getSearchResultsByUser op arg_taskType arg_userId arg_limit
  recv_getSearchResultsByUser ip
send_getSearchResultsByUser op arg_taskType arg_userId arg_limit = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("getSearchResultsByUser", T.M_CALL, seqn)
  write_GetSearchResultsByUser_args op (GetSearchResultsByUser_args{getSearchResultsByUser_args_taskType=arg_taskType,getSearchResultsByUser_args_userId=arg_userId,getSearchResultsByUser_args_limit=arg_limit})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_getSearchResultsByUser ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_GetSearchResultsByUser_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (getSearchResultsByUser_result_ex res)
  P.return $ getSearchResultsByUser_result_success res
getLatestSearchResult (ip,op) arg_userId = do
  send_getLatestSearchResult op arg_userId
  recv_getLatestSearchResult ip
send_getLatestSearchResult op arg_userId = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("getLatestSearchResult", T.M_CALL, seqn)
  write_GetLatestSearchResult_args op (GetLatestSearchResult_args{getLatestSearchResult_args_userId=arg_userId})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_getLatestSearchResult ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_GetLatestSearchResult_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (getLatestSearchResult_result_ex res)
  P.return $ getLatestSearchResult_result_success res
getSearchResult (ip,op) arg_searchResultId = do
  send_getSearchResult op arg_searchResultId
  recv_getSearchResult ip
send_getSearchResult op arg_searchResultId = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("getSearchResult", T.M_CALL, seqn)
  write_GetSearchResult_args op (GetSearchResult_args{getSearchResult_args_searchResultId=arg_searchResultId})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_getSearchResult ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_GetSearchResult_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (getSearchResult_result_ex res)
  P.return $ getSearchResult_result_success res
startSession (ip,op) arg_searchResultId arg_taskType = do
  send_startSession op arg_searchResultId arg_taskType
  recv_startSession ip
send_startSession op arg_searchResultId arg_taskType = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("startSession", T.M_CALL, seqn)
  write_StartSession_args op (StartSession_args{startSession_args_searchResultId=arg_searchResultId,startSession_args_taskType=arg_taskType})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_startSession ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_StartSession_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (startSession_result_ex res)
  P.return $ startSession_result_success res
stopSession (ip,op) arg_sessionId = do
  send_stopSession op arg_sessionId
  recv_stopSession ip
send_stopSession op arg_sessionId = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("stopSession", T.M_CALL, seqn)
  write_StopSession_args op (StopSession_args{stopSession_args_sessionId=arg_sessionId})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_stopSession ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_StopSession_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (stopSession_result_ex res)
  P.return ()
getNextChunk (ip,op) arg_sessionId = do
  send_getNextChunk op arg_sessionId
  recv_getNextChunk ip
send_getNextChunk op arg_sessionId = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("getNextChunk", T.M_CALL, seqn)
  write_GetNextChunk_args op (GetNextChunk_args{getNextChunk_args_sessionId=arg_sessionId})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_getNextChunk ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_GetNextChunk_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (getNextChunk_result_ex res)
  P.return $ getNextChunk_result_success res
submitAnnotation (ip,op) arg_sessionId arg_unitId arg_communication = do
  send_submitAnnotation op arg_sessionId arg_unitId arg_communication
  recv_submitAnnotation ip
send_submitAnnotation op arg_sessionId arg_unitId arg_communication = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("submitAnnotation", T.M_CALL, seqn)
  write_SubmitAnnotation_args op (SubmitAnnotation_args{submitAnnotation_args_sessionId=arg_sessionId,submitAnnotation_args_unitId=arg_unitId,submitAnnotation_args_communication=arg_communication})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_submitAnnotation ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_SubmitAnnotation_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (submitAnnotation_result_ex res)
  P.return ()
