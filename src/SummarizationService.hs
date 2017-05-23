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

module SummarizationService where
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

import qualified Communication_Types
import qualified Services_Types
import qualified Structure_Types
import qualified Uuid_Types


import qualified Service
import Summarization_Types
import qualified SummarizationService_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Summarize_args = Summarize_args  { summarize_args_query :: SummarizationRequest
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Summarize_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` summarize_args_query record  
instance QC.Arbitrary Summarize_args where 
  arbitrary = M.liftM Summarize_args (QC.arbitrary)
  shrink obj | obj == default_Summarize_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Summarize_args{summarize_args_query = summarize_args_query obj} then P.Nothing else P.Just $ default_Summarize_args{summarize_args_query = summarize_args_query obj}
    ]
from_Summarize_args :: Summarize_args -> T.ThriftVal
from_Summarize_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v48 -> P.Just (1, ("query",from_SummarizationRequest _v48))) $ summarize_args_query record
  ]
write_Summarize_args :: (T.Protocol p, T.Transport t) => p t -> Summarize_args -> P.IO ()
write_Summarize_args oprot record = T.writeVal oprot $ from_Summarize_args record
encode_Summarize_args :: (T.Protocol p, T.Transport t) => p t -> Summarize_args -> LBS.ByteString
encode_Summarize_args oprot record = T.serializeVal oprot $ from_Summarize_args record
to_Summarize_args :: T.ThriftVal -> Summarize_args
to_Summarize_args (T.TStruct fields) = Summarize_args{
  summarize_args_query = P.maybe (summarize_args_query default_Summarize_args) (\(_,_val50) -> (case _val50 of {T.TStruct _val51 -> (to_SummarizationRequest (T.TStruct _val51)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Summarize_args _ = P.error "not a struct"
read_Summarize_args :: (T.Transport t, T.Protocol p) => p t -> P.IO Summarize_args
read_Summarize_args iprot = to_Summarize_args <$> T.readVal iprot (T.T_STRUCT typemap_Summarize_args)
decode_Summarize_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Summarize_args
decode_Summarize_args iprot bs = to_Summarize_args $ T.deserializeVal iprot (T.T_STRUCT typemap_Summarize_args) bs
typemap_Summarize_args :: T.TypeMap
typemap_Summarize_args = Map.fromList [(1,("query",(T.T_STRUCT typemap_SummarizationRequest)))]
default_Summarize_args :: Summarize_args
default_Summarize_args = Summarize_args{
  summarize_args_query = default_SummarizationRequest}
data Summarize_result = Summarize_result  { summarize_result_success :: Summary
  , summarize_result_ex :: P.Maybe Services_Types.ServicesException
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Summarize_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` summarize_result_success record   `H.hashWithSalt` summarize_result_ex record  
instance QC.Arbitrary Summarize_result where 
  arbitrary = M.liftM Summarize_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_Summarize_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Summarize_result{summarize_result_success = summarize_result_success obj} then P.Nothing else P.Just $ default_Summarize_result{summarize_result_success = summarize_result_success obj}
    , if obj == default_Summarize_result{summarize_result_ex = summarize_result_ex obj} then P.Nothing else P.Just $ default_Summarize_result{summarize_result_ex = summarize_result_ex obj}
    ]
from_Summarize_result :: Summarize_result -> T.ThriftVal
from_Summarize_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v54 -> (1, ("ex",Services_Types.from_ServicesException _v54))) <$> summarize_result_ex record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v54 -> P.Just (0, ("success",from_Summary _v54))) $ summarize_result_success record
    , (\_v54 -> (1, ("ex",Services_Types.from_ServicesException _v54))) <$> summarize_result_ex record
    ]
    )
write_Summarize_result :: (T.Protocol p, T.Transport t) => p t -> Summarize_result -> P.IO ()
write_Summarize_result oprot record = T.writeVal oprot $ from_Summarize_result record
encode_Summarize_result :: (T.Protocol p, T.Transport t) => p t -> Summarize_result -> LBS.ByteString
encode_Summarize_result oprot record = T.serializeVal oprot $ from_Summarize_result record
to_Summarize_result :: T.ThriftVal -> Summarize_result
to_Summarize_result (T.TStruct fields) = Summarize_result{
  summarize_result_success = P.maybe (summarize_result_success default_Summarize_result) (\(_,_val56) -> (case _val56 of {T.TStruct _val57 -> (to_Summary (T.TStruct _val57)); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  summarize_result_ex = P.maybe (P.Nothing) (\(_,_val56) -> P.Just (case _val56 of {T.TStruct _val58 -> (Services_Types.to_ServicesException (T.TStruct _val58)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Summarize_result _ = P.error "not a struct"
read_Summarize_result :: (T.Transport t, T.Protocol p) => p t -> P.IO Summarize_result
read_Summarize_result iprot = to_Summarize_result <$> T.readVal iprot (T.T_STRUCT typemap_Summarize_result)
decode_Summarize_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Summarize_result
decode_Summarize_result iprot bs = to_Summarize_result $ T.deserializeVal iprot (T.T_STRUCT typemap_Summarize_result) bs
typemap_Summarize_result :: T.TypeMap
typemap_Summarize_result = Map.fromList [(0,("success",(T.T_STRUCT typemap_Summary))),(1,("ex",(T.T_STRUCT Services_Types.typemap_ServicesException)))]
default_Summarize_result :: Summarize_result
default_Summarize_result = Summarize_result{
  summarize_result_success = default_Summary,
  summarize_result_ex = P.Nothing}
data GetCapabilities_args = GetCapabilities_args deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetCapabilities_args where
  hashWithSalt salt record = salt  
instance QC.Arbitrary GetCapabilities_args where 
  arbitrary = QC.elements [GetCapabilities_args]
from_GetCapabilities_args :: GetCapabilities_args -> T.ThriftVal
from_GetCapabilities_args record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_GetCapabilities_args :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_args -> P.IO ()
write_GetCapabilities_args oprot record = T.writeVal oprot $ from_GetCapabilities_args record
encode_GetCapabilities_args :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_args -> LBS.ByteString
encode_GetCapabilities_args oprot record = T.serializeVal oprot $ from_GetCapabilities_args record
to_GetCapabilities_args :: T.ThriftVal -> GetCapabilities_args
to_GetCapabilities_args (T.TStruct fields) = GetCapabilities_args{

  }
to_GetCapabilities_args _ = P.error "not a struct"
read_GetCapabilities_args :: (T.Transport t, T.Protocol p) => p t -> P.IO GetCapabilities_args
read_GetCapabilities_args iprot = to_GetCapabilities_args <$> T.readVal iprot (T.T_STRUCT typemap_GetCapabilities_args)
decode_GetCapabilities_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetCapabilities_args
decode_GetCapabilities_args iprot bs = to_GetCapabilities_args $ T.deserializeVal iprot (T.T_STRUCT typemap_GetCapabilities_args) bs
typemap_GetCapabilities_args :: T.TypeMap
typemap_GetCapabilities_args = Map.fromList []
default_GetCapabilities_args :: GetCapabilities_args
default_GetCapabilities_args = GetCapabilities_args{
}
data GetCapabilities_result = GetCapabilities_result  { getCapabilities_result_success :: (Vector.Vector SummarizationCapability)
  , getCapabilities_result_ex :: P.Maybe Services_Types.ServicesException
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetCapabilities_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` getCapabilities_result_success record   `H.hashWithSalt` getCapabilities_result_ex record  
instance QC.Arbitrary GetCapabilities_result where 
  arbitrary = M.liftM GetCapabilities_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_GetCapabilities_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetCapabilities_result{getCapabilities_result_success = getCapabilities_result_success obj} then P.Nothing else P.Just $ default_GetCapabilities_result{getCapabilities_result_success = getCapabilities_result_success obj}
    , if obj == default_GetCapabilities_result{getCapabilities_result_ex = getCapabilities_result_ex obj} then P.Nothing else P.Just $ default_GetCapabilities_result{getCapabilities_result_ex = getCapabilities_result_ex obj}
    ]
from_GetCapabilities_result :: GetCapabilities_result -> T.ThriftVal
from_GetCapabilities_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v66 -> (1, ("ex",Services_Types.from_ServicesException _v66))) <$> getCapabilities_result_ex record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v66 -> P.Just (0, ("success",T.TList (T.T_STRUCT typemap_SummarizationCapability) $ P.map (\_v68 -> from_SummarizationCapability _v68) $ Vector.toList _v66))) $ getCapabilities_result_success record
    , (\_v66 -> (1, ("ex",Services_Types.from_ServicesException _v66))) <$> getCapabilities_result_ex record
    ]
    )
write_GetCapabilities_result :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_result -> P.IO ()
write_GetCapabilities_result oprot record = T.writeVal oprot $ from_GetCapabilities_result record
encode_GetCapabilities_result :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_result -> LBS.ByteString
encode_GetCapabilities_result oprot record = T.serializeVal oprot $ from_GetCapabilities_result record
to_GetCapabilities_result :: T.ThriftVal -> GetCapabilities_result
to_GetCapabilities_result (T.TStruct fields) = GetCapabilities_result{
  getCapabilities_result_success = P.maybe (getCapabilities_result_success default_GetCapabilities_result) (\(_,_val70) -> (case _val70 of {T.TList _ _val71 -> (Vector.fromList $ P.map (\_v72 -> (case _v72 of {T.TStruct _val73 -> (to_SummarizationCapability (T.TStruct _val73)); _ -> P.error "wrong type"})) _val71); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  getCapabilities_result_ex = P.maybe (P.Nothing) (\(_,_val70) -> P.Just (case _val70 of {T.TStruct _val74 -> (Services_Types.to_ServicesException (T.TStruct _val74)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetCapabilities_result _ = P.error "not a struct"
read_GetCapabilities_result :: (T.Transport t, T.Protocol p) => p t -> P.IO GetCapabilities_result
read_GetCapabilities_result iprot = to_GetCapabilities_result <$> T.readVal iprot (T.T_STRUCT typemap_GetCapabilities_result)
decode_GetCapabilities_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetCapabilities_result
decode_GetCapabilities_result iprot bs = to_GetCapabilities_result $ T.deserializeVal iprot (T.T_STRUCT typemap_GetCapabilities_result) bs
typemap_GetCapabilities_result :: T.TypeMap
typemap_GetCapabilities_result = Map.fromList [(0,("success",(T.T_LIST (T.T_STRUCT typemap_SummarizationCapability)))),(1,("ex",(T.T_STRUCT Services_Types.typemap_ServicesException)))]
default_GetCapabilities_result :: GetCapabilities_result
default_GetCapabilities_result = GetCapabilities_result{
  getCapabilities_result_success = Vector.empty,
  getCapabilities_result_ex = P.Nothing}
process_summarize (seqid, iprot, oprot, handler) = do
  args <- read_Summarize_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.summarize handler (summarize_args_query args)
        let res = default_Summarize_result{summarize_result_success = val}
        T.writeMessageBegin oprot ("summarize", T.M_REPLY, seqid)
        write_Summarize_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_Summarize_result{summarize_result_ex = P.Just e}
        T.writeMessageBegin oprot ("summarize", T.M_REPLY, seqid)
        write_Summarize_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("summarize", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
process_getCapabilities (seqid, iprot, oprot, handler) = do
  args <- read_GetCapabilities_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.getCapabilities handler
        let res = default_GetCapabilities_result{getCapabilities_result_success = val}
        T.writeMessageBegin oprot ("getCapabilities", T.M_REPLY, seqid)
        write_GetCapabilities_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_GetCapabilities_result{getCapabilities_result_ex = P.Just e}
        T.writeMessageBegin oprot ("getCapabilities", T.M_REPLY, seqid)
        write_GetCapabilities_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("getCapabilities", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "summarize" -> process_summarize (seqid,iprot,oprot,handler)
  "getCapabilities" -> process_getCapabilities (seqid,iprot,oprot,handler)
  _ -> Service.proc_ handler (iprot,oprot) (name,typ,seqid)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- T.readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  T.readMessageEnd iprot
  P.return P.True
