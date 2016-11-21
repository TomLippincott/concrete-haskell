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

module SearchProxyService where
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


import qualified Service
import Search_Types
import qualified SearchProxyService_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Search_args = Search_args  { search_args_query :: SearchQuery
  , search_args_provider :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Search_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` search_args_query record   `H.hashWithSalt` search_args_provider record  
instance QC.Arbitrary Search_args where 
  arbitrary = M.liftM Search_args (QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_Search_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Search_args{search_args_query = search_args_query obj} then P.Nothing else P.Just $ default_Search_args{search_args_query = search_args_query obj}
    , if obj == default_Search_args{search_args_provider = search_args_provider obj} then P.Nothing else P.Just $ default_Search_args{search_args_provider = search_args_provider obj}
    ]
from_Search_args :: Search_args -> T.ThriftVal
from_Search_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v106 -> P.Just (1, ("query",from_SearchQuery _v106))) $ search_args_query record
  , (\_v106 -> P.Just (2, ("provider",T.TString $ E.encodeUtf8 _v106))) $ search_args_provider record
  ]
write_Search_args :: (T.Protocol p, T.Transport t) => p t -> Search_args -> P.IO ()
write_Search_args oprot record = T.writeVal oprot $ from_Search_args record
encode_Search_args :: (T.Protocol p, T.Transport t) => p t -> Search_args -> LBS.ByteString
encode_Search_args oprot record = T.serializeVal oprot $ from_Search_args record
to_Search_args :: T.ThriftVal -> Search_args
to_Search_args (T.TStruct fields) = Search_args{
  search_args_query = P.maybe (search_args_query default_Search_args) (\(_,_val108) -> (case _val108 of {T.TStruct _val109 -> (to_SearchQuery (T.TStruct _val109)); _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  search_args_provider = P.maybe (search_args_provider default_Search_args) (\(_,_val108) -> (case _val108 of {T.TString _val110 -> E.decodeUtf8 _val110; _ -> P.error "wrong type"})) (Map.lookup (2) fields)
  }
to_Search_args _ = P.error "not a struct"
read_Search_args :: (T.Transport t, T.Protocol p) => p t -> P.IO Search_args
read_Search_args iprot = to_Search_args <$> T.readVal iprot (T.T_STRUCT typemap_Search_args)
decode_Search_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Search_args
decode_Search_args iprot bs = to_Search_args $ T.deserializeVal iprot (T.T_STRUCT typemap_Search_args) bs
typemap_Search_args :: T.TypeMap
typemap_Search_args = Map.fromList [(1,("query",(T.T_STRUCT typemap_SearchQuery))),(2,("provider",T.T_STRING))]
default_Search_args :: Search_args
default_Search_args = Search_args{
  search_args_query = default_SearchQuery,
  search_args_provider = ""}
data Search_result = Search_result  { search_result_success :: SearchResult
  , search_result_ex :: P.Maybe Services_Types.ServicesException
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Search_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` search_result_success record   `H.hashWithSalt` search_result_ex record  
instance QC.Arbitrary Search_result where 
  arbitrary = M.liftM Search_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_Search_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Search_result{search_result_success = search_result_success obj} then P.Nothing else P.Just $ default_Search_result{search_result_success = search_result_success obj}
    , if obj == default_Search_result{search_result_ex = search_result_ex obj} then P.Nothing else P.Just $ default_Search_result{search_result_ex = search_result_ex obj}
    ]
from_Search_result :: Search_result -> T.ThriftVal
from_Search_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v113 -> (1, ("ex",Services_Types.from_ServicesException _v113))) <$> search_result_ex record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v113 -> P.Just (0, ("success",from_SearchResult _v113))) $ search_result_success record
    , (\_v113 -> (1, ("ex",Services_Types.from_ServicesException _v113))) <$> search_result_ex record
    ]
    )
write_Search_result :: (T.Protocol p, T.Transport t) => p t -> Search_result -> P.IO ()
write_Search_result oprot record = T.writeVal oprot $ from_Search_result record
encode_Search_result :: (T.Protocol p, T.Transport t) => p t -> Search_result -> LBS.ByteString
encode_Search_result oprot record = T.serializeVal oprot $ from_Search_result record
to_Search_result :: T.ThriftVal -> Search_result
to_Search_result (T.TStruct fields) = Search_result{
  search_result_success = P.maybe (search_result_success default_Search_result) (\(_,_val115) -> (case _val115 of {T.TStruct _val116 -> (to_SearchResult (T.TStruct _val116)); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  search_result_ex = P.maybe (P.Nothing) (\(_,_val115) -> P.Just (case _val115 of {T.TStruct _val117 -> (Services_Types.to_ServicesException (T.TStruct _val117)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Search_result _ = P.error "not a struct"
read_Search_result :: (T.Transport t, T.Protocol p) => p t -> P.IO Search_result
read_Search_result iprot = to_Search_result <$> T.readVal iprot (T.T_STRUCT typemap_Search_result)
decode_Search_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Search_result
decode_Search_result iprot bs = to_Search_result $ T.deserializeVal iprot (T.T_STRUCT typemap_Search_result) bs
typemap_Search_result :: T.TypeMap
typemap_Search_result = Map.fromList [(0,("success",(T.T_STRUCT typemap_SearchResult))),(1,("ex",(T.T_STRUCT Services_Types.typemap_ServicesException)))]
default_Search_result :: Search_result
default_Search_result = Search_result{
  search_result_success = default_SearchResult,
  search_result_ex = P.Nothing}
data GetProviders_args = GetProviders_args deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetProviders_args where
  hashWithSalt salt record = salt  
instance QC.Arbitrary GetProviders_args where 
  arbitrary = QC.elements [GetProviders_args]
from_GetProviders_args :: GetProviders_args -> T.ThriftVal
from_GetProviders_args record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_GetProviders_args :: (T.Protocol p, T.Transport t) => p t -> GetProviders_args -> P.IO ()
write_GetProviders_args oprot record = T.writeVal oprot $ from_GetProviders_args record
encode_GetProviders_args :: (T.Protocol p, T.Transport t) => p t -> GetProviders_args -> LBS.ByteString
encode_GetProviders_args oprot record = T.serializeVal oprot $ from_GetProviders_args record
to_GetProviders_args :: T.ThriftVal -> GetProviders_args
to_GetProviders_args (T.TStruct fields) = GetProviders_args{

  }
to_GetProviders_args _ = P.error "not a struct"
read_GetProviders_args :: (T.Transport t, T.Protocol p) => p t -> P.IO GetProviders_args
read_GetProviders_args iprot = to_GetProviders_args <$> T.readVal iprot (T.T_STRUCT typemap_GetProviders_args)
decode_GetProviders_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetProviders_args
decode_GetProviders_args iprot bs = to_GetProviders_args $ T.deserializeVal iprot (T.T_STRUCT typemap_GetProviders_args) bs
typemap_GetProviders_args :: T.TypeMap
typemap_GetProviders_args = Map.fromList []
default_GetProviders_args :: GetProviders_args
default_GetProviders_args = GetProviders_args{
}
data GetProviders_result = GetProviders_result  { getProviders_result_success :: (Vector.Vector LT.Text)
  , getProviders_result_ex :: P.Maybe Services_Types.ServicesException
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetProviders_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` getProviders_result_success record   `H.hashWithSalt` getProviders_result_ex record  
instance QC.Arbitrary GetProviders_result where 
  arbitrary = M.liftM GetProviders_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_GetProviders_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetProviders_result{getProviders_result_success = getProviders_result_success obj} then P.Nothing else P.Just $ default_GetProviders_result{getProviders_result_success = getProviders_result_success obj}
    , if obj == default_GetProviders_result{getProviders_result_ex = getProviders_result_ex obj} then P.Nothing else P.Just $ default_GetProviders_result{getProviders_result_ex = getProviders_result_ex obj}
    ]
from_GetProviders_result :: GetProviders_result -> T.ThriftVal
from_GetProviders_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v125 -> (1, ("ex",Services_Types.from_ServicesException _v125))) <$> getProviders_result_ex record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v125 -> P.Just (0, ("success",T.TList T.T_STRING $ P.map (\_v127 -> T.TString $ E.encodeUtf8 _v127) $ Vector.toList _v125))) $ getProviders_result_success record
    , (\_v125 -> (1, ("ex",Services_Types.from_ServicesException _v125))) <$> getProviders_result_ex record
    ]
    )
write_GetProviders_result :: (T.Protocol p, T.Transport t) => p t -> GetProviders_result -> P.IO ()
write_GetProviders_result oprot record = T.writeVal oprot $ from_GetProviders_result record
encode_GetProviders_result :: (T.Protocol p, T.Transport t) => p t -> GetProviders_result -> LBS.ByteString
encode_GetProviders_result oprot record = T.serializeVal oprot $ from_GetProviders_result record
to_GetProviders_result :: T.ThriftVal -> GetProviders_result
to_GetProviders_result (T.TStruct fields) = GetProviders_result{
  getProviders_result_success = P.maybe (getProviders_result_success default_GetProviders_result) (\(_,_val129) -> (case _val129 of {T.TList _ _val130 -> (Vector.fromList $ P.map (\_v131 -> (case _v131 of {T.TString _val132 -> E.decodeUtf8 _val132; _ -> P.error "wrong type"})) _val130); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  getProviders_result_ex = P.maybe (P.Nothing) (\(_,_val129) -> P.Just (case _val129 of {T.TStruct _val133 -> (Services_Types.to_ServicesException (T.TStruct _val133)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetProviders_result _ = P.error "not a struct"
read_GetProviders_result :: (T.Transport t, T.Protocol p) => p t -> P.IO GetProviders_result
read_GetProviders_result iprot = to_GetProviders_result <$> T.readVal iprot (T.T_STRUCT typemap_GetProviders_result)
decode_GetProviders_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetProviders_result
decode_GetProviders_result iprot bs = to_GetProviders_result $ T.deserializeVal iprot (T.T_STRUCT typemap_GetProviders_result) bs
typemap_GetProviders_result :: T.TypeMap
typemap_GetProviders_result = Map.fromList [(0,("success",(T.T_LIST T.T_STRING))),(1,("ex",(T.T_STRUCT Services_Types.typemap_ServicesException)))]
default_GetProviders_result :: GetProviders_result
default_GetProviders_result = GetProviders_result{
  getProviders_result_success = Vector.empty,
  getProviders_result_ex = P.Nothing}
data GetCapabilities_args = GetCapabilities_args  { getCapabilities_args_provider :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetCapabilities_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` getCapabilities_args_provider record  
instance QC.Arbitrary GetCapabilities_args where 
  arbitrary = M.liftM GetCapabilities_args (QC.arbitrary)
  shrink obj | obj == default_GetCapabilities_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetCapabilities_args{getCapabilities_args_provider = getCapabilities_args_provider obj} then P.Nothing else P.Just $ default_GetCapabilities_args{getCapabilities_args_provider = getCapabilities_args_provider obj}
    ]
from_GetCapabilities_args :: GetCapabilities_args -> T.ThriftVal
from_GetCapabilities_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v136 -> P.Just (1, ("provider",T.TString $ E.encodeUtf8 _v136))) $ getCapabilities_args_provider record
  ]
write_GetCapabilities_args :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_args -> P.IO ()
write_GetCapabilities_args oprot record = T.writeVal oprot $ from_GetCapabilities_args record
encode_GetCapabilities_args :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_args -> LBS.ByteString
encode_GetCapabilities_args oprot record = T.serializeVal oprot $ from_GetCapabilities_args record
to_GetCapabilities_args :: T.ThriftVal -> GetCapabilities_args
to_GetCapabilities_args (T.TStruct fields) = GetCapabilities_args{
  getCapabilities_args_provider = P.maybe (getCapabilities_args_provider default_GetCapabilities_args) (\(_,_val138) -> (case _val138 of {T.TString _val139 -> E.decodeUtf8 _val139; _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetCapabilities_args _ = P.error "not a struct"
read_GetCapabilities_args :: (T.Transport t, T.Protocol p) => p t -> P.IO GetCapabilities_args
read_GetCapabilities_args iprot = to_GetCapabilities_args <$> T.readVal iprot (T.T_STRUCT typemap_GetCapabilities_args)
decode_GetCapabilities_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetCapabilities_args
decode_GetCapabilities_args iprot bs = to_GetCapabilities_args $ T.deserializeVal iprot (T.T_STRUCT typemap_GetCapabilities_args) bs
typemap_GetCapabilities_args :: T.TypeMap
typemap_GetCapabilities_args = Map.fromList [(1,("provider",T.T_STRING))]
default_GetCapabilities_args :: GetCapabilities_args
default_GetCapabilities_args = GetCapabilities_args{
  getCapabilities_args_provider = ""}
data GetCapabilities_result = GetCapabilities_result  { getCapabilities_result_success :: (Vector.Vector SearchCapability)
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
  (let exns = M.catMaybes [ (\_v142 -> (1, ("ex",Services_Types.from_ServicesException _v142))) <$> getCapabilities_result_ex record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v142 -> P.Just (0, ("success",T.TList (T.T_STRUCT typemap_SearchCapability) $ P.map (\_v144 -> from_SearchCapability _v144) $ Vector.toList _v142))) $ getCapabilities_result_success record
    , (\_v142 -> (1, ("ex",Services_Types.from_ServicesException _v142))) <$> getCapabilities_result_ex record
    ]
    )
write_GetCapabilities_result :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_result -> P.IO ()
write_GetCapabilities_result oprot record = T.writeVal oprot $ from_GetCapabilities_result record
encode_GetCapabilities_result :: (T.Protocol p, T.Transport t) => p t -> GetCapabilities_result -> LBS.ByteString
encode_GetCapabilities_result oprot record = T.serializeVal oprot $ from_GetCapabilities_result record
to_GetCapabilities_result :: T.ThriftVal -> GetCapabilities_result
to_GetCapabilities_result (T.TStruct fields) = GetCapabilities_result{
  getCapabilities_result_success = P.maybe (getCapabilities_result_success default_GetCapabilities_result) (\(_,_val146) -> (case _val146 of {T.TList _ _val147 -> (Vector.fromList $ P.map (\_v148 -> (case _v148 of {T.TStruct _val149 -> (to_SearchCapability (T.TStruct _val149)); _ -> P.error "wrong type"})) _val147); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  getCapabilities_result_ex = P.maybe (P.Nothing) (\(_,_val146) -> P.Just (case _val146 of {T.TStruct _val150 -> (Services_Types.to_ServicesException (T.TStruct _val150)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetCapabilities_result _ = P.error "not a struct"
read_GetCapabilities_result :: (T.Transport t, T.Protocol p) => p t -> P.IO GetCapabilities_result
read_GetCapabilities_result iprot = to_GetCapabilities_result <$> T.readVal iprot (T.T_STRUCT typemap_GetCapabilities_result)
decode_GetCapabilities_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetCapabilities_result
decode_GetCapabilities_result iprot bs = to_GetCapabilities_result $ T.deserializeVal iprot (T.T_STRUCT typemap_GetCapabilities_result) bs
typemap_GetCapabilities_result :: T.TypeMap
typemap_GetCapabilities_result = Map.fromList [(0,("success",(T.T_LIST (T.T_STRUCT typemap_SearchCapability)))),(1,("ex",(T.T_STRUCT Services_Types.typemap_ServicesException)))]
default_GetCapabilities_result :: GetCapabilities_result
default_GetCapabilities_result = GetCapabilities_result{
  getCapabilities_result_success = Vector.empty,
  getCapabilities_result_ex = P.Nothing}
data GetCorpora_args = GetCorpora_args  { getCorpora_args_provider :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetCorpora_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` getCorpora_args_provider record  
instance QC.Arbitrary GetCorpora_args where 
  arbitrary = M.liftM GetCorpora_args (QC.arbitrary)
  shrink obj | obj == default_GetCorpora_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetCorpora_args{getCorpora_args_provider = getCorpora_args_provider obj} then P.Nothing else P.Just $ default_GetCorpora_args{getCorpora_args_provider = getCorpora_args_provider obj}
    ]
from_GetCorpora_args :: GetCorpora_args -> T.ThriftVal
from_GetCorpora_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v153 -> P.Just (1, ("provider",T.TString $ E.encodeUtf8 _v153))) $ getCorpora_args_provider record
  ]
write_GetCorpora_args :: (T.Protocol p, T.Transport t) => p t -> GetCorpora_args -> P.IO ()
write_GetCorpora_args oprot record = T.writeVal oprot $ from_GetCorpora_args record
encode_GetCorpora_args :: (T.Protocol p, T.Transport t) => p t -> GetCorpora_args -> LBS.ByteString
encode_GetCorpora_args oprot record = T.serializeVal oprot $ from_GetCorpora_args record
to_GetCorpora_args :: T.ThriftVal -> GetCorpora_args
to_GetCorpora_args (T.TStruct fields) = GetCorpora_args{
  getCorpora_args_provider = P.maybe (getCorpora_args_provider default_GetCorpora_args) (\(_,_val155) -> (case _val155 of {T.TString _val156 -> E.decodeUtf8 _val156; _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetCorpora_args _ = P.error "not a struct"
read_GetCorpora_args :: (T.Transport t, T.Protocol p) => p t -> P.IO GetCorpora_args
read_GetCorpora_args iprot = to_GetCorpora_args <$> T.readVal iprot (T.T_STRUCT typemap_GetCorpora_args)
decode_GetCorpora_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetCorpora_args
decode_GetCorpora_args iprot bs = to_GetCorpora_args $ T.deserializeVal iprot (T.T_STRUCT typemap_GetCorpora_args) bs
typemap_GetCorpora_args :: T.TypeMap
typemap_GetCorpora_args = Map.fromList [(1,("provider",T.T_STRING))]
default_GetCorpora_args :: GetCorpora_args
default_GetCorpora_args = GetCorpora_args{
  getCorpora_args_provider = ""}
data GetCorpora_result = GetCorpora_result  { getCorpora_result_success :: (Vector.Vector LT.Text)
  , getCorpora_result_ex :: P.Maybe Services_Types.ServicesException
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable GetCorpora_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` getCorpora_result_success record   `H.hashWithSalt` getCorpora_result_ex record  
instance QC.Arbitrary GetCorpora_result where 
  arbitrary = M.liftM GetCorpora_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_GetCorpora_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_GetCorpora_result{getCorpora_result_success = getCorpora_result_success obj} then P.Nothing else P.Just $ default_GetCorpora_result{getCorpora_result_success = getCorpora_result_success obj}
    , if obj == default_GetCorpora_result{getCorpora_result_ex = getCorpora_result_ex obj} then P.Nothing else P.Just $ default_GetCorpora_result{getCorpora_result_ex = getCorpora_result_ex obj}
    ]
from_GetCorpora_result :: GetCorpora_result -> T.ThriftVal
from_GetCorpora_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v159 -> (1, ("ex",Services_Types.from_ServicesException _v159))) <$> getCorpora_result_ex record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v159 -> P.Just (0, ("success",T.TList T.T_STRING $ P.map (\_v161 -> T.TString $ E.encodeUtf8 _v161) $ Vector.toList _v159))) $ getCorpora_result_success record
    , (\_v159 -> (1, ("ex",Services_Types.from_ServicesException _v159))) <$> getCorpora_result_ex record
    ]
    )
write_GetCorpora_result :: (T.Protocol p, T.Transport t) => p t -> GetCorpora_result -> P.IO ()
write_GetCorpora_result oprot record = T.writeVal oprot $ from_GetCorpora_result record
encode_GetCorpora_result :: (T.Protocol p, T.Transport t) => p t -> GetCorpora_result -> LBS.ByteString
encode_GetCorpora_result oprot record = T.serializeVal oprot $ from_GetCorpora_result record
to_GetCorpora_result :: T.ThriftVal -> GetCorpora_result
to_GetCorpora_result (T.TStruct fields) = GetCorpora_result{
  getCorpora_result_success = P.maybe (getCorpora_result_success default_GetCorpora_result) (\(_,_val163) -> (case _val163 of {T.TList _ _val164 -> (Vector.fromList $ P.map (\_v165 -> (case _v165 of {T.TString _val166 -> E.decodeUtf8 _val166; _ -> P.error "wrong type"})) _val164); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  getCorpora_result_ex = P.maybe (P.Nothing) (\(_,_val163) -> P.Just (case _val163 of {T.TStruct _val167 -> (Services_Types.to_ServicesException (T.TStruct _val167)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_GetCorpora_result _ = P.error "not a struct"
read_GetCorpora_result :: (T.Transport t, T.Protocol p) => p t -> P.IO GetCorpora_result
read_GetCorpora_result iprot = to_GetCorpora_result <$> T.readVal iprot (T.T_STRUCT typemap_GetCorpora_result)
decode_GetCorpora_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> GetCorpora_result
decode_GetCorpora_result iprot bs = to_GetCorpora_result $ T.deserializeVal iprot (T.T_STRUCT typemap_GetCorpora_result) bs
typemap_GetCorpora_result :: T.TypeMap
typemap_GetCorpora_result = Map.fromList [(0,("success",(T.T_LIST T.T_STRING))),(1,("ex",(T.T_STRUCT Services_Types.typemap_ServicesException)))]
default_GetCorpora_result :: GetCorpora_result
default_GetCorpora_result = GetCorpora_result{
  getCorpora_result_success = Vector.empty,
  getCorpora_result_ex = P.Nothing}
process_search (seqid, iprot, oprot, handler) = do
  args <- read_Search_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.search handler (search_args_query args) (search_args_provider args)
        let res = default_Search_result{search_result_success = val}
        T.writeMessageBegin oprot ("search", T.M_REPLY, seqid)
        write_Search_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_Search_result{search_result_ex = P.Just e}
        T.writeMessageBegin oprot ("search", T.M_REPLY, seqid)
        write_Search_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("search", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
process_getProviders (seqid, iprot, oprot, handler) = do
  args <- read_GetProviders_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.getProviders handler
        let res = default_GetProviders_result{getProviders_result_success = val}
        T.writeMessageBegin oprot ("getProviders", T.M_REPLY, seqid)
        write_GetProviders_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_GetProviders_result{getProviders_result_ex = P.Just e}
        T.writeMessageBegin oprot ("getProviders", T.M_REPLY, seqid)
        write_GetProviders_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("getProviders", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
process_getCapabilities (seqid, iprot, oprot, handler) = do
  args <- read_GetCapabilities_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.getCapabilities handler (getCapabilities_args_provider args)
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
process_getCorpora (seqid, iprot, oprot, handler) = do
  args <- read_GetCorpora_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.getCorpora handler (getCorpora_args_provider args)
        let res = default_GetCorpora_result{getCorpora_result_success = val}
        T.writeMessageBegin oprot ("getCorpora", T.M_REPLY, seqid)
        write_GetCorpora_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_GetCorpora_result{getCorpora_result_ex = P.Just e}
        T.writeMessageBegin oprot ("getCorpora", T.M_REPLY, seqid)
        write_GetCorpora_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("getCorpora", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "search" -> process_search (seqid,iprot,oprot,handler)
  "getProviders" -> process_getProviders (seqid,iprot,oprot,handler)
  "getCapabilities" -> process_getCapabilities (seqid,iprot,oprot,handler)
  "getCorpora" -> process_getCorpora (seqid,iprot,oprot,handler)
  _ -> Service.proc_ handler (iprot,oprot) (name,typ,seqid)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- T.readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  T.readMessageEnd iprot
  P.return P.True
