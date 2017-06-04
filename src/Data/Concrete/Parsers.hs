{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers
       ( communicationParsers
       , ProtoSection(..)
       , ProtoCommunication
       , CommunicationParser
       , toCommunications
       ) where

import qualified Data.Map as Map
import Data.Vector (fromList, Vector)
import Data.Map (Map)
import Data.Concrete.Utils (createAnnotationMetadata, getUUID)
import Data.Concrete ( default_Communication
                     , Communication(..)
                     , default_Section
                     , Section(..)
                     , default_AnnotationMetadata
                     , AnnotationMetadata(..)
                     , default_CommunicationMetadata
                     , CommunicationMetadata(..)
                     , default_Sentence
                     , Sentence(..)
                     , default_TextSpan
                     , TextSpan(..)
                     )

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, pack)
import qualified Data.Concrete.Parsers.JSON.Megaparsec as M
import qualified Data.Concrete.Parsers.JSON.Attoparsec as A
import qualified Data.Concrete.Parsers.CONLL.Attoparsec as C

data ProtoSection = ProtoSection { start :: Int
                                 , end :: Int
                                 , label :: String
                                 , kind :: String
                                 }
                    
type ProtoCommunication = (Text, [ProtoSection])

type CommunicationParser = (Text -> Either String [ProtoCommunication])

communicationParsers :: Map String (String, CommunicationParser)
communicationParsers = Map.fromList [ ("AJSON", ("JSON array of arbitrary objects (Attoparsec)", A.fromText))
                                    , ("MJSON", ("JSON array of arbitrary objects (Megaparsec)", M.fromText))
                                    , ("CONLL", ("CONLL format", C.fromText))
                                    , ("PENN", ("PENN Treebank format", C.fromText))
                                    ]

toCommunications :: [ProtoCommunication] -> IO [Communication]
toCommunications ps = sequence [toCommunication i p | (i, p) <- zip [1..] ps]

toCommunication :: Int -> ProtoCommunication -> IO Communication
toCommunication i (t, ss) = do
  sections <- toSections ss
  uuid <- getUUID
  metadata <- createAnnotationMetadata "concrete-haskell ingester"
  return $ default_Communication { communication_text=Just t
                                 , communication_id=(pack . show) i
                                 , communication_uuid=uuid
                                 , communication_type=pack "test"
                                 , communication_sectionList=(Just . fromList) sections
                                 , communication_metadata=metadata
                                 }

toSections :: [ProtoSection] -> IO [Section]
toSections ps = sequence $ map toSection ps

toSection :: ProtoSection -> IO Section
toSection ProtoSection{..} = do
  uuid <- getUUID
  return $ default_Section { section_uuid=uuid
                           , section_kind=pack kind
                           , section_label=(Just . pack) label
                           , section_textSpan=Just $ TextSpan (fromIntegral start) (fromIntegral end)
                           }
