{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-|
This module is designed to enable converting arbitrary formats
described as simple context-free (or even context-sensitive)
grammars into Concrete Communication objects.

UUIDs are generated when the Communications are serialized,
and so the module avoids the need for any impure computations
until that point.
-}
module Data.Concrete.Parsers
       ( communicationParsers
       , ingest
       ) where

import qualified Data.Map as Map
import Data.Vector (fromList, Vector)
import Data.Map (Map)
import Data.Concrete.Utils (createAnnotationMetadata, getUUID, writeCommunication)
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
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import Control.Monad.State (runStateT)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, pack)
import Data.Concrete.Types
import Data.Concrete.Parsers.Types
import Control.Monad.IO.Class (liftIO)
import Text.Megaparsec (runParserT', initialPos, State(..), unsafePos)
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector, fromList, snoc, empty)
import qualified Data.Concrete.Parsers.JSON as JSON
import qualified Data.Concrete.Parsers.CONLL as CONLL
import qualified Data.Concrete.Parsers.HTML as HTML
import qualified Data.Concrete.Parsers.XML as XML
import qualified Data.Concrete.Parsers.CSV as CSV
import qualified Data.Concrete.Parsers.Email as Email


communicationParsers = Map.fromList [ ("JSON", ("JSON array of arbitrary objects", JSON.arrayOfObjectsP))
                                    -- , ("CONLL", ("CONLL format", CONLL.arrayOfObjectsP))
                                    -- , ("HTML", ("HTML format", HTML.arrayOfObjectsP))
                                    -- , ("XML", ("XML format", XML.arrayOfObjectsP))
                                    -- , ("CSV", ("CSV format", CSV.arrayOfObjectsP))
                                    -- , ("Email", ("Email format", Email.arrayOfObjectsP))                                    
                                    ]

ingest :: CommunicationAction -> CommunicationParser a -> Text -> [String] -> String -> String -> IO ()
ingest a p t cs i ct= do
  let s = State { stateInput=t
                , statePos=NE.fromList $ [initialPos "JSON"]
                , stateTokensProcessed=0
                , stateTabWidth=unsafePos 8
                }
  runStateT (runParserT' p s) (Bookkeeper (default_Communication { communication_sectionList=Just empty }) Map.empty [] [] a cs (pack i) ct)
  return ()
