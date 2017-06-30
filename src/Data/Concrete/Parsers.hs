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
import Text.Megaparsec (runParserT', initialPos, State(..), unsafePos, parseErrorPretty)
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector, fromList, snoc, empty)
import qualified Data.Concrete.Parsers.JSON as JSON
import qualified Data.Concrete.Parsers.CONLL as CONLL
import qualified Data.Concrete.Parsers.HTML as HTML
import qualified Data.Concrete.Parsers.XML as XML
import qualified Data.Concrete.Parsers.CSV as CSV
import qualified Data.Concrete.Parsers.Email as Email
import qualified Data.Concrete.Parsers.PTB as PTB


communicationParsers = Map.fromList [ ( "JSON"
                                      , ( "JSON array of arbitrary objects"
                                        , JSON.parser
                                        , [ "catchphrase"
                                          , "relatives.0.name"
                                          ]
                                        , "id_${name}"
                                        )
                                      )
                                    , ( "CSV"
                                      , ( "CSV format (with header, commas)"
                                        , CSV.parser True ','
                                        , [ "technology"
                                          , "Bush"
                                          , "Gore"
                                          ]
                                        , "id_${county}"
                                        )
                                      )
                                    -- , ( "PTB"
                                    --   , ( "PENN Treebank format"
                                    --     , PTB.parser
                                    --     , []
                                    --     , "id_${}"
                                    --     )
                                    --   )
                                    -- , ("CONLL"
                                    --   , ( "CONLL format"
                                    --     , CONLL.parser
                                    --     , []
                                    --     , "id_${}"
                                    --     )
                                    --   )
                                    -- , ("HTML", ("HTML format", HTML.parser))
                                    -- , ("XML", ("XML format", XML.parser))
                                    -- , ("Email", ("Email format", Email.parser))
                                    ]

ingest :: CommunicationAction -> CommunicationParser a -> Text -> [String] -> String -> String -> IO ()
ingest a p t cs i ct = do
  let s = State { stateInput=t
                , statePos=NE.fromList $ [initialPos "Text File"]
                , stateTokensProcessed=0
                , stateTabWidth=unsafePos 8
                }
  ((_, e), _) <- runStateT (runParserT' p s) (Bookkeeper (default_Communication { communication_sectionList=Just empty }) Map.empty [] [] a cs (pack i) ct 0)
  case e of
    Left x -> putStrLn $ parseErrorPretty x
    _ -> return ()
