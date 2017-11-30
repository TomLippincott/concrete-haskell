{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-|
Description : Parsers and utilities for ingesting various formats to Concrete

This module is designed to enable converting arbitrary formats
described as simple context-free (or even context-sensitive)
grammars into Concrete Communication objects.

A base UUID is generated randomly for each Communication, and subsequent UUIDs
within the Communication are increments of the base: this allows for more
efficient compression.
-}
module Data.Concrete.Parsers
       ( communicationParsers
       , ingest
       ) where

import qualified Data.Map as Map
import Data.Vector (fromList, Vector)
import Data.Map (Map)
import Data.Concrete.Utils (createAnnotationMetadata, getUUID, writeCommunication)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import Control.Monad.State (runStateT)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Data.Concrete.Parsers.Types
import Control.Monad.IO.Class (liftIO)
import Text.Megaparsec.Char (space)
import Text.Megaparsec (runParserT', initialPos, State(..), mkPos, parseErrorPretty, eof, ParsecT)
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import Data.Vector (Vector, fromList, snoc, empty)
import Data.Concrete.Parsers.Utils (finalizeCommunication)
import qualified Data.Concrete.Parsers.JSON as JSON
import Control.Monad.Identity (Identity(..))
import Data.Conduit.List (unfold)
import Conduit

import Text.Printf (printf)
import qualified Data.Concrete.Parsers.CONLL as CONLL
import qualified Data.Concrete.Parsers.CSV as CSV
import qualified Data.Concrete.Parsers.PTB as PTB
import qualified Data.Concrete.Parsers.HTML as HTML
import qualified Data.Concrete.Parsers.XML as XML
import qualified Data.Concrete.Parsers.Email as Email
import Control.Monad (void, join, liftM)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- | List of ingest configurations and default parameters
communicationParsers = [( "JSON-ARRAY"
                        , ( "JSON array of objects"
                          , JSON.arraySource
                          , [ "catchphrase"
                            , "relatives.0.name"
                            ]
                          , "json_${name}"
                          )
                        )
                       , ( "JSON-SEQUENCE"
                         , ( "One JSON object per line"
                           , JSON.sequenceSource
                           , [ "author"
                             , "subreddit"
                             ]
                           , "json-lines_${name}"
                           )
                         )
                       , ("CONLL-U"
                         , ( "CONLL-U format"
                           , CONLL.sequenceSource CONLL.ufields
                           , ["sentence"]
                           , "conll_${}"
                           )
                         )
                       , ( "PTB"
                         , ( "PENN Treebank format"
                           , PTB.sequenceSource
                           , ["sentence"] :: [Text]
                           , "ptb_${}" :: Text
                           )
                         )
                       -- , ( "CSV"
                       --   , ( "CSV format (with header, commas)"
                       --     , CSV.sequenceSource True ','
                       --     , [ "technology"
                       --       , "Bush"
                       --       , "Gore"
                       --       ]
                       --     , "csv_${county}"
                       --     )
                       --   )
                       -- , ("HTML"
                       --   , ("HTML format"
                       --     , HTML.sequenceSource
                       --     , []
                       --     , "id_${}"
                       --     )
                       --   )
                       -- , ("XML"
                       --   , ("XML format"
                       --     , XML.sequenceSource
                       --     , []
                       --     , "id_${}"
                       --     )
                       --   )
                       -- , ("Email"
                       --   , ("Email format"
                       --     , Email.sequenceSource
                       --     , []
                       --     , "id_${}"
                       --     )
                       --   )
                       ]

-- | Run CommunicationAction on each entry created during the ingest process
ingest :: (Text -> ConduitM () Communication IO ()) -> (Communication -> IO ()) -> (Communication -> Bool) -> Text -> [Text] -> Text -> Text -> IO ()
ingest src cb filt txt cs idStr ct = do
  runConduit $ src txt .| mergeSource (yieldMany [1..]) .| mapMC (finalizeCommunication idStr []) .| mapMC cb .| sinkNull
