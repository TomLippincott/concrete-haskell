{-# LANGUAGE DeriveGeneric, OverloadedStrings, BangPatterns #-}
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
import Control.Monad.State (runStateT, evalStateT)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, pack)
import Data.Concrete.Autogen.Communication_Types (default_Communication, Communication(..))
import Data.Concrete.Parsers.Types
import Control.Monad.IO.Class (liftIO)
import Text.Megaparsec (runParserT', initialPos, State(..), mkPos, parseErrorPretty, eof)
import Text.Megaparsec.Char (space)
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector, fromList, snoc, empty)
import qualified Data.Concrete.Parsers.JSON as JSON
import qualified Data.Concrete.Parsers.CONLL as CONLL
import qualified Data.Concrete.Parsers.HTML as HTML
import qualified Data.Concrete.Parsers.XML as XML
import qualified Data.Concrete.Parsers.CSV as CSV
import qualified Data.Concrete.Parsers.Email as Email
import qualified Data.Concrete.Parsers.PTB as PTB

-- | List of ingest configurations and default parameters
communicationParsers = [( "JSON"
                        , ( "JSON array of arbitrary objects"
                          , JSON.arrayParser
                          , [ "catchphrase"
                            , "relatives.0.name"
                            ]
                          , "json_${name}"
                          )
                        )
                       , ( "JSON-LINES"
                         , ( "One JSON object per line"
                           , JSON.lineParser
                           , [ "author"
                             , "subreddit"
                             ]
                           , "json-lines_${name}"
                           )
                         )
                       , ( "CSV"
                         , ( "CSV format (with header, commas)"
                           , CSV.fileParser Nothing ','
                           , [ "technology"
                             , "Bush"
                             , "Gore"
                             ]
                           , "csv_${county}"
                           )
                         )
                       , ("CONLL-U"
                         , ( "CONLL-U format"
                           , CONLL.fileParser CONLL.conllufields
                           , ["sentence"]
                           , "conll_${}"
                           )
                         )
                       , ( "PTB"
                         , ( "PENN Treebank format"
                           , PTB.fileParser
                           , ["sentence"]
                           , "ptb_${}"
                           )
                         )


                       -- , ("HTML"
                       --   , ("HTML format"
                       --     , HTML.parser
                       --     , []
                       --     , "id_${}"
                       --     )
                       --   )
                       , ("XML"
                         , ("XML format"
                           , XML.parser
                           , []
                           , "id_${}"
                           )
                         )
                       -- , ("Email"
                       --   , ("Email format"
                       --     , Email.parser
                       --     , []
                       --     , "id_${}"
                       --     )
                       --   )
                       ]

-- | Run CommunicationAction on each entry created during the ingest process
ingest :: CommunicationParser [Communication] -> Text -> [Text] -> Text -> Text -> IO [Communication]
--ingest :: ParsecT Void Text m [Communication] -> Text -> [Text] -> Text -> Text -> IO [Communication]
ingest p t cs i ct = do
  let s = State { stateInput=t
                , statePos=NE.fromList $ [initialPos "Text File"]
                , stateTokensProcessed=0
                , stateTabWidth=mkPos 8
                }
  (_, e) <- evalStateT (runParserT' p s) (Bookkeeper (default_Communication { communication_sectionList=Just empty }) Map.empty [] [] [] [] cs i ct 0 0)
  case e of
    Left x -> (putStrLn $ parseErrorPretty x) >> return []
    Right t -> return t
