{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.Types ( Bookkeeper(..)
                                   , CommunicationParser
                                   , CommunicationAction
                                   , PathComponent(..)
                                   , Path
                                   , IngestStream
                                   ) where

import Data.Text.Lazy (Text)
import Data.Concrete.Autogen.Communication_Types (Communication)
import Data.Concrete.Autogen.Structure_Types (Section, Sentence, Token)
import Text.Megaparsec (ParsecT)
import Data.Map (Map)
import Control.Monad.State (StateT)
import GHC.Int
import Data.Void (Void)
import Conduit
import Control.Monad.Identity (Identity)

type IngestStream = ConduitM Text Communication (ResourceT IO) ()

-- | A 'CommunicationAction' gets called on each Communication
--   as parsing proceeds
type CommunicationAction = Communication -> IO ()

-- | A 'PathComponent' represents one step in navigating a parse
--   tree.  The meaning will be format-specific: a "step" could
--   correspond to any number of parse rules.  See the definition
--   of 'Path' for an example.
data PathComponent = Index Int -- ^ An index into an anonymous sequence of successors of a path
                   | Name String -- ^ The name of an identifiable successor of a path

-- | A 'Path' is a sequence of 'PathComponent's that identifies
--   a particular location in the document being parsed, usually
--   the *current* location, as a sequence of indices and strings.
--   Think of it as the sequence of values you would use to
--   index a Python-style object of nested dictionaries and lists.
--   The meaning of the indices and strings depends on the format
--   being parsed: for example, in parsing this HTML:
--
-- >    <list id="abc">
-- >      <li>one</li>
-- >      <li style="final">two</li>
-- >    </list>
--
--   If we treat the "list" element as the top-level object, we
--   might, as we parse the document, generate the paths:
--
-- >    [Name "id"]
-- >    [Index 1]
-- >    [Index 2]
-- >    [Index 2, Name "style"]
--
--   The trick is to take a parser for a data format, and augment
--   the rules so that the current 'Path' is always correct and
--   meaningful.
type Path = [PathComponent]

-- | A 'Bookkeeper' tracks information about an ongoing attempt
--   to parse a Text stream into Communication objects.
data Bookkeeper = Bookkeeper { communication :: Communication
                             , valueMap :: Map String String -- | An arbitrary string-to-string map
                             , path :: [String]
                             , sections :: [Section] -- | List of Sections accumulated for the Communication currently being parsed
                             , sentences :: [Sentence] -- | List of Sections accumulated for the Communication currently being parsed
                             , tokens :: [Token] -- | List of Sections accumulated for the Communication currently being parsed
                             --, contentSections :: [String]
                             --, commId :: Text
                             --, commType :: Text
                             --, commNum :: Int
                             , offset :: GHC.Int.Int32
                             }

-- | A StatefulParser is just a Megaparsec Parser that carries
--   a State, and has access to the IO monad.
type StatefulParser s a = ParsecT Void Text (StateT s Identity) a

-- | A 'CommunicationParser' is a stateful Megaparsec parser that, as it
--   processes a Text stream, builds a list of Concrete Communications.
--type CommunicationParser a = ParsecT Void Text Identity a
type CommunicationParser a = StatefulParser Bookkeeper a
