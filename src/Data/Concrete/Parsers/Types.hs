{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.Types ( Bookkeeper(..)
                                   , CommunicationParser
                                   , CommunicationAction
                                   ) where

import Data.Text.Lazy (Text)
import Data.Concrete (Communication, Section)
import Text.Megaparsec (ParsecT)
import Text.Megaparsec.Error (Dec)
import Data.Map (Map)
import Control.Monad.State (StateT)

-- | A 'CommunicationAction' gets called on each Communication
--   as parsing proceeds
type CommunicationAction = Communication -> IO ()
                    
-- | A 'Bookkeeper' tracks information about an ongoing attempt
--   to parse a Text stream into Communication objects.
data Bookkeeper = Bookkeeper { communication :: Communication
                             , valueMap :: Map String String -- | An arbitrary string-to-string map
                             , path :: [String]
                             , sections :: [Section] -- | List of Sections accumulated for the Communication currently being parsed
                             , action :: CommunicationAction
                             , contentSections :: [String]
                             , commId :: Text
                             , commType :: String
                             , commNum :: Int
                             }

-- | A 'CommunicationParser' is a stateful Megaparsec parser that, as it
--   processes a Text stream, builds a list of Concrete Communications.
type CommunicationParser a = ParsecT Dec Text (StateT Bookkeeper IO) a
