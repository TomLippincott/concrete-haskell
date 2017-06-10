{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Types
       ( --ProtoSection(..)
       --, ProtoCommunication
       ) where

import Data.Text.Lazy (Text)
import Data.Concrete (Communication)


-- data ProtoSection = ProtoSection { start :: Int
--                                  , end :: Int
--                                  , label :: String
--                                  , kind :: String
--                                  }
                    
-- type ProtoCommunication = (Text, [ProtoSection])

-- type CommunicationStorer = [Communication] -> IO ()

-- -- | A 'Bookkeeper' tracks information about an ongoing attempt
-- --   to parse a Text stream into Communication objects.
-- data Bookkeeper = Bookkeeper { valueMap :: Map String String -- | An arbitrary string-to-string map
--                              , comms :: [Communication] -- | List of Communications that have been parsed so far
--                              , sections :: [Section] -- | List of Sections accumulated for the Communication currently being parsed
--                              }

-- -- | A 'CommunicationParser' is a stateful Megaparsec parser that, as it
-- --   processes a Text stream, builds a list of Concrete Communications.
-- type CommunicationParser a = ParsecT Dec Text (State Bookkeeper) a


-- --type CommParser ParsecT Dec Text (S.State Bookkeeping) ()
