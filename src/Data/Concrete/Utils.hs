{-# LANGUAGE DeriveGeneric #-}
module Data.Concrete.Utils
       (
         jsonToCommunication
       ) where

import GHC.Generics
import qualified Data.Concrete as C
import Data.Concrete (Communication)
import Data.Text
import Data.Aeson
import Data.ByteString.Lazy
import Data.Map

instance FromJSON Communication where
  parseJSON (Object v) = return C.default_Communication

jsonToCommunication :: ByteString -> IO (Maybe Communication)
jsonToCommunication t = do
  return $ (decode t :: Maybe Communication)

