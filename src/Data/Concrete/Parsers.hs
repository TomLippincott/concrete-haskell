{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers
       ( --M.fromText
       --, A.fromText
       ) where

import Data.Concrete (Communication)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Concrete.Parsers.JSON.Megaparsec as M
import qualified Data.Concrete.Parsers.JSON.Attoparsec as A

