{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.Utils ( communicationRule
                                   , sectionRule
                                   , Located(..)
                                   ) where

import Data.Text.Lazy (Text, pack, unpack, replace)
import qualified Data.Text.Lazy as T
import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser, CommunicationAction)
import Text.Megaparsec (ParsecT, getParserState, stateTokensProcessed, match)
import Text.Megaparsec.Error (Dec)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, get, put)
import Data.Concrete (default_Communication, Communication(..), default_Section, Section(..), default_TextSpan, TextSpan(..))
import Data.Concrete.Utils (getUUID, createAnnotationMetadata)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (Vector, fromList, snoc, empty, cons, toList)
import Data.Maybe (fromJust)

substr :: T.Text -> Int -> Int -> String
substr t s e = T.unpack res
  where
    (_, start) = T.splitAt (fromIntegral s) t    
    res = T.take (fromIntegral $ e - s) start


makeId :: [(Text, Text)] -> Text -> Int -> Text
makeId ss i n = foldr (\ (a, b) x -> T.replace (T.concat ["${", a, "}"]) b x) i (("", (pack . show) n):ss)

communicationRule :: (Communication -> Communication) -> CommunicationParser a -> CommunicationParser a
communicationRule tr p = do
  offset <- (fromIntegral . stateTokensProcessed) <$> getParserState  
  (t, o) <- match p
  bs@(Bookkeeper {..}) <- get
  let sections = (toList . fromJust) (communication_sectionList communication)
  (u:us) <- liftIO $ sequence (replicate (length sections + 1) getUUID)
  m <- liftIO $ createAnnotationMetadata "concrete-haskell ingester"
  let sections' = [s { section_uuid=u'
                     , section_kind=if elem ((unpack . fromJust) section_label) contentSections then "content" else "metadata"
                     , section_textSpan=(\ (Just (TextSpan{..})) -> Just $ TextSpan (textSpan_start - offset) (textSpan_ending - offset)) section_textSpan
                     } | (u', s@(Section{..})) <- zip us sections]

      sectionVals = [(fromJust section_label, pack $ substr (pack t) ((fromIntegral . textSpan_start . fromJust) section_textSpan) ((fromIntegral . textSpan_ending . fromJust) section_textSpan)) | Section{..} <- sections']
      c = communication { communication_metadata=m
                        , communication_text=Just $ pack t
                        , communication_uuid=u
                        , communication_id=makeId sectionVals commId commNum
                        , communication_sectionList=Just $ fromList sections'
                        }
  put $ bs { communication=default_Communication { communication_sectionList=Just empty }, valueMap=Map.fromList [], sections=[], commNum=commNum + 1 }
  liftIO $ action (tr c)
  return o

sectionRule :: (Section -> Section) -> CommunicationParser a -> CommunicationParser a
sectionRule t p = do
  s <- (fromIntegral . stateTokensProcessed) <$> getParserState
  v <- p
  e <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs@(Bookkeeper {..}) <- get
  let path' = (intercalate "." (reverse path))
      section = t $ default_Section { section_label=(Just . pack) path'
                                    , section_textSpan=Just $ TextSpan s e
                                    }
  if length path == 0
    then
      return ()
    else
      put $ bs { communication=communication { communication_sectionList=(cons section) <$> (communication_sectionList communication) } }
  return v

class Located a where
  getTextSpan :: a -> TextSpan
  setTextSpan :: TextSpan -> a -> a
  adjustTextSpan :: Integral i => i -> i -> a -> a
  adjustTextSpan s' e' a = setTextSpan (ts { textSpan_start=textSpan_start + (fromIntegral s')
                                           , textSpan_ending=textSpan_ending + (fromIntegral e')
                                           }) a
    where
      ts@(TextSpan {..}) = getTextSpan a

instance Located Section where
  getTextSpan s = (fromJust . section_textSpan) s
  setTextSpan ts s = s { section_textSpan=Just ts }
  
