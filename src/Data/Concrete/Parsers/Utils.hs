{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.Utils ( communicationRule
                                   , sectionRule
                                   , sentenceRule
                                   , tokenRule
                                   , pathDictionaryRule
                                   , pathDictionaryKeyRule                                   
                                   , pathArrayRule
                                   , pathArrayEntryRule
                                   , pushPathComponent
                                   , popPathComponent
                                   , modifyPathComponent
                                   , incrementPathComponent                                   
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
import Control.Monad.State (State, get, put, modify, modify')
import Data.Concrete.Autogen.Communication_Types (Communication(..), default_Communication)
import Data.Concrete.Autogen.Structure_Types (Section(..), default_Section, Token(..), default_Token, Sentence(..), default_Sentence, TokenizationKind(..), Tokenization(..), default_Tokenization, TokenList(..), default_TokenList)
import Data.Concrete.Autogen.Spans_Types (TextSpan(..), default_TextSpan, AudioSpan(..), default_AudioSpan)
import Data.Concrete.Utils (getUUID, createAnnotationMetadata, incrementUUID)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (Vector, fromList, snoc, empty, cons, toList)
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Text.Printf (printf)

-- | Wraps a rule corresponding to a Token
tokenRule :: (Token -> Token) -> CommunicationParser a -> CommunicationParser a
tokenRule t p = do
  s <- (fromIntegral . stateTokensProcessed) <$> getParserState
  v <- p
  e <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs@(Bookkeeper {..}) <- get
  let token = t $ default_Token { token_textSpan=Just $ TextSpan (s - offset) (e - offset)
                                , token_tokenIndex=fromIntegral $ length tokens
                                }
  put $ bs { tokens=token:tokens
           }
  return v
  
-- | Wraps a rule corresponding to a Sentence
sentenceRule :: (Sentence -> Sentence) -> CommunicationParser a -> CommunicationParser a
sentenceRule t p = do
  s <- (fromIntegral . stateTokensProcessed) <$> getParserState
  v <- p
  e <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs@(Bookkeeper {..}) <- get
  u <- liftIO getUUID
  m <- liftIO $ createAnnotationMetadata "concrete-haskell ingester"
  let tokenList = default_TokenList { tokenList_tokenList=V.fromList (reverse tokens)
                                    }
      tokenization = default_Tokenization { tokenization_tokenList=Just tokenList
                                          , tokenization_metadata=m
                                          , tokenization_uuid=u
                                          , tokenization_kind=TOKEN_LIST
                                          }
      sentence = t $ default_Sentence { sentence_textSpan=Just $ TextSpan (s - offset) (e - offset)
                                      , sentence_tokenization=Just tokenization
                                      }
  put $ bs { sentences=sentence:sentences
           , tokens=[]
           }
  return v
  
-- | Wraps a rule corresponding to a Section
sectionRule :: (Section -> Section) -> CommunicationParser a -> CommunicationParser a
sectionRule t p = do
  s <- (fromIntegral . stateTokensProcessed) <$> getParserState
  v <- p
  e <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs@(Bookkeeper {..}) <- get
  let path' = (intercalate "." (reverse path))
      section = t $ default_Section { section_label=(Just . pack) path'
                                    , section_textSpan=Just $ TextSpan s e
                                    , section_sentenceList=if length sentences == 0 then Nothing else Just $ V.fromList sentences
                                    }
  if length path == 0
    then
    put $ bs { communication=communication { communication_sectionList=(cons section) <$> (communication_sectionList communication) }
             , sentences=[]
             , tokens=[]
             }
    else
    put $ bs { communication=communication { communication_sectionList=(cons section) <$> (communication_sectionList communication) }
             , sentences=[]
             , tokens=[]
             }
  return v

-- | Wraps a rule that corresponds to a Communication
communicationRule :: (Communication -> Communication) -> CommunicationParser a -> CommunicationParser a
communicationRule tr p = do
  offset <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs' <- get
  put $ bs' { offset=offset }
  (t, o) <- match p
  bs@(Bookkeeper {..}) <- get
  let sections = (toList . fromJust) (communication_sectionList communication)
  u <- liftIO getUUID
  let us = iterate incrementUUID u
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
  clearState
  return o

-- | Extracts a sub-string from a text object
substr :: T.Text -> Int -> Int -> String
substr t s e = T.unpack res
  where
    (_, start) = T.splitAt (fromIntegral s) t
    res = T.take (fromIntegral $ e - s) start

-- | Performs variable substitution on an ID string
makeId :: [(Text, Text)] -> Text -> Int -> Text
makeId ss i n = foldr (\ (a, b) x -> T.replace (T.concat ["${", a, "}"]) b x) i (("", (pack . show) n):ss)

-- | Resets the "Communication-building" state inside the parser
clearState :: CommunicationParser ()
clearState = do
  bs <- get
  let s = bs { communication=default_Communication { communication_sectionList=Just empty }
             , sections=[]
             , path=[]
             }
  put s
  return ()

-- | Wraps a rule corresponding to an array-like object, pushing a dummy
--   numeric component onto the path at the start, and popping at the end
pathArrayRule :: CommunicationParser a -> CommunicationParser a
pathArrayRule p = do
  pushPathComponent (show (-1))
  (m, r) <- match p
  popPathComponent
  return r

-- | Wraps a rule corresponding to an array-like entry, incrementing the
--   top-level path component
pathArrayEntryRule :: CommunicationParser a -> CommunicationParser a
pathArrayEntryRule p = do
  incrementPathComponent
  p

-- | Wraps a rule corresponding to a dictionary-like object, pushing a
--   dummy string component onto the path, and popping at the end
pathDictionaryRule :: CommunicationParser a -> CommunicationParser a
pathDictionaryRule p = do
  pushPathComponent "PLACEHOLDER"
  (m, r) <- match p
  popPathComponent
  return r

-- | Wraps a rule corresponding to a key in a dictionary-like object, modifying
--   the current head of the stack
pathDictionaryKeyRule :: CommunicationParser String -> CommunicationParser String
pathDictionaryKeyRule p = do
  t' <- p
  modifyPathComponent (\x -> t')
  return ""

-- | Push a string component onto the path
pushPathComponent :: String -> CommunicationParser ()
pushPathComponent s = do
  Bookkeeper{..} <- get
  modify (\ bs -> bs { path=s:path })
  return ()

-- | Pop the top-level component from the path
popPathComponent :: CommunicationParser String
popPathComponent = do
  Bookkeeper{..} <- get
  let (p:ps) = path
  modify (\ bs -> bs { path=ps })
  return p

-- | Modify the top-level path component
modifyPathComponent :: (String -> String) -> CommunicationParser ()
modifyPathComponent f = do
  Bookkeeper{..} <- get
  let (p:ps) = path
      p' = f p
  modify (\ bs -> bs { path=p':ps })
  return ()

-- | Increment the top-level path component as an integer
incrementPathComponent :: CommunicationParser Int
incrementPathComponent = do
  Bookkeeper{..} <- get
  let p = (read $ head path) :: Int
      p' = p + 1
  modify (\ bs -> bs { path=(show p'):(tail path) })
  return p'

-- | A data structure that is positioned inside a document and whose boundaries can be adjusted
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
  
instance Located Sentence where
  getTextSpan s = (fromJust . sentence_textSpan) s
  setTextSpan ts s = s { sentence_textSpan=Just ts }

instance Located Token where
  getTextSpan s = (fromJust . token_textSpan) s
  setTextSpan ts s = s { token_textSpan=Just ts }
