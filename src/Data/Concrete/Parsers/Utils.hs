{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards, FlexibleContexts #-}
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
                                   , unfoldParse
                                   , unfoldParseArray
                                   , unfoldParseNewline
                                   , finalizeCommunication
                                   ) where

import Data.Text.Lazy (Text, pack, unpack, replace)
import qualified Data.Text.Lazy as T
import Data.List (intercalate)
import Data.Concrete.Parsers.Types (Bookkeeper(..), CommunicationParser, CommunicationAction)
import Text.Megaparsec (ParsecT, getParserState, stateTokensProcessed, match, State(..), mkPos, initialPos, runParserT', parseErrorPretty)
import Text.Megaparsec.Char (char, oneOf, space, newline)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, get, put, modify, modify')
--import Data.Concrete.Prelude
import Data.Concrete.Autogen.Communication_Types (Communication(..), default_Communication)
import Data.Concrete.Autogen.Structure_Types (Section(..), default_Section, Token(..), default_Token, Sentence(..), default_Sentence, TokenizationKind(..), Tokenization(..), default_Tokenization, TokenList(..), default_TokenList)
import Data.Concrete.Autogen.Spans_Types (TextSpan(..), default_TextSpan, AudioSpan(..), default_AudioSpan)
import Data.Concrete.Autogen.Metadata_Types (default_AnnotationMetadata)
import Data.Concrete.Autogen.Uuid_Types (default_UUID)
import Data.Concrete.Utils (getUUID, createAnnotationMetadata, incrementUUID)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (Vector, fromList, snoc, empty, cons, toList)
import qualified Data.Vector as V
import Data.Maybe (fromJust, catMaybes)
import Text.Printf (printf)
import Conduit
import Data.Conduit.List (unfold, unfoldM)
import Control.Monad.State (runStateT)
import Control.Lens hiding (cons)
--import Data.Concrete.Autogen.Lens.Communication hiding (communication)
--import Data.Concrete.Autogen.Lens.Section
import Data.Concrete.Prelude hiding (communication)

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
  let u = default_UUID
      m = default_AnnotationMetadata
  --u <- liftIO getUUID
  --m <- liftIO $ createAnnotationMetadata "concrete-haskell ingester"
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
communicationRule :: (Communication -> Communication) -> CommunicationParser a -> CommunicationParser Communication
communicationRule tr p = do
  offset <- (fromIntegral . stateTokensProcessed) <$> getParserState
  bs' <- get
  put $ bs' { offset=offset }
  (t, o) <- match p
  bs@(Bookkeeper {..}) <- get
  let sections = (toList . fromJust) (communication_sectionList communication)
      u = default_UUID
  --u <- liftIO getUUID
  let us = iterate incrementUUID u
      m = default_AnnotationMetadata
  --m <- liftIO $ createAnnotationMetadata "concrete-haskell ingester"
  let sections' = [s { section_uuid=u'
                     , section_kind="" -- if elem ((unpack . fromJust) section_label) contentSections then "content" else "metadata"
                     , section_textSpan=(\ (Just (TextSpan{..})) -> Just $ TextSpan (textSpan_start - offset) (textSpan_ending - offset)) section_textSpan
                     } | (u', s@(Section{..})) <- zip us sections]

      sectionVals = [(fromJust section_label, substr t ((fromIntegral . textSpan_start . fromJust) section_textSpan) ((fromIntegral . textSpan_ending . fromJust) section_textSpan)) | Section{..} <- sections']
      c = communication { communication_metadata=m
                        , communication_text=Just t
                        , communication_uuid=u
                        -- , communication_id=makeId sectionVals commId commNum
                        , communication_sectionList=Just $ fromList sections'
                        }
  put $ bs { communication=default_Communication { communication_sectionList=Just empty }, valueMap=Map.fromList [], sections=[] }
  clearState
  return c

-- | Extracts a sub-string from a text object
substr :: T.Text -> Int -> Int -> Text
substr t s e = res
  where
    (_, start) = T.splitAt (fromIntegral s) t
    res = T.take (fromIntegral $ e - s) start

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

-- | Performs variable substitution on an ID string
makeId :: Communication -> Text -> Int -> Text
makeId c idStr n = foldr (\ (a, b) x -> T.replace (T.concat ["${", a, "}"]) b x) idStr (("", (pack . show) n):ss'')
  where
    Just ss' = V.toList <$> c ^. _communication_sectionList    
    Just t = communication_text c
    ss'' = map (\s -> ((fromJust . section_label) s, (spanText t . fromJust . section_textSpan) s)) ss'

    --k = map (\s -> (section_label s, spanText t s)) (catMaybes (map section_textSpan ss'))

finalizeCommunication :: Text -> [Text] -> (Int, Communication) -> IO Communication
finalizeCommunication idStr cs (i, c) = return $ c & _communication_id .~ cid
  where
    cid = makeId c idStr i
      
oneParse b p s = case runStateT (runParserT' p s) b of
                   Identity ((_, Left e), _) -> Nothing -- error $ parseErrorPretty e
                   Identity ((s', Right c), _) -> Just (c, s')

unfoldParse :: Monad m => CommunicationParser Communication -> Text -> ConduitM () Communication m ()
unfoldParse p t = unfoldC (oneParse b p) s
  where
    s = State { stateInput=t
              , statePos=NE.fromList $ [initialPos "Text File"]
              , stateTokensProcessed=0
              , stateTabWidth=mkPos 8
              }
    b = Bookkeeper (default_Communication { communication_sectionList=Just empty }) Map.empty [] [] [] [] 0

unfoldParseArray :: Monad m => CommunicationParser Communication -> Text -> ConduitM () Communication m ()
unfoldParseArray p t = unfoldC (oneParse b p') s
  where
    t' = T.dropWhile (\c -> c /= '{') t
    s = State { stateInput=t'
              , statePos=NE.fromList $ [initialPos "Text File"]
              , stateTokensProcessed=0
              , stateTabWidth=mkPos 8
              }
    b = Bookkeeper (default_Communication { communication_sectionList=Just empty }) Map.empty [] [] [] [] 0        
    p' = do
      c <- p
      space
      oneOf [',', ']']
      space
      return c

unfoldParseNewline :: Monad m => CommunicationParser Communication -> Text -> ConduitM () Communication m ()
unfoldParseNewline p t = unfoldC (oneParse b p') s
  where
    s = State { stateInput=t
              , statePos=NE.fromList $ [initialPos "Text File"]
              , stateTokensProcessed=0
              , stateTabWidth=mkPos 8
              }
    b = Bookkeeper (default_Communication { communication_sectionList=Just empty }) Map.empty [] [] [] [] 0        
    p' = do
      c <- p
      newline
      return c

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

spanText :: Text -> TextSpan -> Text
spanText t ts = substr t (fromIntegral $ textSpan_start ts) (fromIntegral $ textSpan_ending ts)
