{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Data.Concrete.Parsers.JSON
       (
         JSON(..)
       ) where

-- import Data.Concrete (Communication)
-- import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
-- import qualified Data.Text.Lazy as T
-- import Data.Functor (($>))
import qualified Data.Map as M
-- import System.IO (Handle)
-- import Control.Monad.IO.Class (liftIO)
-- --import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
-- import Data.Attoparsec.Text.Lazy (parse, eitherResult)
-- import qualified Data.Attoparsec.ByteString as B
-- import qualified Data.Attoparsec.Text.Lazy as AT
import Data.Scientific (scientific, Scientific(..))

-- import Text.Megaparsec.Char (spaceChar, char)
-- import qualified Text.Megaparsec.Lexer as L
-- import Text.Megaparsec.Combinator (choice, sepBy, sepBy1, between)
-- import Text.Megaparsec.Error (Dec)
-- import Text.Megaparsec.Pos (defaultTabWidth)
-- import Text.Megaparsec.Prim (Parsec, ParsecT, setTokensProcessed)
-- import qualified Text.Megaparsec as MT
-- import Text.Megaparsec ( parseErrorPretty
--                        , many
--                        , eof
--                        , (<|>)
--                        , space
--                        , try
--                        , hexDigitChar
--                        , count
--                        , manyTill
--                        , anyChar
--                        , getParserState
--                        , State(..)
--                        , ParsecT
--                        , SourcePos(..)
--                        , Pos(..)
--                        , initialPos
--                        , runParserT
--                        , runParserT'
--                        , some
--                        , match
--                        )
data JSON = JNumber Scientific
          | JText Text
          | JArray [JSON]
          | JObject (M.Map String JSON)
          | JBool Bool
          | JNull
            deriving (Show)

-- type Input = Text
-- --type CommunicationParser a = Parsec Dec Input a -- (S.StateT BuildState IO) a 

-- data JSON = JNumber Scientific
--           | JText Text
--           | JArray [JSON]
--           | JObject (M.Map String JSON)
--           | JBool Bool
--           | JNull
--             deriving (Show)

-- attoFromText t = AT.eitherResult $ AT.parse aJsonP t

-- aJsonP :: AT.Parser JSON
-- aJsonP = do
--   AT.skipSpace
--   j <- AT.choice [aStringP, aNullP, aNumberP, aBoolP, aArrayP, aObjectP]
--   --, aNumberP, aStringP, aBoolP, aObjectP, aArrayP]
--   AT.skipSpace
--   return j

-- aNullP = AT.string "null" >> return JNull
-- aNumberP = AT.scientific >>= (\x -> return (JNumber x))
-- aBoolP = AT.string "true" <|> AT.string "false" >>= (\x -> return $ JBool (x == "true"))

-- aEscapedChar = do
--   AT.char '\\'
--   AT.choice [
--     AT.char '\"' $> '\"',   -- A boring list of hardcoded values from the spec.
--     AT.char '\\' $> '\\',
--     AT.char '/'  $> '/' ,
--     AT.char 'n'  $> '\n',
--     AT.char 'r'  $> '\r',
--     AT.char 'f'  $> '\f',
--     AT.char 't'  $> '\t',
--     AT.char 'b'  $> '\b']
-- --    aUnicodeEscape ]


-- -- aUnicodeEscape = do
-- --   AT.char 'u'
-- --   code <- AT.count 4 (AT.choice [AT.digit, AT.letter])
-- --   return $ toEnum (read ("0x" ++ code))

-- aLexeme m = m >>= (\x -> AT.skipSpace >> return x)
-- aSymbol m = AT.string m >>= (\x -> AT.skipSpace >> return x)
--   --L.symbol space
-- aBrackets = aBetween (aSymbol "[") (aSymbol "]")
-- aBraces = aBetween (aSymbol "{") (aSymbol "}")
-- --lexeme = L.lexeme space
-- aComma = aSymbol ","
-- aBetween o c m = o >> m >>= (\x -> c >> return x)

-- aStringLiteral = aLexeme $ do 
--   AT.char '\"'
--   (aEscapedChar <|> AT.anyChar) `manyTill` AT.char '\"'
  
-- aStringP = do
--   c <- T.pack <$> aStringLiteral
--   --S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), c):idVals })
--   return $ JText c

-- aArrayEntryP = do
--   --bs@(BuildState{..}) <- S.get
--   --let p = (read $ head path) :: Int
--   --S.put $ bs { path=(show (p + 1)):(tail path) }  
--   aJsonP
  
-- aArrayP = do
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=(show (-1)):path })
--   c <- aBrackets (aArrayEntryP `AT.sepBy` aComma)
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
--   return $ JArray c

--   -- data BuildState = BuildState { path :: [String]
-- --                              , sections :: [Section]
-- --                              , comms :: [Communication]
-- --                              , offset :: Int
-- --                              , params :: Parameters
-- --                              , idVals :: [(Text, Text)]
-- --                              , out :: Handle
-- --                              }
-- aPairP = do
--   key <- aStringLiteral
--   aSymbol ":"
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=key:path })
--   value <- aJsonP
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
--   return (key, value)

-- aObjectP = do
--   c <- M.fromList <$> aBraces (aPairP `sepBy` aComma)
--   return $ JObject c


-- megaFromText t = MT.runParserT' mJsonP t

-- type CommunicationParser a = Parsec Dec Text a 

-- mJsonP :: CommunicationParser JSON -- MT.Parsec Dec Text
-- mJsonP = do
--   j <- mLexeme $ MT.choice [mNullP, mNumberP, mStringP, mBoolP, mObjectP, mArrayP]
--   --uuid <- liftIO CU.getUUID
--   -- let path' = (intercalate "." (reverse path))
--   --     section = default_Section { section_uuid=uuid
--   --                               , section_label=Just $ T.pack path'
--   --                               , section_textSpan=Just $ TextSpan (s - (fromIntegral offset)) (e - (fromIntegral offset))
--   --                               , section_kind=if path' `elem` (contentSectionTypes params) then "content" else "metadata"
--   --                               }
--   -- S.put $ bs { sections=section:sections }
--   return j

-- mNullP = do
--   mSymbol "null"
--   --S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), T.pack "null"):idVals })    
--   return $ JNull
  
-- mBoolP = do
--   c <- mSymbol "true"  <|> mSymbol "false"
--   --S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), T.pack c):idVals })  
--   return $ JBool (c == "true")
  
-- mNumberP = do
--   (s, c) <- MT.match $ L.signed (pure ()) (try L.float <|> (fromInteger <$> L.integer))
--   --S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), T.pack s):idVals })  
--   return $ JNumber (scientific 1 2) --c


-- mEscapedChar = do
--   char '\\'
--   choice [
--     char '\"' $> '\"',   -- A boring list of hardcoded values from the spec.
--     char '\\' $> '\\',
--     char '/'  $> '/' ,
--     char 'n'  $> '\n',
--     char 'r'  $> '\r',
--     char 'f'  $> '\f',
--     char 't'  $> '\t',
--     char 'b'  $> '\b',
--     mUnicodeEscape ]

-- mUnicodeEscape = do
--   char 'u'
--   code <- MT.count 4 MT.hexDigitChar
--   return $ toEnum (read ("0x" ++ code))

-- mStringLiteral = lexeme $ do 
--   char '\"'
--   (mEscapedChar <|> MT.anyChar) `MT.manyTill` char '\"'
  
-- mStringP = do
--   c <- T.pack <$> mStringLiteral
--   --S.modify (\bs@(BuildState{..}) -> bs { idVals=(T.pack (intercalate "." (reverse path)), c):idVals })
--   return $ JText c

-- mArrayEntryP = do
--   --bs@(BuildState{..}) <- S.get
--   --let p = (read $ head path) :: Int
--   --S.put $ bs { path=(show (p + 1)):(tail path) }  
--   mJsonP
  
-- mArrayP = do
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=(show (-1)):path })
--   c <- mBrackets (mArrayEntryP `MT.sepBy` mComma)
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
--   return $ JArray c
  
-- mPairP = do
--   key <- mStringLiteral
--   mSymbol ":"
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=key:path })
--   value <- mJsonP
--   --S.modify' (\ bs@(BuildState{..}) -> bs { path=tail path})
--   return (key, value)

-- mObjectP = do
--   c <- M.fromList <$> mBraces (mPairP `sepBy` mComma)
--   return $ JObject c

-- --makeId :: [(Text, Text)] -> Text -> Text
-- --makeId ss i = foldr (\ (a, b) x -> T.replace (T.concat ["${", a, "}"]) b x) i ss

-- mTopLevelObjectP = do
--   offset <- (fromIntegral . MT.stateTokensProcessed) <$> MT.getParserState
--   --S.modify (\bs -> bs { offset=offset })
--   (ts, _) <- MT.match mObjectP
--   end <- (fromIntegral . MT.stateTokensProcessed) <$> MT.getParserState
--   return end
--   --uuid <- liftIO CU.getUUID
--   --BuildState{..} <- S.get
--   -- let comm = default_Communication { communication_id=makeId idVals ((T.pack . commId) params)
--   --                                  , communication_type=T.pack $ commType params
--   --                                  , communication_uuid=uuid
--   --                                  , communication_text=Just $ T.pack ts
--   --                                  , communication_sectionList=(Just . fromList . reverse) sections
--   --                                  } 
--   -- comms' <- if length comms >= 100
--   --           then
--   --             (liftIO $ CU.writeCommunications out comms) >> return [comm]
              
--   --           else
--   --             return $ comm:comms
--   -- S.modify (\ bs@(BuildState{..}) -> bs { sections=[]
--   --                                       , comms=comms'
--   --                                       , idVals=[]
--   --                                       })

-- mSymbol = L.symbol space
-- mBrackets = MT.between (mSymbol "[") (mSymbol "]")
-- mBraces = MT.between (mSymbol "{") (mSymbol "}")
-- mLexeme = L.lexeme space
-- mComma = mSymbol ","
