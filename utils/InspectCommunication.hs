{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module Main (main) where

import Data.Monoid ((<>))
import qualified Data.Concrete.Utils as CU
import qualified Data.Concrete as C
import qualified Options.Applicative as O
import Options.Applicative (option, auto, short, long, metavar, strOption, help, execParser, info, helper, fullDesc, progDesc, many)
import qualified Data.Vector as V
import Control.Monad (liftM, mapM)
import Data.List (intercalate, concat)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromJust, catMaybes, maybeToList)

data Parameters = Parameters { inputFile :: String
                             } deriving (Show)

parameters :: O.Parser Parameters
parameters = Parameters
             <$> strOption (short 'i'
                            <> long "input"
                            <> metavar "INPUT_FILE"
                            <> help "Text file of JSON objects"
                           )

showSection :: T.Text -> C.Section -> String
showSection t s = "\t" ++ ((T.unpack . fromJust . C.section_label) s) ++ " " ++ (T.unpack $ C.section_kind s) ++ "--->" ++ t'
  where
    C.TextSpan s' e' = (fromJust . C.section_textSpan) s
    t' = substr t (fromIntegral s') (fromIntegral e')

showCommunication :: C.Communication -> String
showCommunication c = (T.unpack $ (C.communication_id c)) ++ " " ++ (T.unpack $ C.communication_type c) ++ "\n" ++ (intercalate "\n" sects) ++ "\n"
  where    
    ss = concat $ map V.toList (maybeToList (C.communication_sectionList c))
    t = (fromJust . C.communication_text) c
    sects = map (showSection t) ss


substr :: T.Text -> Int -> Int -> String
substr t s e = T.unpack res
  where
    (_, start) = T.splitAt (fromIntegral s) t    
    res = T.take (fromIntegral $ e - s) start

main = do
  ps <- execParser opts
  cs <- CU.readCommunications (inputFile ps)
  putStr $ intercalate "\n" (map show cs) ++ "\n"
  putStr $ intercalate "\n" [showCommunication c | c <- cs] ++ "\n"
  where
    opts = info (helper <*> parameters)
           ( fullDesc
             <> progDesc "Inspect a tar file of serialized Communications"
           )
