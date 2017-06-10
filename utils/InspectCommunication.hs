{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module Main (main) where

import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Data.Concrete.Utils as CU
import qualified Data.Concrete as C
import qualified Options.Applicative as O
import Options.Applicative (option, auto, short, long, metavar, strOption, help, execParser, info, helper, fullDesc, progDesc, many)
import qualified Data.Vector as V
import Control.Monad (liftM, mapM, join)
import Data.List (intercalate, concat)
import qualified Data.Text.Lazy as T
import Data.Maybe (fromJust, catMaybes, maybeToList)
import System.IO (stdin, stdout, stderr, openFile, Handle, IOMode(..), hPutStrLn)
import System.FilePath (takeExtension)
data Parameters = Parameters { inputFile :: Maybe String
                             } deriving (Show)

parameters :: O.Parser Parameters
parameters = Parameters
             <$> (O.optional $ strOption (short 'i'
                                          <> long "input"
                                          <> metavar "INPUT_FILE"
                                          <> help "Communications to inspect, serialized with Compact protocol, where the file name is interpreted by ending (.zip, .gz, .tgz) backing off to uncompressed sequence of Communications.  If ommitted, read uncompressed sequence of Communications from stdin."
                                         )
                 )

main = do
  ps <- execParser opts
  cs <- case inputFile ps of
    Just f -> case takeExtension f of
      ".tar" -> error "tar not implemented yet"
      ".tgz" -> error "compressed tar not implemented yet"
      ".tbz2" -> error "compressed tar not implemented yet"
      ".zip" -> error "zip not implemented yet"
      ".bz2" -> error "bzip not implemented yet"
      ".gz" -> CU.readCommunicationsFromBytes <$> ((liftM GZip.decompress . BS.readFile) f)      
      _ -> CU.readCommunicationsFromBytes <$> (BS.readFile f)
    Nothing -> CU.readCommunicationsFromBytes <$> (BS.hGetContents stdin)
  cs' <- cs
  putStr $ ((T.unpack . T.concat) $ map CU.showCommunication cs')
  where
    opts = info (helper <*> parameters)
           ( fullDesc
             <> progDesc "Inspect a tar file of serialized Communications"
           )
