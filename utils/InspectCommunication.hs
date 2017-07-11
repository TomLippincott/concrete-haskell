{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, FlexibleContexts, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module Main (main) where

import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
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
                                          <> help "Communications to inspect, serialized with Compact protocol, where the file name is interpreted by ending (.zip, .gz, .bz2, .tgz, .tbz2) backing off to uncompressed sequence of Communications.  If no file is specified, read uncompressed sequence of Communications from stdin."
                                         )
                 )

main = do
  ps <- execParser opts
  cs <- join $ case inputFile ps of
    Just f -> case takeExtension f of
      ".tar" -> CU.readCommunicationsFromTar <$> (BS.readFile f)
      ".tgz" -> CU.readCommunicationsFromTar <$> ((liftM GZip.decompress . BS.readFile) f)
      ".tbz2" -> CU.readCommunicationsFromTar <$> ((liftM BZip.decompress . BS.readFile) f)
      ".zip" -> return $ CU.readCommunicationsFromZip f
      ".bz2" -> CU.readCommunicationsFromBytes <$> ((liftM BZip.decompress . BS.readFile) f)
      ".gz" -> CU.readCommunicationsFromBytes <$> ((liftM GZip.decompress . BS.readFile) f)
      _ -> CU.readCommunicationsFromBytes <$> (BS.readFile f)
    Nothing -> CU.readCommunicationsFromBytes <$> (BS.hGetContents stdin)
  putStr $ ((T.unpack . T.concat) $ [CU.showCommunication (head cs)])
  where
    opts = info (helper <*> parameters)
           ( fullDesc
             <> progDesc "Inspect a tar file of serialized Communications"
           )
