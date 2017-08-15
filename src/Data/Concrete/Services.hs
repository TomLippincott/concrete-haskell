{-|
Description: Scaffolding and implementations of Concrete services
-}

module Data.Concrete.Services ( runConcreteService
                              , connectToService
                              , Compression(..)
                              ) where

import Thrift.Transport.Handle hiding (HandleSource)
import Thrift.Transport.Framed (FramedTransport, openFramedTransport)
import Thrift.Protocol.Compact (CompactProtocol(..))
import Thrift.Server (runThreadedServer)
import System.IO (Handle)
import Network (PortID(..), accept)

-- | Compression methods supported by concrete-haskell: the appropriate
--   method is usually determined by file extension.
data Compression = BZip | GZip

-- | This function runs an arbitrary Thrift service, threaded, on the given port.
runConcreteService :: Int -> (h -> (CompactProtocol (FramedTransport Handle), CompactProtocol (FramedTransport Handle)) -> IO Bool) -> h -> IO ()
runConcreteService port processor handler = runThreadedServer acceptor handler processor port'
  where
    port' = (PortNumber ((read . show) port))
    acceptor s = do
      (h, _, _) <- accept s
      t <- openFramedTransport h
      return (CompactProtocol t, CompactProtocol t)

-- | This function connects as a client to an arbitrary Thrift service on the given host and port.
connectToService :: String -> Int -> IO (CompactProtocol (FramedTransport Handle), CompactProtocol (FramedTransport Handle))
connectToService h p = do
  transport <- hOpen (h, PortNumber $ fromIntegral p)
  transport' <- openFramedTransport transport
  let protocol = CompactProtocol transport'
  return (protocol, protocol)
