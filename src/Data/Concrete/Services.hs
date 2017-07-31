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

data Compression = BZip | GZip

runConcreteService :: Int -> h -> (h -> (CompactProtocol (FramedTransport Handle), CompactProtocol (FramedTransport Handle)) -> IO Bool) -> IO ()
runConcreteService port handler processor= runThreadedServer acceptor handler processor port'
  where
    port' = (PortNumber ((read . show) port))
    acceptor s = do
      (h, _, _) <- accept s
      t <- openFramedTransport h
      return (CompactProtocol t, CompactProtocol t)

connectToService :: String -> Int -> IO (CompactProtocol (FramedTransport Handle), CompactProtocol (FramedTransport Handle))
connectToService h p = do
  transport <- hOpen (h, PortNumber $ fromIntegral p)
  transport' <- openFramedTransport transport
  let protocol = CompactProtocol transport'
  return (protocol, protocol)
