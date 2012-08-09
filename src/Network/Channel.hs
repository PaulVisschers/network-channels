module Network.Channel where

import GHC.IO.Handle
import Network
import Control.Applicative

data Channel a b = Channel Handle HostName PortNumber (Maybe Socket)

connectTo :: HostName -> PortNumber -> IO (Channel a b)
connectTo hostName portNumber = do
  handle <- Network.connectTo hostName (PortNumber portNumber)
  hSetBuffering handle LineBuffering
  return (Channel handle hostName portNumber Nothing)

acceptOn :: PortNumber -> IO (Channel a b)
acceptOn portNumber = withSocketsDo $ do
  socket <- listenOn (PortNumber portNumber)
  (handle, hostName, portNumber) <- accept socket
  hSetBuffering handle LineBuffering
  return (Channel handle hostName portNumber (Just socket))

close :: Channel a b -> IO ()
close (Channel _ _ _ msocket) = maybe (return ()) sClose msocket

send :: Show a => a -> Channel a b -> IO ()
send x (Channel handle _ _ _) = hPutStr handle (show x ++ "\n")

receive :: Read b => Channel a b -> IO b
receive (Channel handle _ _ _) = read <$> hGetLine handle

isEmpty :: Channel a b -> IO Bool
isEmpty (Channel handle _ _ _) = not <$> hWaitForInput handle 0