module Network.Channel where

import GHC.IO.Handle
import Network
import Control.Concurrent.MVar
import Control.Applicative

data Channel a b = Channel (MVar Handle) (Maybe Socket)

connectTo :: HostName -> PortNumber -> IO (Channel a b)
connectTo hostName portNumber = do
  handle <- Network.connectTo hostName (PortNumber portNumber)
  hSetBuffering handle LineBuffering
  hvar <- newMVar handle
  return (Channel hvar Nothing)

acceptOn :: PortNumber -> IO (Channel a b)
acceptOn portNumber = withSocketsDo $ do
  socket <- listenOn (PortNumber portNumber)
  (handle, _, _) <- accept socket
  hSetBuffering handle LineBuffering
  hvar <- newMVar handle
  return (Channel hvar (Just socket))

close :: Channel a b -> IO ()
close (Channel hvar msocket) = do
  handle <- takeMVar hvar
  hClose handle
  maybe (return ()) sClose msocket

send :: Show a => a -> Channel a b -> IO ()
send x = withChannel $ \h -> hPutStr h (show x ++ "\n")

receive :: Read b => Channel a b -> IO b
receive = withChannel $ \h -> read <$> hGetLine h

isEmpty :: Channel a b -> IO Bool
isEmpty = withChannel $ \h -> hWaitForInput h 0

tryReceive :: Read b => Channel a b -> IO (Maybe b)
tryReceive = withChannel $ \h -> do
  b <- hWaitForInput h 0
  if b
    then Just . read <$> hGetLine h
    else return Nothing
  
withChannel :: (Handle -> IO c) -> Channel a b -> IO c
withChannel f (Channel hvar _) = do
  h <- takeMVar hvar
  x <- f h
  putMVar hvar h
  return x