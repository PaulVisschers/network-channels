module Network.Channel.Client where

import GHC.IO.Handle
import Network
import Control.Concurrent.MVar
import Control.Applicative

data Channel a b = Channel (MVar Handle)

connect :: HostName -> PortNumber -> IO (Channel a b)
connect hostName portNumber = do
  handle <- connectTo hostName (PortNumber portNumber)
  hSetBuffering handle LineBuffering
  hvar <- newMVar handle
  return (Channel hvar)

close :: Channel a b -> IO ()
close = withChannel hClose

send :: Show b => b -> Channel a b -> IO ()
send x = withChannel $ \h -> hPutStr h (show x ++ "\n")

receive :: Read a => Channel a b -> IO a
receive = withChannel $ \h -> read <$> hGetLine h

isEmpty :: Channel a b -> IO Bool
isEmpty = withChannel $ \h -> not <$> hWaitForInput h 0

tryReceive :: Read a => Channel a b -> IO (Maybe a)
tryReceive = withChannel $ \h -> do
  b <- not <$> hWaitForInput h 0
  if b
    then return Nothing
    else Just . read <$> hGetLine h
  
withChannel :: (Handle -> IO c) -> Channel a b -> IO c
withChannel f (Channel hvar) = do
  h <- takeMVar hvar
  x <- f h
  putMVar hvar h
  return x