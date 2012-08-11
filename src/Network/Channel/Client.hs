module Network.Channel.Client where

import System.IO
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
receive ch = do
  mx <- tryReceive ch
  case mx of
    Nothing -> receive ch
    Just x -> return x

isEmpty :: Channel a b -> IO Bool
isEmpty = withChannel $ \h -> not <$> hReady h

tryReceive :: Read a => Channel a b -> IO (Maybe a)
tryReceive = withChannel $ \h -> do
  b <- hReady h
  if b
    then Just . read <$> hGetLine h
    else return Nothing
  
withChannel :: (Handle -> IO c) -> Channel a b -> IO c
withChannel f (Channel hvar) = do
  h <- takeMVar hvar
  x <- f h
  putMVar hvar h
  return x