module Network.Channel.Client (
  Channel,
  connect,
  close,
  send,
  receive
  ) where

import System.IO
import Network
import Control.Concurrent.MVar
import Control.Applicative

data Channel a b = Channel (MVar Handle)

connect :: HostName -> PortNumber -> IO (Channel a b)
connect hostName portNumber = do
  h <- connectTo hostName (PortNumber portNumber)
  hSetBuffering h LineBuffering
  mvar <- newMVar h
  return (Channel mvar)

close :: Channel a b -> IO ()
close = atomically hClose

send :: Show b => b -> Channel a b -> IO ()
send x = atomically $ \h -> hPutStr h (show x ++ "\n")

receive :: Read a => Channel a b -> IO [a]
receive = atomically $ \h -> receive' h where

  receive' :: Read a => Handle -> IO [a]
  receive' h = do
    b <- hReady h
    if b then do
      x <- read <$> hGetLine h
      xs <- receive' h
      return (x : xs)
    else do
      return []

atomically :: (Handle -> IO c) -> Channel a b -> IO c
atomically f (Channel mvar) = do
  h <- takeMVar mvar
  x <- f h
  putMVar mvar h
  return x