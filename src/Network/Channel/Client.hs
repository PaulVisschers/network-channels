module Network.Channel.Client (
  Channel,
  connect,
  close,
  send,
  receive
  ) where

import System.IO
import Network
import Control.Applicative

data Channel a b = Channel Handle

connect :: HostName -> PortNumber -> IO (Channel a b)
connect hostName portNumber = do
  h <- connectTo hostName (PortNumber portNumber)
  hSetBuffering h LineBuffering
  return (Channel h)

close :: Channel a b -> IO ()
close (Channel h) = hClose h

send :: Show b => b -> Channel a b -> IO ()
send x (Channel h) = hPutStr h (show x ++ "\n")

receive :: Read a => Channel a b -> IO [a]
receive (Channel h) = receive' h where

  receive' :: Read a => Handle -> IO [a]
  receive' h = do
    b <- hReady h
    if b then do
      x <- read <$> hGetLine h
      xs <- receive' h
      return (x : xs)
    else do
      return []