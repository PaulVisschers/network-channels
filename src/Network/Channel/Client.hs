module Network.Channel.Client where

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

receive :: Read a => Channel a b -> IO a
receive (Channel h) = read <$> hGetLine h

isEmpty :: Channel a b -> IO Bool
isEmpty (Channel h) = not <$> hReady h

receiveAll :: Read a => Channel a b -> IO [a]
receiveAll ch = do
  b <- isEmpty ch
  if b then do
    return []
  else do
    x <- receive ch
    xs <- receiveAll ch
    return (x : xs)