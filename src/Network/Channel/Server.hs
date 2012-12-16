{-# LANGUAGE TupleSections, DoAndIfThenElse #-}
module Network.Channel.Server (
  Channel,
  new,
  close,
  send,
  broadcast,
  receive,
  receiveAll,
  ) where

import System.IO
import Network
import Control.Concurrent
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Network.Channel.Raw as Raw

newtype Channel k a b = Channel (MVar (Bool, k, Map.Map k Handle))

new :: (Enum k, Ord k) => PortNumber -> IO (Channel k a b)
new n = withSocketsDo $ do
  mvar <- newEmptyMVar
  let ch = Channel mvar
  socket <- listenOn (PortNumber n)
  forkIO (acceptConnections socket ch)
  putMVar mvar (True, toEnum 0, Map.empty)
  return ch

acceptConnections :: (Enum k, Ord k) => Socket -> Channel k a b -> IO ()
acceptConnections socket (Channel mvar) = do
  (h, _, _) <- accept socket
  (open, next, hs) <- takeMVar mvar
  if open then do
    hSetBuffering h LineBuffering
    putMVar mvar (open, succ next, Map.insert next h hs)
    acceptConnections socket (Channel mvar)
  else do
    hClose h
    sClose socket
    putMVar mvar (open, next, hs)

close :: Enum k => Channel k a b -> IO ()
close (Channel mvar) = do
  (open, next, hs) <- takeMVar mvar
  mapM_ hClose (Map.elems hs)
  putMVar mvar (False, toEnum 0, Map.empty)

send :: (Ord k, Show b) => k -> b -> Channel k a b -> IO ()
send k x = atomically $ \hs -> Raw.send x (hs Map.! k)

broadcast :: Show b => b -> Channel k a b -> IO ()
broadcast x = atomically $ \hs -> mapM_ (Raw.send x) (Map.elems hs)

receive :: (Ord k, Read a) => k -> Channel k a b -> IO [a]
receive k = atomically $ \hs -> Raw.receive (hs Map.! k)

receiveAll :: (Ord k, Read a) => Channel k a b -> IO (Map k [a])
receiveAll = atomically $ \hs -> Map.fromList <$> mapM (\(k, h) -> (k,) <$> Raw.receive h) (Map.toList hs)
-- TODO: Add detection of closed handles, and remove them from the handle map.

atomically :: (Map k Handle -> IO c) -> Channel k a b -> IO c
atomically f (Channel mvar) = do
  (open, next, hs) <- takeMVar mvar
  x <- f hs
  putMVar mvar (open, next, hs)
  return x