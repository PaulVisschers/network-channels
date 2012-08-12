{-# LANGUAGE DoAndIfThenElse #-}
module Network.Channel.Raw where

import System.IO
import Control.Applicative

send :: Show a => a -> Handle -> IO ()
send x h = hPutStr h (show x ++ "\n")

receiveOne :: Read a => Handle -> IO a
receiveOne h = read <$> hGetLine h

receive :: Read a => Handle -> IO [a]
receive h = do
    b <- hReady h
    if b then do
      x <- receiveOne h
      xs <- receive h
      return (x : xs)
    else
      return []