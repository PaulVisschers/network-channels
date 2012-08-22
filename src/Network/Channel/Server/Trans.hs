{-# LANGUAGE FlexibleContexts #-}
module Network.Channel.Server.Trans (
  Channel,
  new,
  close,
  withChannel,
  send,
  broadcast,
  receive,
  receiveAll
  )where

import qualified Network.Channel.Server as Chan
import Network.Channel.Server (Channel)
import Data.Map (Map)
import Control.Monad.Reader
import Network (PortNumber)

new :: (Enum k, MonadIO m, Ord k) => PortNumber -> m (Channel k a b)
new = liftIO . Chan.new

close :: (Enum k, MonadIO m) => Channel k a b -> m ()
close = liftIO . Chan.close

withChannel :: (Enum k, MonadIO m, Ord k) => Channel k a b -> ReaderT (Channel k a b) m c -> m c
withChannel ch rt = runReaderT rt ch

send :: (MonadIO m, MonadReader (Channel k a b) m, Ord k, Show b) => k -> b -> m ()
send k x = ask >>= liftIO . Chan.send k x

broadcast :: (MonadIO m, MonadReader (Channel k a b) m, Show b) => b -> m ()
broadcast x = ask >>= liftIO . Chan.broadcast x

receive :: (MonadIO m, MonadReader (Channel k a b) m, Ord k, Read a) => k -> m [a]
receive k = ask >>= liftIO . Chan.receive k

receiveAll :: (MonadIO m, MonadReader (Channel k a b) m, Ord k, Read a) => m (Map k [a])
receiveAll = ask >>= liftIO . Chan.receiveAll