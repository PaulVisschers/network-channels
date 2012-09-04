{-# LANGUAGE FlexibleContexts #-}
module Network.Channel.Client.Trans (
  Channel,
  connect,
  close,
  withChannel,
  send,
  receive
  )where

import qualified Network.Channel.Client as Chan
import Network.Channel.Client (Channel)
import Control.Monad.Reader
import Network (HostName, PortNumber)

connect :: MonadIO m => HostName -> PortNumber -> m (Channel a b)
connect name num = liftIO $ Chan.connect name num

close :: MonadIO m => Channel a b -> m ()
close = liftIO . Chan.close

withChannel :: MonadIO m => Channel a b -> ReaderT (Channel a b) m c -> m c
withChannel ch rt = runReaderT rt ch

send :: (MonadIO m, MonadReader (Channel a b) m, Show b) => b -> m ()
send x = ask >>= liftIO . Chan.send x

receive :: (MonadIO m, MonadReader (Channel a b) m, Read a) => m [a]
receive = ask >>= liftIO . Chan.receive