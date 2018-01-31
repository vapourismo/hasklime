{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskLime.Plugin (
    -- * Plugin monad
    PluginM,
    send,

    -- * Actual plugin
    Plugin (..),
) where

import           GHC.Generics

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Data.Aeson
import qualified Data.ByteString.Lazy       as ByteStringLazy
import           Data.Default

import           HaskLime.C
import           HaskLime.Thread

-- | Plugin monad
newtype PluginM o a =
    PluginM (ReaderT (o -> IO ()) IO a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadFail, MonadIO)

-- | Run action.
runPluginM :: (o -> IO ()) -> PluginM o a -> IO a
runPluginM send (PluginM reader) =
    runReaderT reader send

-- | Send a message.
send :: o -> PluginM o ()
send message = PluginM (ReaderT ($ message))

-- | Incoming message
data Message a
    = Kill
    | Join
    | Message { messageContents :: a }
    deriving (Show, Eq, Ord, Generic)

instance FromJSON a => FromJSON (Message a)

-- | Plugin description
data Plugin i o =
    Plugin
        { pluginActivation   :: PluginM o ()
        , pluginDeactivation :: PluginM o ()
        , pluginMessage      :: i -> PluginM o ()
        , pluginError        :: String -> PluginM o () }

instance Default (Plugin i o) where
    def =
        Plugin
            { pluginActivation = pure ()
            , pluginDeactivation = pure ()
            , pluginMessage = pure (pure ())
            , pluginError = pure (pure ()) }

instance (FromJSON i, ToJSON o) => Activation (Plugin i o) where
    toActivate = toActivate . activateWithSender

-- | Generate haskell activation function for the given 'Plugin' description.
activateWithSender :: (FromJSON i, ToJSON o) => Plugin i o -> HaskellActivate
activateWithSender plugin sendByteString =
    handleByteString plugin <$> spawn (runAction (pluginActivation plugin))
    where
        sender = sendByteString . ByteStringLazy.toStrict . encode

        runAction = runPluginM sender

        handleByteString plugin thread message =
            case eitherDecodeStrict message of
                Left error    -> runAction (pluginError plugin error)
                Right Join    -> kill thread >> runAction (pluginDeactivation plugin)
                Right Kill    -> await thread >> runAction (pluginDeactivation plugin)
                Right message -> runAction (pluginMessage plugin (messageContents message))
