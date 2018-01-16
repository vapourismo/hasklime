{-# LANGUAGE DeriveGeneric #-}

module HaskLime.Plugin (
    Sender,
    Plugin (..),
    activatePlugin
) where

import           GHC.Generics

import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteStringLazy

import           HaskLime.C
import           HaskLime.Thread

-- | Incoming message
data Message a
    = Kill
    | Join
    | Message { messageContents :: a }
    deriving (Show, Eq, Ord, Generic)

instance FromJSON a => FromJSON (Message a)

-- | Plugin description
data Plugin a =
    Plugin
        { pluginActivation   :: IO ()
        , pluginDeactivation :: IO ()
        , pluginMessage      :: a -> IO ()
        , pluginError        :: String -> IO () }

-- | Sender function that transmits to Python
type Sender a = a -> IO ()

-- | Activate a 'Plugin'.
activatePlugin :: (FromJSON i, ToJSON o) => (Sender o -> Plugin i) -> CActivate
activatePlugin mkPlugin =
    activate $ \ sendByteString -> do
        let plugin = mkPlugin (sendByteString . ByteStringLazy.toStrict . encode)
        handleByteString plugin <$> spawn (pluginActivation plugin)
    where
        handleByteString plugin thread message =
            case eitherDecodeStrict message of
                Left error    -> pluginError plugin error
                Right Join    -> kill thread >> pluginDeactivation plugin
                Right Kill    -> await thread >> pluginDeactivation plugin
                Right message -> pluginMessage plugin (messageContents message)
