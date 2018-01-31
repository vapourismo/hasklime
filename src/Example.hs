module Example (haskLimeActivate) where

import HaskLime.C
import HaskLime.Plugin

foreign export ccall "haskLimeActivate"
    haskLimeActivate :: Activate

-- | Activation function which will be called from Python.
haskLimeActivate :: Activate
haskLimeActivate =
    toActivate examplePlugin

-- | Example plugin
examplePlugin :: Plugin String String
examplePlugin =
    Plugin
        { pluginActivation   = send "Hello"
        , pluginDeactivation = send "Bye"
        , pluginMessage      = onMessage
        , pluginError        = onError }
    where
        onMessage message = send ("Message: " ++ message)

        onError error = send ("Error: " ++ error)
