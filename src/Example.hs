module Example (haskLimeActivate) where

import HaskLime.Plugin

foreign export ccall "haskLimeActivate"
    haskLimeActivate :: CActivate

-- | Activation function which will be called from Python.
haskLimeActivate :: CActivate
haskLimeActivate =
    activatePlugin $ \ send ->
        Plugin
            { pluginActivation   = send "Hello"
            , pluginDeactivation = send "Bye"
            , pluginMessage      = onMessage send
            , pluginError        = onError send }
    where
        onMessage send message = send ("Message: " ++ message)

        onError send error = send ("Error: " ++ error)
