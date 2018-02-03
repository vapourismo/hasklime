-- Do not touch this file, please.

module ImportantExports () where

import HaskLime.Exports

-- Do not remove!
foreign export ccall "freeResponse"
    freeResponse :: FreeResponse

-- Do not remove!
foreign export ccall "freeEnvironment"
    freeEnvironment :: FreeEnvironment
