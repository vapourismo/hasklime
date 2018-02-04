module HaskLime.Exports (free, freeStablePtr) where

import Foreign

foreign export ccall "freePtr"
    free :: Ptr a -> IO ()

foreign export ccall "freeStablePtr"
    freeStablePtr :: StablePtr a -> IO ()
