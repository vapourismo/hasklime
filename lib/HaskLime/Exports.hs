{-# LANGUAGE RankNTypes #-}

module HaskLime.Exports (
    -- * Responses
    FreeResponse,
    freeResponse,

    -- * Environments
    FreeEnvironment,
    freeEnvironment,
) where

import Foreign.Marshal
import Foreign.Ptr
import Foreign.StablePtr

-- | Helper for freeing responses
type FreeResponse = forall a. Ptr a -> IO ()

-- | Helper for freeing responses
freeResponse :: FreeResponse
freeResponse = free

-- | Helper for freeing environments
type FreeEnvironment = forall a. StablePtr a -> IO ()

-- | Helper for freeing environments
freeEnvironment :: FreeEnvironment
freeEnvironment = freeStablePtr
