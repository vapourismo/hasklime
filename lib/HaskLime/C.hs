{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskLime.C (
    -- * Aliases for external interfaces
    Sender,
    SenderPtr,
    Activate,

    -- * Activation class
    Activation (..),

    -- * Aliases for haskell interfaces
    HaskellSender,
    HaskellActivate
) where

import           Control.Exception
import           Control.Monad

import qualified Data.ByteString   as ByteString

import           Foreign.C
import           Foreign.Ptr

foreign import ccall "dynamic"
    fromSenderFunPtr :: SenderPtr -> Sender

foreign import ccall "wrapper"
    toSenderFunPtr :: Sender -> IO SenderPtr

-- | Sender function
type Sender = CString -> IO ()

-- | Pointer to 'Sender'
type SenderPtr = FunPtr Sender

-- | Activation function
type Activate = SenderPtr -> IO SenderPtr

-- | Turn 'SenderPtr' into a more Haskell-friendly function.
fromSenderPtr :: SenderPtr -> HaskellSender
fromSenderPtr ptrSend message =
    ByteString.useAsCString message sendCString
    where
        !sendCString = fromSenderFunPtr ptrSend

-- | Turn a Haskell-friendly function into a 'SenderPtr'.
toSenderPtr :: HaskellSender -> IO SenderPtr
toSenderPtr send =
    toSenderFunPtr (ByteString.packCString >=> send)

-- | Something that can be activated from outside
class Activation a where
    -- | Generate the activation function.
    toActivate :: a -> Activate

instance Activation Activate where
    toActivate = id

-- | Haskell sender function
type HaskellSender =  ByteString.ByteString -> IO ()

-- | Haskell activation function
type HaskellActivate = HaskellSender -> IO HaskellSender

instance Activation HaskellActivate where
    toActivate init ptrSend = do
        sender <- evaluate (fromSenderPtr ptrSend)
        receiver <- init sender
        toSenderPtr receiver
