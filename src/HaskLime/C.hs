module HaskLime.C (
    CActivate,
    activate
) where

import           Control.Monad

import qualified Data.ByteString as ByteString

import           Foreign.C
import           Foreign.Ptr

foreign import ccall "dynamic"
    fromSenderFunPtr :: CSenderPtr -> CSender

foreign import ccall "wrapper"
    toSenderFunPtr :: CSender -> IO CSenderPtr

-- | Sender function
type CSender = CString -> IO ()

-- | Pointer to 'CSender'
type CSenderPtr = FunPtr CSender

-- | Activation function
type CActivate = FunPtr CSender -> IO (FunPtr CSender)

-- | Turn 'CSenderPtr' into a more Haskell-friendly function.
fromSenderPtr :: CSenderPtr -> (ByteString.ByteString -> IO ())
fromSenderPtr ptrSend message =
    ByteString.useAsCString message sendCString
    where
        sendCString = fromSenderFunPtr ptrSend

-- | Turn a Haskell-friendly function into a 'CSenderPtr'.
toSenderPtr :: (ByteString.ByteString -> IO ()) -> IO CSenderPtr
toSenderPtr send =
    toSenderFunPtr (ByteString.packCString >=> send)

-- | Generate the activation function that will later be called from Python. The callback passed to
-- 'activate' receives a function to send messages to Python and shall return a function that
-- handles messages from Python.
activate :: ((ByteString.ByteString -> IO ()) -> IO (ByteString.ByteString -> IO ())) -> CActivate
activate init ptrSend =
    init (fromSenderPtr ptrSend) >>= toSenderPtr
