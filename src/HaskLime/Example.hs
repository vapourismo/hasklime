{-# LANGUAGE OverloadedStrings #-}

module HaskLime.Example where

import qualified Data.ByteString as ByteString

import           Foreign.C
import           Foreign.Ptr

foreign import ccall "dynamic"
    fromSenderFunPtr :: FunPtr CSender -> CSender

foreign import ccall "wrapper"
    toSenderFunPtr :: CSender -> IO (FunPtr CSender)

foreign export ccall "haskLimeInit"
    haskLimeInit :: CInitializer

type CSender = CString -> IO ()

type CInitializer = FunPtr CSender -> IO (FunPtr CSender)

haskLimeInit :: CInitializer
haskLimeInit =
    initPlugin $
        Plugin
            { pluginHandler    = \ _ _ -> pure ()
            , pluginActivate   = \ _ -> pure ()
            , pluginDeactivate = \ _ -> pure () }

type Sender = ByteString.ByteString -> IO ()

data Plugin =
    Plugin
        { pluginHandler    :: Sender -> ByteString.ByteString -> IO ()
        , pluginActivate   :: Sender -> IO ()
        , pluginDeactivate :: Sender -> IO () }

initPlugin :: Plugin -> CInitializer
initPlugin plugin ptrSend = do
    toSenderFunPtr $ \ cStr -> do
        str <- ByteString.packCString cStr
        pluginHandler plugin send str
    where
        sendCString = fromSenderFunPtr ptrSend
        send msg    = ByteString.useAsCString msg sendCString
