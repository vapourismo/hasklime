{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.ByteString as ByteString

import           HaskLime

f :: Bool -> IO Bool
f = pure . not

g :: ByteString.ByteString -> IO ByteString.ByteString
g = pure . ByteString.reverse

$(export 'f)
$(export 'g)
