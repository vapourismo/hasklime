{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module HaskLime.Interface (
    -- * Interface
    Interface (..),

    -- * Interface types
    JSON (..),
    Ref (..)
) where

import           Data.Aeson

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as ByteString (toStrict)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)

import           Foreign
import           Foreign.C

-- | A type which can be convert to and from something C-like.
class Interface a where
    type CType a

    fromC :: CType a -> IO a

    toC :: a -> IO (CType a)

instance Interface () where
    type CType () = ()

    fromC = pure

    toC = pure

-- | Transported as JSON-encoded C string
newtype JSON a = JSON {fromJSON :: a}

instance (ToJSON a, FromJSON a) => Interface (JSON a) where
    type CType (JSON a) = CString

    fromC string
        | string == nullPtr = error "Given string is a null pointer"
        | otherwise         = decode <$> ByteString.packCString string
        where
            decode = maybe (error "Failed to parse") JSON . decodeStrict'

    toC (JSON value) =
        ByteString.unsafeUseAsCString strictValue $ \ string -> do
            copy <- mallocArray0 valueLength
            copyArray copy string valueLength
            copy <$ pokeElemOff copy valueLength 0
        where
            strictValue = ByteString.toStrict (encode value)
            valueLength = ByteString.length strictValue

-- | Transportered as stable pointer
newtype Ref a = Ref {deRef :: a}

instance Interface (Ref a) where
    type CType (Ref a) = StablePtr a

    fromC x = Ref <$> deRefStablePtr x

    toC (Ref x) = newStablePtr x
