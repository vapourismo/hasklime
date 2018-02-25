{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module HaskLime.Interface (
    -- * Interface
    Interface (..),

    -- * Interface types
    JSON (..),
    Ref (..)
) where

import           Data.Aeson             hiding (fromJSON)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as ByteString (toStrict)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)

import           Foreign
import           Foreign.C

-- | Functions for converting a type to and from its C-representation
class Interface a where
    type CType a
    type CType a = a

    -- | Cast from C type
    fromC :: CType a -> IO a

    default fromC :: a ~ CType a => CType a -> IO a
    fromC = pure

    -- | Cast to C type
    toC :: a -> IO (CType a)

    default toC :: a ~ CType a => a -> IO (CType a)
    toC = pure

instance Interface () where

instance Interface (Ptr a)

instance Interface CBool

instance Interface CClock

instance Interface CDouble

instance Interface CFile

instance Interface CFloat

instance Interface CFpos

instance Interface CInt

instance Interface CIntMax

instance Interface CIntPtr

instance Interface CJmpBuf

instance Interface CLLong

instance Interface CLong

instance Interface CPtrdiff

instance Interface CSChar

instance Interface CShort

instance Interface CSigAtomic

instance Interface CSize

instance Interface CSUSeconds

instance Interface CTime

instance Interface CUChar

instance Interface CUInt

instance Interface CUIntMax

instance Interface CUIntPtr

instance Interface CULLong

instance Interface CULong

instance Interface CUSeconds

instance Interface CUShort

instance Interface CWchar

instance Interface Bool

instance Interface ByteString.ByteString where
    type CType ByteString.ByteString = CString

    fromC string
        | string == nullPtr = pure ByteString.empty
        | otherwise         = ByteString.packCString string

    toC value =
        ByteString.unsafeUseAsCString value $ \ string -> do
            copy <- mallocArray0 valueLength
            copyArray copy string valueLength
            copy <$ pokeElemOff copy valueLength 0
        where
            valueLength = ByteString.length value

-- | Transported as JSON-encoded C string
newtype JSON a = JSON {fromJSON :: a}

instance (ToJSON a, FromJSON a) => Interface (JSON a) where
    type CType (JSON a) = CString

    fromC string =
        maybe (error "Failed to parse") JSON . decodeStrict' <$> fromC string

    toC = toC . ByteString.toStrict . encode . fromJSON

-- | Transportered as stable pointer
newtype Ref a = Ref {deRef :: a}

instance Interface (Ref a) where
    type CType (Ref a) = StablePtr a

    fromC x = Ref <$> deRefStablePtr x

    toC (Ref x) = newStablePtr x
