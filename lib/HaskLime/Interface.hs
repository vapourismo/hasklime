{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Tagged

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

    -- | Name of the wrapper on foreign side
    foreignWrapperName :: Tagged a ByteString.ByteString

instance Interface () where
    foreignWrapperName = Tagged "Unit"

instance Interface (Ptr a) where
    foreignWrapperName = Tagged "Pointer"

instance Interface CBool where
    foreignWrapperName = Tagged "Bool"

-- instance Interface CClock

instance Interface CDouble where
    foreignWrapperName = Tagged "Double"

-- instance Interface CFile

instance Interface CFloat where
    foreignWrapperName = Tagged "Float"

-- instance Interface CFpos

instance Interface CInt where
    foreignWrapperName = Tagged "Int"

-- instance Interface CIntMax

-- instance Interface CIntPtr

-- instance Interface CJmpBuf

instance Interface CLLong where
    foreignWrapperName = Tagged "LongLong"

instance Interface CLong where
    foreignWrapperName = Tagged "Long"

-- instance Interface CPtrdiff

-- instance Interface CSChar

instance Interface CShort where
    foreignWrapperName = Tagged "Short"

-- instance Interface CSigAtomic

instance Interface CSize where
    foreignWrapperName = Tagged "Size"

-- instance Interface CSUSeconds

-- instance Interface CTime

-- instance Interface CUChar

instance Interface CUInt where
    foreignWrapperName = Tagged "UInt"

-- instance Interface CUIntMax

-- instance Interface CUIntPtr

instance Interface CULLong where
    foreignWrapperName = Tagged "ULongLong"

instance Interface CULong where
    foreignWrapperName = Tagged "ULong"

-- instance Interface CUSeconds

instance Interface CUShort where
    foreignWrapperName = Tagged "UShort"

-- instance Interface CWchar

instance Interface Bool where
    type CType Bool = CBool

    toC False = pure (CBool 0)
    toC True  = pure (CBool 1)

    fromC (CBool 0) = pure False
    fromC _         = pure True

    foreignWrapperName = Tagged "Bool"

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

    foreignWrapperName = Tagged "ByteString"

-- | Transported as JSON-encoded C string
newtype JSON a = JSON {fromJSON :: a}

instance (ToJSON a, FromJSON a) => Interface (JSON a) where
    type CType (JSON a) = CString

    fromC string =
        maybe (error "Failed to parse") JSON . decodeStrict' <$> fromC string

    toC = toC . ByteString.toStrict . encode . fromJSON

    foreignWrapperName = Tagged "JSON"

-- | Transportered as stable pointer
newtype Ref a = Ref {deRef :: a}

instance Interface (Ref a) where
    type CType (Ref a) = StablePtr a

    fromC x = Ref <$> deRefStablePtr x

    toC (Ref x) = newStablePtr x

    foreignWrapperName = Tagged "Ref"
