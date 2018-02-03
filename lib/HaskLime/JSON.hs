module HaskLime.JSON (
    JSON (..),
    fromJSON,
    toJSON,

    castJSON,

    FromJSON,
    ToJSON
) where

import           Data.Aeson             hiding (fromJSON, toJSON)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as ByteStringLazy
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)

import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable

-- | JSON-encoded strings
newtype JSON a = JSON CString

-- | Parse JSON contained within the given 'CString'.
fromJSON :: FromJSON a => JSON a -> IO (Maybe a)
fromJSON (JSON string)
    | string == nullPtr = pure Nothing
    | otherwise         = decodeStrict' <$> ByteString.packCString string

-- | Generate a 'CString' contains the JSON-representation of the given value. The 'CString' needs
-- to be 'free'd manually.
toJSON :: ToJSON a => a -> IO (JSON a)
toJSON value = do
    ByteString.unsafeUseAsCString strictValue $ \ string -> do
        copy <- mallocArray0 valueLength
        copyArray copy string valueLength
        JSON copy <$ pokeElemOff copy valueLength 0
    where
        strictValue =
            ByteStringLazy.toStrict (encode value)

        valueLength =
            ByteString.length strictValue

-- | Cast JSON-encoded value.
castJSON :: JSON a -> JSON b
castJSON (JSON a) = JSON a
