module HaskLime.JSON (
    CJSON (..),
    fromCJSON,
    toCJSON,

    FromJSON,
    ToJSON
) where

import           Data.Aeson

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as ByteStringLazy
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)

import           Foreign
import           Foreign.C

-- | JSON-encoded strings
newtype CJSON a = CJSON CString

-- | Parse JSON contained within the given 'CString'.
fromCJSON :: FromJSON a => CJSON a -> IO (Maybe a)
fromCJSON (CJSON string)
    | string == nullPtr = pure Nothing
    | otherwise         = decodeStrict' <$> ByteString.packCString string

-- | Generate a 'CString' contains the JSON-representation of the given value. The 'CString' needs
-- to be 'free'd manually.
toCJSON :: ToJSON a => a -> IO (CJSON a)
toCJSON value =
    ByteString.unsafeUseAsCString strictValue $ \ string -> do
        copy <- mallocArray0 valueLength
        copyArray copy string valueLength
        CJSON copy <$ pokeElemOff copy valueLength 0
    where
        strictValue =
            ByteStringLazy.toStrict (encode value)

        valueLength =
            ByteString.length strictValue
