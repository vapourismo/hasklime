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

import Foreign.StablePtr
import HaskLime.JSON

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
    type CType (JSON a) = CJSON a

    fromC json = maybe (error "Failed to parse") JSON <$> fromCJSON json

    toC (JSON value) = toCJSON value

-- | Transportered as stable pointer
newtype Ref a = Ref {deRef :: a}

instance Interface (Ref a) where
    type CType (Ref a) = StablePtr a

    fromC x = Ref <$> deRefStablePtr x

    toC (Ref x) = newStablePtr x
