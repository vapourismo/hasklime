{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskLime.Artifacts (
    Method,
    toMethod,
    toMethod',

    Property,
    toProperty,
    toProperty',

    Function,
    toFunction,
    toFunction',

    Activate,
    toActivate,
    toActivate'
) where

import           Control.Exception

import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import           Data.Void

import           Foreign.Ptr
import           Foreign.StablePtr

import           HaskLime.JSON

-- | Create a JSON object representing an error.
mkErrorObject :: Text.Text -> IO (JSON a)
mkErrorObject message =
    castJSON <$> toJSON (Aeson.Object (HashMap.singleton "error" (Aeson.toJSON message)))

-- | Create a JSON object representing an exception.
mkExceptionObject :: SomeException -> IO (JSON a)
mkExceptionObject (SomeException exception) =
    mkErrorObject (Text.pack (show exception))

-- | Activation function that takes a request parameter @req@ and produces an environment @env@
type Activate req env = JSON req -> IO (StablePtr env)

-- | Convert to an activation function.
toActivate :: FromJSON req => (req -> IO env) -> Activate req env
toActivate activate requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            pure (castPtrToStablePtr nullPtr)

        Just request -> do
            env <- activate request
            newStablePtr env

-- | Convert to an activiation function that does not use its request parameter.
toActivate' :: IO env -> Activate Void env
toActivate' activate _ = do
    env <- activate
    newStablePtr env

-- | Function that takes an environment @env@, a parameter @req@ and produces an response @res@
type Method env req res = StablePtr env -> JSON req -> IO (JSON res)

-- | Convert to a method.
toMethod :: (FromJSON req, ToJSON res) => (env -> req -> IO res) -> Method env req res
toMethod impl envPtr requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            mkErrorObject "Failed to parse request"

        Just request -> do
            env <- deRefStablePtr envPtr
            catch (impl env request >>= toJSON) mkExceptionObject

-- | Convert to a method that does not produce a response.
toMethod' :: FromJSON req => (env -> req -> IO ()) -> Method env req ()
toMethod' impl envPtr requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            mkErrorObject "Failed to parse request"

        Just request -> do
            env <- deRefStablePtr envPtr
            catch (JSON nullPtr <$ impl env request) mkExceptionObject

-- | Function that takes an environment @res@ and gives a response @res@
type Property env res = StablePtr env -> IO (JSON res)

-- | Convert to a property.
toProperty :: ToJSON res => (env -> IO res) -> Property env res
toProperty impl envPtr = do
    env <- deRefStablePtr envPtr
    catch (impl env >>= toJSON) mkExceptionObject

-- | Convert to a property that does not produce a response.
toProperty' :: (env -> IO ()) -> Property env ()
toProperty' impl envPtr = do
    env <- deRefStablePtr envPtr
    catch (JSON nullPtr <$ impl env) mkExceptionObject

-- | Function takes a parameter @req@ and produces a response @res@
type Function req res = JSON req -> IO (JSON res)

-- | Convert to a function.
toFunction :: (FromJSON req, ToJSON res) => (req -> IO res) -> Function req res
toFunction impl requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            mkErrorObject "Failed to parse request"

        Just request ->
            catch (impl request >>= toJSON) mkExceptionObject

-- | Convert to a function that does not produce a response.
toFunction' :: FromJSON req => (req -> IO ()) -> Function req ()
toFunction' impl requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            mkErrorObject "Failed to parse request"

        Just request ->
            catch (JSON nullPtr <$ impl request) mkExceptionObject
