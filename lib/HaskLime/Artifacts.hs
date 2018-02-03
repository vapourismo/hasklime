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

wrapError :: Text.Text -> IO (JSON a)
wrapError message =
    castJSON <$> toJSON (Aeson.Object (HashMap.singleton "error" (Aeson.toJSON message)))

wrapException :: SomeException -> IO (JSON a)
wrapException (SomeException e) =
    wrapError (Text.pack (show e))

type Activate e i = JSON i -> IO (StablePtr e)

toActivate :: FromJSON i => (i -> IO e) -> Activate e i
toActivate activate requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            pure (castPtrToStablePtr nullPtr)

        Just request -> do
            env <- activate request
            newStablePtr env

toActivate' :: IO e -> Activate e Void
toActivate' activate _ = do
    env <- activate
    newStablePtr env

type Method e i o = StablePtr e -> JSON i -> IO (JSON o)

toMethod :: (FromJSON i, ToJSON o) => (e -> i -> IO o) -> Method e i o
toMethod impl envPtr requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            wrapError "Failed to parse request"

        Just request -> do
            env <- deRefStablePtr envPtr
            catch (impl env request >>= toJSON) wrapException

toMethod' :: FromJSON i => (e -> i -> IO ()) -> Method e i ()
toMethod' impl envPtr requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            wrapError "Failed to parse request"

        Just request -> do
            env <- deRefStablePtr envPtr
            catch (JSON nullPtr <$ impl env request) wrapException

type Property e o = StablePtr e -> IO (JSON o)

toProperty :: ToJSON o => (e -> IO o) -> Property e o
toProperty impl envPtr = do
    env <- deRefStablePtr envPtr
    catch (impl env >>= toJSON) wrapException

toProperty' :: (e -> IO ()) -> Property e ()
toProperty' impl envPtr = do
    env <- deRefStablePtr envPtr
    catch (JSON nullPtr <$ impl env) wrapException

type Function i o = JSON i -> IO (JSON o)

toFunction :: (FromJSON i, ToJSON o) => (i -> IO o) -> Function i o
toFunction impl requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            wrapError "Failed to parse request"

        Just request ->
            catch (impl request >>= toJSON) wrapException

toFunction' :: FromJSON i => (i -> IO ()) -> Function i ()
toFunction' impl requestJson =
    fromJSON requestJson >>= \case
        Nothing ->
            wrapError "Failed to parse request"

        Just request ->
            catch (JSON nullPtr <$ impl request) wrapException
