module HaskLime.Thread (
    Thread,
    spawn,
    await,
    kill
) where

import Control.Concurrent
import Control.Exception

-- | Thread
data Thread a =
    Thread
        { threadId     :: ThreadId
        , threadJoiner :: MVar (Either SomeException a) }

-- | Spawn a 'Thread' using the given 'IO' operation.
spawn :: IO a -> IO (Thread a)
spawn action = do
    joinerVar <- newEmptyMVar
    threadId <- forkFinally action (putMVar joinerVar)
    pure (Thread threadId joinerVar)

-- | Wait for the 'Thread' to terminate, then retrieve its result.
await :: Thread a -> IO (Either SomeException a)
await thread =
    readMVar (threadJoiner thread)

-- | Kill the 'Thread'. It might have terminated already; in that case 'Right' is returned.
kill :: Thread a -> IO (Either SomeException a)
kill thread = do
    killThread (threadId thread)
    await thread
