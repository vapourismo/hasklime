module Example where

import Foreign.C
import HaskLime.JSON

foreign export ccall "test"
    test :: JSON [Int] -> IO (JSON [Int])

newtype Test = Test CInt

test :: JSON [Int] -> IO (JSON [Int])
test inputJson = do
    mbInput <- fromJSON inputJson
    toJSON (maybe [] (map (+ 2)) mbInput)
