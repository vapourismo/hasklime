module Example where

import HaskLime

foreign export ccall "testActivate"
    testActivate :: Activate Int Int

testActivate :: Activate Int Int
testActivate = toActivate pure

foreign export ccall "testMethod"
    testMethod :: Method Int Int Int

testMethod :: Method Int Int Int
testMethod = toMethod (\ e i -> pure (e + i))

foreign export ccall "testProperty"
    testProperty :: Property Int Int

testProperty :: Property Int Int
testProperty = toProperty pure

foreign export ccall "testFunction"
    testFunction :: Function Int Int

testFunction :: Function Int Int
testFunction = toFunction (\ i -> pure (i * 2))

foreign export ccall "testDelete"
    testDelete :: Property Int ()

testDelete :: Property Int ()
testDelete =
    toProperty' (\ e -> putStrLn ("Bye " ++ show e))
