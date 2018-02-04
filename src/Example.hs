{-# LANGUAGE TemplateHaskell #-}

module Example where

import HaskLime.Interface
import HaskLime.TH

testDelete :: Ref Int -> IO ()
testDelete (Ref e) = putStrLn ("Bye " ++ show e)

$(export_ 'testDelete)
