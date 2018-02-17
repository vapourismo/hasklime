{-# LANGUAGE TemplateHaskell #-}

module Example where

import HaskLime.Interface
import HaskLime.TH

create :: JSON Int -> IO (Ref Int)
create (JSON x) = pure (Ref x)

dump :: Ref Int -> IO (JSON Int)
dump (Ref x) = pure (JSON x)

increment :: Ref Int -> IO (Ref Int)
increment (Ref x) = pure (Ref (x + 1))

$(export 'create)
$(export 'dump)
$(export 'increment)
