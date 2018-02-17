{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskLime.Procedure (
    IsProcedure,
    IsExportable,
    ExportProcedure,
    exportProcedure
) where

import Control.Applicative
import Control.Monad

import HaskLime.Interface

-- | Heterogeneous list
data Inputs ts where
    Nil  :: Inputs '[]
    Cons :: t -> Inputs ts -> Inputs (t : ts)

-- | Convert all types in the given list to the corresponding 'CType'.
type family CTypes ts where
    CTypes '[]      = '[]
    CTypes (t : ts) = CType t : CTypes ts

-- | Instance of 'Inputs' that takes the corresponding 'CType's as parameters.
type CInputs ts = Inputs (CTypes ts)

-- | Convert inputs from their corresponding 'CType' inputs.
class FromCInputs ts where
    fromCInputs :: CInputs ts -> IO (Inputs ts)

instance FromCInputs '[] where
    fromCInputs Nil = pure Nil

instance (Interface t, FromCInputs ts) => FromCInputs (t : ts) where
    fromCInputs (Cons x xs) = liftA2 Cons (fromC x) (fromCInputs xs)

-- | A procedure function that takes all its parameters as 'Inputs'
type Procedure ts r = Inputs ts -> IO r

-- | Isomorphicsm between @f@ andD @Procedure ps r@
class IsProcedure f ps r | ps r -> f, f -> ps r where
    toProcedure :: f -> Procedure ps r

    fromProcedure :: Procedure ps r -> f

instance IsProcedure (IO r) '[] r where
    toProcedure = pure

    fromProcedure cont = cont Nil

instance IsProcedure b ps r => IsProcedure (a -> b) (a : ps) r where
    toProcedure fun (Cons input inputs) = toProcedure (fun input) inputs

    fromProcedure cont input = fromProcedure (cont . Cons input)

-- | Instance of 'Procedure' that takes the corresponding 'CType's as parameters and produces a
-- matching 'CType' as output.
type CProcedure ps r = Procedure (CTypes ps) (CType r)

-- | Convert a 'Procedure' to its corresponding 'CProcedure'.
toCProcedure :: (FromCInputs ps, Interface r) => Procedure ps r -> CProcedure ps r
toCProcedure cont = fromCInputs >=> cont >=> toC

-- | Compute the type of the exported function for a given procedure type.
type family ExportProcedure f where
    ExportProcedure (IO r)   = IO (CType r)
    ExportProcedure (a -> b) = CType a -> ExportProcedure b

-- |
type IsExportable f ps r =
    ( FromCInputs ps
    , Interface r
    , IsProcedure (ExportProcedure f) (CTypes ps) (CType r) )

-- | Turn a given procedure into one that can be exported.
exportProcedure :: (IsProcedure f ps r, IsExportable f ps r) => f -> ExportProcedure f
exportProcedure = fromProcedure . toCProcedure . toProcedure
