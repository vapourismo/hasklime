{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module HaskLime.TH (export, exportAs) where

import Language.Haskell.TH

import HaskLime.Procedure

-- | Define an export wrapper for the given name and type.
defineExportWrapper :: Name -> Type -> Q (Dec, Dec, Name, Type)
defineExportWrapper name typ = do
    wrapperName <- newName ("__export_" ++ nameBase name)
    pure (wrapperSignature wrapperName, wrapperImplementation wrapperName, wrapperName, wrapperType)
    where
        wrapperType =
            AppT (ConT ''ExportProcedure) typ

        wrapperSignature wrapperName =
            SigD wrapperName wrapperType

        wrapperImplementation wrapperName =
            ValD (VarP wrapperName) (NormalB (AppE (VarE 'toExportProcedure) (VarE name))) []

-- | Export the given name.
export :: Name -> Q [Dec]
export name = exportAs name (nameBase name)

-- | Export the given name using the given alias.
exportAs :: Name -> String -> Q [Dec]
exportAs name alias =
    reify name >>= \case
        VarI name typ _ -> do
            (sig, impl, wName, wType) <- defineExportWrapper name typ
            pure [sig, impl, ForeignD (ExportF CCall alias wName wType)]

        _ -> fail "Given name does not refer to a variable"
