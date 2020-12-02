{-# LANGUAGE TemplateHaskell #-}

module Distribution.PackDeps.Lens
    (
      P.PackInfo
    , piVersion
    , piDesc
    , piEpoch
    , P.DescInfo
    , diHaystack
    , diDeps
    , diLibDeps
    , diPackage
    , diRevision
    , diSynopsis
    , P.Newest
    , P.loadNewest
    , Dependency
    , depName
    , depRange
    , PackageName
    , unPackageName
    , mkPackageName
    , packageName
    ) where

import Control.Lens hiding (makeLenses)
import Distribution.PackDeps.Lens.Machinery (makeLenses)
import Distribution.Package hiding (packageName)
import Distribution.Version (VersionRange)
import qualified Distribution.PackDeps as P

makeLenses ''P.PackInfo
makeLenses ''P.DescInfo

depName :: Lens' Dependency PackageName
depName =
  lens (\(Dependency n _ _) -> n) (\(Dependency _ r ls) n -> Dependency n r ls)

depRange :: Lens' Dependency VersionRange
depRange =
  lens (\(Dependency _ r _) -> r) (\(Dependency n _ ls) r -> Dependency n r ls)

packageName :: Iso' PackageName String
packageName = iso unPackageName mkPackageName
