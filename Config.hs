{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Config where

type PackageName = String

type PackageVersion = String

data Visibility = Exposed | Hidden
  deriving (Eq, Show)

data Package = Package {
  packageName       :: PackageName
, packageVersion    :: PackageVersion
, packageVisibility :: Visibility
, packageComesWith  :: Maybe GhcVersion
} deriving (Eq, Show)

type GhcVersion = String

type PlatformVersion = String

data Platform = Platform {
  platformVersion     :: PlatformVersion
, platformGhcVersion  :: GhcVersion
, platformPackages    :: [Package]
} deriving (Eq, Show)


-- | Construct an exposed package.
package :: PackageName -> PackageVersion -> Maybe GhcVersion -> Package
package name version = Package name version Exposed

platform
  :: PlatformVersion
  -> GhcVersion
  -> [Maybe GhcVersion -> Package]
  -> [Maybe GhcVersion -> Package]
  -> Platform
platform version ghcVersion ghc hp = Platform version ghcVersion $ addGhcVersion ghc ++ addNothing hp
  where
    addGhcVersion = ghcPackages ghcVersion
    addNothing    = map ($ Nothing)

ghcPackages version = map ($ Just version)

-- | A list of Haskell Platform releases.
--
-- How to add a new release?
--
-- (1) ghc-pkg list
-- (2) s/\s*\(.*\)-\([^-]*\)$/  , package "\1" "\2"
-- (3) manually adjust `packageExposed` for hidden packages
-- (4) add to `releases`
--
releases :: [Platform]
releases =
  platform "2012.4.0.0" "7.4.2"  ghc_7_4_2  hp_2012_4_0_0 :
  platform "2012.2.0.0" "7.4.1"  ghc_7_4_1  hp_2012_2_0_0 :
  platform "2011.4.0.0" "7.0.4"  ghc_7_0_4  hp_2011_4_0_0 :
  platform "2011.2.0.1" "7.0.3"  ghc_7_0_3  hp_2011_2_0_1 :
  platform "2011.2.0.0" "7.0.2"  ghc_7_0_2  hp_2011_2_0_0 :
  platform "2010.2.0.0" "6.12.3" ghc_6_12_3 hp_2010_2_0_0 :
  []

isPlatformPackage :: PackageName -> Bool
isPlatformPackage p = notElem p $ [
  -- GHC includes some packages, that are not part of the Haskell Platform
    "binary"
  , "bin-package-db"
  , "dph-base"
  , "dph-par"
  , "dph-prim-interface"
  , "dph-prim-par"
  , "dph-prim-seq"
  , "dph-seq"
  , "ffi"
  , "ghc"
  , "ghc-binary"
  , "ghc-prim"
  , "haskell-platform"
  , "hoopl"
  , "integer-gmp"
  , "rts"
  ]

uninterestingPackages = [
  -- only included with ghc-6.12.3 (and maybe before?)
    "dph-base"
  , "dph-par"
  , "dph-prim-interface"
  , "dph-prim-par"
  , "dph-prim-seq"
  , "dph-seq"

  -- does not export anything for ghc 7.0.*
  , "integer-gmp"

  -- the following packages do not provide any modules
  , "rts"
  , "ffi"
  , "haskell-platform"
  ]

ghc_latest = ghcPackages "7.6.1" ghc_7_6_1

ghc_7_6_1 = [
    package "Cabal" "1.16.0"
  , package "array" "0.4.0.1"
  , package "base" "4.6.0.0"
  , package "bin-package-db" "0.0.0.0"
  , package "binary" "0.5.1.1"
  , package "bytestring" "0.10.0.0"
  , package "containers" "0.5.0.0"
  , package "deepseq" "1.3.0.1"
  , package "directory" "1.2.0.0"
  , package "filepath" "1.3.0.1"
  , Package "ghc" "7.6.1" Hidden
  , package "ghc-prim" "0.3.0.0"
  , Package "haskell2010" "1.1.1.0" Hidden
  , Package "haskell98" "2.0.0.2" Hidden
  , package "hoopl" "3.9.0.0"
  , package "hpc" "0.6.0.0"
  , package "integer-gmp" "0.5.0.0"
  , package "old-locale" "1.0.0.5"
  , package "old-time" "1.1.0.1"
  , package "pretty" "1.1.1.0"
  , package "process" "1.1.0.2"
  , package "rts" "1.0"
  , package "template-haskell" "2.8.0.0"
  , package "time" "1.4.0.1"
  , package "unix" "2.6.0.0"
  ]

ghc_7_4_2 = [
    package "Cabal" "1.14.0"
  , package "array" "0.4.0.0"
  , package "base" "4.5.1.0"
  , package "bin-package-db" "0.0.0.0"
  , package "binary" "0.5.1.0"
  , package "bytestring" "0.9.2.1"
  , package "containers" "0.4.2.1"
  , package "deepseq" "1.3.0.0"
  , package "directory" "1.1.0.2"
  , package "extensible-exceptions" "0.1.1.4"
  , package "filepath" "1.3.0.0"
  , Package "ghc" "7.4.2" Hidden
  , package "ghc-prim" "0.2.0.0"
  , Package "haskell2010" "1.1.0.1" Hidden
  , Package "haskell98" "2.0.0.1" Hidden
  , package "hoopl" "3.8.7.3"
  , package "hpc" "0.5.1.1"
  , package "integer-gmp" "0.4.0.0"
  , package "old-locale" "1.0.0.4"
  , package "old-time" "1.1.0.0"
  , package "pretty" "1.1.1.0"
  , package "process" "1.1.0.1"
  , package "rts" "1.0"
  , package "template-haskell" "2.7.0.0"
  , package "time" "1.4"
  , package "unix" "2.5.1.1"
  ]

hp_2012_4_0_0 = [
    package "GLUT" "2.1.2.1"
  , package "HTTP" "4000.2.5"
  , package "HUnit" "1.2.5.1"
  , package "OpenGL" "2.2.3.1"
  , package "QuickCheck" "2.5.1.1"
  , package "async" "2.0.1.3"
  , package "cgi" "3001.1.7.4"
  , package "fgl" "5.4.2.4"
  , package "haskell-platform" "2012.4.0.0"
  , package "haskell-src" "1.0.1.5"
  , package "html" "1.0.1.2"
  , package "mtl" "2.1.2"
  , package "network" "2.3.1.0"
  , package "parallel" "3.2.0.3"
  , package "parsec" "3.1.3"
  , package "primitive" "0.5.0.1"
  , package "random" "1.0.1.1"
  , package "regex-base" "0.93.2"
  , package "regex-compat" "0.95.1"
  , package "regex-posix" "0.95.2"
  , package "split" "0.2.1.1"
  , package "stm" "2.4"
  , package "syb" "0.3.7"
  , package "text" "0.11.2.3"
  , package "transformers" "0.3.0.0"
  , package "vector" "0.10.0.1"
  , package "xhtml" "3000.2.1"
  , package "zlib" "0.5.4.0"
  ]

ghc_7_4_1 = [
    package "Cabal" "1.14.0"
  , package "array" "0.4.0.0"
  , package "base" "4.5.0.0"
  , package "bin-package-db" "0.0.0.0"
  , package "binary" "0.5.1.0"
  , package "bytestring" "0.9.2.1"
  , package "containers" "0.4.2.1"
  , package "deepseq" "1.3.0.0"
  , package "directory" "1.1.0.2"
  , package "extensible-exceptions" "0.1.1.4"
  , package "filepath" "1.3.0.0"
  , Package "ghc" "7.4.1" Hidden
  , package "ghc-prim" "0.2.0.0"
  , Package "haskell2010" "1.1.0.1" Hidden
  , Package "haskell98" "2.0.0.1" Hidden
  , package "hoopl" "3.8.7.3"
  , package "hpc" "0.5.1.1"
  , package "integer-gmp" "0.4.0.0"
  , package "old-locale" "1.0.0.4"
  , package "old-time" "1.1.0.0"
  , package "pretty" "1.1.1.0"
  , package "process" "1.1.0.1"
  , package "rts" "1.0"
  , package "template-haskell" "2.7.0.0"
  , package "time" "1.4"
  , package "unix" "2.5.1.0"
  ]

hp_2012_2_0_0 = [
    package "GLUT" "2.1.2.1"
  , package "HTTP" "4000.2.3"
  , package "HUnit" "1.2.4.2"
  , package "OpenGL" "2.2.3.1"
  , package "QuickCheck" "2.4.2"
  , package "cgi" "3001.1.7.4"
  , package "fgl" "5.4.2.4"
  , package "haskell-platform" "2012.2.0.0"
  , package "haskell-src" "1.0.1.5"
  , package "html" "1.0.1.2"
  , package "mtl" "2.1.1"
  , package "network" "2.3.0.13"
  , package "parallel" "3.2.0.2"
  , package "parsec" "3.1.2"
  , package "random" "1.0.1.1"
  , package "regex-base" "0.93.2"
  , package "regex-compat" "0.95.1"
  , package "regex-posix" "0.95.1"
  , package "stm" "2.3"
  , package "syb" "0.3.6.1"
  , package "text" "0.11.2.0"
  , package "transformers" "0.3.0.0"
  , package "xhtml" "3000.2.1"
  , package "zlib" "0.5.3.3"
  ]

ghc_7_0_4 = [
    package "Cabal" "1.10.2.0"
  , package "array" "0.3.0.2"
  , package "base" "4.3.1.0"
  , package "bin-package-db" "0.0.0.0"
  , package "bytestring" "0.9.1.10"
  , package "containers" "0.4.0.0"
  , package "directory" "1.1.0.0"
  , package "extensible-exceptions" "0.1.1.2"
  , package "ffi" "1.0"
  , package "filepath" "1.2.0.0"
  , Package "ghc" "7.0.4" Hidden
  , Package "ghc-binary" "0.5.0.2" Hidden
  , package "ghc-prim" "0.2.0.0"
  , Package "haskell2010" "1.0.0.0" Hidden
  , package "haskell98" "1.1.0.1"
  , package "hpc" "0.5.0.6"
  , package "integer-gmp" "0.2.0.3"
  , package "old-locale" "1.0.0.2"
  , package "old-time" "1.0.0.6"
  , package "pretty" "1.0.1.2"
  , package "process" "1.0.1.5"
  , package "random" "1.0.0.3"
  , package "rts" "1.0"
  , package "template-haskell" "2.5.0.0"
  , package "time" "1.2.0.3"
  , package "unix" "2.4.2.0"
  ]

hp_2011_4_0_0 = [
    package "GLUT" "2.1.2.1"
  , package "HTTP" "4000.1.2"
  , package "HUnit" "1.2.4.2"
  , package "OpenGL" "2.2.3.0"
  , package "QuickCheck" "2.4.1.1"
  , package "cgi" "3001.1.7.4"
  , package "deepseq" "1.1.0.2"
  , package "fgl" "5.4.2.4"
  , package "haskell-platform" "2011.4.0.0"
  , package "haskell-src" "1.0.1.4"
  , package "html" "1.0.1.2"
  , package "mtl" "2.0.1.0"
  , package "network" "2.3.0.5"
  , package "parallel" "3.1.0.1"
  , package "parsec" "3.1.1"
  , package "regex-base" "0.93.2"
  , package "regex-compat" "0.95.1"
  , package "regex-posix" "0.95.1"
  , package "stm" "2.2.0.1"
  , package "syb" "0.3.3"
  , package "text" "0.11.1.5"
  , package "transformers" "0.2.2.0"
  , package "xhtml" "3000.2.0.4"
  , package "zlib" "0.5.3.1"
  ]

ghc_7_0_3 = [
    package "Cabal" "1.10.1.0"
  , package "array" "0.3.0.2"
  , package "base" "4.3.1.0"
  , package "bin-package-db" "0.0.0.0"
  , package "bytestring" "0.9.1.10"
  , package "containers" "0.4.0.0"
  , package "directory" "1.1.0.0"
  , package "extensible-exceptions" "0.1.1.2"
  , package "ffi" "1.0"
  , package "filepath" "1.2.0.0"
  , Package "ghc" "7.0.3" Hidden
  , Package "ghc-binary" "0.5.0.2" Hidden
  , package "ghc-prim" "0.2.0.0"
  , Package "haskell2010" "1.0.0.0" Hidden
  , package "haskell98" "1.1.0.1"
  , package "hpc" "0.5.0.6"
  , package "integer-gmp" "0.2.0.3"
  , package "old-locale" "1.0.0.2"
  , package "old-time" "1.0.0.6"
  , package "pretty" "1.0.1.2"
  , package "process" "1.0.1.5"
  , package "random" "1.0.0.3"
  , package "rts" "1.0"
  , package "template-haskell" "2.5.0.0"
  , package "time" "1.2.0.3"
  , package "unix" "2.4.2.0"
  ]

hp_2011_2_0_1 = [
    package "GLUT" "2.1.2.1"
  , package "HTTP" "4000.1.1"
  , package "HUnit" "1.2.2.3"
  , package "OpenGL" "2.2.3.0"
  , package "QuickCheck" "2.4.0.1"
  , package "cgi" "3001.1.7.4"
  , package "deepseq" "1.1.0.2"
  , package "fgl" "5.4.2.3"
  , package "haskell-platform" "2011.2.0.1"
  , package "haskell-src" "1.0.1.4"
  , package "html" "1.0.1.2"
  , package "mtl" "2.0.1.0"
  , package "network" "2.3.0.2"
  , package "parallel" "3.1.0.1"
  , package "parsec" "3.1.1"
  , package "regex-base" "0.93.2"
  , package "regex-compat" "0.93.1"
  , package "regex-posix" "0.94.4"
  , package "stm" "2.2.0.1"
  , package "syb" "0.3"
  , package "text" "0.11.0.6"
  , package "transformers" "0.2.2.0"
  , package "xhtml" "3000.2.0.1"
  , package "zlib" "0.5.3.1"
  ]

ghc_7_0_2 = [
    package "Cabal" "1.10.1.0"
  , package "array" "0.3.0.2"
  , package "base" "4.3.1.0"
  , package "bin-package-db" "0.0.0.0"
  , package "bytestring" "0.9.1.10"
  , package "containers" "0.4.0.0"
  , package "directory" "1.1.0.0"
  , package "extensible-exceptions" "0.1.1.2"
  , package "ffi" "1.0"
  , package "filepath" "1.2.0.0"
  , Package "ghc" "7.0.2" Hidden
  , Package "ghc-binary" "0.5.0.2" Hidden
  , package "ghc-prim" "0.2.0.0"
  , Package "haskell2010" "1.0.0.0" Hidden
  , package "haskell98" "1.1.0.1"
  , package "hpc" "0.5.0.6"
  , package "integer-gmp" "0.2.0.3"
  , package "old-locale" "1.0.0.2"
  , package "old-time" "1.0.0.6"
  , package "pretty" "1.0.1.2"
  , package "process" "1.0.1.5"
  , package "random" "1.0.0.3"
  , package "rts" "1.0"
  , package "template-haskell" "2.5.0.0"
  , package "time" "1.2.0.3"
  , package "unix" "2.4.2.0"
  ]

hp_2011_2_0_0 = [
    package "GLUT" "2.1.2.1"
  , package "HTTP" "4000.1.1"
  , package "HUnit" "1.2.2.3"
  , package "OpenGL" "2.2.3.0"
  , package "QuickCheck" "2.4.0.1"
  , package "cgi" "3001.1.7.4"
  , package "deepseq" "1.1.0.2"
  , package "fgl" "5.4.2.3"
  , package "haskell-platform" "2011.2.0.0"
  , package "haskell-src" "1.0.1.4"
  , package "html" "1.0.1.2"
  , package "mtl" "2.0.1.0"
  , package "network" "2.3.0.2"
  , package "parallel" "3.1.0.1"
  , package "parsec" "3.1.1"
  , package "regex-base" "0.93.2"
  , package "regex-compat" "0.93.1"
  , package "regex-posix" "0.94.4"
  , package "stm" "2.2.0.1"
  , package "syb" "0.3"
  , package "text" "0.11.0.5"
  , package "transformers" "0.2.2.0"
  , package "xhtml" "3000.2.0.1"
  , package "zlib" "0.5.3.1"
  ]

ghc_6_12_3 = [
    package "Cabal" "1.8.0.6"
  , package "array" "0.3.0.1"
  -- , package "base" "3.0.3.2"
  , package "base" "4.2.0.2"
  , package "bin-package-db" "0.0.0.0"
  , package "bytestring" "0.9.1.7"
  , package "containers" "0.3.0.0"
  , package "directory" "1.0.1.1"
  , Package "dph-base" "0.4.0" Hidden
  , Package "dph-par" "0.4.0" Hidden
  , Package "dph-prim-interface" "0.4.0" Hidden
  , Package "dph-prim-par" "0.4.0" Hidden
  , Package "dph-prim-seq" "0.4.0" Hidden
  , Package "dph-seq" "0.4.0" Hidden
  , package "extensible-exceptions" "0.1.1.1"
  , package "ffi" "1.0"
  , package "filepath" "1.1.0.4"
  , Package "ghc" "6.12.3" Hidden
  , Package "ghc-binary" "0.5.0.2" Hidden
  , package "ghc-prim" "0.2.0.0"
  , package "haskell98" "1.0.1.1"
  , package "hpc" "0.5.0.5"
  , package "integer-gmp" "0.2.0.1"
  , package "old-locale" "1.0.0.2"
  , package "old-time" "1.0.0.5"
  , package "pretty" "1.0.1.1"
  , package "process" "1.0.1.3"
  , package "random" "1.0.0.2"
  , package "rts" "1.0"
  , package "syb" "0.1.0.2"
  , package "template-haskell" "2.4.0.1"
  , package "time" "1.1.4"
  , package "unix" "2.4.0.2"
  ]

hp_2010_2_0_0 = [
    package "GLUT" "2.1.2.1"
  , package "HTTP" "4000.0.9"
  , package "HUnit" "1.2.2.1"
  , package "OpenGL" "2.2.3.0"
  , package "QuickCheck" "2.1.1.1"
  , package "cgi" "3001.1.7.3"
  , package "deepseq" "1.1.0.0"
  , package "fgl" "5.4.2.3"
  , package "haskell-platform" "2010.2.0.0"
  , package "haskell-src" "1.0.1.3"
  , package "html" "1.0.1.2"
  , package "mtl" "1.1.0.2"
  , package "network" "2.2.1.7"
  , package "parallel" "2.2.0.1"
  , package "parsec" "2.1.0.1"
  , package "regex-base" "0.93.2"
  , package "regex-compat" "0.93.1"
  , package "regex-posix" "0.94.2"
  , package "stm" "2.1.2.1"
  , package "xhtml" "3000.2.0.1"
  , package "zlib" "0.5.2.0"
  ]
