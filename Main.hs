

data Platform = Platform {
  platformName      :: String
, platformPackages  :: [Package]
}

data Package = Package {
  packageName    :: String
, packageVersion :: String
, packageExposed :: Bool
}

package :: String -> String -> Package
package name version = Package name version True

-- s/\(.*\)-\([^-]*\)$/  , package "\1" "\2"

hp_2011_4_0_0 = [
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
  , package "ghc" "7.0.4"
  , package "ghc-binary" "0.5.0.2"
  , package "ghc-prim" "0.2.0.0"
  , package "haskell2010" "1.0.0.0"
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
  , package "GLUT" "2.1.2.1"
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
