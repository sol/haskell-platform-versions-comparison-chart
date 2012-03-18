{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Prelude hiding (div)
import           Data.Foldable (forM_)
import           Data.List (nub)

import           Data.Map   (Map)
import qualified Data.Map as Map

import           Text.Blaze.Html5 hiding (head, map, style)
import qualified Text.Blaze.Html5 as Html
import           Text.Blaze.Renderer.String (renderHtml)
import           Text.Blaze.Html5.Attributes hiding (title, name)

type PackageName = String
type PackageVersion = String
data Visibility = Exposed | Hidden
  deriving (Eq, Show)

data Package = Package {
  packageName       :: PackageName
, packageVersion    :: PackageVersion
, packageVisibility :: Visibility
} deriving (Eq, Show)

type PlatformVersion = String

data Platform = Platform {
  platformVersion     :: PlatformVersion
, platformGhcPackages :: [Package]
, platformPackages    :: [Package]
} deriving (Eq, Show)

main :: IO ()
main = putStrLn . renderHtml . (docTypeHtml ! lang "en") $ do
  Html.head $ do
    meta ! charset "utf-8"
    link ! rel "stylesheet" ! type_ "text/css" ! href "css/bootstrap.css"
    title "Haskell Platform Versions Comparison Chart"

  body . (div ! class_ "container") $ do
    div !class_ "page-header" $ h1 $ do
      small "Haskell Platform "
      "Versions Comparison Chart"
    blockquote . p $ do
      em "Ever wanted to know what version of a package is in what Haskell Platform?"
      br
      em "Here you are!"
    table ! class_ "table table-bordered" $ do
      thead . tr $ do
        th ""
        mapM_ (th . toHtml) versions
      tbody $ do
        forM_ packages $ \(name, xs) -> tr $ do
          th (toHtml name)
          showVersions xs
    footer $ do
      "brought to you by "
      a ! href "https://github.com/sol" $ "Simon Hengel"
      ", source on "
      a ! href "https://github.com/sol/haskell-platform-versions-comparison-chart" $ "GitHub"

  where
    showVersions xs = go versions
      where
        go (v1:v2:vs) = do
          case (lookup v1 xs, lookup v2 xs) of
            (Nothing, _)       -> td ""
            (Just p1, Just p2) -> showPackageVersion (packageVersion p1 /= packageVersion p2) p1
            (Just p1, Nothing) -> showPackageVersion False p1
          go (v2:vs)
        go (v:[])    = maybe (td "") (showPackageVersion False) (lookup v xs)


    showPackageVersion :: Bool -> Package -> Html
    showPackageVersion changed (Package _ version visibility)
      | changed   = (td . new . toHtml) s
      | otherwise = (td . toHtml) s
      where
        new = Html.span ! class_ "alert-success"
        s = case visibility of
          Exposed -> version
          Hidden  -> "(" ++ version ++ ")"

    -- package names in the order they appear in the latest release
    packageNamesLatest :: [PackageName]
    packageNamesLatest = map packageName (platformGhcPackages latest ++ platformPackages latest)
      where
        latest = head releases

    packagesLatest :: [(PackageName, [(PlatformVersion, Package)])]
    packagesLatest = map (\name -> (name, packageIndex Map.! name)) packageNamesLatest

    -- all packages, packages that are in the latest release first (keeping the
    -- order they appear in the latest release)
    packages :: [(PackageName, [(PlatformVersion, Package)])]
    packages = nub (packagesLatest ++ Map.toList packageIndex)

    packageIndex :: Map PackageName [(PlatformVersion, Package)]
    packageIndex = foldr f Map.empty releases
      where
        f (Platform version xs ys) m = foldr g m (xs ++ ys)
          where
            g x@(Package name _ _) = Map.insertWith' (++) name [(version, x)]

    versions :: [PlatformVersion]
    versions = [v | Platform v _ _ <- releases]

-- | Construct an exposed package.
package :: PackageName -> PackageVersion -> Package
package name version = Package name version Exposed

-- | A list of Haskell Platform releases.
--
-- How to add a new release?
--
-- (1) ghc-pkg list
-- (2) s/\(.*\)-\([^-]*\)$/  , package "\1" "\2"
-- (3) manually adjust `packageExposed` for hidden packages
-- (4) add to `releases`
--
releases :: [Platform]
releases =
  Platform "2011.4.0.0" ghc_7_0_4  hp_2011_4_0_0 :
  Platform "2011.2.0.1" ghc_7_0_3  hp_2011_2_0_1 :
  Platform "2011.2.0.0" ghc_7_0_2  hp_2011_2_0_0 :
  Platform "2010.2.0.0" ghc_6_12_3 hp_2010_2_0_0 :
  []

ghc_7_0_4 :: [Package]
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

hp_2011_4_0_0 :: [Package]
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

ghc_7_0_3 :: [Package]
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

hp_2011_2_0_1 :: [Package]
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

ghc_7_0_2 :: [Package]
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

hp_2011_2_0_0 :: [Package]
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

ghc_6_12_3 :: [Package]
ghc_6_12_3 = [
    package "Cabal" "1.8.0.6"
  , package "array" "0.3.0.1"
  , package "base" "3.0.3.2"
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

hp_2010_2_0_0 :: [Package]
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
