{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Layout (chart) where

import           Prelude hiding (div)
import           Control.Monad (unless, when)
import           Data.Foldable (forM_)
import           Data.List (sortBy, find)
import           Data.Char (toLower)
import           Data.Function (on)
import           Data.String
import           Data.Monoid (mempty)

import qualified Data.Map as Map

import           Text.Blaze.Html5 hiding (p, head, map, style)
import qualified Text.Blaze.Html5 as Html
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5.Attributes hiding (title, name, id)

import           Config

-- | The generated document.
chart :: String
chart = renderHtml . (docTypeHtml ! lang "en") $ do
  Html.head $ do
    meta ! charset "utf-8"
    link ! rel "stylesheet" ! type_ "text/css" ! href "http://sol.github.com/get-bootstrapped/bootstrap/2.0.2/css/bootstrap.min.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "css/custom.css"

    title "Haskell Platform Versions Comparison Chart"

  body . (div ! class_ "container") $ do
    div !class_ "page-header" $ h1 $ do
      small "Haskell Platform "
      "Versions Comparison Chart"
    blockquote . Html.p $ do
      em "Ever wanted to know what version of a package is in what Haskell Platform?"
      br
      em "Here you are!"

    noscript . (div ! class_ "alert alert-error") $ do
      "The latest package versions on Hackage will only be shown if you "
      a ! href "http://enable-javascript.com/" $
        "enable JavaScript"
      "."

    h2 "Platform Libraries"
    Html.p $ do
      "The "
      hp
      " is the blessed set of libraries and tools on"
      " which to build further Haskell libraries and applications.  It is"
      " intended to provide a comprehensive, stable, and quality tested base for"
      " Haskell projects to work from."
    table ! class_ "table table-bordered" $ do
      thead . tr $ do
        th ""
        th ! class_ "latest" $ "Hackage"
        mapM_ (th . platformLink . fst) versions
      tbody $ do
        forM_ packages $ \(name, xs) -> when (isPlatformPackage name) . tr $ do

          -- package name
          th (toHtml name)

          -- latest version
          td ! dataAttribute "package" (fromString name) ! class_ "latest" $ do
            let url = "http://hackage.haskell.org/package/" ++ name
            a ! href (fromString url) $ "???"

          -- other versions
          showVersions xs


    h2 "GHC Libraries"
    Html.p $ do
      " GHC provides additional libraries which are not part of the "
      hp
      "."
    table ! class_ "table table-bordered" $ do
      thead . tr $ do
        th ""
        th "Latest GHC"
        mapM_ (th . toHtml . snd) versions
      tbody $ do
        forM_ packages $ \(name, xs) -> unless (isPlatformPackage name) . tr $ do

          -- package name
          th (toHtml name)

          -- latest version
          case find ((== name) . packageName) ghc_latest of
            Just p -> do
              let versionChanged = (packageVersion . snd . head) xs /= packageVersion p
              showPackage versionChanged p
            Nothing -> th mempty

          -- other versions
          showVersions xs

    forkMe "https://github.com/sol/haskell-platform-versions-comparison-chart"

    load "http://code.jquery.com/jquery-1.7.2.min.js"
    load "js/activity-indicator.js" -- from https://github.com/neteye/jquery-plugins
    load "js/custom.js"

  where
    hp = a ! href "http://hackage.haskell.org/platform/" $ "Haskell Platform"

    load s = script ! src s $ ""

    showVersions :: [(PlatformVersion, Package)] -> Html
    showVersions xs = go (map fst versions)
      where
        go (v1:v2:vs) = do
          case (lookup v1 xs, lookup v2 xs) of
            (Nothing, _)       -> td ""
            (Just p1, Just p2) -> showPackage (packageVersion p1 /= packageVersion p2) p1
            (Just p1, Nothing) -> showPackage False p1
          go (v2:vs)
        go (v:[])    = maybe (td "") (showPackage False) (lookup v xs)
        go []        = return ()

    showPackage :: Bool -> Package -> Html
    showPackage changed p@(Package _ version _ _) = (cell . new . packageLink) p
      where
        cell = td ! dataAttribute "version" (fromString version)
        new
          | changed   = (! class_ "alert-success")
          | otherwise = id

    -- group packages by package name
    packages :: [(PackageName, [(PlatformVersion, Package)])]
    packages =  (sortCI . ignoreUninteresting . Map.toList . foldr f Map.empty) releases
      where
        f (Platform version _ xs) m = foldr g m xs
          where
            g x@(Package name _ _ _) = Map.insertWith' (++) name [(version, x)]

        -- sort, ignore case
        sortCI = sortBy (compare `on` (map toLower . fst))

        ignoreUninteresting = filter ((`notElem` uninterestingPackages) . fst)

    versions :: [(PlatformVersion, GhcVersion)]
    versions = [(v, ghc) | Platform v ghc _ <- releases]

-- | Create link to package documentation.
packageLink :: Package -> Html
packageLink (Package name version _ mGhcVersion) = a ! href (fromString url) $ toHtml version
  where
    url
      | (not . isPlatformPackage) name, Just ghc <- mGhcVersion
                   = ghcDocUrl ghc name version
      | otherwise  = "http://hackage.haskell.org/package/" ++ name ++ "-" ++ version


-- | Construct URL to documentation of ghc package.
ghcDocUrl :: GhcVersion -> PackageName -> PackageVersion -> String
ghcDocUrl ghc name version = "http://www.haskell.org/ghc/docs/" ++ ghc ++ "/html/libraries/" ++ name ++ "-" ++ version ++ "/index.html"

-- | Create link to platform cabal file.
platformLink :: PlatformVersion -> Html
platformLink version = a ! href (fromString url) $ toHtml version
  where
    url = "https://raw.github.com/sol/haskell-platform-versions-comparison-chart/gh-pages/cabal/" ++ version ++ "/haskell-platform.cabal"

-- | Create a "Fork me on GitHub" link.
forkMe :: AttributeValue -> Html
forkMe url = do
  a ! href url $ do
    img ! style "position: absolute; top: 0; right: 0; border: 0;"
        ! alt "Fork me on GitHub"
        ! src "https://s3.amazonaws.com/github/ribbons/forkme_right_orange_ff7600.png"
