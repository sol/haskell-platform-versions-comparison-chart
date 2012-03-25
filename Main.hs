{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where
import           Prelude hiding (div)
import           Data.Foldable (forM_)
import           Data.List (sortBy)
import           Data.Char (toLower)
import           Data.Function (on)
import           Data.String

import qualified Data.Map as Map

import           Text.Blaze.Html5 hiding (p, head, map, style)
import qualified Text.Blaze.Html5 as Html
import           Text.Blaze.Renderer.String (renderHtml)
import           Text.Blaze.Html5.Attributes hiding (title, name, id)

import           Config
import           Util

main :: IO ()
main = writeFile "index.html" . (++ "\n") . renderHtml . (docTypeHtml ! lang "en") $ do
  Html.head $ do
    meta ! charset "utf-8"
    link ! rel "stylesheet" ! type_ "text/css" ! href "css/bootstrap.css"
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
    table ! class_ "table table-bordered" $ do
      thead . tr $ do
        th ""
        th ! class_ "hackage-header" $ "Hackage"
        mapM_ (th . toHtml) versions
      tbody $ do
        forM_ packages $ \(name, xs) -> tr $ do
          th (toHtml name)
          td ! dataAttribute "package" (fromString name) ! class_ "hackage-version" $ ""
          showVersions xs

    forkMe "https://github.com/sol/haskell-platform-versions-comparison-chart"

    load "js/jquery.js"
    -- load "js/bootstrap.js"
    load "js/activity-indicator.js" -- from https://github.com/neteye/jquery-plugins
    load "js/custom.js"

  where
    load s = script ! src s $ ""

    showVersions :: [(PlatformVersion, Package)] -> Html
    showVersions xs = go versions
      where
        go (v1:v2:vs) = do
          case (lookup v1 xs, lookup v2 xs) of
            (Nothing, _)       -> td ""
            (Just p1, Just p2) -> showPackageVersion (packageVersion p1 /= packageVersion p2) p1
            (Just p1, Nothing) -> showPackageVersion False p1
          go (v2:vs)
        go (v:[])    = maybe (td "") (showPackageVersion False) (lookup v xs)
        go []        = return ()


    showPackageVersion :: Bool -> Package -> Html
    showPackageVersion changed p =
      (td . new) s
      where
        s = case packageVisibility p of
          Exposed -> createDocLink p
          Hidden  -> "(" >> createDocLink p >> ")"
        new
          | changed   = Html.span ! class_ "alert-success"
          | otherwise = id

    -- | Create link to package documentation.
    createDocLink :: Package -> Html
    createDocLink p
      -- for now, we only care about ghc
      | name == "ghc" = a ! href (fromString ghcDoc) $ toHtml version
      | otherwise     = toHtml version
      where
        Package name version _ = p
        ghcDoc = "http://www.haskell.org/ghc/docs/" ++ version ++ "/html/libraries/ghc-" ++ version ++ "/index.html"

    -- group packages by package name
    packages :: [(PackageName, [(PlatformVersion, Package)])]
    packages =  (sortCI . filterNonAPI . Map.toList . foldr f Map.empty) releases
      where
        f (Platform version xs ys) m = foldr g m (xs ++ ys)
          where
            g x@(Package name _ _) = Map.insertWith' (++) name [(version, x)]

        -- sort, ignore case
        sortCI = sortBy (compare `on` (map toLower . fst))

        -- exclude non API packages
        filterNonAPI = filter ((`notElem` non_api_packages) . fst)

    versions :: [PlatformVersion]
    versions = [v | Platform v _ _ <- releases]
