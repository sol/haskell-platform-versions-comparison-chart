NOTE: The Haskell Platform website comes with its own
[Changelog](http://www.haskell.org/platform/changelog.html) now, which is more
up-to-date.

The chart is at
http://sol.github.com/haskell-platform-versions-comparison-chart/.

## Updating the chart

 1. Add `haskell-platform.cabal` to `cabal/<version>`
 2. Follow the instructions in `Config.hs`
 3. Make sure that the `colgroup`s in `mkTable` in `Layout.hs` cover all
    columns.
 4. Make sure that `.first-column` and `.other-columns` in `css/custom.css` add
    up to 100%.
