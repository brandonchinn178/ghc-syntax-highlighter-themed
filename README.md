# ghc-syntax-highlighter-themed

[![](https://img.shields.io/github/actions/workflow/status/brandonchinn178/ghc-syntax-highlighter-themed/ci.yml?branch=main)](https://github.com/brandonchinn178/ghc-syntax-highlighter-themed/actions)
[![](https://img.shields.io/hackage/v/ghc-syntax-highlighter-themed)](https://hackage.haskell.org/package/ghc-syntax-highlighter-themed)

Rendering HTML from `ghc-syntax-highlighter` output that can be styled with third-party theme providers. Compatible with themes from:

* [highlight.js](https://highlightjs.org)
* [Prism](https://prismjs.com/)

Want to add another provider? Make a PR!

## Quickstart

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.SyntaxHighlighter.Themed.HighlightJS (renderHaskell)

main :: IO ()
main = do
  -- read this file!
  code <- Text.readFile "Main.hs"

  Text.writeFile "index.html" . Text.unlines $
    [ "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/default.min.css'>"
    , fromJust $ renderHaskell code
    ]
```
