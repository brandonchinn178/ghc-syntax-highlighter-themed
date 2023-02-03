{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.SyntaxHighlighter.Themed (
  ThemeProviderConfig (..),
  renderHaskellWith,
  renderTokensWith,
  renderTokenWith,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.SyntaxHighlighter (Token (..), tokenizeHaskell)

data ThemeProviderConfig = ThemeProviderConfig
  { containerClass :: Text
  , getTokenClass :: Token -> Text
  }

renderHaskellWith :: ThemeProviderConfig -> Text -> Maybe Text
renderHaskellWith config = fmap (renderTokensWith config) . tokenizeHaskell

-- | Convert tokens output by 'tokenizeHaskell' into an HTML snippet.
renderTokensWith :: ThemeProviderConfig -> [(Token, Text)] -> Text
renderTokensWith config = wrap . foldMap (renderTokenWith config)
  where
    wrap s =
      Text.concat
        [ "<pre>"
        , "<code class=\"language-haskell " <> containerClass config <> "\">"
        , s
        , "</code>"
        , "</pre>"
        ]

renderTokenWith :: ThemeProviderConfig -> (Token, Text) -> Text
renderTokenWith ThemeProviderConfig{getTokenClass} (token, s) =
  Text.concat
    [ "<span class=\"" <> getTokenClass token <> "\">"
    , s
    , "</span>"
    ]
