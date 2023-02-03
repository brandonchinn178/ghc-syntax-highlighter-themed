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
  , getTokenClass :: Token -> Maybe Text
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
        , "<code class='language-haskell " <> containerClass config <> "'>"
        , s
        , "</code>"
        , "</pre>"
        ]

renderTokenWith :: ThemeProviderConfig -> (Token, Text) -> Text
renderTokenWith ThemeProviderConfig{getTokenClass} (token, rawText) =
  case getTokenClass token of
    Nothing -> escapedText
    Just cls ->
      Text.concat
        [ "<span class='" <> cls <> "'>"
        , escapedText
        , "</span>"
        ]
  where
    escapedText =
      Text.replace "<" "&lt;"
        . Text.replace ">" "&gt;"
        . Text.replace "&" "&amp;"
        $ rawText
