{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.SyntaxHighlighter.Themed.HighlightJS (
  renderHaskell,

  -- * Manual processing
  renderTokens,
  renderToken,
  config,
) where

import Data.Text (Text)
import GHC.SyntaxHighlighter (Token (..))
import GHC.SyntaxHighlighter.Themed

-- | The primary entrypoint that will tokenize Haskell code with
-- @ghc-syntax-highlighter@ and render HTML compatible with
-- highlight.js themes.
--
-- Returns Nothing if the input could not be parsed.
renderHaskell :: Text -> Maybe Text
renderHaskell = renderHaskellWith config

-- | Convert tokens output by 'tokenizeHaskell' into an HTML snippet.
renderTokens :: [(Token, Text)] -> Text
renderTokens = renderTokensWith config

renderToken :: (Token, Text) -> Text
renderToken = renderTokenWith config

config :: ThemeProviderConfig
config =
  ThemeProviderConfig
    { containerClass = "hljs"
    , getTokenClass = \case
        KeywordTok -> "hljs-keyword"
        PragmaTok -> "hljs-meta"
        SymbolTok -> ""
        VariableTok -> ""
        ConstructorTok -> "hljs-type"
        OperatorTok -> ""
        CharTok -> "hljs-string"
        StringTok -> "hljs-string"
        IntegerTok -> "hljs-number"
        RationalTok -> "hljs-number"
        CommentTok -> "hljs-comment"
        SpaceTok -> ""
        OtherTok -> ""
    }
