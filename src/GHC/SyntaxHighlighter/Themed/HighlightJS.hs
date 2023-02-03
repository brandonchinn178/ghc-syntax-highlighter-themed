{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.SyntaxHighlighter.Themed.HighlightJS (
  renderHaskell,
  config,
) where

import Data.Text (Text)
import GHC.SyntaxHighlighter (Token (..))
import GHC.SyntaxHighlighter.Themed

-- | The primary entrypoint that will tokenize Haskell code with
-- @ghc-syntax-highlighter@ and render HTML compatible with
-- [highlight.js](https://highlightjs.org) themes.
--
-- Returns Nothing if the input could not be parsed.
renderHaskell :: Text -> Maybe Text
renderHaskell = renderHaskellWith config

config :: ThemeProviderConfig
config =
  ThemeProviderConfig
    { containerClass = "hljs"
    , getTokenClass = \case
        KeywordTok -> Just "hljs-keyword"
        PragmaTok -> Just "hljs-meta"
        SymbolTok -> Nothing
        VariableTok -> Nothing
        ConstructorTok -> Just "hljs-type"
        OperatorTok -> Nothing
        CharTok -> Just "hljs-string"
        StringTok -> Just "hljs-string"
        IntegerTok -> Just "hljs-number"
        RationalTok -> Just "hljs-number"
        CommentTok -> Just "hljs-comment"
        SpaceTok -> Nothing
        OtherTok -> Nothing
    }
