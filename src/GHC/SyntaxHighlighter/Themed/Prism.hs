{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.SyntaxHighlighter.Themed.Prism (
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
-- Prism themes.
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
    { containerClass = ""
    , getTokenClass = \case
        KeywordTok -> "token keyword"
        PragmaTok -> "token comment"
        SymbolTok -> "token punctuation"
        VariableTok -> ""
        ConstructorTok -> "token class-name"
        OperatorTok -> "token operator"
        CharTok -> "token char"
        StringTok -> "token string"
        IntegerTok -> "token number"
        RationalTok -> "token number"
        CommentTok -> "token comment"
        SpaceTok -> ""
        OtherTok -> ""
    }
