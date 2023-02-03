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
        KeywordTok -> Just "token keyword"
        PragmaTok -> Just "token comment"
        SymbolTok -> Just "token punctuation"
        VariableTok -> Nothing
        ConstructorTok -> Just "token class-name"
        OperatorTok -> Just "token operator"
        CharTok -> Just "token char"
        StringTok -> Just "token string"
        IntegerTok -> Just "token number"
        RationalTok -> Just "token number"
        CommentTok -> Just "token comment"
        SpaceTok -> Nothing
        OtherTok -> Nothing
    }
