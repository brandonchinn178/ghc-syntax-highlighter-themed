{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Test.Tasty
import Test.Tasty.Golden

import qualified GHC.SyntaxHighlighter.Themed.HighlightJS as HighlightJS
import qualified GHC.SyntaxHighlighter.Themed.Prism as Prism

main :: IO ()
main =
  defaultMain . testGroup "ghc-syntax-highlighter-themed" $
    [ goldenVsStringDiff (Text.unpack name) diff ("test/output-" <> Text.unpack name <> ".html") $ do
      input <- Text.readFile "test/input.hs"
      case render input of
        Nothing -> error "input.hs was invalid"
        Just output ->
          pure . ByteString.Lazy.fromStrict . Text.encodeUtf8 . Text.unlines $
            [ "<link rel='stylesheet' href='" <> link <> "'>"
            , output
            ]
    | ThemeProvider{..} <- allThemes
    ]
  where
    diff ref new = ["diff", "-u", ref, new]

data ThemeProvider = ThemeProvider
  { name :: Text
  , link :: Text
  , render :: Text -> Maybe Text
  }

allThemes :: [ThemeProvider]
allThemes =
  [ ThemeProvider
      { name = "highlight.js"
      , link = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/default.min.css"
      , render = HighlightJS.renderHaskell
      }
  , ThemeProvider
      { name = "Prism"
      , link = "https://cdnjs.cloudflare.com/ajax/libs/prism/9000.0.1/themes/prism.min.css"
      , render = Prism.renderHaskell
      }
  ]
