module Main where

import Prelude()
import Relude
--
import Lib
--
import Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { targetSelection :: PredefinedSelection
  } deriving Show

data Command
  = Cat
  deriving (Eq, Show)

{-
標準 selection
-}
main :: IO ()
main = do
  (opts, cmd) <-
    simpleOptions "0.1.0"
                  "X11 clipboard/selection utility."
                  ""
                  globalOptsParser $
    do
      addCommand
        "cat"
        "Echo the selection to stdout."
        (const Cat)
        (pure ())

  case cmd of
    Cat -> absurd <$> catCommand (targetSelection opts)

{-| グローバルオプションのパーサ

TODO: 複数指定した場合のエラーが分かりにくい。
例えば --primary --secondary と指定した場合、エラーメッセージは

  Invalid option `--secondary'

となる。欲しいエラーメッセージは「primary/secondary/clipboard から一つ選んで」というもの。

TODO: helpでの表示が微妙

Usage: copycat [--version] [--help] ([--primary] | [--secondary] | [--clipboard]) COMMAND

どっちかっていうと [--primary | --secondary | --clipboard] として欲しい。

-}
globalOptsParser :: Parser GlobalOpts
globalOptsParser =
  let
    targetSelection :: Parser PredefinedSelection
    targetSelection = asum
      [ flag' PrimarySelection   $ long "primary" <> help "operate on the PRIMARY selection."
      , flag' SecondarySelection $ long "secondary" <> help "operate on the SECONDARY selection."
      , flag' ClipboardSelection $ long "clipboard" <> help "operate on the CLIPBOARD selection (default)."
      ]
  in
    GlobalOpts <$> (targetSelection <|> pure ClipboardSelection)
