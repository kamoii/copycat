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
  = Echo
  deriving (Eq, Show)

{-
標準 selection
-}
main :: IO ()
main = do
  (opts, cmd) <-
    simpleOptions "0.1.0"
                  "X11 clipboard/selection utility."
                  "desc"
                  globalOptsParser $
    do
      addCommand
        "echo"
        "Echo the selection to stdout."
        (const Echo)
        (pure ())

  case cmd of
    Echo -> absurd <$> echoCommand

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
      [ flag' PrimarySelection   (long "primary")
      , flag' SecondarySelection (long "secondary")
      , flag' ClipboardSelection (long "clipboard")
      ]
  in
    GlobalOpts <$> (targetSelection <|> pure ClipboardSelection)
