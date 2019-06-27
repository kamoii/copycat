module Main where

import Prelude()
import Relude
--
import Lib
--
import Options.Applicative.Simple

data PredefinedSelection
  = PrimarySelection
  | SecondarySelection
  | ClipboardSelection
  deriving (Eq, Show)

data GlobalOpts = GlobalOpts
  { targetSelections :: [PredefinedSelection]
  } deriving Show

data Command
  = Echo
  deriving Show

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

  print opts
  print cmd
  -- absurd <$> copycat

globalOptsParser :: Parser GlobalOpts
globalOptsParser =
  let
    targetSelections = fmap catMaybes $ sequenceA $ map optional
      [ flag' PrimarySelection   (long "primary")
      , flag' SecondarySelection (long "secondary")
      , flag' ClipboardSelection (long "clipboard")
      ]
  in
    GlobalOpts <$> targetSelections
