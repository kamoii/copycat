{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( catCommand
  , PredefinedSelection(..)
  ) where

import Prelude()
import Relude
import Control.Monad.Loops
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign.C.Types (CUChar(..))
import Foreign.Marshal.Array (withArrayLen)
import qualified Data.ByteString as BS
import UnliftIO.Exception

data PredefinedSelection
  = PrimarySelection
  | SecondarySelection
  | ClipboardSelection
  deriving (Eq, Show)

selectionName :: PredefinedSelection -> String
selectionName PrimarySelection = "PRIMARY"
selectionName SecondarySelection = "SECONDARY"
selectionName ClipboardSelection = "CLIPBOARD"

{-
連続読み出しモード

2019/06/23
X11 clipboardとの連携時のemacs の yank の挙動。
もし clipboad から none が返った場合、emacs自体の ring からの値が使われる。
-}
catCommand :: PredefinedSelection -> IO Void
catCommand psel = withDisplay $ \display -> withSimpleWindow display $ \window -> do
  sel <- internAtom display (selectionName psel) False
  flip iterateM_ Nothing $ \content' -> do
    beOwnerTill content' display window sel
    txtMaybe <- readSelectionText display window sel
    case txtMaybe of
      Just txt -> putTextLn txt *> pure (Just txt)
      Nothing -> pure Nothing

withDisplay =
  bracket (openDisplay "") closeDisplay

withSimpleWindow display =
  bracket
    (createSimpleWindow display (defaultRootWindow display) 0 0 1 1 0 0 0)
    (destroyWindow display)

{-
現在 UTF8_STRING固定。なので画像入れられても何もできない。
Nothing になるケースは何だ？
-}
readSelectionText :: Display -> Window -> Atom -> IO (Maybe Text)
readSelectionText display window sel = do
  inp <- internAtom display "clipboard_get" False
  target <- internAtom display "UTF8_STRING" True
  xConvertSelection display sel target inp window currentTime
  untilJust $ do
    ev <- getNextEvent display
    if ev_event_type ev == selectionNotify
      then fmap (Just . decodeUtf8) <$> getWindowPropertyBS display inp window
      else pure Nothing

{-
OWNER権を取得した後に奪わえるまで待つ。
Request に対しては取りあえず何も反応しない。
-}
beOwnerTill :: Maybe Text -> Display -> Window -> Atom -> IO ()
beOwnerTill content' display window sel = do
  xSetSelectionOwner display sel window currentTime
  untilJust $ do
    ev <- getNextEvent display
    case ev of
      SelectionClear { ev_selection } ->
        pure $ Just ()
      SelectionRequest {} -> do
        let handler = maybe sendNoContent serveContent content'
        handler display ev
        pure Nothing
      _ ->
        pure Nothing

{-| リクエストに対応する。

 * 要求するターゲット(ev_target) がこちらが返せる形式か
 * ev_property が None の可能性があり(/* Property is set to None by "obsolete" clients. */)

-}
serveContent
  :: Text
  -> Display
  -> Event
  -> IO ()
serveContent content display ev@SelectionRequest{ .. } = do
  utf8 <- internAtom display "UTF8_STRING" False
  if
    | ev_property == none -> do
        sendSelectionNotifyEvent display ev none
    | ev_target == utf8 -> do
        void $ withArrayLen (bsToCUChars $ encodeUtf8 content) $ \len strPtr ->
          xChangeProperty
            display
            ev_requestor
            ev_property
            utf8 8 propModeReplace strPtr (fromIntegral len)
        sendSelectionNotifyEvent display ev ev_property
    | otherwise -> do
        sendSelectionNotifyEvent display ev none

sendNoContent :: Display -> Event -> IO ()
sendNoContent display ev = sendSelectionNotifyEvent display ev none

sendSelectionNotifyEvent
  :: Display
  -> Event
  -> Atom
  -> IO ()
sendSelectionNotifyEvent display SelectionRequest{ .. } prop =
  allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev ev_requestor ev_selection ev_target prop ev_time
    sendEvent display ev_requestor False 0 ev

-- TODO: これは何をしている？
-- https://hackage.haskell.org/package/X11-1.9/docs/Graphics-X11-Xlib-Event.html
-- イベントって window単位ではなくて、display単位なんだ...
getNextEvent :: Display -> IO Event
getNextEvent display = allocaXEvent $ \ev -> do
  nextEvent display ev
  getEvent ev

-- 何故か Xlib では BS として読み出す方法がないので
getWindowPropertyBS :: Display -> Atom -> Window -> IO (Maybe ByteString)
getWindowPropertyBS d a w = do
  words <- rawGetWindowProperty 8 d a w
  pure $ BS.pack . map (\(CUChar w8) -> w8) <$> words

bsToCUChars :: ByteString -> [CUChar]
bsToCUChars = BS.unpack >>> map CUChar

{-
newtype CChar = CChar Int8
-}


{-
setClipboardString :: String -> IO ()
setClipboardString str = do
    (display, window, clipboards) <- initialSetup
    mapM_ (\atom -> xSetSelectionOwner display atom window currentTime) clipboards
    void $ forkProcess $ do
        hClose stdin
        hClose stdout
        hClose stderr
        setCurrentDirectory "/"
        advertiseSelection display clipboards (stringToChars str)
        cleanup display window

advertiseSelection :: Display -> [Atom] -> [CUChar] -> IO ()
advertiseSelection display clipboards' str = allocaXEvent (go clipboards')
  where
    go [] _ = return ()
    go clipboards evPtr = do
      nextEvent display evPtr
      ev <- getEvent evPtr
      case ev of
          SelectionRequest {..} -> do
              target' <- getAtomName display ev_target
              res <- handleOutput display ev_requestor ev_property target' str
              sendSelectionNotify display ev_requestor ev_selection ev_target res ev_time
              go clipboards evPtr

 #if MIN_VERSION_X11(1,8,0)
          SelectionClear {..} -> go (filter (/= ev_selection) clipboards) evPtr
 #else
          _ | ev_event_type ev == selectionClear -> do
              target <- peekByteOff evPtr 40 :: IO Atom
              go (filter (/= target) clipboards) evPtr
 #endif
          _ -> go clipboards evPtr


sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev req sel target prop time
    sendEvent display req False 0 ev

stringToChars :: String -> [CUChar]
stringToChars = map fromIntegral . encode
-}
