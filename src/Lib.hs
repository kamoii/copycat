{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Prelude()
import Relude
import Control.Monad.Loops
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign.C.Types (CUChar(..))
import qualified Data.ByteString as BS
import UnliftIO.Exception

{-
連続読み出しモード

2019/06/23
X11 clipboardとの連携時のemacs の yank の挙動。
もし clipboad から none が返った場合、emacs自体の ring からの値が使われる。
-}
copycat :: IO Void
copycat = withDisplay $ \display -> withSimpleWindow display $ \window -> do
  sel <- internAtom display "CLIPBOARD" False
  forever $ do
    beOwnerTill display window sel
    txtMaybe <- readSelectionText display window sel
    case txtMaybe of
      Just txt -> putTextLn txt
      Nothing -> pure ()

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
beOwnerTill :: Display -> Window -> Atom -> IO ()
beOwnerTill display window sel = do
  xSetSelectionOwner display sel window currentTime
  untilJust $ do
    ev <- getNextEvent display
    case ev of
      SelectionClear { ev_selection } ->
        pure $ Just ()
      SelectionRequest {} -> do
        -- owner 中にリクエストが来た場合は無視
        rejectSelectionRequest display ev
        pure Nothing
      _ ->
        pure Nothing

-- SelectionRequest {..} -> do
--     -- target' <- getAtomName display ev_target
--     -- res <- handleOutput display ev_requestor ev_property target' str
--     -- sendSelectionNotify display ev_requestor ev_selection ev_target res ev_time
--     -- go clipboards evPtr

{-
拒否する場合は roperty に None を指定した SelectionNotify イベントをリ
クエスト元に送信する。
-}
rejectSelectionRequest
  :: Display
  -> Event
  -> IO ()
rejectSelectionRequest
  display
  SelectionRequest{ ev_requestor, ev_selection, ev_target, ev_time } =
  allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev ev_requestor ev_selection ev_target none ev_time
    sendEvent display ev_requestor False 0 ev
rejectSelectionRequest _ _ =
  throwString "??"

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

handleOutput :: Display -> Window -> Atom -> Maybe String -> [CUChar] -> IO Atom
handleOutput display req prop (Just "UTF8_STRING") str = do
    prop' <- getAtomName display prop
    if isNothing prop' then handleOutput display req prop Nothing str else do
        target <- internAtom display "UTF8_STRING" True
        void $ withArrayLen str $ \len str' ->
            xChangeProperty display req prop target 8 propModeReplace str'
                            (fromIntegral len)
        return prop
handleOutput _ _ _ _ _ = return none

sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev req sel target prop time
    sendEvent display req False 0 ev

stringToChars :: String -> [CUChar]
stringToChars = map fromIntegral . encode
-}
