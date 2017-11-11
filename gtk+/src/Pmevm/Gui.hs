--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Pmevm.Gui
  ( gmevm
  ) where

#include <haskell>
import Paths_pmevm_gtk
import Data.Pmevm
import Data.Pmevm.Keyboard
import Data.Pmevm.Monitor

data UI = UI
  { uiWindow :: !Window
  , uiPort0 :: !(Vector Stack)
  , uiPort1 :: !(Vector Stack)
  , uiPort2 :: !(Vector Stack)
  , uiKeys :: !(Vector ToggleButton)
  , uiReset :: !Button
--  , uiMC :: !Button
--  , uiSbs :: !Switch
  }

builderGet :: GObject a => Builder -> (ManagedPtr a -> a) -> S.Text -> IO a
builderGet b casting t = builderGetObject b t >>= unsafeCastTo casting . fromMaybe (error $ ST.unpack t)

buildUI :: FilePath -> IO UI
buildUI file = do
  b <- builderNewFromFile $ ST.pack file
  window <- builderGet b Window "applicationWindow"
  p0 <- V.generateM 8 $ \i -> builderGet b Stack ("port0_" <> ST.pack (show i))
  p1 <- V.generateM 8 $ \i -> builderGet b Stack ("port1_" <> ST.pack (show i))
  p2 <- V.generateM 8 $ \i -> builderGet b Stack ("port2_" <> ST.pack (show i))
  keys <- V.generateM 16 $ \i -> builderGet b ToggleButton ("k" <> ST.pack (show i))
  r <- builderGet b Button "reset"
--  mc <- builderGetObject b castToButton ("mc" :: S.Text)
--  sbs <- builderGetObject b castToSwitch ("s-b-s" :: S.Text)
  return $ UI window p0 p1 p2 keys r-- mc sbs

uiKeyboard :: UI -> IO Keyboard
uiKeyboard ui = V.ifoldM' (\k i b -> ($ k) <$> set (key i) <$> toggleButtonGetActive b) initKeyboard (uiKeys ui)

fps :: Double
fps = 50.0

clockSpeedInMHz :: Int64
clockSpeedInMHz = 1

passedTicks :: TimeSpec -> IO (TimeSpec, Int64)
passedTicks t0 = do
  t <- getTime Monotonic
  return (t, clockSpeedInMHz * ((sec t - sec t0) * 1000000 + (nsec t - nsec t0) `div` 1000))

data UIData = UIData
  { _dComputer :: !ComputerWithPorts
  , _dStartTime :: !TimeSpec
  , _dStartTicks :: !Int64
  }
makeLenses ''UIData

updatePort :: Vector Stack -> Word8 -> IO ()
updatePort p v = do
  forM_ [0 .. 7] $ \i -> do
     let s = fromMaybe (error "frame") $ p !? i
     K.set s [stackVisibleChildName := if testBit v i then "1" else "0"]

data ComputerState = ComputerState !ComputerWithPorts !Int64
sTicks :: ComputerState -> Int64
sTicks (ComputerState _ t) = t

computerStep :: Keyboard -> ComputerState -> ComputerState
computerStep k (ComputerState c t) =
  let (cn, d) = keyboardStep k c in
  ComputerState cn (t + d)

frame :: UI -> IORef UIData -> IO ()
frame ui d = do
  dx <- readIORef d
  (t, ticks) <- passedTicks $ view dStartTime dx
  let c = view dComputer dx
  updatePort (uiPort0 ui) (view port0 c)
  updatePort (uiPort1 ui) (view port1 c)
  updatePort (uiPort2 ui) (view port2 c)
  keyboard <- uiKeyboard ui
  let
    ComputerState cn ct
      = fromMaybe (error "frame_")
      $ listToMaybe
      $ dropWhile ((< ticks) . sTicks)
      $ iterate (computerStep keyboard) $ ComputerState c (view dStartTicks dx)
  modifyIORef' d $ set dComputer cn . set dStartTime t . set dStartTicks (ct - ticks)

keyEnterValue :: Word32
keyEnterValue = 0xFF00 + 13

keySpaceValue :: Word32
keySpaceValue = 32

data KeyData = KeyData { _kdButton :: !Bool, _kdEnter :: !Bool, _kdSpace :: !Bool, _kdGesture :: !Bool }
makeLenses ''KeyData

updateKey :: ToggleButton -> IORef KeyData -> (KeyData -> KeyData) -> IO ()
updateKey b d t = do
  modifyIORef' d t
  toggleButtonSetActive b =<< (\x -> view kdButton x || view kdEnter x || view kdSpace x || view kdGesture x) <$> readIORef d

buttonPress :: ToggleButton -> IORef KeyData -> EventButton -> IO ()
buttonPress b d e = do
  1 <- getEventButtonButton e
  updateKey b d $ set kdButton True

buttonRelease :: ToggleButton -> IORef KeyData -> EventButton -> IO ()
buttonRelease b d e = do
  1 <- getEventButtonButton e
  updateKey b d $ set kdButton False

keyPress :: ToggleButton -> IORef KeyData -> EventKey -> IO ()
keyPress b d e = do
  k <- getEventKeyKeyval e
  t <- case k of
    x | x == keyEnterValue -> return kdEnter
    x | x == keySpaceValue -> return kdSpace
    _ -> mzero
  updateKey b d $ set t True

keyRelease :: ToggleButton -> IORef KeyData -> EventKey -> IO ()
keyRelease b d e = do
  k <- getEventKeyKeyval e
  t <- case k of
    x | x == keyEnterValue -> return kdEnter
    x | x == keySpaceValue -> return kdSpace
    _ -> mzero
  updateKey b d $ set t False

keyGesturePress :: ToggleButton -> IORef KeyData -> Word32 -> EventKey -> IO ()
keyGesturePress b d key_value e = do
  k <- getEventKeyKeyval e
  case k of
    x | x == key_value -> return ()
    _ -> mzero
  updateKey b d $ set kdGesture True

keyGestureRelease :: ToggleButton -> IORef KeyData -> Word32 -> EventKey -> IO ()
keyGestureRelease b d key_value e = do
  k <- getEventKeyKeyval e
  case k of
    x | x == key_value -> return ()
    _ -> mzero
  updateKey b d $ set kdGesture False

resetCpu :: IORef UIData -> IO ()
resetCpu d = modifyIORef' d $ over (dComputer . computer) $ setPC 0

tryEvent :: IO () -> IO Bool
tryEvent handler
  = (flip $ catchJust $ \e -> if isUserError e && ioeGetErrorString e == "mzero" then Just () else Nothing) (const $ return False)
  $ (flip catch) (\e -> let _ = e :: PatternMatchFail in return False)
  $ handler >> return True

gmevm :: IO ()
gmevm = do
  a0 <- getProgName
  av <- getArgs
  _ <- K.init $ Just $ ST.pack <$> (a0 : av)
  ui <- buildUI =<< getDataFileName "ui.glade"
  void $ onWidgetDeleteEvent (uiWindow ui) $ \_ -> mainQuit >> return True
  V.forM_ (uiKeys ui) $ \k -> do
    kd <- newIORef $ KeyData False False False False
    void $ onWidgetButtonPressEvent k $ \e -> tryEvent $ buttonPress k kd e
    void $ onWidgetButtonReleaseEvent k $ \e -> tryEvent $ buttonRelease k kd e
    void $ onWidgetKeyPressEvent k $ \e -> tryEvent $ keyPress k kd e
    void $ onWidgetKeyReleaseEvent k $ \e -> tryEvent $ keyRelease k kd e
    code :: Word32 <- (read . ST.unpack) <$> widgetGetName k
    void $ onWidgetKeyPressEvent (uiWindow ui) $ \e -> tryEvent $ keyGesturePress k kd code e
    void $ onWidgetKeyReleaseEvent (uiWindow ui) $ \e -> tryEvent $ keyGestureRelease k kd code e
  let comp = initPorts $ setProgram monitor initComputer
  t <- getTime Monotonic
  d <- newIORef $ UIData comp t 0
  void $ onButtonClicked (uiReset ui) $ resetCpu d
  _ <- timeoutAdd PRIORITY_HIGH (round $ 1000.0 / fps) (frame ui d >> return True)
  widgetShowAll (uiWindow ui)
  K.main
