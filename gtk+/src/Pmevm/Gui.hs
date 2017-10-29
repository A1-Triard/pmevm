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
--  , uiReset :: !Button
--  , uiMC :: !Button
--  , uiSbs :: !Switch
  }

buildUI :: String -> IO UI
buildUI file = do
  b <- builderNew
  builderAddFromFile b file
  window <- builderGetObject b castToWindow ("applicationWindow" :: S.Text)
  p0 <- V.generateM 8 $ \i -> builderGetObject b castToStack ("port0_" <> ST.pack (show i))
  p1 <- V.generateM 8 $ \i -> builderGetObject b castToStack ("port1_" <> ST.pack (show i))
  p2 <- V.generateM 8 $ \i -> builderGetObject b castToStack ("port2_" <> ST.pack (show i))
  keys <- V.generateM 16 $ \i -> builderGetObject b castToToggleButton ("k" <> ST.pack (show i))
--  reset <- builderGetObject b castToButton ("reset" :: S.Text)
--  mc <- builderGetObject b castToButton ("mc" :: S.Text)
--  sbs <- builderGetObject b castToSwitch ("s-b-s" :: S.Text)
  return $ UI window p0 p1 p2 keys-- reset mc -- sbs

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
  updatePort (uiPort0 ui) (port0 c)
  updatePort (uiPort1 ui) (port1 c)
  updatePort (uiPort2 ui) (port2 c)
  keyboard <- uiKeyboard ui
  let
    ComputerState cn ct
      = fromMaybe (error "frame_")
      $ listToMaybe
      $ dropWhile ((< ticks) . sTicks)
      $ iterate (computerStep keyboard) $ ComputerState c (view dStartTicks dx)
  modifyIORef' d $ set dComputer cn . set dStartTime t . set dStartTicks (ct - ticks)

gmevm :: IO ()
gmevm = do
  void initGUI
  ui <- buildUI =<< getDataFileName "ui.glade"
  void $ on (uiWindow ui) deleteEvent $ tryEvent $ lift mainQuit
  let comp = setProgram monitor initComputer
  t <- getTime Monotonic
  d <- newIORef $ UIData (ComputerWithPorts comp 0 0 0 0) t 0
  _ <- timeoutAdd (frame ui d >> return True) $ round (1000.0 / fps)
  widgetShowAll (uiWindow ui)
  mainGUI
