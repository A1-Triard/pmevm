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

data UI = UI
  { uiWindow :: !Window
--  , uiPort0 :: !(Vector Stack)
--  , uiPort1 :: !(Vector Stack)
--  , uiPort2 :: !(Vector Stack)
--  , uiKeyboard :: !(Vector Button)
--  , uiReset :: !Button
--  , uiMC :: !Button
--  , uiSbs :: !Switch
  }

buildUI :: String -> IO UI
buildUI file = do
  b <- builderNew
  builderAddFromFile b file
  window <- builderGetObject b castToWindow ("applicationWindow" :: S.Text)
--  port0 <- V.generateM 8 $ \i -> builderGetObject b castToStack ("port0_" <> ST.pack (show i))
--  port1 <- V.generateM 8 $ \i -> builderGetObject b castToStack ("port1_" <> ST.pack (show i))
--  port2 <- V.generateM 8 $ \i -> builderGetObject b castToStack ("port2_" <> ST.pack (show i))
--  keys <- V.generateM 16 $ \i -> builderGetObject b castToButton ("k" <> ST.pack (show i))
--  reset <- builderGetObject b castToButton ("reset" :: S.Text)
--  mc <- builderGetObject b castToButton ("mc" :: S.Text)
--  sbs <- builderGetObject b castToSwitch ("s-b-s" :: S.Text)
  return $ UI window --port0 port1 port2 keys reset mc -- sbs

gmevm :: IO ()
gmevm = do
  void initGUI
  ui <- buildUI =<< getDataFileName "ui.glade"
  void $ on (uiWindow ui) deleteEvent $ tryEvent $ lift mainQuit
  widgetShowAll (uiWindow ui)
  mainGUI
