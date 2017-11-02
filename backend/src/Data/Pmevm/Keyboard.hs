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

module Data.Pmevm.Keyboard
  ( Keyboard
  , key0
  , key1
  , key2
  , key3
  , key4
  , key5
  , key6
  , key7
  , key8
  , key9
  , keyA
  , keyB
  , keyC
  , keyD
  , keyE
  , keyF
  , initKeyboard
  , key
  , ComputerWithPorts
  , initPorts
  , computer
  , port0
  , port1
  , port2
  , keyboardStep
  ) where

#include <haskell>
import Data.Pmevm

data Keyboard = Keyboard
  { _key0 :: Bool
  , _key1 :: Bool
  , _key2 :: Bool
  , _key3 :: Bool
  , _key4 :: Bool
  , _key5 :: Bool
  , _key6 :: Bool
  , _key7 :: Bool
  , _key8 :: Bool
  , _key9 :: Bool
  , _keyA :: Bool
  , _keyB :: Bool
  , _keyC :: Bool
  , _keyD :: Bool
  , _keyE :: Bool
  , _keyF :: Bool
  }
makeLenses ''Keyboard

initKeyboard :: Keyboard
initKeyboard = Keyboard False False False False False False False False False False False False False False False False

key :: Int -> Lens' Keyboard Bool
key 0 = key0
key 1 = key1
key 2 = key2
key 3 = key3
key 4 = key4
key 5 = key5
key 6 = key6
key 7 = key7
key 8 = key8
key 9 = key9
key 10 = keyA
key 11 = keyB
key 12 = keyC
key 13 = keyD
key 14 = keyE
key 15 = keyF
key _ = error "key"

data ComputerWithPorts = ComputerWithPorts
  { _computer :: !Computer
  , _port0 :: !Word8
  , _port1 :: !Word8
  , _port2 :: !Word8
  , _port3 :: !Word8
  }
makeLenses ''ComputerWithPorts

initPorts :: Computer -> ComputerWithPorts
initPorts c = ComputerWithPorts c 0 0 0 0

keyboard :: Keyboard -> Word8 -> Word8
keyboard k i =
  let i0 = (i .&. 0x01) == 0 in
  let i1 = (i .&. 0x02) == 0 in
  let i2 = (i .&. 0x04) == 0 in
  let i3 = (i .&. 0x08) == 0 in
  let o0 = if i0 && view key3 k || i1 && view key2 k || i2 && view key1 k || i3 && view key0 k then 0 else 1 in
  let o1 = if i0 && view key7 k || i1 && view key6 k || i2 && view key5 k || i3 && view key4 k then 0 else 1 in
  let o2 = if i0 && view keyB k || i1 && view keyA k || i2 && view key9 k || i3 && view key8 k then 0 else 1 in
  let o3 = if i0 && view keyF k || i1 && view keyE k || i2 && view keyD k || i3 && view keyC k then 0 else 1 in
  (o3 `shift` 3) .|. (o2 `shift` 2) .|. (o1 `shift` 1) .|. o0

keyboardStep :: Keyboard -> ComputerWithPorts -> (ComputerWithPorts, Int64)
keyboardStep k c =
  let (cn, po, t) = cpuStep (\n -> if n == 3 then keyboard k (view port3 c) else 0) (view computer c) in
  case po of
    Just (PortOut 0 v) -> (set computer cn $ set port0 v c, t)
    Just (PortOut 1 v) -> (set computer cn $ set port1 v c, t)
    Just (PortOut 2 v) -> (set computer cn $ set port2 v c, t)
    Just (PortOut 3 v) -> (set computer cn $ set port3 v c, t)
    _ -> (set computer cn c, t)
