module Data.Pmevm.Keyboard.Native where

#include <haskell>
import Data.Pmevm

data Keyboard = Keyboard
  { key0 :: Bool
  , key1 :: Bool
  , key2 :: Bool
  , key3 :: Bool
  , key4 :: Bool
  , key5 :: Bool
  , key6 :: Bool
  , key7 :: Bool
  , keyHB :: Bool
  , keyLB :: Bool
  , keyX :: Bool
  , keyP :: Bool
  , keyF1 :: Bool
  , keyF2 :: Bool
  , keyF3 :: Bool
  , keyF4 :: Bool
  }

initKeyboard :: Keyboard
initKeyboard = Keyboard False False False False False False False False False False False False False False False False

keyboardStep :: Keyboard -> Computer -> Computer
keyboardStep k c =
  let i = getPortOut 3 c in
  let i0 = (i .&. 0x01) == 0 in
  let i1 = (i .&. 0x02) == 0 in
  let i2 = (i .&. 0x04) == 0 in
  let i3 = (i .&. 0x08) == 0 in
  let o0 = if i0 && key3 k || i1 && key7 k || i2 && keyP k || i3 && keyF4 k then 0 else 1 in
  let o1 = if i0 && key2 k || i1 && key6 k || i2 && keyX k || i3 && keyF3 k then 0 else 1 in
  let o2 = if i0 && key1 k || i1 && key5 k || i2 && keyLB k || i3 && keyF2 k then 0 else 1 in
  let o3 = if i0 && key0 k || i1 && key4 k || i2 && keyHB k || i3 && keyF1 k then 0 else 1 in
  let o = (o3 `shift` 3) .|. (o2 `shift` 2) .|. (o1 `shift` 1) .|. o0 in
  setPortIn 3 o c
