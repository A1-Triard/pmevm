module Data.Pmevm.Monitor.Native where

#include <haskell>
import Data.Pmevm

monitor :: Program
monitor = Program
  [ (0o014, 0o000, 0o076, "          MVI A 000Q        ")
  , (0o014, 0o001, 0o000, "                            ")
  , (0o014, 0o002, 0o026, "          MVI D 024Q        ")
  , (0o014, 0o003, 0o024, "                            ")
  , (0o014, 0o004, 0o202, "      M1: ADD D             ")
  , (0o014, 0o005, 0o025, "          DCR D             ")
  , (0o014, 0o006, 0o302, "          JNZ M1            ")
  , (0o014, 0o007, 0o004, "                            ")
  , (0o014, 0o010, 0o014, "                            ")
  , (0o014, 0o011, 0o323, "          OUT 000Q          ")
  , (0o014, 0o012, 0o000, "                            ")
  , (0o014, 0o013, 0o166, "          HLT               ")
  ]
