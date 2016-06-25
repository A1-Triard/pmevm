module Data.Pmevm
  ( CPURegister (..)
  , CPURegisterExt (..)
  , CPURegisterPair (..)
  , CPURegisterWord (..)
  , CPUCondition (..)
  , CPUOperation (..)
  , operationCode
  , cpuOperation
  , cpuStep
  , Computer
  , initComputer
  , hl
  , getPort
  , setPort
  , getMemory
  , setMemory
  , isCPUHalted
  , areInterruptsEnabled
  , getPC
  , setPC
  , getRegister
  , getFlag
  ) where

import Data.Pmevm.Native
