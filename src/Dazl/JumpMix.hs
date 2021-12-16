-- |
{-# LANGUAGE OverloadedRecordDot #-}

module Dazl.JumpMix where

import Data.Bits (xor, shiftR, (.|.), popCount)
import Data.Word (Word64, Word32)

data JumpMixState = JumpMixState
  { seed  :: {-# UNPACK #-} !Word64,
    gamma :: {-# UNPACK #-} !Word64
  }

new :: Word64 -> JumpMixState
new seed = JumpMixState { seed = seed, gamma = goldenGamma }

goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

jump :: Word64 -> JumpMixState -> JumpMixState
jump n s = s { seed = s.gamma * n }

mix32 :: Word64 -> Word32
mix32 z0 =
  let
    z1, z2 :: Word64
    z1 = (z0 `xor` (z0 `shiftR` 33)) * 0x62a9d9ed799705f5
    z2 = ((z1 `xor` (z1 `shiftR` 28)) * 0xcb24d0a5c88c35b3) `shiftR` 32
  in
    fromIntegral z2

mixGamma :: Word64 -> Word64
mixGamma z0 =
  let
    z1, z2, z3 :: Word64
    z1 = (z0 `xor` (z0 `shiftR` 33)) * 0xff51afd7ed558ccd
    z2 = (z1 `xor` (z1 `shiftR` 33)) * 0xc4ceb9fe1a85ec53
    z3 = (z3 `xor` (z2 `shiftR` 33)) .|. 1

    n :: Int
    n = popCount (z3 `xor` (z3 `shiftR` 1))
  in
    if n < 24
    then z3 `xor` 0xaaaaaaaaaaaaaaaa
    else z3
