-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Sidecar.HashQFD where

import Prelude

import Data.Array (fold, foldl)
import Data.Char (toCharCode)
import Data.Int.Bits as Bits
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Unfoldable (replicate)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Simple.JSON (writeJSON)

-- Fast, stable signature for a QFD: hash of its canonical JSON
qfdSignature :: QueryFunctionDescription -> String
qfdSignature qfd =
  let json = writeJSON qfd
      h    = djb2 json
      -- Make it an unsigned 32-bit and render as hex-ish string
      u    = Bits.zshr h 0
  in "qfd-" <> toBase16 u

-- djb2 string hash (32-bit)
djb2 :: String -> Int
djb2 s =
  foldl
    (\h ch -> (Bits.shl h 5 + h) + toCharCode ch) 5381
    (CU.toCharArray s)

-- Render an Int as lowercase hex (8 digits)
toBase16 :: Int -> String
toBase16 x =
  let hex = toHex32 x
  in if String.length hex < 8 then fold (replicate (8 - String.length hex) "0") <> hex else hex

-- Minimal hex encoder for 32-bit ints
toHex32 :: Int -> String
toHex32 n =
  let
    nybble i = (Bits.zshr n (i * 4)) Bits..&. 0xF
    digit v = case v of
      0  -> "0"
      1  -> "1"
      2  -> "2"
      3  -> "3"
      4  -> "4"
      5  -> "5"
      6  -> "6"
      7  -> "7"
      8  -> "8"
      9  -> "9"
      10 -> "a"
      11 -> "b"
      12 -> "c"
      13 -> "d"
      14 -> "e"
      15 -> "f"
      _  -> "0"
  in digit (nybble 7) <> digit (nybble 6) <> digit (nybble 5) <> digit (nybble 4)
   <> digit (nybble 3) <> digit (nybble 2) <> digit (nybble 1) <> digit (nybble 0)
