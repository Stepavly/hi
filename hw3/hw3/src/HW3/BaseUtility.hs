module HW3.BaseUtility
  (
    funNames
  , funNamesMap
  ) where

import Data.Map.Strict (Map, fromList)
import HW3.Base (HiFun (..))

-- | List of pairs (`HiFun`, name of `HiFun` used in interpreter).
funNames :: [(HiFun, String)]
funNames = [
  (HiFunDiv, "div"),
  (HiFunMul, "mul"),
  (HiFunAdd, "add"),
  (HiFunSub, "sub"),
  (HiFunAnd, "and"),
  (HiFunOr, "or"),
  (HiFunLessThan, "less-than"),
  (HiFunGreaterThan, "greater-than"),
  (HiFunEquals, "equals"),
  (HiFunNotLessThan, "not-less-than"),
  (HiFunNotGreaterThan, "not-greater-than"),
  (HiFunNotEquals, "not-equals"),
  (HiFunNot, "not"),
  (HiFunIf, "if"),
  (HiFunLength, "length"),
  (HiFunToUpper, "to-upper"),
  (HiFunToLower, "to-lower"),
  (HiFunReverse, "reverse"),
  (HiFunTrim, "trim"),
  (HiFunList, "list"),
  (HiFunRange, "range"),
  (HiFunFold, "fold"),
  (HiFunPackBytes, "pack-bytes"),
  (HiFunUnpackBytes, "unpack-bytes"),
  (HiFunEncodeUtf8, "encode-utf8"),
  (HiFunDecodeUtf8, "decode-utf8"),
  (HiFunZip, "zip"),
  (HiFunUnzip, "unzip"),
  (HiFunSerialise, "serialise"),
  (HiFunDeserialise, "deserialise"),
  (HiFunRead, "read"),
  (HiFunWrite, "write"),
  (HiFunMkDir, "mkdir"),
  (HiFunChDir, "cd"),
  (HiFunParseTime, "parse-time"),
  (HiFunRand, "rand"),
  (HiFunEcho, "echo"),
  (HiFunCount, "count"),
  (HiFunKeys, "keys"),
  (HiFunValues, "values"),
  (HiFunInvert, "invert")]

-- | `Map` from `HiFun` to name used in interpreter.
funNamesMap :: Map HiFun String
funNamesMap = fromList funNames
