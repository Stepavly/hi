module HW3.Pretty
  (
    prettyValue
  ) where

import qualified Data.ByteString as BS (ByteString, unpack)
import Data.Map.Strict (assocs, lookup)
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import HW3.Base (HiAction (..), HiValue (..))
import HW3.BaseUtility (funNamesMap)
import Prettyprinter (Doc, Pretty (pretty), viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

-- | Prettifies `q` + `r` / `den` ratio.
prettyMixedFraction :: Integer -> Integer -> Integer -> Doc AnsiStyle
prettyMixedFraction q r den
  | q == 0 = pretty r <> pretty "/" <> pretty den
  | r < 0 = pretty q <> pretty "-" <> pretty (abs r) <> pretty "/" <> pretty den
  | otherwise = pretty q <> pretty "+" <> pretty r <> pretty "/" <> pretty den

-- | Prettifies `Data.ByteString.ByteString`.
prettyByteString :: BS.ByteString -> Doc AnsiStyle
prettyByteString s = foldl (<+>) (pretty "[#") (map (\x -> pretty (printf "%02x" x :: String)) (BS.unpack s) ++ [pretty "#]"])

-- | Prettifies `HiValue`.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) =
  let den = denominator n
      (q, r) = quotRem (numerator n) den
  in if r == 0
     then pretty q
     else case fromRationalRepetendUnlimited n of
       (s, Nothing) -> pretty $ formatScientific Fixed Nothing s -- finite length fraction
       (_, Just _)  -> prettyMixedFraction q r den -- mixed or proper fraction
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString s) = viaShow s
prettyValue (HiValueFunction f) = pretty (Data.Map.Strict.lookup f funNamesMap)
prettyValue (HiValueList list) = 
  pretty "[ " <> (if null list then pretty "" else foldl1 (\a b -> a <> pretty ", " <> b) (fmap prettyValue list)) <> pretty " ]"

prettyValue (HiValueBytes s) = prettyByteString s
prettyValue (HiValueTime time) = pretty "parse-time(\"" <> viaShow time <> pretty "\")"
prettyValue (HiValueDict dict) =
  pretty "{ " <> (if null dict then pretty "" else prettyDict) <> pretty " }"
  where
    prettyDict :: Doc AnsiStyle
    prettyDict = foldl1 (\a b -> a <> pretty ", " <> b) (map (\(a, b) -> prettyValue a <> pretty ": " <> prettyValue b) (assocs dict))

prettyValue (HiValueAction (HiActionRead path)) = pretty "read(" <> viaShow path <> pretty ")"
prettyValue (HiValueAction (HiActionWrite path content)) = pretty "write(" <> viaShow path <> pretty ", " <> prettyByteString content <> pretty ")"
prettyValue (HiValueAction (HiActionMkDir dir)) = pretty "mkdir(" <> viaShow dir <> pretty ")"
prettyValue (HiValueAction (HiActionChDir dir)) = pretty "cd(" <> viaShow dir <> pretty ")"
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionRand a b)) = pretty "rand(" <> pretty a <> pretty ", " <> pretty b <> pretty ")"
prettyValue (HiValueAction (HiActionEcho s)) = pretty "echo(" <> viaShow s <> pretty ")"
