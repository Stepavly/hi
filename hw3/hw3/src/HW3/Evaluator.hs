{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW3.Evaluator
  (
    eval
  ) where

import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression,
                               compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (foldl1')
import qualified Data.Map.Strict as DM
import Data.Ratio (denominator, numerator)
import Data.Semigroup (Semigroup (..))
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW3.Base
import Text.Read (readMaybe)
import Prelude hiding (seq)

type HiErrorMonad m a = ExceptT HiError m a

-- | Helper function for evaluating binary functions.
bifunc :: HiMonad m => (HiValue -> HiValue -> HiErrorMonad m HiValue) -> [HiExpr] -> HiErrorMonad m HiValue
bifunc f [a, b] = do
  a' <- evalExcept a
  b' <- evalExcept b
  f a' b'
bifunc _ _ = throwE HiErrorArityMismatch

-- | Helper function for evaluating unary functions.
unaryfunc :: HiMonad m => (HiValue -> HiErrorMonad m HiValue) -> [HiExpr] -> HiErrorMonad m HiValue
unaryfunc f [a] = do
  a' <- evalExcept a
  f a'
unaryfunc _ _ = throwE HiErrorArityMismatch

-- | Helper function for lazy evaluating logical functions.
evalBiBoolFunction :: HiMonad m => Bool -> [HiExpr] -> HiErrorMonad m HiValue
evalBiBoolFunction zero [a, b] = do
  x <- evalExcept a
  xBool <- toBool x
  if xBool == zero
  then return x
  else do evalExcept b
evalBiBoolFunction _ _ = throwE HiErrorArityMismatch

-- | Helper function for evaluating comparing functions.
evalBiValFunction :: HiMonad m => (HiValue -> HiValue -> Bool) -> [HiExpr] -> HiErrorMonad m HiValue
evalBiValFunction f = bifunc $ \x y -> return $ HiValueBool $ f x y

-- | Extracts `Rational` from `HiValue`, if not presented throws `HiErrorInvalidArgument`.
extractNumber :: HiMonad m => HiValue -> HiErrorMonad m Rational
extractNumber (HiValueNumber n) = return n
extractNumber _                 = throwE HiErrorInvalidArgument

-- | Extracts `Int` from `HiValue`, if not presented or not an int throws `HiErrorInvalidArgument`.
extractInt :: HiMonad m => HiValue -> HiErrorMonad m Int
extractInt n = do
  n' <- extractNumber n
  let num = numerator n'
      den = denominator n'
      n'' = num `div` den
  if num `mod` den /= 0 || not (fromIntegral (minBound :: Int) <= n'' && n'' <= fromIntegral (maxBound :: Int))
  then throwE HiErrorInvalidArgument
  else return $ fromInteger n''

-- | Extracts `Data.Text.Text` from `HiValue`, if not presented throws `HiErrorInvalidArgument`.
extractString :: HiMonad m => HiValue -> HiErrorMonad m DT.Text
extractString (HiValueString n) = return n
extractString _                 = throwE HiErrorInvalidArgument

-- | Extracts `Data.Sequence.Seq` from `HiValue`, if not presented throws `HiErrorInvalidArgument`.
extractList :: HiMonad m => HiValue -> HiErrorMonad m (DS.Seq HiValue)
extractList (HiValueList n) = return n
extractList _               = throwE HiErrorInvalidArgument

-- | Extracts `Data.ByteString.ByteString` from `HiValue`, if not presented throws `HiErrorInvalidArgument`.
extractBytes :: HiMonad m => HiValue -> HiErrorMonad m BS.ByteString
extractBytes (HiValueBytes a) = return a
extractBytes _                = throwE HiErrorInvalidArgument

-- | Transforms `HiValue` to boolean.
toBool :: HiMonad m => HiValue -> HiErrorMonad m Bool
toBool = \case
  HiValueBool b -> return b
  HiValueNull   -> return False
  _             -> return True

-- | Evaluates `HiFunAdd`.
evalAdd :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalAdd = bifunc $ \a b -> case a of
    HiValueNumber a' -> HiValueNumber . (a'+) <$> extractNumber b
    HiValueString a' -> HiValueString . (a'<>) <$> extractString b
    HiValueList a'   -> HiValueList . (a' DS.><) <$> extractList b
    HiValueBytes a'  -> HiValueBytes . (a'<>) <$> extractBytes b
    HiValueTime a'   -> HiValueTime . (\b' -> addUTCTime (fromRational b') a') <$> extractNumber b
    _                -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunSub`.
evalSub :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalSub = bifunc $ \a b -> case a of
    HiValueNumber a' -> HiValueNumber . (a'-) <$> extractNumber b
    HiValueTime a' ->
      case b of
        HiValueNumber b' -> return $ HiValueTime $ addUTCTime (fromRational b') a'
        HiValueTime b'   -> return $ HiValueNumber $ toRational $ diffUTCTime a' b'
        _                -> throwE HiErrorInvalidArgument
    _ -> throwE HiErrorInvalidArgument

-- | Helper function to replicate container with custom replicator.
containerReplicate :: HiMonad m => (Int -> a -> a) -> a -> Rational -> HiErrorMonad m a
containerReplicate repl s n = do
  n' <- extractInt (HiValueNumber n)
  if n' <= 0
  then throwE HiErrorInvalidArgument
  else return $ repl n' s

-- | Evaluates `HiFunMul`.
evalMul :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalMul = bifunc $ \a b -> do
  b' <- extractNumber b
  case a of
    HiValueNumber a' -> return $ HiValueNumber (a' * b')
    HiValueString a' -> HiValueString <$> containerReplicate stimes a' b'
    HiValueList a'   -> HiValueList <$> containerReplicate stimes a' b'
    HiValueBytes a'  -> HiValueBytes <$> containerReplicate stimes a' b'
    _                -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunDiv`.
evalDiv :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalDiv = bifunc $ \a b -> case a of
    HiValueNumber a' -> do
      b' <- extractNumber b
      if b' == 0 then throwE HiErrorDivideByZero else return $ HiValueNumber (a' / b')
    HiValueString a' -> HiValueString . ((a' <> DT.pack "/") <>) <$> extractString b
    _ -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunNot`.
evalNot :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalNot = unaryfunc $ \x -> do
  case x of
    HiValueBool b -> return $ HiValueBool (not b)
    _ -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunIf`.
evalIf ::  HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalIf [cond, a, b] = do
  cond' <- evalExcept cond
  case cond' of
    HiValueBool True  -> evalExcept a
    HiValueBool False -> evalExcept b
    _                 -> throwE HiErrorInvalidArgument
evalIf _ = throwE HiErrorArityMismatch

-- | Evaluates `HiFunLength`.
evalLength :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalLength = unaryfunc $ \case
  HiValueString a -> return $ HiValueNumber $ fromIntegral $ DT.length a
  HiValueList a   -> return $ HiValueNumber $ fromIntegral $ DS.length a
  HiValueBytes a  -> return $ HiValueNumber $ fromIntegral $ BS.length a
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunToUpper`.
evalToUpper :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalToUpper = unaryfunc $ \case
  HiValueString a -> return $ HiValueString $ DT.toUpper a
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunToLower`.
evalToLower :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalToLower = unaryfunc $ \case
  HiValueString a -> return $ HiValueString $ DT.toLower a
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunReverse`.
evalReverse :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalReverse = unaryfunc $ \case
  HiValueString a -> return $ HiValueString $ DT.reverse a
  HiValueList a   -> return $ HiValueList $ DS.reverse a
  HiValueBytes a  -> return $ HiValueBytes $ BS.reverse a
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunTrim`.
evalTrim :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalTrim = unaryfunc $ \case
  HiValueString a -> return $ HiValueString $ DT.strip a
  _               -> throwE HiErrorInvalidArgument

-- | Fits value $x$ to range $[low, high]$.
limitToRange :: Int -> Int -> Int
limitToRange len i = min (max i 0) len

-- | Evaluates index for slicing.
evalIndex :: HiMonad m => Int -> HiValue -> Bool -> HiErrorMonad m Int
evalIndex len n isStart =
  case n of
    HiValueNull -> return $ if isStart then 0 else len
    HiValueNumber _ ->  do
      i <- extractInt n
      return $ limitToRange len $ if i < 0 then len + i else i
    _ -> throwE HiErrorInvalidArgument

-- | Evaluates indexing.
evalIndexing :: HiMonad m => HiValue -> Rational -> HiErrorMonad m HiValue
evalIndexing s n = do
  len <- evalLength [HiExprValue s] >>= extractInt
  i <- extractInt (HiValueNumber n)
  if 0 <= i && i < len
  then case s of
    HiValueString s' -> return $ HiValueString $ DT.singleton $ DT.index s' (fromIntegral i)
    HiValueList seq  -> return $ DS.index seq i
    HiValueBytes s'  -> return $ HiValueNumber $ fromIntegral $ BS.index s' (fromIntegral i)
    _                -> throwE HiErrorInvalidFunction -- Actually invalid case because of evaluated `len`
  else return HiValueNull

-- | Evaluates splicing.
evalSplice :: HiMonad m => HiValue -> HiValue -> HiValue -> HiErrorMonad m HiValue
evalSplice s from to = do
  len <- evalLength [HiExprValue s] >>= extractInt
  l <- evalIndex len from True
  r <- evalIndex len to False
  if 0 <= l && l <= r && r <= len
  then case s of
    HiValueString s' -> return $ HiValueString $ DT.drop (fromIntegral l) (DT.take (fromIntegral r) s')
    HiValueList seq -> return $ HiValueList $ DS.drop l (DS.take r seq)
    HiValueBytes s' -> return $ HiValueBytes $ BS.drop (fromIntegral l) (BS.take (fromIntegral r) s')
    _ -> throwE HiErrorInvalidFunction  -- Actually invalid case because of evaluated `len`
  else case s of
    HiValueString _ -> return $ HiValueString DT.empty
    HiValueList _   -> return $ HiValueList DS.empty
    HiValueBytes _  -> return $ HiValueBytes BS.empty
    _               -> return HiValueNull

-- | Evaluates `HiFunList`.
evalList :: HiMonad m => [HiExpr] -> HiErrorMonad m (DS.Seq HiValue)
evalList [] = return DS.empty
evalList (a:args) = do
  args' <- evalList args
  a' <- evalExcept a
  return $ a' DS.<| args'

-- | Evaluates `HiFunRange`.
evalRange :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalRange = bifunc $ \l r -> do
  l' <- extractNumber l
  r' <- extractNumber r
  return $ HiValueList $ HiValueNumber <$> DS.fromList [l'..r']

-- | Evaluates `HiFunFold`.
evalFold :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalFold = bifunc $ \f args ->
  case args of
    HiValueList list ->
      case DS.length list of
        0 -> return HiValueNull
        1 -> return $ DS.index list 0
        _ ->
          let evaledList = map (evalExcept . HiExprValue) (toList list)
          in foldl1' 
            (\a b -> do 
              a' <- a
              b' <- b
              evalExcept (HiExprApply (HiExprValue f) [HiExprValue a', HiExprValue b'])) evaledList
    _ -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunPackBytes`.
evalPackBytes :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalPackBytes = unaryfunc $ \case
  HiValueList s -> packBytes (toList s) <&> (HiValueBytes . BS.pack)
  _             -> throwE HiErrorInvalidArgument
  where
    packBytes :: HiMonad m => [HiValue] -> HiErrorMonad m [Word8]
    packBytes [] = return []
    packBytes (x:xs) = do
      x' <- extractInt x
      xs' <- packBytes xs
      if 0 <= x' && x' <= 255 then return (fromIntegral x' : xs') else throwE HiErrorInvalidArgument

-- | Helper function for evaluating unary `Data.ByteString.ByteString` functions.
evalUnaryBytes :: HiMonad m => (BS.ByteString -> HiErrorMonad m HiValue) -> [HiExpr] -> HiErrorMonad m HiValue
evalUnaryBytes f = unaryfunc $ \case
  HiValueBytes s -> f s
  _              -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunUnpackBytes`.
evalUnpackBytes :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalUnpackBytes = evalUnaryBytes $ \s ->
  return $ HiValueList $ DS.fromList $ BS.foldr' (\a b -> HiValueNumber (fromIntegral a) : b) [] s

-- | Evaluates `HiFunEncodeUtf8`.
evalEncodeUtf8 :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalEncodeUtf8 = unaryfunc $ \case
  HiValueString s -> return $ HiValueBytes $ encodeUtf8 s
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunDecodeUtf8`.
evalDecodeUtf8 :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalDecodeUtf8 = evalUnaryBytes $ \s ->
  case decodeUtf8' s of
    Right s' -> return $ HiValueString s'
    _        -> return HiValueNull

-- | Evaluates `HiFunZip`.
evalZip :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalZip = evalUnaryBytes $ \s ->
  return $ HiValueBytes $ BL.toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } (BL.fromStrict s)

-- | Evaluates `HiFunUnzip`.
evalUnzip :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalUnzip = evalUnaryBytes $ \s -> return $ HiValueBytes $ BL.toStrict $ decompress $ BL.fromStrict s

-- | Evaluates `HiFunSerialise`.
evalSerialise :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalSerialise = unaryfunc (return . HiValueBytes . BL.toStrict . serialise)

-- | Evaluates `HiFunDeserialise`.
evalDeserialise :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalDeserialise = evalUnaryBytes $ \s -> return $ deserialise (BL.fromStrict s)

-- | Evaluates `HiFunRead`.
evalRead :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalRead = unaryfunc $ \case
  HiValueString s -> return $ HiValueAction $ HiActionRead $ DT.unpack s
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunWrite`.
evalWrite :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalWrite = bifunc $ \path s -> do
  path' <- extractString path
  case s of
    HiValueString _ -> do
      bytes <- evalEncodeUtf8 [HiExprValue s] >>= extractBytes
      return $ HiValueAction $ HiActionWrite (DT.unpack path') bytes
    HiValueBytes bytes -> return $ HiValueAction $ HiActionWrite (DT.unpack path') bytes
    _ -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunMkDir`.
evalMkDir :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalMkDir = unaryfunc $ \case
  HiValueString dir -> return $ HiValueAction $ HiActionMkDir (DT.unpack dir)
  _                 -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunChDir`.
evalChDir :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalChDir = unaryfunc $ \case
  HiValueString dir -> return $ HiValueAction $ HiActionChDir (DT.unpack dir)
  _                 -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunParseTime`.
evalParseTime :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalParseTime = unaryfunc $ \case
  HiValueString s ->
    case readMaybe (DT.unpack s) of
      Nothing -> throwE HiErrorInvalidArgument
      Just s' -> return $ HiValueTime s'
  _ -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunRand`.
evalRand :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalRand = bifunc $ \a b -> do
  a' <- extractInt a
  b' <- extractInt b
  if a' <= b'
  then return $ HiValueAction $ HiActionRand a' b'
  else throwE HiErrorInvalidArgument

-- | Evaluates `HiFunEcho`.
evalEcho :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalEcho = unaryfunc $ \case
  HiValueString s -> return $ HiValueAction $ HiActionEcho s
  _               -> throwE HiErrorInvalidArgument

-- | Helper function for map construction.
mapTransformer :: (a -> a -> a) -> [(HiValue, a)] -> (a -> HiValue) -> HiValue
mapTransformer f list toValue = HiValueDict $ DM.map toValue $ DM.fromListWith f list

-- | Helper functino for counting container values.
containerCount :: [HiValue] -> HiValue
containerCount container = mapTransformer (+) (zip container (repeat 1)) HiValueNumber

-- | Evaluates `HiFunCount`.
evalCount :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalCount = unaryfunc $ \case
  HiValueString s -> return $ containerCount (map (\s' -> HiValueString $ DT.pack [s']) (DT.unpack s))
  HiValueBytes s  -> return $ containerCount (map (HiValueNumber . fromIntegral) (BS.unpack s))
  HiValueList s   -> return $ containerCount (toList s)
  _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunKeys`.
evalKeys :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalKeys = unaryfunc $ \case
  HiValueDict dict -> return $ HiValueList $ DS.fromList (DM.keys dict)
  _                -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunValues`.
evalValues :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalValues = unaryfunc $ \case
  HiValueDict dict -> return $ HiValueList $ DS.fromList (DM.elems dict)
  _                -> throwE HiErrorInvalidArgument

-- | Evaluates `HiFunInvert`.
evalInvert :: HiMonad m => [HiExpr] -> HiErrorMonad m HiValue
evalInvert = unaryfunc $ \case
  HiValueDict dict -> return $ mapTransformer (DS.><) (map (\(k, v) -> (v, DS.singleton k)) (DM.toList dict)) HiValueList
  _ -> throwE HiErrorInvalidArgument

-- | Evaluates `HiExpr` with `HiError` exception.
evalExcept :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExcept (HiExprValue val) = return val
evalExcept (HiExprApply (HiExprValue (HiValueNumber _)) _) = throwE HiErrorInvalidFunction
evalExcept (HiExprApply (HiExprValue (HiValueBool _)) _) = throwE HiErrorInvalidFunction
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) args) = evalAdd args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunSub)) args) = evalSub args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunMul)) args) = evalMul args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) args) = evalDiv args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) args) = evalBiBoolFunction False args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunOr)) args) = evalBiBoolFunction True args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) args) = evalBiValFunction (==) args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) args) = evalBiValFunction (/=) args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) args) = evalBiValFunction (<) args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) args) = evalBiValFunction (>) args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) args) = evalBiValFunction (>=) args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) args) = evalBiValFunction (<=) args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) args) = evalIf args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunNot)) args) = evalNot args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunLength)) args) = evalLength args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunToLower)) args) = evalToLower args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunToUpper)) args) = evalToUpper args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunReverse)) args) = evalReverse args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunTrim)) args) = evalTrim args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunList)) args) = do
  args' <- evalList args
  return $ HiValueList args'
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunRange)) args) = evalRange args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunFold)) args) = evalFold args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunPackBytes)) args) = evalPackBytes args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunUnpackBytes)) args) = evalUnpackBytes args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunEncodeUtf8)) args) = evalEncodeUtf8 args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunDecodeUtf8)) args) = evalDecodeUtf8 args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunZip)) args) = evalZip args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunUnzip)) args) = evalUnzip args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunSerialise)) args) = evalSerialise args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunDeserialise)) args) = evalDeserialise args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunRead)) args) = evalRead args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunWrite)) args) = evalWrite args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunMkDir)) args) = evalMkDir args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunChDir)) args) = evalChDir args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunParseTime)) args) = evalParseTime args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunRand)) args) = evalRand args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunEcho)) args) = evalEcho args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunCount)) args) = evalCount args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunKeys)) args) = evalKeys args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunValues)) args) = evalValues args
evalExcept (HiExprApply (HiExprValue (HiValueFunction HiFunInvert)) args) = evalInvert args
evalExcept (HiExprDict dict) = do
  dict' <- mapM (\(key, value) -> do key' <- evalExcept key; value' <- evalExcept value; return (key', value')) dict
  return $ HiValueDict $ DM.fromList dict'
evalExcept (HiExprApply expr args) = do
  res <- evalExcept expr
  case res of
    f@(HiValueFunction _) -> evalExcept $ HiExprApply (HiExprValue f) args
    HiValueDict dict -> do
      args' <- mapM evalExcept args
      case args' of
        [key] ->
          case DM.lookup key dict of
            Nothing    -> return HiValueNull
            Just value -> return value
        _ -> throwE HiErrorInvalidFunction
    _ -> do
      args' <- mapM evalExcept args
      case args' of
        [HiValueNumber i] -> evalIndexing res i
        [l, r]            -> evalSplice res l r
        [_]               -> throwE HiErrorInvalidArgument 
        _                 -> throwE HiErrorInvalidFunction
evalExcept (HiExprRun e) = do
  e' <- evalExcept e
  case e' of
    HiValueAction a -> lift $ runAction a
    _               -> throwE HiErrorInvalidArgument

-- | Evaluates `HiExpr` to a `HiValue` or `HiError`
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT (evalExcept e)
