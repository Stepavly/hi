{-| Contex free grammar for non-operator parser:

@
expr -> (value | '(' expr ')' | dict ) args* ''!''*
value -> HiValue
dict -> '{' (eps | dictEntry (',' dictEntry)*) '}'
dictEntry -> expr '':'' expr
args -> '(' (eps | exprWithOperators (',' exprWithOperators)*) ')'
@
-}

module HW3.Parser
  (
    parse
  ,HiActionParsec) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString, singleton)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Text (pack)
import Data.Void (Void)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import HW3.BaseUtility (funNames)
import Numeric (readHex)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char (hexDigitChar)
import qualified Text.Megaparsec.Char as L
import Text.Megaparsec.Char.Lexer (scientific, charLiteral)
import Control.Applicative.Combinators (manyTill)

type HiListParsec a = M.Parsec Void String [a]
type HiValueParsec = M.Parsec Void String HiValue
type HiParsec a = M.Parsec Void String a
type HiActionParsec = M.Parsec Void String HiAction

-- | Parse zero or more spaces.
parseSpaces :: M.Parsec Void String ()
parseSpaces = void $ M.optional L.space1

-- | Parse string skipping leading and trailing spaces.
string :: String -> M.Parsec Void String String
string s = (parseSpaces *> L.string s) <* parseSpaces

-- | Parse sequence of values.
parseSequence :: Show e => M.Parsec Void String a -- ^ Parse starting character of the sequence.
   -> M.Parsec Void String b -- ^ Parse ending character of the sequence.
   -> HiParsec e -- ^ Parse the first element of the sequence.
   -> HiParsec e -- ^ Parse the second, third, ..., n-th element of the sequence.
   -> HiListParsec e
parseSequence parseStart parseEnd parseHead parseTail = do
  void parseStart
  h <- M.optional parseHead
  if isJust h
  then do
    t <- M.many parseTail
    void parseEnd
    return $ maybeToList h ++ t
  else do
    void parseEnd
    return []

-- | Parse identifier.
parseIdentifier :: M.Parsec Void String HiExpr
parseIdentifier = do
  void $ L.char '.'
  literals <- ((:) <$> M.satisfy isAlpha <*> M.many (M.satisfy isAlphaNum)) `M.sepBy` L.char '-'
  return $ HiExprValue $ HiValueString $ Data.Text.pack $ Data.List.intercalate "-" literals

-- | Parse arguments in one of the forms:
parseArgs :: M.Parsec Void String [HiExpr]
parseArgs =
  parseSequence (string "(") (string ")") parseExprWithOperators (string "," *> parseExprWithOperators) M.<|>
  do e <- parseIdentifier; return [e]

-- | Parse string with associated `HiFun`.
parseFunName :: String -> HiFun -> HiValueParsec
parseFunName name fun = do
  void $ string name
  return $ HiValueFunction fun

-- | Parse string with associated `HiAction`.
parseFunActionName :: String -> HiAction -> HiValueParsec
parseFunActionName name fun = do
  void $ string name
  return $ HiValueAction fun

-- | Parse `HiValueNumber`.
parseNum :: HiValueParsec
parseNum = do
  sign <- M.optional (string "-")
  n <- (parseSpaces *> scientific) <* parseSpaces
  return $ HiValueNumber $ toRational $ if isNothing sign then n else negate n

-- | Parse `HiValueBool`.
parseBool :: HiValueParsec
parseBool = do
  token <- M.choice [string "true", string "false"]
  return $ HiValueBool $ token == "true"

-- | Parse `HiValueString` in quotes.
parseString :: HiValueParsec
parseString = do
  s <- L.char '"' >> manyTill charLiteral (L.char '"')
  parseSpaces
  return $ HiValueString $ Data.Text.pack s

-- | Parse `HiValueNull`.
parseNull :: HiValueParsec
parseNull = do
  void $ string "null"
  return HiValueNull

-- | Parse list of values.
parseList :: HiParsec HiExpr
parseList = do
  list <- parseSequence (string "[") (string "]") parseExprWithOperators (string "," *> parseExprWithOperators)
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) list

-- | Parse `Data.ByteString.ByteString` represented as a sequence of hex numbers.
parseByteString :: HiValueParsec
parseByteString = M.try $ do
  bs <- parseSequence (string "[#") (string "#]") parseHexNum parseHexNum
  return $ HiValueBytes $ mconcat bs
  where
  parseHexNum :: M.Parsec Void String ByteString
  parseHexNum = do
    b1 <- hexDigitChar
    b2 <- hexDigitChar
    L.space1 M.<|> M.notFollowedBy hexDigitChar
    return $ singleton $ fst $ head (readHex [b1, b2])

-- | Parse dictionary entry.
parseDictEntry :: M.Parsec Void String (HiExpr, HiExpr)
parseDictEntry = do
  key <- parseExprWithOperators
  void $ string ":"
  value <- parseExprWithOperators
  return (key, value)

-- | Parse `HiExprDict`.
parseDict :: HiParsec HiExpr
parseDict = do
  dict <- parseSequence (string "{") (string "}") parseDictEntry (string "," *> parseDictEntry)
  return $ HiExprDict dict

-- | All possible `HiValue` tokens.
valueTokens :: [HiValueParsec]
valueTokens =
  foldMap (\(fun, name) -> [parseFunName name fun]) funNames ++ [
  parseNum,
  parseBool,
  parseString,
  parseNull,
  parseByteString,
  parseFunActionName "cwd" HiActionCwd,
  parseFunActionName "now" HiActionNow]

-- | Parse `HiExpr`.
parseExpr :: M.Parsec Void String HiExpr
parseExpr = do
  e <- parseSpaces *> (parseBrackets M.<|> parseInner M.<|> parseDict M.<|> parseList)
  args <- M.many parseArgs
  run <- M.optional (string "!")
  parseSpaces
  return $ maybe id (const HiExprRun) run (foldl HiExprApply e args)
  where
  parseInner = do
    token <- M.choice valueTokens
    return $ HiExprValue token
  parseBrackets = M.between (string "(") (string ")") parseExprWithOperators

-- | Operators ordered by priority.
table :: [[Operator (M.Parsec Void String) HiExpr]]
table = reverse [
  [binaryR "||" HiFunOr],
  [binaryR "&&" HiFunAnd],
  [binaryN "==" HiFunEquals, binaryN "/=" HiFunNotEquals,
   binaryN ">=" HiFunNotLessThan, binaryN "<=" HiFunNotGreaterThan,
   binaryN "<" HiFunLessThan, binaryN ">" HiFunGreaterThan],
  [binaryL "+" HiFunAdd, binaryL "-" HiFunSub],
  [binaryL "*" HiFunMul,
   InfixL ((\a b -> HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) [a, b]) <$ M.try (string "/" <* M.notFollowedBy (L.char '=')))]
  ]

binaryR :: String -> HiFun -> Operator (M.Parsec Void String) HiExpr
binaryR name f = InfixR ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ string name)

binaryL :: String -> HiFun -> Operator (M.Parsec Void String) HiExpr
binaryL name f = InfixL ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ string name)

binaryN :: String -> HiFun -> Operator (M.Parsec Void String) HiExpr
binaryN name f = InfixN ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ string name)

parseExprWithOperators :: M.Parsec Void String HiExpr
parseExprWithOperators = makeExprParser parseExpr table

-- | Run parser.
parse :: String -> Either (M.ParseErrorBundle String Void) HiExpr
parse = M.runParser (parseExprWithOperators <* M.eof) ""
