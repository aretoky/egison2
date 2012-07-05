module Language.Egison.Numerical where
import Language.Egison.Types
import Control.Monad.Error
--import Data.Char hiding (isNumber)
--import Data.Fixed
--import Numeric
--import Text.Printf

boolBinop :: (Bool -> Bool -> Bool) -> [EgisonVal] -> ThrowsError EgisonVal
boolBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
boolBinop op aparams = mapM unpackBool aparams >>= return . Bool . foldl1 op

numericSglop :: (Integer -> Integer) -> [EgisonVal] -> ThrowsError EgisonVal
numericSglop op [x] = unpackNum x >>= return . Number . op
numericSglop _ params = throwError $ NumArgs 1 params

floatSglop :: (Double -> Double) -> [EgisonVal] -> ThrowsError EgisonVal
floatSglop op [x] = unpackFloat x >>= return . Float . op
floatSglop _ params = throwError $ NumArgs 1 params

floatNumSglop :: (Double -> Integer) -> [EgisonVal] -> ThrowsError EgisonVal
floatNumSglop op [x] = unpackFloat x >>= return . Number . op
floatNumSglop _ params = throwError $ NumArgs 1 params

numericBinop :: (Integer -> Integer -> Integer) -> [EgisonVal] -> ThrowsError EgisonVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op aparams = mapM unpackNum aparams >>= return . Number . foldl1 op

floatBinop :: (Double -> Double -> Double) -> [EgisonVal] -> ThrowsError EgisonVal
floatBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
floatBinop op aparams = mapM unpackFloat aparams >>= return . Float . foldl1 op

charBoolBinop :: (Char -> Char -> Bool) -> [EgisonVal] -> ThrowsError EgisonVal
charBoolBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
charBoolBinop op aparams = mapM unpackChar aparams >>= doOp
  where doOp [a, b] = return $ Bool $ op a b
        doOp _ = throwError $ Default "Unexpected error in numCharBinop"

strBoolBinop :: (String -> String -> Bool) -> [EgisonVal] -> ThrowsError EgisonVal
strBoolBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
strBoolBinop op aparams = mapM unpackString aparams >>= doOp
  where doOp [a, b] = return $ Bool $ op a b
        doOp _ = throwError $ Default "Unexpected error in numCharBinop"

numBoolBinop :: (Integer -> Integer -> Bool) -> [EgisonVal] -> ThrowsError EgisonVal
numBoolBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numBoolBinop op aparams = mapM unpackNum aparams >>= doOp
  where doOp [a, b] = return $ Bool $ op a b
        doOp _ = throwError $ Default "Unexpected error in numBoolBinop"

floatBoolBinop :: (Double -> Double -> Bool) -> [EgisonVal] -> ThrowsError EgisonVal
floatBoolBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
floatBoolBinop op aparams = mapM unpackFloat aparams >>= doOp
  where doOp [a, b] = return $ Bool $ op a b
        doOp _ = throwError $ Default "Unexpected error in floatBoolBinop"

-- - Begin GenUtil - http://repetae.net/computer/haskell/GenUtil.hs
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x : xs) = (f v x) >>= \ a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"
-- end GenUtil

floatRound, floatFloor, floatCeiling, floatTruncate :: [EgisonVal] -> ThrowsError EgisonVal
floatRound [(Float n)] = return $ Float $ fromInteger $ round n
floatRound [x] = throwError $ TypeMismatch "floatber" x
floatRound badArgList = throwError $ NumArgs 1 badArgList

floatFloor [(Float n)] = return $ Float $ fromInteger $ floor n
floatFloor [x] = throwError $ TypeMismatch "number" x
floatFloor badArgList = throwError $ NumArgs 1 badArgList

floatCeiling [(Float n)] = return $ Float $ fromInteger $ ceiling n
floatCeiling [x] = throwError $ TypeMismatch "number" x
floatCeiling badArgList = throwError $ NumArgs 1 badArgList

floatTruncate [(Float n)] = return $ Float $ fromInteger $ truncate n
floatTruncate [x] = throwError $ TypeMismatch "number" x
floatTruncate badArgList = throwError $ NumArgs 1 badArgList

numSqrt, numExpt :: [EgisonVal] -> ThrowsError EgisonVal
numSqrt [(Float n)] = if n >= 0 then return $ Float $ sqrt n
                                else throwError $ Default "negative number to sqrt"
numSqrt [x] = throwError $ TypeMismatch "number" x
numSqrt badArgList = throwError $ NumArgs 1 badArgList

numExpt [(Number n), (Number p)] = return $ Float $ (fromInteger n) ^ p
numExpt [(Float n), (Number p)] = return $ Float $ n ^ p
numExpt [_, y] = throwError $ TypeMismatch "integer" y
numExpt badArgList = throwError $ NumArgs 2 badArgList

numExp :: [EgisonVal] -> ThrowsError EgisonVal
numExp [(Number n)] = return $ Float $ exp $ fromInteger n
numExp [(Float n)] = return $ Float $ exp n
numExp [x] = throwError $ TypeMismatch "number" x
numExp badArgList = throwError $ NumArgs 1 badArgList

numLog :: [EgisonVal] -> ThrowsError EgisonVal
numLog [(Number n)] = return $ Float $ log $ fromInteger n
numLog [(Float n)] = return $ Float $ log n
numLog [x] = throwError $ TypeMismatch "number" x
numLog badArgList = throwError $ NumArgs 1 badArgList

-- |Convert a number to a string; radix is optional, defaults to base 10
--numToString :: [EgisonVal] -> IOThrowsError EgisonVal
--numToString [(Number n), (Number radix)] = do
--  case radix of
--    2 -> do -- Nice tip from StackOverflow question #1959715
--             liftIO $ stringToCharCollection $ showIntAtBase 2 intToDigit n ""
--    8 -> liftIO $ stringToCharCollection $ printf "%o" n
--    10 -> liftIO $ stringToCharCollection $ printf "%d" n
--    16 -> liftIO $ stringToCharCollection $ printf "%x" n
--    _ -> throwError $ BadSpecialForm "Invalid radix value" $ Number radix

-- |Convert a float to a string; radix is optional, defaults to base 10
--floatToString :: [EgisonVal] -> IOThrowsError EgisonVal
--floatToString [(Float n)] = liftIO $ stringToCharCollection $ show n
--floatToString [x] = throwError $ TypeMismatch "number" x
--floatToString badArgList = throwError $ NumArgs 1 badArgList

isEgisonEOF :: [EgisonVal] -> ThrowsError EgisonVal
isEgisonEOF [EOF] = return $ Bool True
isEgisonEOF [_] = return $ Bool False
isEgisonEOF badArgList = throwError $ NumArgs 1 badArgList

-- - end Numeric operations section


-- |Extract an bool from the given value, throwing a type error if
--  the wrong type is passed.
unpackBool :: EgisonVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

-- |Extract an char from the given value, throwing a type error if
--  the wrong type is passed.
unpackChar :: EgisonVal -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "char" notChar

-- |Extract an char from the given value, throwing a type error if
--  the wrong type is passed.
unpackString :: EgisonVal -> ThrowsError String
unpackString (String str) = return str
unpackString notString = throwError $ TypeMismatch "string" notString

-- |Extract an integer from the given value, throwing a type error if
--  the wrong type is passed.
unpackNum :: EgisonVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- |Extract an double from the given value, throwing a type error if
--  the wrong type is passed.
unpackFloat :: EgisonVal -> ThrowsError Double
unpackFloat (Float n) = return n
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat
