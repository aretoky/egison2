-- simple PEG parser

import Control.Monad
import Data.List

-- convert character pattern to string pattern
char_str :: (MonadPlus m) => (Char -> Bool) -> (String -> m String)
char_str cp ""=mzero
char_str cp (c:cs)=if (cp c) then (return cs) else mzero

peg_eps :: (MonadPlus m) => (String -> m String)
peg_eps s=return s

peg_or :: (MonadPlus m) => (String -> m String) -> (String -> m String) -> (String -> m String)
peg_or p1 p2 s=(p1 s) `mplus` (p2 s)

peg_concat :: (MonadPlus m) => (String -> m String) -> (String -> m String) -> (String -> m String)
peg_concat p1 p2 s = (p1 s) >>= p2

peg_ast :: (MonadPlus m) => (String -> m String) -> (String -> m String)
peg_ast p = peg_eps `peg_or` (p `peg_concat` (peg_ast p))

peg_plus :: (MonadPlus m) => (String -> m String) -> (String -> m String)
peg_plus p = p `peg_concat` (peg_ast p)

-- numchar-pat = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
numchar_pat :: (Char -> Bool)
numchar_pat c = any (==c) ['0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9']

-- num-plus = numchar-pat+
num_plus :: (MonadPlus m) => (String -> m String)
num_plus = peg_plus $ char_str numchar_pat

-- parexpr = '[' expr ']'
parexpr :: (MonadPlus m) => (String -> m String)
parexpr = (char_str (=='[')) `peg_concat` expr `peg_concat` (char_str (==']'))

-- value = num-plus | parexpr
value :: (MonadPlus m) => (String -> m String)
value = num_plus `peg_or` parexpr

-- prod = value (('*' | '/') value)*
prod :: (MonadPlus m) => (String -> m String)
prod = value `peg_concat` (peg_ast $ ((char_str (=='*')) `peg_or` (char_str (=='/'))) `peg_concat` value)

-- sum = prod (('+' | '-') prod)*
psum :: (MonadPlus m) => (String -> m String)
psum = prod `peg_concat` (peg_ast $ ((char_str (=='+')) `peg_or` (char_str (=='-'))) `peg_concat` prod)

-- expr = psum | '-' psum
expr :: (MonadPlus m) => (String -> m String)
expr = psum `peg_or` ((char_str (=='-')) `peg_concat` psum)

teststrings :: [String]
teststrings = ["12E","1-2E","-12E","1+2-3-4E","1+2-3*4E","1+-2E","[12]+3*4E","-[12]+3*4E","-[12]+3*4/6E"]

tests :: [[String]]
tests = map expr teststrings

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix str= fmap reverse $ stripPrefix (reverse suffix) $ reverse str

lastmatchedtests :: [(String,String)]
lastmatchedtests = map (\s->(s,maybe "" id $ stripSuffix (last$expr s) s)) teststrings

srepeat 0 l = []
srepeat i l = l ++ (srepeat (i-1) l)

longtestdata n = "["++(srepeat n "12+3*")++"4]+1E"

longtest :: [String]
longtest = expr $ longtestdata 10000 -- 7sec on ghci, 5sec on ghc

main = putStr $ show $ longtest


