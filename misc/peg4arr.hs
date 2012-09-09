{-# LANGUAGE BangPatterns #-}

-- haskell can parse 200000 in 6sec by ghc

import Data.Array
import qualified Data.Set as Set

type ParsedData = Array Int (Char,Array Int (Set.Set Int))
type NumAndFunc = (Int,ParsedData->Int->(Set.Set Int))
type Env = (Int,[NumAndFunc])

mybind s f = Set.foldr (\x sr->(f x) `Set.union` sr) Set.empty s

getres :: ParsedData -> Int -> Int -> (Set.Set Int)
getres larr i num =
  if inRange (bounds larr) i then
    (snd (larr!i))!num
  else
    Set.empty

init_i :: Env
init_i = (0,[peg_eps_res])

char_str :: (Char->Bool) -> Env -> (NumAndFunc,Env)
char_str char_pat_f (num,plist) = (res, (num+1, plist++[res]))
 where
  res = (num+1, resf)
  resf larr i=
    if inRange (bounds larr) i then
      if (char_pat_f (fst (larr!i))) then (Set.singleton (i+1)) else Set.empty
    else
      Set.empty

peg_eps_res :: NumAndFunc
peg_eps_res = (0,\larr i->Set.singleton i)

peg_eps :: Env -> (NumAndFunc,Env)
peg_eps (num,plist) = (peg_eps_res,(num,plist))

peg_or :: (Env -> (NumAndFunc,Env)) -> (Env -> (NumAndFunc,Env)) -> Env -> (NumAndFunc,Env)
peg_or p1 p2 (num,plist) = 
  let (ps1,(num1,plist1)) = p1 (num,plist) in
  let (ps2,(num2,plist2)) = p2 (num1,plist1) in
  let res = (num2+1 , \larr i -> (getres larr i (fst ps1)) `Set.union` (getres larr i (fst ps2))) in
  (res,(num2+1,plist2++[res]))

peg_cat :: (Env -> (NumAndFunc,Env)) -> (Env -> (NumAndFunc,Env)) -> Env -> (NumAndFunc,Env)
peg_cat p1 p2 (num,plist) = 
  let (ps1,(num1,plist1)) = p1 (num,plist) in
  let (ps2,(num2,plist2)) = p2 (num1,plist1) in
  let res = (num2+1 , \larr i -> (getres larr i (fst ps1)) `mybind` (\j->(getres larr j (fst ps2))) ) in
  (res,(num2+1,plist2++[res]))

peg_existing :: (NumAndFunc,Env) -> Env -> (NumAndFunc,Env)
peg_existing ps env =
  (fst ps,env)

peg_asta :: (Env -> (NumAndFunc,Env)) -> Env -> (NumAndFunc,Env)
peg_asta p env = res
 where
  res = (peg_or peg_eps $ peg_cat p $ peg_existing res) env

peg_plus :: (Env -> (NumAndFunc,Env)) -> Env -> (NumAndFunc,Env)
peg_plus p env =
  let (pres,penv) = p env in
  (peg_cat (peg_existing (pres,penv)) (peg_asta $ peg_existing (pres,penv))) penv

numchar_pat :: (Char->Bool)
numchar_pat c = c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' || c == '7' || c == '8' || c == '9'

num_asta = peg_asta $ char_str numchar_pat
num_plus = peg_plus $ char_str numchar_pat

expr env = exprres
 where
  parexpr = peg_cat (peg_cat (char_str (=='[')) (peg_existing exprres)) (char_str (==']'))
  value = peg_or num_plus parexpr
  prod = peg_cat value $ peg_asta $ peg_cat (peg_or (char_str (=='*')) (char_str (=='/'))) value
  psum = peg_cat prod $ peg_asta $ peg_cat (peg_or (char_str (=='+')) (char_str (=='-'))) prod
  expr_e = peg_or psum $ peg_cat (char_str (=='-')) psum
  exprres = expr_e env

ortest = peg_or (char_str numchar_pat) (char_str (=='['))
cattest = peg_cat (char_str numchar_pat) (char_str (=='['))

makelistfunc :: [t -> Int -> b] -> t -> Int -> [b]
makelistfunc fs x i = map (\f->f x i) fs

makelistfuncsnd :: [NumAndFunc] -> ParsedData -> Int -> Array Int (Set.Set Int)
makelistfuncsnd nfs =
  let len = length nfs in
  (\x i-> array (0,len-1) $ map (\nf->(fst nf,(snd nf) x i)) nfs)


parse :: (Env -> (NumAndFunc,Env)) -> String -> (Set.Set Int)
parse peg str =
  (snd (allparsed!0))!idx
  --allparsed
   where
    !((idx,pl),(num,arr)) = peg init_i
    !fs=makelistfuncsnd arr
    len = length str
    allparsed = array (0,len-1) contlist
    contlist = snd $ foldl (\(i,lst) x->(i+1,(i,(x,fs allparsed i)):lst)) (0,[]) str

longtestdata n = "["++(concat $ replicate n "12+3*")++"4]+1E"

main = putStr $ show $ parse expr $ longtestdata 40000

{-
parse expr "12E"
parse expr "[12]+3*4E"
parse expr $ longtestdata 10000 -- 10sec on ghci

-}

