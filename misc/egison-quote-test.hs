{-# Language TemplateHaskell, QuasiQuotes #-}
module Main where

--import Language.Haskell.TH
import Language.Egison.Quote

combination3_2 :: [(Int, Int)]
combination3_2 = [egison| (match-all {1 2 3} (Multiset Integer) [<cons $x <cons $y _>> [x y]]) :: [(Int, Int)] |]

main :: IO ()
main = do
  let ps = combination3_2
  putStrLn $ show ps