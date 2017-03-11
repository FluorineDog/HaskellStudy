import Splay
import System.Random

main::IO()
main = do
  return ()
  let 
    maxRange = 10000000::Int
    len = 1000000
    seed = mkStdGen 0
    seqi = take len $ randomRs (0, maxRange) seed
    splaytr = foldr (\a->insert a $ "") empty seqi
    sumi = recur 0 splaytr
  print $ seqi !! (len-1) 
  print $ seqi !! 0 
  print sumi
  return ()
    where
      recur::Integer->Splay->Integer
      recur x0 tr = let tr' = find 1000000 tr in
        case peek tr' of 
          Nothing -> x0
          Just (x, _) ->
            recur (x0+ toInteger x) tr''
              where 
                tr'' = remove x tr'
