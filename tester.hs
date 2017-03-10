import Splay
import System.Random

main::IO()
main = do
  return ()
  let 
    maxRange = 10000::Int
    len = 100
    seed = mkStdGen 100
    seqi = take len $ randomRs (0, maxRange) seed
    splaytr = foldr (\a->insert a $ show a) empty seqi
  recur splaytr
  return ()
    where
      recur tr 
        | nullTr tr = return ()
        | otherwise = do 
          print x
          recur tr''
            where 
              tr' = find 0 tr
              x = peek tr'
              tr'' = remove (fst x) tr'
           

