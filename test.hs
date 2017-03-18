import Control.Monad.Writer 
import Data.Monoid
import qualified Data.Sequence as DS

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  

-- finalCountDown :: Int -> Writer (DiffList String) ()  
-- finalCountDown 0 = do  
--     tell (toDiffList ["0"])  
-- finalCountDown x = do  
--     finalCountDown (x-1)  
--     tell (toDiffList [show x])  
-- main::IO()
-- main = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000 

finalCountDown :: Int -> Writer (DS.Seq String) ()  
finalCountDown 0 = do  
    tell $ DS.fromList ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell $ DS.fromList [show x]  
main::IO()
main = mapM_ putStrLn . snd . runWriter $ finalCountDown 500000