import Splay
import Prelude hiding (init)
type S = Int
newtype SM x = SM {getSM::S->(x,S)} 

instance Functor SM where
  fmap f (SM fx) = SM $ \s0->let (x, s) = (fx s0) in (f x, s)

instance (Applicative SM) where
  pure x = SM(\s->(x,s))
  SM ff <*> SM fx =SM $ \s0-> 
    let 
      (x, s1) = fx s0
      (f, s2) = ff s1
    in (f x, s2)
    
instance (Show a)=>Show (SM a) where
  show (SM f) = Prelude.show (f undefined)

main::IO()
main = print res

instance Monad SM where 
  -- return x = SM (\s0->(x, s0))
  -- (>>=)::SM a->(a->SM b)->SM b
  SM f >>= bigF = SM $ \s0 -> let  
      (x, s1) = f s0 
      SM f2 = bigF x
    in 
      f2 s1
  -- SM f1 >> SM f2 = SM $ f2 . snd . f1 

init::SM ()
init = SM (\_->((),0))

incr::SM ()
incr = SM(\s->((), s+1))

disp::SM Int
disp = SM(\s->(s, s))

push::Int->SM ()
push = \s -> SM(\_->((), s))

res::SM () 
res = do
  init
  incr
  incr
  incr
  incr
  incr
  x<-disp
  push (x*100)