{-# LANGUAGE ExistentialQuantification         #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Timstuff.Learn where


import Control.Monad

import Data.IORef

import Control.Concurrent as CC

main = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

class Eq2 a where
      eq :: a -> a -> Bool

elem2 :: (Eq2 a) => a -> [a] -> Bool
elem2 a b = True

data Tree a = Leaf a | Branch (Tree a) (Tree a)

leftmostLeaf :: Tree a -> a
leftmostLeaf (Leaf a) = a
leftmostLeaf (Branch a b) = leftmostLeaf a

testTree :: Tree Integer
testTree = (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2))

bot = 1/0

--foo :: [x] -> Bool
foo [0, _] = 1
foo [bot, 1] = 2 --bot is a variable, not 'bot' the function, therefore can be anything

foo2 a = case a of
  1 -> True
  0 -> False


--reqs : [a]
reqs = client cinit resps

--resps : [a]
resps = server reqs


client cinit ~(resp : resps) = cinit :
       case (next resp) of
         -1 -> []
         _ ->  client (next resp) resps
server (req : reqs) = process req : server reqs
server [] = []
cinit = 0
next x
  | x < 10 = x
  | True = -1
  
process x = x + 1


  
fib@(1:tfib)    = 1 : 1 : [ a+b | (a,b) <- zip fib tfib ]

-- class C a where
--   m  :: a -> b

-- data Fff  = Fff

-- instance Show Fff where
--   show Fff = "Fff"

-- instance C Fff  where
--   m x = x


-- instance Functor ((->) f) where
--   fmap f1 f2 = f1 . f2

instance Functor ((,,) f g) where
  fmap f (a,b,c) = (a,b,(f c))
  

-- data Foo3 = X | Y

-- instance Show Foo3 where

readInt :: String -> Int
readInt x = read x

zzz =
    do x <- [1,2,3]
       y <- [1,2,3]
       z <- return (x /= y)
       True <- return z
       return (x,y,z)

zzz' :: IO Int
zzz' =
  do x <- return 1
     y <- return 2
     return (x + y)
     

mmm :: [Int]
mmm =
    return 2 >>= (\x -> return 3 >>= (\y -> return (x+y)))


data S = A | B | C
     deriving (Show)
                
data SM a = SM (S -> (a,S))  -- The monadic type

instance Functor SM where
  fmap atob (SM fa) = SM (\s -> let (a,s1) = (fa s) in
                                ((atob a),s1))
                                            

instance Applicative SM where
  pure x = SM (\s -> (x,s))
  (<*>) (SM f) (SM fa) = SM (\s ->
                         let (atob,s1) = (f s) 
                             (a,s2) = fa s1 in
                         (atob a,s2))
                         
instance Monad SM where
  -- defines state propagation
  SM c1 >>= fc2         =  SM (\s0 -> let (r,s1) = c1 s0 
                                          SM c2 = fc2 r in
                                         c2 s1)
  return k              =  SM (\s -> (k,s))

 -- extracts the state from the monad
readSM                  :: SM S
readSM                  =  SM (\s -> (s,s))

 -- updates the state of the monad
updateSM                :: (S -> S) -> SM ()  -- alters the state
updateSM f              =  SM (\s -> ((), f s)) 

-- run a computation in the SM monad
runSM                   :: S -> SM a -> (a,S)
runSM s0 (SM c)         =  c s0


maybePrint :: IORef Bool -> IORef Bool -> IO ()
maybePrint myRef yourRef = do
   atomicWriteIORef myRef True
   yourVal <- readIORef yourRef
   unless yourVal $ putStrLn "critical section"

mp :: IO ()
mp = do
   r1 <- newIORef False
   r2 <- newIORef False
   CC.forkIO $ maybePrint r1 r2
   CC.forkIO $ maybePrint r2 r1
   CC.threadDelay 1000000


--data Foo = Foo { fight :: String, bananas :: Int } deriving (Show)

data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
  show (SB s) = "SHOWBOX showbox ShowBox!!! " ++ Prelude.show s

foob :: forall a b. (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval =
    postProcess val
    where
        val :: b
        val = maybe onNothin onJust mval
        

-- MkFoo :: forall a. a -> (a -> Bool) -> Foo
-- Nil   :: Foo

-- {-# LANGUAGE ExistentialQuantification #-}
-- data EQList = forall a. EQList [a]
-- eqListLen :: EQList -> Int
-- eqListLen (EQList x) = length x

{-# LANGUAGE RankNTypes #-}
data EQList = forall a. EQList [a]
eqListLen :: EQList -> Int
eqListLen (EQList x) = length x
