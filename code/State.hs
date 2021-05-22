{- A Haskell version of the companion code for "State of effectful programming".
   Tested with GHCi 8.6.5. -}
import Control.Monad.ST (runST)
import Data.STRef       (newSTRef, readSTRef, writeSTRef)
-- State monad
newtype State s a = State { runState :: s -> (a,s) }

-- | State is a functor
instance Functor (State s) where
  fmap f m = State (\st -> let (x, st') = runState m st in
                           (f x, st'))

-- | State is an applicative functor
instance Applicative (State s) where
  pure x = State (\st -> (x, st))
  m1 <*> m2 = State (\st -> let (f, st') = runState m1 st in
                            runState (fmap f m2) st')

-- | State is a monad
instance Monad (State s) where
  return = pure
  m >>= k = State (\st -> let (x, st') = runState m st in
                          runState (k x) st')

-- | State operations
get :: () -> State s s
get () = State (\st -> (st, st))

put :: s -> State s ()
put st = State (\st' -> ((), st))

-- Continuation monad
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- | Cont is a functor
instance Functor (Cont r) where
  fmap f k = Cont (\g -> runCont k (\x -> g (f x)))

-- | Cont is an applicative functor
instance Applicative (Cont r) where
  pure x  = Cont (\k -> k x)
  k <*> k' = Cont (\r -> runCont k
                   (\k'' -> runCont k'
                     (\x -> r (k'' x))))

-- | Cont is a monad
instance Monad (Cont r) where
  return  = pure
  m >>= k = Cont (\k' -> runCont m
                   (\x -> runCont (k x)
                     (\y -> k' y)))

-- | State operations

getk :: () -> Cont (State s a) s
getk () = Cont (\k -> State (\st -> runState (k st) st))

putk :: s -> Cont (State s a) ()
putk st' = Cont (\k -> State (\st -> runState (k ()) st'))

-- Free monad
data Free f a = Return a
              | Op (f (Free f a))

-- | Free is a functor
instance Functor f => Functor (Free f) where
  fmap f (Return x) = Return (f x)
  fmap f (Op y)     = Op (fmap (fmap f) y)

-- | Free is an applicative functor
instance Functor f => Applicative (Free f) where
  pure  = Return
  (Return f) <*> xs = fmap f xs
  (Op f)     <*> xs = Op (fmap (\g -> g <*> xs) f)

-- | Free is a monad
instance Functor f => Monad (Free f) where
  return = Return
  (Return x) >>= k = k x
  (Op y)     >>= k = Op (fmap (\m' -> m' >>= k) y)

-- | Auxiliary function for constructing operation nodes
do' :: Functor f => f a -> Free f a
do' op = Op (fmap Return op)

-- Instantiate Free with state
data FreeState s r = Get (s -> r)
                   | Put s (() -> r)

-- | FreeState is a functor
instance Functor (FreeState s) where
  fmap f (Get k) = Get (\st -> f (k st))
  fmap f (Put st' k) = Put st' (\() -> f (k ()))

-- | State operations
get' :: () -> Free (FreeState s) s
get' () = do' (Get (\x -> x))

put' :: s -> Free (FreeState s) ()
put' st = do' (Put st (\() -> ()))

-- | State handler
runState' :: s -> Free (FreeState s) a -> (a, s)
runState' st0 (Op (Get k))    = runState' st0 (k st0)
runState' st0 (Op (Put st k)) = runState' st (k ())
runState' st0 (Return x)      = (x, st0)

-- Generic state example
incrEven :: Monad m => (() -> m Int, Int -> m ()) -> () -> m Bool
incrEven (get, put) () = get () >>= (\st -> put (1 + st) >>= (\() -> return (even st)))

runExamples :: Int -> [(String, (Bool, Int))]
runExamples st0 = map (\(s, f) -> (s, f st0)) examples
  where examples = [ ("builtin state", \st -> runST $ do
                         st' <- newSTRef st
                         v   <- readSTRef st'
                         writeSTRef st' (v + 1)
                         v'  <- readSTRef st'
                         return (even v, v'))
                   , ("pure state passing", \st -> (even st, st + 1))
                   , ("state monad", \st -> runState (incrEven (get, put) ()) st)
                   , ("continuation monad", \st -> runState (runCont (incrEven (getk, putk) ())
                                                              (\x -> State (\st -> (x, st)))) st)
                   , ("free monad", \st -> runState' st (incrEven (get', put') ())) ]
