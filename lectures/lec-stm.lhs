Concurrency
=========== 

\begin{code}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--diff" @-}
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent hiding (readMVar)
import Control.Concurrent.STM
import Control.Monad
import System.IO
import Data.IORef
import System.Environment (getArgs)
import System.Random 

import qualified Data.ByteString as B
import Network.HTTP
import Network.Browser
import Network.URI
import Data.Time
import Text.Printf
\end{code}


\begin{code}
-- newRef   :: a -> IO (IORef a)
-- readRef  :: IORef a -> IO a
-- writeRef :: IORef a -> a -> IO ()


moas :: IO ()
moas = do p <- newIORef (10 :: Int) 
          incr p 
          s <- readIORef p 
          putStrLn $ show s

incr :: IORef Int -> IO ()
incr p = do v <- readIORef p      -- v  = *p
            writeIORef p (v + 1)  -- *p = v + 1
\end{code}

1. Mutable State Via IORef
--------------------------

\begin{code}
newtype AccountIO = AIO (IORef Int)

newAccountIO ::  Int -> IO AccountIO
newAccountIO n 
  | n >= 0 
  = do {p <- newIORef n ; return (AIO p)} -- AIO <$> newIORef n
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       AIO <$> newIORef 0

showBalanceIO ::  AccountIO -> IO ()
showBalanceIO (AIO r) 
  = do bal <- readIORef r
       putStrLn $ "Current Balance: " ++ show bal

depositIO ::  AccountIO -> Int -> IO ()
depositIO (AIO r) n
  = do bal <- readIORef r
       if (bal + n < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else writeIORef r (bal + n)

main1 :: IO ()
main1 = do a <- newAccountIO 0
           forM (replicate 5 10) $
             depositIO a 
           showBalanceIO a   -- should be $50
\end{code}

2. Forking A Thread 
---------------------
\begin{spec}
forever act = do act
                 forever act
\end{spec}

\begin{code}
main2 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A') -- thread that writes 'A'
           forkIO $ forever (putChar 'B') -- thread that writes 'B'
           threadDelay oneSec             -- shutdown after 1 sec

oneSec :: Int
oneSec = 10 ^ (5 :: Int)
\end{code}

3. Randomly Fuzzing the Thread Scheduler
-------------------------------------

\begin{code}
toss       :: Int -> Int -> IO Int
toss lo hi = getStdRandom (randomR (lo, hi))

pauseRandom :: IO ()
pauseRandom = do n <- toss 0 5 
                 threadDelay $ n * oneSec

main3 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A' >> pauseRandom) -- thread that writes 'A'
           forkIO $ forever (putChar 'B' >> pauseRandom) -- thread that writes 'B'
           threadDelay $ 10 * oneSec                     -- shutdown after 1 sec
\end{code}

3. Data Races due to sharing
----------------------------

Bank account revisited; depositing with different threads, and fuzzed scheduler

\begin{code}
depositIO' ::  AccountIO -> Int -> IO ()
depositIO' (AIO r) n
  = do i   <- myThreadId 
       bal <- readIORef r
       putStrLn $ printf "Thread id = %s deposit's = %d bal = %d" (show i) n bal 
       pauseRandom           -- comment out and you get right answer
       if ((bal + n) < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else do putStrLn $ printf "Thread id = %s write bal = %d" (show i) (bal + n)
                 writeIORef r (bal + n)

main4 ::  IO ()
main4 = do a <- newAccountIO 0
           mapM_ (forkIO . depositIO' a) (replicate 5 10) 
           threadDelay (50 * oneSec) -- shutdown after 1 sec
           showBalanceIO a           -- should be $50 but isn't due to DATA RACES!
\end{code}

4. MVars: Vanilla
---------------------

Shared Message-Box-Variables `MVar`

* Message Box has EITHER an `a` or is EMPTY
    
~~~~~{.haskell}
data MVar a
~~~~~

* Create an EMPTY Reference

~~~~~{.haskell}
    newEmptyMVar :: IO (MVar a)
~~~~~

* Create a FULL reference

~~~~~{.haskell}
    newMVar	     :: a -> IO (MVar a)
~~~~~

* Blocks until box is full, takes out contents

~~~~~{.haskell}
    takeMVar	 :: MVar a -> IO a
~~~~~

* Overwrites contents with new contents

~~~~~{.haskell}
    putMVar	     :: MVar a -> a -> IO ()
~~~~~

**Uses of `MVar`**

1. An MVar is a useful container for shared mutable state, e.g. common design pattern 
   where threads need read and write access to some state, is to represent the state 
   value as an ordinary immutable Haskell data structure stored in an MVar.
 
2. An MVar is a one-place channel, which can be used for asynchronous communication
   between two threads.
 
3. An MVar () is a lock; takeMVar acquires the lock and putMVar releases it again. 
   An MVar used in this way can protect shared mutable state or critical sections.


5. MVars: BankAccount/Deposit 
------------------------------

Bank account revisited; using MVars, depositing with different
threads, and fuzzed scheduler

\begin{code}
newtype AccountMV = AMV (MVar Int)

newAccountMV :: Int -> IO AccountMV
newAccountMV n 
  | n >= 0 
  = AMV <$> newMVar n
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       AMV <$> newMVar 0

readMVar :: MVar a -> IO a
readMVar r = do x <- takeMVar r     -- read the value ...
                putMVar r x         -- ... also put the value back in!
                return x

showBalanceMV ::  AccountMV -> IO ()
showBalanceMV (AMV r) 
  = do bal <- readMVar r
       putStrLn $ "Current Balance: " ++ show bal

depositMV :: AccountMV -> Int -> IO ()
depositMV (AMV r) n
  = do bal <- takeMVar r            -- ALL other threads will now be blocked!
       pauseRandom                  -- No matter, you get right answer
       putStrLn $ "Cannot withdraw, balance below " ++ show bal 
       if (bal + n < 0) 
         then do putMVar r bal      -- Put the balance back in
                 putStrLn $ "Cannot withdraw, balance below " ++ show n 
         else putMVar r (bal + n)   -- Put the extra MONEY into the account 

main5 :: IO ()
main5 = do a <- newAccountMV 0
           mapM_ (forkIO . depositMV a) (replicate 5 10) 
           threadDelay (10 * oneSec) -- shutdown after 1 sec
           showBalanceMV a          -- should be $50, but why so slow?
\end{code}

6. Asynchrony Via MVars
-----------------------


Can implement "asynchronous" function calls 
(aka "futures", "promises") using `MVar`s. 
Other languages "features" are Haskell's,
"functions"...
   
A type representing an Asynchronous Computation

\begin{code}
newtype Async a = Async (MVar a)
\end{code}

Function to execute an IO action `async`-hronously 

\begin{code}
async :: IO a -> IO (Async a)
async action = do m <- newEmptyMVar
                  forkIO $ do x <- action 
                              putMVar m x
                  return (Async m)
\end{code}

Function to `wait` for the result of `Async` computation

\begin{code}
wait :: Async a -> IO a
wait (Async m) = readMVar m
\end{code}

**Application:** Download a bunch of URLs asynchronously, **without blocking** on each other

To demo the below, build with:
  
~~~~~{.haskell}
    $ ghc --make -threaded lec-stm.hs
~~~~~

Run with:

~~~~~{.haskell}
        $ ./lec-stm 6 +RTS -n4
        $ ./lec-stm 7 +RTS -n4
        $ ./lec-stm 8 +RTS -n4
~~~~~

A list of URLs

\begin{code}
urls :: [String]
urls = [ "http://www.google.com"
       , "http://www.buzzfeed.com"
       , "http://www.reddit.com/r/haskell"
       , "http://www.nytimes.com"
       ]
\end{code}

Reading a SINGLE URL

\begin{code}
timeDownload     :: String -> IO ()
timeDownload url = do (page, time) <- timeit $ getURL url
                      printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time
\end{code}

Reading ALL the URLs **in sequence** (i.e. a single thread) with `mapM`

\begin{code}
main6 = do (_ , time) <- timeit $ mapM timeDownload urls 
           printf "TOTAL download time: %.2fs\n" time
\end{code}

Recall that:

\begin{spec}
mapM          :: (a -> IO b) -> [a] -> IO [b]
mapM f []     = return []
mapM f (x:xs) = do y  <- f x
                   ys <- mapM f xs
                   return (y : ys)
\end{spec}

Reading ALL the URLs **asynchronously** with `async` 

\begin{code}
main7 = do (_, time) <- timeit $ asyncDownload 
           printf "TOTAL download time: %.2fs\n" time

asyncDownload :: IO [()]
asyncDownload
      = do tasks <- mapM (async . timeDownload) urls 
           mapM wait tasks 

amapM :: (a -> IO b) -> [a] -> IO [b]
amapM f xs 
  = do tasks <- mapM (async . f) xs 
       mapM wait tasks 


\end{code}

Spawn-then-wait in parallel is a general pattern;
lets **generalize** into `asyncMapM`

\begin{code}
asyncMapM :: (a -> IO b) -> [a] -> IO [b]
asyncMapM f xs = mapM (async . f) xs >>= mapM wait
\end{code}

**Note:** *Exact* type as `mapM` -- so you can
literally just drop-in replace to get parallelism.

Reading ALL URLs with `asyncMapM`

\begin{code}
main8 = do (_, time) <- timeit $ asyncMapM timeDownload urls
           printf "TOTAL download time: %.2fs\n" time
\end{code}


7. Java-style Synchronization via MVars
---------------------------------------

Next, lets see how `MVars` give us Java style `synchronize`, 
as a simple function, no need to extend the language.

**Key idea:** Execute an action **while holding lock**

**MVars as Locks**

\begin{code}
type Lock = MVar () 
\end{code}

**Acquiring Locks**

+ Just **take** the `MVar`s contents! 
+ Subsequent calls to `acquire` will block...

\begin{code}
acquire   :: MVar a -> IO a
acquire l = takeMVar l
\end{code}


**Releasing Locks**

+ Just **put back** the `MVar`s contents!
+ Subsequent calls to `acquire` will now get unblocked...

\begin{code}
release   :: MVar () -> IO ()
release l = putMVar l () 
\end{code}

**Synchronized "Blocks"**

Now, a Java style `synchronize` block 

```Java
synchronized (l) {
  statement
}
```

is just a simple Haskell function:

\begin{code}
synchronize       :: Lock -> IO a -> IO a
synchronize l act = do acquire l
                       x <- act
                       release l
                       return x
\end{code}

**Exercise** Can you think of a bug in the above? How would you fix it?


8. "Synchronized" Bank Accounts
-------------------------------


A `synchronize`d deposit that prevents races...

\begin{code}
main9 :: IO ()
main9 = do 
  l <- newMVar ()                                           -- global lock, with dummy unit value
  a <- newAccountIO 0                                       -- create the account
  asyncMapM (synchronize l . depositIO' a) (replicate 5 10) -- dump money with synchronize 
  showBalanceIO a                                           -- will be $50
\end{code}

**Exercise:** What happens if you comment out the `synchronize l` ?

Btw, why are we using `asyncMapM`  not something like `forkMapM`? 

Try to use this instead of `asyncMapM` above and see if you can figure it out. 

\begin{code}
forkMapM :: (a -> IO ()) -> [a] -> IO ()
forkMapM f xs = mapM_ (forkIO . f) xs
\end{code}

**Hint:** after forking, parent does not wait for children to finish...


9. GLOBAL Locks means Zero Concurrency 
--------------------------------------

Pretty silly if the **entire** bank froze up to handle
a **single** customer!


`AccountMV` has a local lock per account, lets simulate 
with explicit lock.

\begin{code}
data AccountL = AL { money :: IORef Int 
                   , lock  :: Lock  
                   }
\end{code}

Create a new "locked" account

\begin{code}
newAccountL   :: Int -> IO AccountL
newAccountL n = do m     <- newIORef n
                   l     <- newMVar ()
                   return $ AL m l

showBalanceL ::  AccountL -> IO ()
showBalanceL (AL r _) 
  = do bal <- readIORef r
       putStrLn $ "Current Balance: " ++ show bal

depositL ::  AccountL -> Int -> IO ()
depositL (AL r _) n
  = do i   <- myThreadId 
       bal <- readIORef r
       putStrLn $ printf "Thread id = %s read n = %d bal = %d" (show i) n bal 
       pauseRandom           -- comment out and you get right answer
       if ((bal + n) < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else do putStrLn $ printf "Thread id = %s write bal = %d" (show i) (bal + n)
                 writeIORef r (bal + n)
\end{code}

Now, lets do concurrent deposits, 
but make sure we use the *same* lock...

\begin{code}
main10 :: IO ()
main10 = do 
  a <- newAccountL 0                                              -- create the account
  asyncMapM (synchronize (lock a) . depositL a) (replicate 5 10)  -- dump money with synchronize 
  showBalanceL a                                                  -- will be $50
\end{code}


10. Transferring between accounts
---------------------------------

\begin{code}
transferL         ::  AccountL -> AccountL -> Int -> IO ()
transferL a1 a2 n = do depositL a1 $ 0 - n                  -- withdrawn n from a1
                       depositL a2 $ n                      -- deposit   n into a2
\end{code}

`syncTransfer` will prevent races ... but cause deadlocks 

\begin{code}
syncTransfer         ::  AccountL -> AccountL -> Int -> IO ()
syncTransfer a1 a2 n = 
  synchronize (lock a1) $ 
    synchronize (lock a2) $ 
      transferL a1 a2 n
\end{code}

**EXERCISE: TRY GET A DEADLOCK**


Can use a GLOBAL lock as in `main9` but zero concurrency ...

11. STM: Software Transactions
------------------------------

 
New type of trans-action

~~~~~{.haskell}
  data STM a    -- transactions that return an `a`
~~~~~

Executed via a special function call

~~~~~{.haskell}
  atomically :: STM a -> IO a
~~~~~

Only shared state is a special kind of transactional 
variable storing an `a`

~~~~~{.haskell}
  data TVar a
~~~~~

With special operations, to **create** a `TVar`

~~~~~{.haskell}
  newTVar :: a -> STM (TVar a)
~~~~~

to **read** a `TVar` 

~~~~~{.haskell}
  readTVar :: TVar a -> STM a
~~~~~

and, to **write** a `TVar`

~~~~~{.haskell}
  writeTVar :: TVar a -> a -> STM ()
~~~~~

Just like operations on `IORef` but with `STM` actions instead


Lets make a **transactional** account

\begin{code}
newtype AccountT = AT (TVar Int) 

newAccountT ::  Int -> STM AccountT
newAccountT n = AT <$> newTVar n


showBalanceT ::  AccountT -> IO ()
showBalanceT (AT r) 
  = do bal <- atomically $ readTVar r
       putStrLn $ "Current Balance: " ++ show bal

depositT ::  AccountT -> Int -> STM ()
depositT (AT r) n
  = do bal <- readTVar r
       if (bal + n < 0) 
         then retry  -- special "abort" action 
         else writeTVar r (bal + n)

main11 :: IO ()
main11 = do a <- atomically $ newAccountT 0
            asyncMapM (atomically . depositT a) (replicate 5 10) 
            showBalanceT a   -- should be $50
\end{code}

Transactions Compose
-------------------- 

Trivial to compose actions without worrying about *deadlock* or *race*

\begin{code}
transferT         ::  AccountT -> AccountT -> Int -> STM ()
transferT a1 a2 n = do depositT a1 $ 0 - n                  -- withdrawn n from a1
                       depositT a2 $ n                      -- deposit   n into a2
\end{code}

No need for **any** synchronization, just say `atomic` 

\begin{code}
atomicTransferT :: AccountT -> AccountT -> Int -> IO ()
atomicTransferT a1 a2 n = atomically $ transferT a1 a2 n
\end{code}

\begin{code}
          
-----------------------------------------------------------
-- | Top-level Driver -------------------------------------
-----------------------------------------------------------

-- main = putStrLn "Hello world"

main, main2, main3, main6, main7, main8 :: IO ()
main         = getArgs >>= (run . head)
  where 
    run "1"  = main1
    run "2"  = main2
    run "3"  = main3
    run "4"  = main4
    run "5"  = main5
    run "6"  = main6
    run "7"  = main7
    run "8"  = main8
    run "9"  = main9
    run "10" = main10
    run "11" = main11
    run cmd  = putStrLn $ "Say what? " ++ cmd


----------------------------------------------------------

-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.

-- | Returns the number of realtime seconds an action takes 
-- https://github.com/simonmar/par-tutorial/blob/master/code/TimeIt.hs

timeit :: IO a -> IO (a,Double)
timeit io = do
     t0 <- getCurrentTime
     a <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))

-- | Simple wrapper around HTTP, allowing proxy use
-- https://github.com/simonmar/par-tutorial/blob/master/code/GetURL.hs

getURL :: String -> IO B.ByteString
getURL url = do
  Network.Browser.browse $ do
    setCheckForProxy True
    setDebugLog Nothing
    setOutHandler (const (return ()))
    (_, rsp) <- request (getRequest' (escapeURIString isUnescapedInURI url))
    return (rspBody rsp)
  where
   getRequest' :: String -> Request B.ByteString
   getRequest' urlString =
    case parseURI urlString of
      Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
      Just u  -> mkRequest GET u

\end{code}
