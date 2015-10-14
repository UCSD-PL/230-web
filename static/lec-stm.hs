#!/usr/bin/env runhaskell

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

-----------------------------------------------------------
-- | 1. Mutable State Via IORef ---------------------------
-----------------------------------------------------------

newtype AccountIO = AIO (IORef Int)

newAccountIO ::  Int -> IO AccountIO
newAccountIO n 
  | n >= 0 
  = liftM AIO (newIORef n)
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       liftM AIO (newIORef 0)

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
           mapM_ (depositIO a) (replicate 5 10) 
           showBalanceIO a   -- should be $50


-----------------------------------------------------------
-- | 2. Forking A Thread ----------------------------------
-----------------------------------------------------------

main2 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A') -- thread that writes 'A'
           forkIO $ forever (putChar 'B') -- thread that writes 'B'
           threadDelay (10^5)             -- shutdown after 1 sec

-----------------------------------------------------------
-- | 3. Randomly Fuzzing the Thread Scheduler -------------
-----------------------------------------------------------

toss       :: Int -> Int -> IO Int
toss lo hi = getStdRandom (randomR (lo, hi))

pauseRandom = do n <- toss 0 10
                 threadDelay $ n * (10 ^ 5)

main3 = do hSetBuffering stdout NoBuffering
           forkIO $ forever (putChar 'A' >> pauseRandom) -- thread that writes 'A'
           forkIO $ forever (putChar 'B' >> pauseRandom) -- thread that writes 'B'
           threadDelay (10^6)                             -- shutdown after 1 sec

-----------------------------------------------------------
-- | 3. Data Races due to sharing -------------------------
-----------------------------------------------------------

-- Bank account revisited; depositing with different threads, and fuzzed scheduler

depositIO' ::  AccountIO -> Int -> IO ()
depositIO' (AIO r) n
  = do i   <- myThreadId 
       bal <- readIORef r
       putStrLn $ printf "Thread id = %s read n = %d bal = %d" (show i) n bal 
       pauseRandom           -- comment out and you get right answer
       if ((bal + n) < 0) 
         then putStrLn $ "Sorry, cannot withdraw. Balance below " ++ show n 
         else do putStrLn $ printf "Thread id = %s write bal = %d" (show i) (bal + n)
                 writeIORef r (bal + n)

main4 ::  IO ()
main4 = do a <- newAccountIO 0
           mapM_ (forkIO . depositIO' a) (replicate 5 10) 
           threadDelay (5 * 10^6)   -- shutdown after 1 sec
           showBalanceIO a          -- should be $50 but isn't due to DATA RACES!

-----------------------------------------------------------
-- | 4. MVars: Vanilla ------------------------------------
-----------------------------------------------------------

{- Shared Message-Box-Variables MVar
    
    -- Message Box has EITHER an `a` or is EMPTY
    data MVar a

    -- Create an EMPTY Reference
    newEmptyMVar :: IO (MVar a)
    
    -- Create a FULL reference
    newMVar	     :: a -> IO (MVar a)

    -- Blocks until box is full, takes out contents
    takeMVar	 :: MVar a -> IO a

    -- Overwrites contents with new contents
    putMVar	     :: MVar a -> a -> IO ()

 Uses:

 (1) An MVar is a useful container for shared mutable state, e.g. common design pattern 
     where threads need read and write access to some state, is to represent the state 
     value as an ordinary immutable Haskell data structure stored in an MVar.
 
 (2) An MVar is a one-place channel, which can be used for asynchronous communication
     between two threads.
 
 (3) An MVar () is a lock; takeMVar acquires the lock and putMVar releases it again. 
     An MVar used in this way can protect shared mutable state or critical sections.

-}

-----------------------------------------------------------
-- | 5. MVars: BankAccount/Deposit ------------------------
-----------------------------------------------------------


-- Bank account revisited; using MVars, depositing with different
-- threads, and fuzzed scheduler

newtype AccountMV = AMV (MVar Int)

newAccountMV :: Int -> IO AccountMV
newAccountMV n 
  | n >= 0 
  = liftM AMV (newMVar n)
  | otherwise
  = do putStrLn "Do I look like a communist?!!"
       liftM AMV (newMVar 0)

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
       if (bal + n < 0) 
         then do putMVar r bal      -- Put the balance back in
                 putStrLn $ "Cannot withdraw, balance below " ++ show n 
         else putMVar r (bal + n)   -- Put the extra MONEY into the account 

main5 :: IO ()
main5 = do a <- newAccountMV 0
           mapM_ (forkIO . depositMV a) (replicate 5 10) 
           showBalanceMV a          -- should be $50, but why so slow?

-----------------------------------------------------------
-- | 6. Asynchrony Via MVars ------------------------------
-----------------------------------------------------------

{- Can implement "asynchronous" function calls (aka "futures", "promises") using MVars
   Other languages "features" are Haskell's, "functions"...
 -}
   
-- | A type representing an Asynchronous Computation
newtype Async a = Async (MVar a)

-- | Function to execute an IO action `async`-hronously 

async :: IO a -> IO (Async a)
async action = do m <- newEmptyMVar
                  forkIO (action >>= putMVar m)
                  return (Async m)

-- | Function to `wait` for the result of `Async` computation
wait :: Async a -> IO a
wait (Async m) = readMVar m

-- | Application: Download a bunch of URLs asynchronously,
-- that is, without blocking on each other

{- To demo the below, build with:
   
        $ ghc --make -threaded lec-stm.hs
   
   Run with:

        $ ./lec-stm 6 +RTS -n4
        $ ./lec-stm 7 +RTS -n4
        $ ./lec-stm 8 +RTS -n4
 -}

-- | A list of URLs

urls = [ "http://www.google.com"
       , "http://www.buzzfeed.com"
       , "http://www.reddit.com/r/haskell"
       , "http://www.nytimes.com"
       ]

-- | Reading a SINGLE URL

timeDownload url = do (page, time) <- timeit $ getURL url
                      printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

-- | Reading ALL the URLs in sequence

main6 = do (_, time) <- timeit $ mapM timeDownload urls
           printf "TOTAL download time: %.2fs\n" time

-- | Reading ALL the URLs with `async` 

main7 = do (_, time) <- timeit $ (mapM (async . timeDownload ) urls >>= mapM wait)
           printf "TOTAL download time: %.2fs\n" time

-- | Generalize into `asyncMapM`

asyncMapM :: (a -> IO b) -> [a] -> IO [b]
asyncMapM f xs = mapM (async . f) xs >>= mapM wait

-- | Reading ALL URLs with `asyncMapM`

main8 = do (_, time) <- timeit $ asyncMapM timeDownload urls
           printf "TOTAL download time: %.2fs\n" time

-----------------------------------------------------------
-- | 7. Lock/Synchronize Via MVars ------------------------
-----------------------------------------------------------

-- `synchronize` is NOT a keyword, JUST a function...

synchronize :: MVar b -> IO a -> IO a
synchronize lock action = do x <- takeMVar lock
                             z <- action
                             putMVar lock x 
                             return z


-- A `synchronize` deposit that prevents races...

main9 :: IO ()
main9 = do 
  l <- newMVar 0                                            -- global lock, with dummy unit value
  a <- newAccountIO 0                                       -- create the account
  asyncMapM (synchronize l . depositIO' a) (replicate 5 10) -- dump money with synchronize 
  showBalanceIO a                                           -- will be $50

-- What happens if you comment out the `synchronize l` ?

-- | Btw, why are we using asyncMapM  not something like? Try to use this
-- instead of asyncMapM above and see if you can figure it out. 
-- Hint: after forking, parent does not wait for children to finish...

forkMapM :: (a -> IO ()) -> [a] -> IO ()
forkMapM f xs = mapM_ (forkIO . f) xs

-----------------------------------------------------------
-- | 8. Zero Concurrency Above, because GLOBAL Lock -------
-----------------------------------------------------------

-- AccountMV has a local lock per account, lets simulate with explicit lock.

data AccountL = AL { money :: IORef Int 
                   , lock  :: MVar ()   
                   }

newAccountL n = liftM2 AL (newIORef n) (newMVar ())

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

-- Make sure we use the *same* lock...

main10 :: IO ()
main10 = do 
  a <- newAccountL 0                                              -- create the account
  asyncMapM (synchronize (lock a) . depositL a) (replicate 5 10)  -- dump money with synchronize 
  showBalanceL a                                                  -- will be $50

--------------------------------------------------------------------
-- | Transferring between accounts ---------------------------------
--------------------------------------------------------------------

transferL         ::  AccountL -> AccountL -> Int -> IO ()
transferL a1 a2 n = do depositL a1 $ 0 - n                  -- withdrawn n from a1
                       depositL a2 $ n                      -- deposit   n into a2

-- | `syncTransfer` will prevent races ... but cause deadlocks 

syncTransfer         ::  AccountL -> AccountL -> Int -> IO ()
syncTransfer a1 a2 n = 
  synchronize (lock a1) $ 
    synchronize (lock a2) $ 
      transferL a1 a2 n

-- | Can use a GLOBAL lock as in `main9` but zero concurrency ... bit pointless.

-----------------------------------------------------------
-- | 9. STM -----------------------------------------------
-----------------------------------------------------------

{-  New type of trans-action
 
        data STM a                          -- transactions that return an `a`

    Executed via a special function call

        atomically :: STM a -> IO a

    Only shared state is a special kind of variable

        data TVar a                         -- transactional variables storing an `a`

    With the operations,

        newTVar :: a -> STM (TVar a)        -- create a TVar

        readTVar :: TVar a -> STM a         -- read a TVar

        writeTVar :: TVar a -> a -> STM ()  -- write a TVar

    Just like operations on IORef but with STM actions instead

 -} 

newtype AccountT = AT (TVar Int) 

newAccountT ::  Int -> STM AccountT
newAccountT n = liftM AT (newTVar n)

showBalanceT ::  AccountT -> IO ()
showBalanceT (AT r) 
  = do bal <- atomically $ readTVar r
       putStrLn $ "Current Balance: " ++ show bal

depositT ::  AccountT -> Int -> STM ()
depositT (AT r) n
  = do bal <- readTVar r
       if (bal + n < 0) 
         then retry                 -- special "abort" action 
         else writeTVar r (bal + n)

main11 :: IO ()
main11 = do a <- atomically $ newAccountT 0
            asyncMapM (atomically . depositT a) (replicate 5 10) 
            showBalanceT a   -- should be $50


-----------------------------------------------------------------------
-- | Transactions Compose! --------------------------------------------
-----------------------------------------------------------------------

transferT         ::  AccountT -> AccountT -> Int -> STM ()
transferT a1 a2 n = do depositT a1 $ 0 - n                  -- withdrawn n from a1
                       depositT a2 $ n                      -- deposit   n into a2


-- | No need for ANY synchronization, just say the word!

atomicTransferT a1 a2 n = atomically $ transferT a1 a2 n


          
-----------------------------------------------------------
-- | Top-level Driver -------------------------------------
-----------------------------------------------------------

-- main = putStrLn "Hello world"

main        = getArgs >>= ( go . head )
  where 
    go "1"  = main1
    go "2"  = main2
    go "3"  = main3
    go "4"  = main4
    go "5"  = main5
    go "6"  = main6
    go "7"  = main7
    go "8"  = main8
    go "9"  = main9
    go "10" = main10
    go "11" = main11
    go cmd = putStrLn $ "Say what? " ++ cmd


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

