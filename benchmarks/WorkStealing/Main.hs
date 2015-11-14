{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE CPP, NamedFieldPuns #-}
module Main where

import Control.Applicative
import Control.Monad

import Control.Concurrent
#ifdef PASTMTL2
import Control.TL2.STM
#else
import Control.Concurrent.STM
#endif

import Data.Maybe
import Data.Word
import Throughput
import Options.Applicative
import System.Environment
import qualified Data.Vector as V
import System.Random.PCG.Fast.Pure
import TDeque
import Data.List(zip5)
import GHC.Conc(numCapabilities)
import Control.Monad
import Debug.Trace
import System.Random.Shuffle
import System.Random

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Word -> Word -> RGen -> IO ((Word,Word), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)

data WSOpts = WSOpts
    { _entries      :: Word
    , _threads      :: Int
    , _initOnly     :: Bool
    , _withoutTM    :: Bool
    , _atomicGroups :: Int
    , _mix          :: Double
    , _qSize        :: Int
    , _maxSpin      :: Int
    , _throughput   :: Int
    } deriving (Show)

rbTreeOpts :: Parser WSOpts
rbTreeOpts = WSOpts
    <$> (option auto)
        (value 800 <> long "entries"      <> short 'e' <> help "Number of values in the tree")
    <*> (option auto)
        (value 8   <> long "threads"      <> short 't' <> help "Number of threads")
    <*> switch
        (             long "initOnly"     <> short 'i' <> help "Initialize only")
    <*> switch
        (             long "withoutTM"    <> short 'w' <> help "No transactions")
    <*> (option auto)
        (value 1   <> long "atomicGroups" <> short 'g' <> help "Lookups per transaction")
    <*> (option auto)
        (value 90  <> long "mix"          <> short 'm' <> help "Read mix percent")
    <*> (option auto)
        (value 40 <> long "dequeSize" <> short 'd' <> help "Size of the Scheduler deques")
    <*> (option auto)
        (value 1 <> long "taskLength" <> short 'l' <> help "Max spin iterations for a task to complete")
    <*> (option auto)
        (value 1000 <> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

data Sched a = Sched{
     no :: !Int,
     workpool :: Deque a,
     gen :: RGen,
     stealCounter :: CountIO,
     localCounter :: CountIO,
     mix :: Double,
     maxWork :: Int --maximum task size
}

type Task = ()

workerThread :: (Sched Int, [Sched Int]) -> IO ()
workerThread (q@Sched{no=myID, workpool, gen, stealCounter, localCounter, mix, maxWork}, allScheds) = go gen 0
  where
    steal [] = retry  --go to sleep until someone posts work to do
    steal (Sched{no,workpool}:scheds)
          | myID == no = steal scheds -- already tried to pop my deque
          | otherwise = stealWork workpool `orElse` steal scheds
    go g 0 = do
       r <- uniformB 100 g
       if r > mix
       then do --spawn task
            r <- uniformB maxWork g
            atomically $ pushWork workpool r
            incCount localCounter
            go g r
       else do --finish sequential task
            (task, c) <- atomically $ (popWork workpool >>= \t -> return(t, localCounter)) `orElse`
                                      (steal allScheds >>= \t -> return(t, stealCounter))
            incCount c
            go g task
    go g i = go g (i-1) --continue working on task
            
mainThread :: (Sched Int, [Sched Int]) -> IO()
mainThread (q@Sched{no, workpool, gen, stealCounter, localCounter, mix, maxWork}, scheds) = go gen 0
  where
    go g 0 = do
       r <- uniformB maxWork g
       atomically $ pushWork workpool r
       incCount localCounter
       go g r
    go g i = go g (i-1) --continue working on task
       
main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> rbTreeOpts)
                (fullDesc <> progDesc "Work stealing benchmark." <> header prog)
    opts <- execParser p

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !m = _mix        opts
        !s = _throughput opts
        ! threads = _threads opts
        !maxSpin = _maxSpin opts
    gs <- initGens threads    
    lcs <- replicateM threads $ newCount 0
    scs <- replicateM threads $ newCount 0

    workpools <- replicateM threads (atomically $ newDeque (_qSize opts))
    
    let states = [ Sched{no = x, workpool= wp, gen = g, localCounter=lc, stealCounter=sc, mix=m, maxWork = maxSpin}
                   | (x, wp, g, lc, sc) <- zip5 [0..] workpools gs lcs scs]
        --randomize the order of schedulers for each thread.  see:
        --https://github.com/simonmar/monad-par/blob/master/monad-par/Control/Monad/Par/Scheds/TraceInternal.hs#L113-L115
        !shuffledStates = map (\s@Sched{no} -> (s, shuffle' states numCapabilities (mkStdGen no))) states
        (mainState : workerStates) = shuffledStates
        
    (t,ta) <- throughputMain (s*1000) (mainThread mainState : map workerThread workerStates)
    localCount <- sum <$> forM lcs readCount
    stealCount <- sum <$> forM scs readCount
    putStrLn $ unwords [ "benchdata:"
                         , "run-time"    , show t
                         , "no-kill-time", show ta
                         , "transactions", show (localCount + stealCount)
                         , "local deque ops", show localCount
                         , "steal ops", show stealCount
                         , "prog"        , prog
                         , "threads"     , show (_threads opts)
                         , "entries"     , show e
                         ]
