{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE CPP, NamedFieldPuns, MagicHash, UnboxedTuples #-}
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
--import TDeque
import ResizableDeque

import Data.List(zip5)
import GHC.Conc(numCapabilities)
import Control.Monad
import Debug.Trace
import System.Random.Shuffle
import System.Random

import GHC.Base(IO(..))
import GHC.Prim(printSTMStats#)

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
    , _depth        :: Double
    , _dequeSize    :: Int
    , _branchFactor :: Double
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
        (value 10  <> long "dagDepth"     <> short 'D' <> help "Maximum depth of the DAG")
    <*> (option auto)
        (value 72 <> long "dequeSize"     <> short 'd' <> help "Size of the Scheduler deques")
    <*> (option auto)
        (value 13 <> long "branchFactor"  <> short 'b' <> help "Branching factor of the DAG")
    <*> (option auto)
        (value 1000 <> long "throughput"  <> short 's' <> help "Throughput runtime in milliseconds")

data Sched a = Sched{
     no :: !Int,
     workpool :: Deque a,
     gen :: RGen,
     stealCounter :: CountIO,
     localCounter :: CountIO,
     depth :: Double,
     branchFactor :: Double
}

printStats :: IO()
printStats = IO $ \s -> (# printSTMStats# s, () #)

type Task = Double
  
worker :: (Sched Task, [Sched Task]) -> IO ()
worker (Sched{no=myID, workpool, gen, stealCounter, localCounter, branchFactor, depth}, allScheds) = go gen
  where
    steal [] = retry  --go to sleep until someone posts work to do
    steal (Sched{no,workpool}:scheds)
          | myID == no = steal scheds -- already tried to pop my deque
          | otherwise = stealWork workpool `orElse` steal scheds
    go g = do
       (task, c) <- atomically $ (popWork workpool >>= \t -> return(t, localCounter)) `orElse`
                                 (steal allScheds >>= \t -> return(t, stealCounter))
       incCount c
       r <- uniformB branchFactor g
       let numChildren = round (r * (1.0 - (task / depth)))
           children = replicate numChildren (task + 1.0)
       forM_ children (\c -> pushWork workpool c)
       go g

mainThread :: (Sched Task, [Sched Task]) -> IO ()
mainThread (Sched{no=myID, workpool, gen, stealCounter, localCounter, branchFactor, depth}, allScheds) = go gen
  where
    steal :: [Sched Task] -> STM Task
    steal [] = return 0.0 --if no one has any work to steal, start over at level 0
    steal (Sched{no,workpool}:scheds)
          | myID == no = steal scheds -- already tried to pop my deque
          | otherwise = stealWork workpool `orElse` steal scheds
    go g = do
       (task, c) <- atomically $ (popWork workpool >>= \t -> return(t, localCounter)) `orElse`
                                 (steal allScheds >>= \t -> return(t, stealCounter))
       incCount c
       r <- uniformB branchFactor g
       let numChildren = round (r * (1.0 - (task / depth)))
           children = replicate numChildren (task + 1.0)
       forM_ children (\c -> pushWork workpool c)
       go g

main :: IO ()
main = do
    prog <- getProgName
    let p = info (helper <*> rbTreeOpts)
                (fullDesc <> progDesc "Work stealing benchmark." <> header prog)
    opts <- execParser p

    putStrLn ("Initial deque size is " ++ show (_dequeSize opts))

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !s = _throughput opts
        !threads = _threads opts
        !branchFactor = _branchFactor opts
        !depth = _depth opts
        
    gs <- initGens threads    
    lcs <- replicateM threads $ newCount 0
    scs <- replicateM threads $ newCount 0

    workpools <- replicateM threads (atomically $ newDeque (_dequeSize opts))
    
    let states = [ Sched{no = x, workpool= wp, gen = g, localCounter=lc, stealCounter=sc, branchFactor, depth}
                   | (x, wp, g, lc, sc) <- zip5 [0..] workpools gs lcs scs]
        --randomize the order of schedulers for each thread.  see:
        --https://github.com/simonmar/monad-par/blob/master/monad-par/Control/Monad/Par/Scheds/TraceInternal.hs#L113-L115
        !shuffledStates = map (\s@Sched{no} -> (s, shuffle' states numCapabilities (mkStdGen no))) states
        (m@(Sched{workpool}, _) : workers) = shuffledStates

    pushWork workpool 0.0 --initialize the first scheduler

    (t,ta) <- throughputMain (s*1000) (mainThread m : map worker workers)
    localCount <- sum <$> forM lcs readCount
    stealCount <- sum <$> forM scs readCount
    putStrLn $ unwords [ "benchdata:"
                         , "run-time"    , show t
                         , "no-kill-time", show ta
                         , "transactions", show (localCount + stealCount)
                         , "local-deque-ops", show localCount
                         , "steal-ops", show stealCount
                         , "prog"        , prog
                         , "threads"     , show (_threads opts)
                         , "entries"     , show e
                         ]
    printStats