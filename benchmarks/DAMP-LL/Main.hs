{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE CPP                #-}
module Main where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Random

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

import Debug.Trace
import qualified Data.Vector as V

import System.Random.PCG.Fast.Pure

#ifdef CAS_LL
#define ATOMIC 
import CASLL
#elif MLCLL
import LockCoupling
#define ATOMIC 
#elif STRAIGHTSTM
#define ATOMIC atomically 
import StraightForwardSTM
#elif DISSECTEDSTM
import DissectedSTM
#define ATOMIC atomically
#else
#error "No linked list implementation specified"
#endif

type BenchTree = ListHandle Word

type RGen = GenIO

initGens :: Int -> IO [RGen]
initGens threads = mapM initialize (map fromIntegral [1..threads])

samples :: Word -> Word -> RGen -> IO ((Word,Word), RGen)
samples sampleMax total g = do
    r <- uniformB sampleMax g
    v <- uniformB (total - 2) g
    return ((r, v+1), g)

data RBTreeOpts = RBTreeOpts
    { _entries      :: Word
    , _threads      :: Int
    , _initOnly     :: Bool
    , _withoutTM    :: Bool
    , _atomicGroups :: Int
    , _mix          :: Double
    , _throughput   :: Int
    } deriving (Show)

rbTreeOpts :: Parser RBTreeOpts
rbTreeOpts = RBTreeOpts
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
        (value 1000<> long "throughput"   <> short 's' <> help "Throughput runtime in milliseconds")

runRSTMEmpty :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMEmpty count g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 10000 :: Word

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      (!(toPercent -> !r,!v),!g') <- samples sampleMax total g
--      traceEventIO "beginT"
      case () of
        () | r <= readRate   -> ATOMIC (doNothing t v)
           | r <= insertRate -> ATOMIC (doNothing t v)
           | otherwise       -> ATOMIC (doNothing t v)
--      traceEventIO "endT"
      incCount count
      go g'
    {-# NOINLINE go #-}

--    doNothing :: BenchTree -> Word -> STM ()
    doNothing t 0 = return ()
    doNothing t _ = return ()


runRSTMSingle :: CountIO -> RGen -> BenchTree -> Word -> Double -> IO ()
runRSTMSingle count g t total readRate = go g
  where
    insertRate = ((100 - readRate) / 2) + readRate
    sampleMax = 10000 :: Word

    readLevel :: Word
    !readLevel   = floor $ readRate / 100.0 * fromIntegral sampleMax
    insertLevel :: Word
    !insertLevel = floor $ insertRate / 100.0 * fromIntegral sampleMax

    toPercent :: Word -> Double
    toPercent r = fromIntegral r * 100 / fromIntegral sampleMax

    go g = do
      (!(!r,!v),!g') <- samples sampleMax total g
      --      traceEventIO "beginT"
      case () of
        () | r <= readLevel   -> (get t v          >> return ())
           | r <= insertLevel -> (insert t v       >> return ())
           | otherwise        -> (delete t v       >> return ())
--      traceEventIO "endT"
      incCount count
      go g'
    {-# NOINLINE go #-}

main :: IO ()
main = do
    whichLL
    prog <- getProgName
    let p = info (helper <*> rbTreeOpts)
                (fullDesc <> progDesc "RBTree benchmark." <> header prog)
    opts <- execParser p

    setNumCapabilities (_threads opts)

    let !e = _entries    opts
        !m = _mix        opts
        !s = _throughput opts

    gs <- initGens (_threads opts)

    t <- newList
    forM_ [0,2..e] $ \a -> insert t a

    cs <- replicateM (_threads opts) $ newCount 0

    unless (_initOnly opts) $ do 
      -- loop forever, stopping after s milliseconds.
      (t,ta) <- case () of
            () | _withoutTM opts -> 
                    throughputMain (s * 1000) (zipWith (\c g -> runRSTMEmpty  c g t e m) cs gs)
               | otherwise ->
                    throughputMain (s * 1000) (zipWith (\c g -> runRSTMSingle c g t e m) cs gs)
      trans <- sum <$> forM cs readCount
      putStrLn $ unwords [ "benchdata:"
                         , "run-time"    , show t
                         , "no-kill-time", show ta
                         , "transactions", show trans
                         , "prog"        , prog
                         , "threads"     , show (_threads opts)
                         , "entries"     , show e
                         ]
