{-# LANGUAGE CPP #-}

-- Based on the work stealing scheduler from the Par monad
-- https://github.com/simonmar/monad-par/blob/master/monad-par/Control/Monad/Par/Scheds/TraceInternal.hs

module WorkStealing()
where

import System.Random.PCG.Fast.Pure
import TDeque
import GHC.Conc(numCapabilities)
import Control.Monad

#ifdef PASTMTL2
import Control.TL2.STM
#else
import Control.Concurrent.STM
#endif

data Sched a = Sched{
     no :: !Int,
     workpool :: Deque a,
     scheds :: [Sched a]
}

type Task = ()

runPar :: Task -> Int -> IO()
runPar t qSize = do
       workpools <- replicateM numCapabilities (atomically $ newDeque qSize) --create scheduling deques
       let states = [ Sched{no = x, workpool=wp, scheds = states}  
                    | (x, wp) <- zip [0..] workpools]
           mainThread = 
       return()


