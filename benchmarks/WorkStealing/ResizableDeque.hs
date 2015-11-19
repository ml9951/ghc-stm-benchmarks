{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, BangPatterns, ExistentialQuantification, NamedFieldPuns #-}

--A double ended queue (deque) to implement work stealing.  Base on:
--https://hackage.haskell.org/package/chaselev-deque-0.5.0.5/docs/src/Data-Concurrent-Deque-ChaseLev.html

module ResizableDeque(newDeque, pushWork, popWork, stealWork, Deque)
where

import Data.Array.MArray
import Debug.Trace

#ifdef PASTMTL2
import Control.TL2.STM
import Data.Array (Array, bounds)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..),
                        IArray(numElements))
import Data.Ix (rangeSize)
import Data.Typeable (Typeable)

newtype TArray i e = TArray (Array i (TVar e)) deriving (Eq, Typeable)

instance MArray TArray e STM where
    getBounds (TArray a) = return (bounds a)
    newArray b e = do
        a <- rep (rangeSize b) (newTVar e)
        return $ TArray (listArray b a)
    newArray_ b = do
        a <- rep (rangeSize b) (newTVar arrEleBottom)
        return $ TArray (listArray b a)
    unsafeRead (TArray a) i = readTVar $ unsafeAt a i
    unsafeWrite (TArray a) i e = writeTVar (unsafeAt a i) e
    getNumElements (TArray a) = return (numElements a)

rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          !x <- m
          go (i-1) (x:xs)

#else
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
#endif


data Deque a = Deque{
     top :: TVar Int,
     bottom :: TVar Int,
     arrPtr :: TVar (TArray Int a) --extra indirection so that we can resize
}    

newDeque :: Int -> STM (Deque a)
newDeque size = do
         top <- newTVar 0
         bottom <- newTVar 0
         arr <- newArray_ (0, size-1)
         arrPtr <- newTVar arr
         return(Deque top bottom arrPtr)

--from: https://hackage.haskell.org/package/chaselev-deque-0.5.0.5/docs/src/Data-Concurrent-Deque-ChaseLev.html
-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ !start !end _fn | start > end = return ()
for_ !start !end fn = loop start
     where
        loop !i | i == end  = return ()
                | otherwise = do fn i; loop (i+1)

pushWork :: Deque a -> a -> IO ()
pushWork d@Deque{top,bottom,arrPtr} obj = do
      x <- atomically $ tryPush
      if x
      then return()
      else do
           t <- readTVarIO top
           atomically $ resize t
      where
        resize t = do
                b <- readTVar bottom
                arr <- readTVar arrPtr
                (l, u) <- getBounds arr
                let !newUpper = u + u + 1
                    !newLen = newUpper + 1
                    !n = u + 1
                newarr <- newArray_ (l, newUpper)
                for_ t b $ \ind -> do
                      x <- readArray arr (ind `mod` n)
                      writeArray newarr (ind `mod` newLen) x
                writeArray newarr (b `mod` newLen) obj
                writeTVar bottom (b+1)
                writeTVar arrPtr newarr
        tryPush = do
                b <- readTVar bottom
                t <- readTVar top
                arr <- readTVar arrPtr
                bounds <- getBounds arr
                let size = b - t
                    len = rangeSize bounds
                if size >= len-1
                then return False
                else do
                     writeArray arr (b `mod` len) obj
                     writeTVar bottom (b+1)
                     return True
          
popWork :: Deque a -> STM a
popWork Deque{top,bottom,arrPtr} = do
     b <- readTVar bottom
     t <- readTVar top
     arr <- readTVar arrPtr
     bounds <- getBounds arr
     let size = b - t
         len = rangeSize bounds
     if size == 0
     then retry
     else do
          let !b' = b-1
          writeTVar bottom b'
          obj <- readArray arr (b' `mod` len)
          return obj

stealWork :: Deque a -> STM a
stealWork Deque{top,bottom,arrPtr} = do
     b <- readTVar bottom
     t <- readTVar top
     arr <- readTVar arrPtr
     bounds <- getBounds arr
     let size = b - t
         len = rangeSize bounds
     if size == 0 
     then retry
     else do
          writeTVar top (t+1)
          obj <- readArray arr (t `mod` len)
          return obj
