{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, BangPatterns, ExistentialQuantification, NamedFieldPuns #-}

--A double ended queue (deque) to implement work stealing.  Base on:
--https://hackage.haskell.org/package/chaselev-deque-0.5.0.5/docs/src/Data-Concurrent-Deque-ChaseLev.html

module TDeque(newDeque, pushWork, popWork, stealWork, Deque)
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
     arr :: TArray Int a
}

newDeque :: Int -> STM (Deque a)
newDeque size = do
         top <- newTVar 0
         bottom <- newTVar 0
         arr <- newArray_ (0, size-1)
         return(Deque top bottom arr)

--Currently, this just retries if the deque is full.
--We could resize the array, but that would require
--another level of indirection for the array
pushWork :: Deque a -> a -> Int -> STM ()
pushWork Deque{top,bottom,arr} obj  id= do
      b <- readTVar bottom
      t <- readTVar top
      bounds <- getBounds arr
      let size = b - t
          len = rangeSize bounds
      if size >= len-1
      then trace (show id ++ ": deque is full\n") retry
      else do
           writeArray arr (b `mod` len) obj
           writeTVar bottom (b+1)

popWork :: Deque a -> Int -> STM a
popWork Deque{top,bottom,arr} id = do
     b <- readTVar bottom
     t <- readTVar top
     bounds <- getBounds arr
     let size = b - t
         len = rangeSize bounds
     if size == 0
     then trace (show id ++ ": local deque is empty\n") retry
     else do
          writeTVar bottom (b-1)
          obj <- readArray arr ((b-1) `mod` len)
          return obj

stealWork :: Deque a -> Int -> STM a
stealWork Deque{top,bottom,arr} id = do
     b <- readTVar bottom
     t <- readTVar top
     bounds <- getBounds arr
     let size = b - t
         len = rangeSize bounds
     if size == 0
     then trace (show id ++ ": trying to steal, but deque is empty\n") retry
     else do
          writeTVar top (t+1)
          obj <- readArray arr (t `mod` len)
          return obj



{-
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

resizeArray top bottom topPtr botPtr obj arrPtr len obj = do
            rawArr <- readTVar arrPtr
            let newLen = len + len - 1
            newarr <- newArray_ (0, newLen)
            for_ top bottom $ \ind -> do
                 x <- readArray rawArr (ind `mod` len)
                 writeArray newarr (ind `mod` newLen) x
            writeArray newarr 
            writeTVar arrPtr newarr
            return newLen
            

--Currently, this just retries if the deque is full.
--We could resize the array, but that would require
--another level of indirection for the array
pushWork :: Deque a -> a -> STM ()
pushWork Deque{top,bottom,arrPtr} obj = do
      b <- readTVar bottom
      t <- readTVar top
      bounds <- getBounds arr
      let size = b - t
          len = rangeSize bounds
      if size >= len-1
      then newLen <- resizeArray top bottom obj arrPtr len obj
      else do
           arr <- readTVar arrPtr
           writeArray arr (b `mod` len) obj
           writeTVar bottom (b+1)

popWork :: Deque a -> STM a
popWork Deque{top,bottom,arr} = do
     b <- readTVar bottom
     t <- readTVar top
     bounds <- getBounds arr
     let size = b - t
         len = rangeSize bounds
     if size == 0
     then retry
     else do
          writeTVar bottom (b+1)
          obj <- readArray arr (b `mod` len)
          return obj

stealWork :: Deque a -> STM a
stealWork Deque{top,bottom,arr} = do
     b <- readTVar bottom
     t <- readTVar top
     bounds <- getBounds arr
     let size = b - t
         len = rangeSize bounds
     if size == 0
     then retry
     else do
          writeTVar top (t+1)
          obj <- readArray arr (t `mod` len)
          return obj
-}