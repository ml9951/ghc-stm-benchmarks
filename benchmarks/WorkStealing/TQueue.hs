{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module TQueue (
        -- * TQueue
        TQueue,
        newTQueue,
        readTQueue,
        writeTQueue,
        mkTQArray,
        TQArray(..)
    ) where

import GHC.Conc

import Data.Typeable (Typeable)
import Data.Array (Array, bounds)
import Data.Array.Base (listArray, arrEleBottom, unsafeAt, MArray(..),
                        IArray(numElements))
import Data.Ix (rangeSize)
import Data.Typeable (Typeable)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)


-- | 'TQueue' is an abstract type representing an unbounded FIFO channel.
--
-- @since 2.4
data TQueue a = TQueue {-# UNPACK #-} !(TVar [a])
                       {-# UNPACK #-} !(TVar [a])
  deriving Typeable

instance Eq (TQueue a) where
  TQueue a _ == TQueue b _ = a == b

-- |Build and returns a new instance of 'TQueue'
newTQueue :: STM (TQueue a)
newTQueue = do
  read  <- newTVar []
  write <- newTVar []
  return (TQueue read write)

-- |Write a value to a 'TQueue'.
writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

-- |Read the next value from the 'TQueue'.
readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do writeTVar read xs'
                  return x
    [] -> do ys <- readTVar write
             case ys of
               [] -> retry
               _  -> case reverse ys of
                       [] -> error "readTQueue"
                       (z:zs) -> do writeTVar write []
                                    writeTVar read zs
                                    return z

-- |TArray is a transactional array, supporting the usual 'MArray'
-- interface for mutable arrays.
--
-- It is currently implemented as @Array ix (TVar e)@,
-- but it may be replaced by a more efficient implementation in the future
-- (the interface will remain the same, however).
--
newtype TQArray i e = TQArray (Array i (TQueue e)) deriving (Eq, Typeable)

instance MArray TQArray e STM where
    getBounds (TQArray a) = return (bounds a)
    newArray b e = do
        a <- rep (rangeSize b) newTQueue
        return $ TQArray (listArray b a)
    newArray_ b = do
        a <- rep (rangeSize b) newTQueue
        return $ TQArray (listArray b a)
    unsafeRead (TQArray a) i = readTQueue $ unsafeAt a i
    unsafeWrite (TQArray a) i e = writeTQueue (unsafeAt a i) e
    getNumElements (TQArray a) = return (numElements a)

-- | Like 'replicateM' but uses an accumulator to prevent stack overflows.
-- Unlike 'replicateM' the returned list is in reversed order.
-- This doesn't matter though since this function is only used to create
-- arrays with identical elements.
rep :: Monad m => Int -> m a -> m [a]
rep n m = go n []
    where
      go 0 xs = return xs
      go i xs = do
          x <- m
          go (i-1) (x:xs)

mkTQArray :: Int -> a -> STM (TQArray Int a)
mkTQArray numQs x = newArray (1, numQs) x



