{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

module DissectedSTM(newList, insert, get, delete, ListHandle, whichLL)
where

#ifdef PASTMTL2
import Control.TL2.STM
whichSTM = "TL2"
#else
import Control.Concurrent.STM
whichSTM = "FGSTM"
#endif

data List a = Node { val :: a
                   , next :: TVar (List a) }
            | DelNode { next :: TVar (List a) }
            | Null
            | Head { next :: TVar (List a) }

data ListHandle a
     = ListHandle { headList :: TVar (TVar (List a)),
                    tailList :: TVar (TVar (List a)) }

whichLL = putStrLn ("Dissected STM: " ++ whichSTM)

get :: Eq a => ListHandle a -> a -> IO Bool
get lh x = searchAndExecute lh x $ \_ _ -> return (return True)

delete :: Eq a => ListHandle a -> a -> IO Bool
delete lh x = searchAndExecute lh x $
               \curPtr curNode ->  do
                       writeTVar curPtr (DelNode{next=next curNode})
                       return(return True)

newList :: IO (ListHandle a)
newList = do 
        null <- atomically (newTVar Null)
        hd <- atomically (newTVar (Head {next = null }))
        hdPtr <- atomically (newTVar hd)
        tailPtr <- atomically (newTVar null)
        return (ListHandle {headList = hdPtr,
                            tailList = tailPtr})

insert :: ListHandle a -> a -> IO ()
insert (ListHandle {tailList = tailPtrPtr}) x = do
          atomically ( do
                     null <- newTVar Null
                     tailPtr <- readTVar tailPtrPtr
                     writeTVar tailPtr (Node {val = x, next = null})
                     writeTVar tailPtrPtr null
                     return()
                     )
          return()

searchAndExecute
    :: Eq a
    => ListHandle a
    -> a
    -> (TVar (List a)
       -> List a
       -> STM (IO Bool))
    -> IO Bool
searchAndExecute (ListHandle { headList = head }) x apply =
    do startPtr <- atomically (readTVar head)
       go startPtr
       where
       go prevPtr = loopSTM $ do
          prevNode <- readTVar prevPtr
          -- head/node/delnode all have next
          let curPtr = next prevNode
          curNode <- readTVar curPtr
          case curNode of
               Node {val = y, next = nextNode } ->
                    if x == y
                    then apply curPtr curNode
                    else return (go curPtr)
               Null ->  -- reached end of list
                    return (return False)
               DelNode { next = nextNode } ->
                       -- delete curNode by setting the next
                       -- of prevNode to next of curNode
                       case prevNode of
                            Node {} -> do
                                 writeTVar prevPtr
                                           (Node {val = val prevNode,
                                                  next = nextNode})
                                 return (go prevPtr)
                            Head {} -> do
                                 writeTVar prevPtr (Head {next = nextNode})
                                 return (go prevPtr)
                            DelNode {} ->
                                    -- if parent deleted simply move ahead
                                    return (go curPtr)

loopSTM :: STM (IO a) -> IO a
loopSTM stm = do
        action <- atomically stm
        action