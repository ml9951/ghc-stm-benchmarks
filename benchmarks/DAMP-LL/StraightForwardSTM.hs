{-# LANGUAGE CPP, ForeignFunctionInterface #-} 

module StraightForwardSTM(newList, insert, get, delete, ListHandle, whichLL)
where

#ifdef PASTMTL2
import Control.TL2.STM
whichSTM = "TL2"
#else
import Control.Concurrent.STM
whichSTM = "FGSTM"
#endif

data List a = Node { val :: a,
                     next :: TVar (List a) }
            | Null
            | Head { next :: TVar (List a) }
data ListHandle a
     = ListHandle { headList :: TVar (TVar (List a)),
                    tailList :: TVar (TVar (List a)) }

whichLL = putStrLn("Straight forward STM: " ++ whichSTM)

newList :: IO (ListHandle a)
newList =
 do null <- atomically (newTVar Null)
    hd <- atomically (newTVar (Head {next = null }))
    hdPtr <- atomically (newTVar hd)
    tailPtr <- atomically (newTVar null)
    return (ListHandle {headList = hdPtr,
                        tailList = tailPtr})
insert :: ListHandle a -> a -> IO ()
insert (ListHandle {tailList = tailPtrPtr}) x = do
  tPtr <- atomically ( do
            null <- newTVar Null
            tailPtr <- readTVar tailPtrPtr
            writeTVar tailPtr
                      (Node {val = x, next = null})
            writeTVar tailPtrPtr null
            return()
           )
  return tPtr

get :: Eq a => ListHandle a -> a -> IO Bool 
get (ListHandle {headList = ptrPtr}) i =
  atomically (
       do ptr <- readTVar ptrPtr
          Head {next = startptr} <- readTVar ptr
          find2 startptr i)
   where
    find2 :: Eq a => TVar (List a) -> a -> STM Bool
    find2 curNodePtr i = do
      curNode <- readTVar curNodePtr
      case curNode of
        Null -> return False
        Node {val = curval, next = curnext} ->
           if (curval == i) then return True
           else find2 curnext i

getNext (Node{next=n}) = return n
getNext (Head{next=n}) = return n

delete (ListHandle{headList=ptrPtr}) i =
        atomically(do startPtr <- readTVar ptrPtr
                      raw <- readTVar startPtr
                      n <- getNext raw
                      delete2 startPtr n)
        where
        delete2 prevPtr currentPtr = do
                curNode <- readTVar currentPtr
                case curNode of
                     Null -> return False
                     Node{val = curval, next = nextNode} ->
                              if curval /= i
                              then delete2 currentPtr nextNode
                              else do 
                                   prevNode <- readTVar prevPtr
                                   case prevNode of
                                        Head{} -> do
                                               writeTVar prevPtr (Head{next=nextNode})
                                               return True
                                        Node{} -> do
                                               writeTVar prevPtr (Node{val=val prevNode, next=nextNode})
                                               return True
