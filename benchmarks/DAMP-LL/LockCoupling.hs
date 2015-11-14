
module LockCoupling(get, delete, newList, ListHandle, whichLL, insert, size)
where

import Control.Concurrent.MVar
import Data.IORef

data List a = Node { val :: a
                   , next :: MVar (List a) }
            | Null
            | Head { next :: MVar (List a) }

data ListHandle a
     = ListHandle { headList :: IORef (MVar (List a)),
                    tailList :: MVar (MVar (List a)) }

whichLL = putStrLn "MVar lock coupling"

size :: ListHandle a -> IO Int
size (ListHandle{headList = head}) = do
     startPtr <- readIORef head
     go startPtr 0
        where
        go ptr i = do
           curNode <- takeMVar ptr
           case curNode of
                Head{next = nextNode} -> go nextNode i
                Node{next = nextNode} -> go nextNode (i+1)
                Null -> return i

insert :: ListHandle a -> a -> IO ()
insert (ListHandle{tailList = tailPtrPtr}) x = do
          tailPtr <- takeMVar tailPtrPtr 
          tail <- takeMVar tailPtr
          null <- newMVar Null
          putMVar tailPtr (Node {val = x, next = null})
          putMVar tailPtrPtr null
          return ()

newList :: IO (ListHandle a)
newList = do 
        null <- newMVar Null
        hd <- newMVar(Head{next = null})
        hdPtr <- newIORef hd    
        tailPtr <- newMVar null
        return(ListHandle {headList = hdPtr, tailList = tailPtr})

get :: Eq a => ListHandle a -> a -> IO Bool
get (ListHandle { headList = head }) x =
  let go prevPtr prevNode = do 
      let curPtr = next prevNode -- head/node have all next
      curNode <- takeMVar curPtr
      case curNode of
           Node {val = y, next = nextNode } ->
                if (x == y)
                then -- node found
                     do putMVar prevPtr prevNode
                        putMVar curPtr curNode
                        return True
                else
                        do putMVar prevPtr prevNode
                           go curPtr curNode -- continue
           Null -> do 
                putMVar prevPtr prevNode
                putMVar curPtr curNode
                return False -- reached end of list
  in do startPtr <- readIORef head
        startNode <- takeMVar startPtr
        go startPtr startNode

delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x = do
       startPtr <- readIORef head
       startNode <- takeMVar startPtr
       go startPtr startNode
          where
          go prevPtr prevNode = do
             let curPtr = next prevNode
             curNode <- takeMVar curPtr
             case curNode of
                  Node{val = y, next = nextNode} ->
                           if x == y
                           then 
                                case prevNode of
                                     Node {} -> do
                                          putMVar prevPtr(Node{val=val prevNode, next=nextNode})
                                          putMVar curPtr curNode
                                          return True
                                     Head {} -> do
                                          putMVar prevPtr (Head{next = nextNode})
                                          putMVar curPtr curNode
                                          return True
                           else do
                                putMVar prevPtr prevNode
                                go curPtr curNode
                  Null -> do
                       putMVar curPtr curNode
                       putMVar prevPtr prevNode
                       return False
