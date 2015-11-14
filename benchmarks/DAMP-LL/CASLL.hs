module CASLL(ListHandle, get, delete, insert, newList, whichLL)
where

import Data.IORef

data List a = Node { val :: a
                   , next :: IORef (List a) }
            | DelNode { next :: IORef (List a) }
            | Null
            | Head { next :: IORef (List a) }
            deriving Eq

instance Show (List a) where
         show (Node _ _) = "Node"
         show (DelNode _) = "DelNode"
         show Null = "Null"
         show (Head _) = "Head"

data ListHandle a
     = ListHandle { headList :: IORef (IORef (List a)),
                    tailList :: IORef (IORef (List a)) }

whichLL = putStrLn "CAS Linked List"

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))


-- we create a new list
newList :: IO (ListHandle a)
newList = 
   do nullPtr <- newIORef Null
      hd <- newIORef (Head {next = nullPtr })
      hdPtr <- newIORef hd
      tailPtr <- newIORef nullPtr
      return (ListHandle {headList = hdPtr, tailList = tailPtr})

atomicWrite :: IORef a -> a -> IO ()
atomicWrite ptr x =
   atomicModifyIORef ptr (\ _ -> (x,()))

-- we add a new node, by overwriting the null tail node
-- we only need to adjust tailList but not headList because
-- of the static Head
-- we return the location of the newly added node
insert :: Eq a => ListHandle a -> a -> IO ()
insert handle@(ListHandle {tailList = tailPtrPtr}) x = do 
          nullPtr <- newIORef Null
          tailPtr <- readIORef tailPtrPtr
          b <- atomCAS tailPtr Null (Node{val = x, next = nullPtr})
          if b
          then atomicWrite tailPtrPtr nullPtr
          else insert handle x


get :: Eq a => ListHandle a -> a -> IO Bool
get (ListHandle { headList = head }) x = do
     startPtr <- readIORef head
     raw <- readIORef startPtr
     go startPtr (next raw)
        where 
        go prevPtr curPtr = do
           curNode <- readIORef curPtr
           case curNode of
                Node{val = y, next = nextNode} ->
                         if x == y 
                         then return True
                         else go curPtr nextNode
                Null -> return False
                DelNode {next = nextNode} -> do
                        prevNode <- readIORef prevPtr
                        case prevNode of
                             Node{} -> do
                                    b <- atomCAS prevPtr prevNode (Node {val=val prevNode, next = nextNode})
                                    if b
                                    then go prevPtr nextNode
                                    else go curPtr nextNode
                             Head{} -> do
                                    b <- atomCAS prevPtr prevNode (Head{next=nextNode})
                                    if b
                                    then go prevPtr nextNode
                                    else go curPtr nextNode
                             DelNode {} -> go curPtr nextNode
                             
{-}
get :: Eq a => ListHandle a -> a -> IO Bool
get (ListHandle { headList = head }) x = do
     startPtr <- readIORef head
     go startPtr 
        where 
        go prevPtr = do
           prevNode <- readIORef prevPtr
           let curPtr = next prevNode
           curNode <- readIORef curPtr
           case curNode of
                Node{val = y, next = nextNode} ->
                         if x == y 
                         then return True
                         else go curPtr 
                Null -> return False
                DelNode {next = nextNode} -> do
                        prevNode <- readIORef prevPtr
                        case prevNode of
                             Node{} -> do
                                    b <- atomCAS prevPtr prevNode (Node {val=val prevNode, next = nextNode})
                                    if b
                                    then go prevPtr 
                                    else go curPtr
                             Head{} -> do
                                    b <- atomCAS prevPtr prevNode (Head{next=nextNode})
                                    if b
                                    then go prevPtr 
                                    else go curPtr 
                             DelNode {} -> go curPtr
-}

delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x = do
       startPtr <- readIORef head
       raw <- readIORef startPtr
       go startPtr (next raw)
          where
          go prevPtr curPtr = do
             curNode <- readIORef curPtr
             case curNode of
                  Node {val = y, next = nextNode } ->
                       if (x == y) 
                       then do -- if parent deleted simply move ahead
                            b <- atomCAS curPtr curNode (DelNode {next = nextNode})
                            if b 
                            then return True
                            else go prevPtr curPtr      -- spin
                       else go curPtr nextNode -- continue
                  Null -> return False
                  DelNode {next = nextNode } -> do
                         prevNode <- readIORef prevPtr
                         case prevNode of
                              Node {} -> do 
                                   b <- atomCAS prevPtr prevNode (Node {val = val prevNode, next = nextNode})
                                   if b 
                                   then go prevPtr nextNode
                                   else go curPtr nextNode
                              Head {} -> do 
                                   b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                   if b 
                                   then go prevPtr nextNode
                                   else go curPtr nextNode
                              DelNode {} -> go curPtr nextNode   -- if parent deleted simply move ahead












