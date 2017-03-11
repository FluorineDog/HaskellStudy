module Splay (
  Splay,
  insert,
  remove,
  peek,
  find, 
  nullTr,
  empty,
)where
  type Key = Int
  type Value = String
  data Splay = Leaf 
    | Node {left::Splay, key::Key, value::Value, right::Splay} deriving (Show)
  
  empty::Splay
  empty = Leaf

  nullTr::Splay->Bool
  nullTr Leaf = True
  nullTr _ = False

  peek::Splay->Maybe(Key, Value)
  peek Leaf = Nothing
  peek (Node _ k v _) = Just (k,v)

  insert::Key->Value->Splay->Splay
  insert rawKey rawValue tr = 
    case reach rawKey tr of 
    Leaf->Node{left = Leaf, key = rawKey, value = rawValue, right = Leaf} 
    Node{left = l, key = k, value = v, right = r} ->     
      case compare rawKey k of 
      EQ->Node{left = l, key = rawKey, value = rawValue, right = r} 
      LT->Node{left = l, key = rawKey, value = rawValue, right = newr}
        where newr = Node{left = Leaf, key = k, value = v, right = r}
      GT->Node{left = newl, key = rawKey, value = rawValue, right = r}
        where newl = Node{left = l, key = k, value = v, right = Leaf}
    
    
  remove::Key->Splay->Splay
  remove rawKey tr = 
    let tr'@Node{left = l, key = k, value = _, right = r} = reach rawKey tr
    in case compare rawKey k of 
    LT -> tr'
    GT -> tr'
    EQ ->( 
      case reach rawKey r of 
        Leaf -> l 
        Node{left = Leaf, key = k2, value = v2, right = r2} -> newroot
          where 
            newroot = Node{left = l, key = k2, value = v2, right = r2}
        _ -> error "asd"
      )

  
  find::Key->Splay->Splay
  find = reach

  reach::Key->Splay->Splay
  reach _ Leaf = Leaf
  reach targetKey tr@Node {left=l, key=k, value=v, right=r} = 
    case compare targetKey k of 
      GT -> (
        case r of 
          Leaf -> tr
          Node {left=l2, key=k2, value=v2, right=r2} ->
            case compare targetKey k2 of 
            GT -> 
              --zig zig
              case reach targetKey r2 of 
                Leaf -> newroot
                  where 
                    newroot = Node{left = newl, key=k2, value=v2, right=Leaf}
                    newl = Node{left=l, key=k, value=v, right=l2}
                Node{left=l3, key=k3, value=v3, right=r3} -> newroot
                  where
                    newroot = Node{left = newl1, key = k3, value = v3, right = r3} 
                    newl1 =   Node{left = newl2, key = k2, value = v2, right = l3}
                    newl2 =   Node{left = l, key = k , value = v , right = l2}
            EQ -> newroot 
              -- shortcut
              where 
                newroot = Node{left = newl, key = k2, value = v2, right = r2}
                newl =    Node{left = l, key = k , value = v , right = l2}
            LT ->
              -- zig-zag 
              case reach targetKey l2 of 
                Leaf -> newroot
                  where
                    newroot = Node{left = newl, key = k2, value = v2, right = r2}
                    newl =  Node{left = l, key = k , value = v , right = Leaf}
                Node{left = l3, key = k3, value = v3, right = r3} -> newroot
                  where 
                    newroot = Node{left = newl, key = k3, value = v3, right = newr}
                    newl = Node{left = l, key = k, value = v, right = l3}
                    newr = Node{left = r3, key = k2, value = v2, right = r2}
        )
 
      EQ -> tr
      LT -> (
        case l of 
          Leaf -> tr
          Node {left=l2, key=k2, value=v2, right=r2} ->
            case compare targetKey k2 of 
            LT -> 
              --zig zig
              case reach targetKey l2 of 
                Leaf -> newroot
                  where 
                    newroot = Node{left = Leaf, key=k2, value=v2, right=newr}
                    newr = Node{left=r2, key=k, value=v, right=r}
                Node{left=l3, key=k3, value=v3, right=r3} -> newroot
                  where
                    newroot = Node{left = l3, key = k3, value = v3, right = newr1} 
                    newr1 =   Node{left = r3, key = k2, value = v2, right = newr2}
                    newr2 =   Node{left = r2, key = k , value = v , right = r }
            EQ -> newroot 
              -- shortcut
              where 
                newroot = Node{left = l2, key = k2, value = v2, right = newr}
                newr =    Node{left = r2, key = k , value = v , right = r }
            GT ->
              -- zig-zag 
              case reach targetKey r2 of 
                Leaf -> newroot
                  where
                    newroot = Node{left = l2, key = k2, value = v2, right = newr}
                    newr =  Node{left = Leaf, key = k , value = v , right = r}
                Node{left = l3, key = k3, value = v3, right = r3} -> newroot
                  where 
                    newroot = Node{left = newl, key = k3, value = v3, right = newr}
                    newl = Node{left = l2, key = k2, value = v2, right=l3}
                    newr = Node{left = r3, key = k, value = v, right=r}
        )

