module Splay (
  Splay,
  insert,
  lookup, 

)where
  import qualified Data.List as List 
  type Key = Int
  type Value = String
  data Splay = Leaf 
    | Node {left::Splay, key::Key, value::Key, right::Splay}
  
  insert::Key->Value->Splay->Splay
  insert _ _ tr = tr

  reach::Key->Splay->Splay
  reach _ Leaf = Leaf
  reach targetkey tr@Node {left=l, key=k, value=v, right=r} = 
    case compare targetkey k of 
      LT -> (
        case l of 
          Leaf -> tr
          Node {left=l2, key=k2, value=v2, right=r2} ->
            case compare targetkey k2 of 
            LT -> 
              --zig zig
              case reach targetkey l2 of 
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
              case reach targetkey r2 of 
                Leaf -> newroot
                  where
                    newroot = Node{left = l2, key = k2, value = v2, right = newr}
                    newr =  Node{left = Leaf, key = k , value = v , right = r}
                Node{left = l3, key = k3, value = v3, right = r3} -> newroot
                  where 
                    newroot = Node{left = newl, key = k3, value = v3, right = newr}
                    newl = Node{left = l2, key = k2, value = v2, right=l3}
                    newr = Node{left = r2, key = k, value = v, right=r}
        )
      EQ -> tr

      GT -> (
        case r of 
          Leaf -> tr
          Node {left=l2, key=k2, value=v2, right=r2} ->
            case compare targetkey k2 of 
            GT -> 
              --zig zig
              case reach targetkey r2 of 
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
            LT ->
              -- zig-zag 
              case reach targetkey l2 of 
                Leaf -> newroot
                  where
                    newroot = Node{left = l2, key = k2, value = v2, right = newr}
                    newr =  Node{left = Leaf, key = k , value = v , right = r}
                Node{left = l3, key = k3, value = v3, right = r3} -> newroot
                  where 
                    newroot = Node{left = newl, key = k3, value = v3, right = newr}
                    newl = Node{left = l2, key = k2, value = v2, right=l3}
                    newr = Node{left = r2, key = k, value = v, right=r}
        )
 
        
    