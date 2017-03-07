type Block = (Int,Int) 
type Layer = (Block, Block, Block)
type FlagPair = (Bool, Bool)
type Final = (Layer, Layer, FlagPair, Bool)

main::IO()
main = print "hello"

merge::Block->Block->Block
merge (l1, r1) (l2, r2) 
    | diff > 0 = (l1, r2+diff)
    | otherwise = (l1-diff, r2)
    where diff = r1-l2

revBlock::Block->Block
revBlock (l, r) = (r, l)

revLayer::Layer->Layer
revLayer (b1, b2, b3) = (revBlock b1, revBlock b2, revblock b3)

mergeLayer::Layer->Layer->Layer
mergeLayer (x1, x2, x3) (y1, y2, y3)
    = (x1, mid, y3)
    where 
        (r, l) = merge x3 y1
        rev = (l, r)
        mid = x2 `merge` rev `merge` y2


