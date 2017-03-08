import qualified Data.Map as Map

main::IO()
main = print $ Map.lookup "betty" phoneBook
phoneBook = Map.fromListWith (\x y -> "$" ++ x++" "++ y++ "#")
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","393-2928")  
    ,("patsy","493-2928")  
    ,("patsy","793-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]
