
lift :: Eq a => [(a,b)] -> (a -> b)
lift ((k,v):xx) y = if y == k then v else lift xx y

lift2:: Eq a => [[a]] -> (a->[a])
lift2 (x:xx) y = if head x == y then tail x else lift2 xx y

union :: Eq a => a -> [a] -> [a] -> [a]
union z [] []     = []
union z [] yy     = union z yy []
union z (x:xx) [] = if z == x then [] else (x:union z xx [])
union z (x:xx) (y:yy) = 
        if z == x && z == y then []
        else if z == x then (y:union z xx yy)
        else if z == y then (x:union z xx yy)
        else (x:y:union z xx yy)

reachable :: Eq a => (a -> [a]) -> [a] -> [[a]]
reachable f xx =
    let knot = foldr (\x yy -> ((x:(foldr (union x) [] (map (lift2 knot) (f x)))):yy) ) [] xx
    in knot

graph =
    [('a',['c','b']),
     ('b',['c','d']),
     ('c',['b','d']),
     ('d',[])]

main = do 
        print graph
        print (reachable (lift graph) (map fst graph))
