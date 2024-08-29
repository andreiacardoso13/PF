module Q50 where

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a <= b = a : enumFromTo' (a + 1) b
                | otherwise = []

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' a b c | b <= c = a : enumFromThenTo' b (b + b - a) c
                      | a <= c = [a]
                      | otherwise = []

--3
concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (h:t) l = h : concatena t l

--4
local :: [a] -> Int -> a
local (h:t) n | n == 0 = h 
              | otherwise = local t (n - 1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a] 
take' _ [] = []
take' 0 _ = []
take' a (h:t) = h : take' (a - 1) t

--7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' a (h:t) = drop' (a - 1) t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h1:t1) (h2:t2) = (h1,h2) : zip' t1 t2

--9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' a b = b : replicate' (a - 1) b

--10
intersperse' :: a -> [a] -> [a]
intersperse' _ [h] = [h]
intersperse' a (h:t) = h : a : intersperse' a t

--11
group :: Eq a => [a] -> [[a]]
group [] = []
group [h] = [[h]]
group (h:t) | elem h (head (group t)) = (h : (head (group t))) : tail (group t)
            | otherwise = [h] : (group t)

--12
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--13
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--14
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (h:t) = [(h:t)] ++ tails' t

--15
heads' :: [[a]] -> [a]
heads' [] = []
heads' (h:t) | length (h:t) == 0 = heads' t
             | otherwise = (head h) : heads' t

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c) :t) = (a,c) : fun t

--18
cola :: [(String, b, c)] -> String
cola [] = []
cola ((a,b,c) : t) = a ++ cola t

--19
idadeFunc :: Int -> Int -> [(String, Int)] -> [String]
idadeFunc ano idade [] = []
idadeFunc ano idade ((nome,nasc):t) | ano - nasc >= idade = [nome] ++ idadeFunc ano idade t
                                    | otherwise = idadeFunc ano idade t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = powerEnumFromAux n m 0

powerEnumFromAux :: Int -> Int -> Int -> [Int]
powerEnumFromAux n m a | a > m-1 = []
                       | otherwise = n^a : powerEnumFromAux n m (a+1)

--21
isPrime :: Int -> Bool 
isPrime n = n >= 2 && isPrimeAux n 2


isPrimeAux :: Int -> Int -> Bool
isPrimeAux n m = if m*m <= n 
                 then if mod n m == 0
                      then False
                      else isPrimeAux n (m+1)
                 else True

--22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _[] = False
isPrefixOf (h1:t1) (h2:t2) | h1 == h2 = isPrefixOf t1 t2
                           | otherwise = False

--23
isSufixOf :: Eq a => [a] -> [a] -> Bool
isSufixOf [] [] = True
isSufixOf _ [] = False
isSufixOf l1 l2 | l1 /= l2 = isSufixOf l1 (tail l2)
                | otherwise = True

--24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (h1:t1) (h2:t2) | h1 == h2 = isSubsequenceOf t1 t2
                                | otherwise = isSubsequenceOf (h1:t1) t2

--25
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n (h:t) = elemIndicesAux n (h:t) 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux n [] a = []
elemIndicesAux n (h:t) a | n == h = a : elemIndicesAux n t (a+1)
                         | otherwise = elemIndicesAux n t (a+1)

--26
nub :: Eq a => [a] -> [a]
nub (h : t) | elem h t = nub t
            | otherwise = h : nub t

--27
delete :: Eq a => a -> [a] -> [a]
delete a [] = []
delete a (h:t) | a == h = t
               | otherwise = h : delete a t 

--28
remover :: Eq a => [a] -> [a] -> [a]
remover l [] = l
remover (h1:t1) (h2:t2) = remover (delete h2 (h1:t1)) t2

--29
union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (h:t) | elem h l = union l t
              | otherwise = union (l ++ [h]) t

--30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) (h1:t1) = intersectAux h (h1:t1) ++ intersect t (h1:t1)

intersectAux :: Eq a => a -> [a]-> [a]
intersectAux a [] = []
intersectAux a (h:t) | a == h = [a]
                     | otherwise = intersectAux a t

--31
insert :: Ord a => a -> [a] -> [a] 
insert n [h] = [h] ++ [n]
insert n (h:t) | n < h = n : h : t
             | n >= h && n < head t = h : n : t
             | otherwise = h : insert n t

--32
unwords' ::[String] -> String
unwords' [] = []
unwords' (h:t) = h ++ " " ++ unwords' t

--33
unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' n ((x,y):xs) | n == x = Just y
                     | otherwise = lookup' n xs

--36
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [h] = [h]
preCrescente (h:t) | h <= head t = h : preCrescente t
                   | otherwise = [h]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

--38
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (h1:t1) (h2:t2) | h1 < h2 = True
                      | h1 > h2 = False
                      | otherwise = menor t1 t2

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((h,h1):t) | a == h = True
                      | otherwise = elemMSet a t

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet ((h,0):t) = converteMSet t
converteMSet [] =[]
converteMSet ((h,h1):t) = h : converteMSet ((h,h1-1):t)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a , 1 )]
insereMSet a ((h,h1):t) | a == h = ((h,h1+1):t)
                        | otherwise = (h,h1) : insereMSet a t
--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a ((h,1):t) = []
removeMSet a [] = []
removeMSet a ((h,h1):t) | a == h = ((h,h1-1):t)
                        | otherwise = (h,h1) : removeMSet a t

--44
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers ((Left a) : t) = (a : b, c)
  where (b,c) = partitionEithers t
partitionEithers ((Right a) : t) = (b, a:c)
  where (b,c) = partitionEithers t

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a) :t) = a : catMaybes t
catMaybes ((Nothing) :t) = catMaybes t

data Movimento = Norte | Sul | Este | Oeste
              deriving Show

--46
caminho :: (Int, Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1 == x2 = if y1 < y2
                                       then Norte : caminho (x1,y1+1) (x2,y2)
                                       else if y1> y2
                                              then Sul : caminho (x1,y1-1) (x2,y2)
                                              else []
                        | x1 < x2 = Este : caminho (x1 + 1, y1) (x2,y2)
                        |otherwise = Oeste : caminho (x1-1, y1) (x2,y2)