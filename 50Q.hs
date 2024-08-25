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