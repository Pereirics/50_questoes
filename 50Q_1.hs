import Data.Char 

enumFromTo' :: Int -> Int -> [Int] --enumFromTo 2 4 -> 2 : enumFromTo (2+1) 4 -> 2:3:enumfromto 4 4 -> 2:3:4 
enumFromTo' a b 
 | a==b = [a]
 | otherwise = a : enumFromTo' (a+1) b 

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' f m l 
 | f>l && m-f > 0 = []
 | f<l && m-f < 0 = []
 | otherwise = f : enumFromThenTo' m (2*m-f)  l 

concatena :: [a] -> [a] -> [a] --(++) [1,2,3] [10,20,30] -> 1: (++) [2,3] [10,20,30] -> 1:2 
concatena  l [] = l
concatena [] l = l
concatena  (h:t) (x:xs) = h : concatena t (x:xs)

elemPos :: [a] -> Int -> a --elemPos [10,20,30] 1 -> elemPos [20,30] 0 -> 20
elemPos [] _ = error "lista vazia"
elemPos (h:t) a 
 | a == 0 = h 
 | otherwise = elemPos t (a-1) 

reverse' ::  [a] -> [a] --reverse' [10,20,30] -> 30: reverse' [10,20] -> 30:20: reverse' [10] -> 30:20:10 -> [30,20,10]
reverse' [] = []
reverse' (h:[]) = [h]
reverse' (h:t) = last t : reverse' (h:(init t))
 
take' ::  Int -> [a] -> [a] --take' 2 [10,20,30] -> 10: take' 1 [20,30] -> 10: 20 : take' 0 [30] 
take' a (h:t)
 | a > length (h:t) = (h:t)
 | a == 0 = [] 
 | otherwise = h: take' (a-1) t 

drop' :: Int -> [a] -> [a] --drop' 2 [10,20,30] -> drop' 1 [20,30] -> drop' 0 [30] -> [30]
drop' a (h:t) 
 | a == 0 = (h:t)
 | a > length (h:t) = []
 | otherwise = drop' (a-1) t  

zip' ::  [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []                                        --zip' [1,2,3] [10,20,30,40] -> (1,10) : zip' [2,3] [20,30,40] -> (1,10) : (2,20) : zip' [3] [30,40] -> (1,10):(2,20):(3:30)
zip' (h:t) (x:xs) 
 | length (h:t) == 1 = [(h,x)]
 | otherwise = (h,x) : zip' t xs 

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (h:t) 
 | a == h = True 
 |otherwise = elem' a t 

replicate' ::  Int -> a ->[a] --replicate' 3 10 -> 10: replicate' 2 10 -> 10:10: ...
replicate' a b 
 | a == 0 = []
 | otherwise = b : replicate' (a-1) b 

intersperse ::  a -> [a] ->[a] --intersperse 1 [10,20,30] -> 10:1: intersperse 1 [20,30] -> 10:1:20:1:30
intersperse a [] = []
intersperse a (h:t)
 | length (h:t) == 1 = (h:t) 
 | length (h:t) == 2 = h:a:t 
 | otherwise = h : a : intersperse a t 

group ::  Eq a => [a] -> [[a]] --group [1,1,1,2,3,1] -> group_loop [1] 1 [1,1,2,3,1] -> group_loop [1,1] 1 [1,2,3,1] -> group_loop [1,1,1] 1 [2,3,1] -> [1,1,1] : group_loop [1] 2 [3,1]
group [] = []
group (h:t) = group_loop [h] h t 
     where group_loop acc c [] = [acc]
           group_loop acc c (y:ys)
            | c == y = group_loop (acc ++ [c]) c ys
            | otherwise = acc : group_loop [y] y ys 

concat' ::  [[a]] -> [a]   --concat [[1],[2,2],[3],[4,4,4],[5],[4]] -> [1] ++ concat [2,2],...
concat' ((h:t):[]) = (h:t) 
concat' ((h:t):(x:xs):ys) = (h:t) ++ concat' ((x:xs):ys) 

inits :: [a] -> [[a]] --inits [11,21,13] -> inits_loop [] [11,21,13] -> [] : inits_loop [11] [21,13] -> [] : [11] : inits_loop [11,21] [13] -> []:[11]:[11,21]:_ inits...
inits [] = []
inits (h:t) = inits_loop [] (h:t)
    where inits_loop acc [] = [acc]
          inits_loop acc (h:t) =  acc : inits_loop (acc ++ [h]) t 

tails :: [a] ->  [[a]] --tails [1,2,3,4] -> [1,2,3,4]: tails_loop [2,3,4] [2,3,4] -> [1,2,3,4] : [2,3,4] : tails_loop [3,4] [3,4] -> [1,2,3,4]:[2,3,4]:[3,4]:tails_loop [4] [4] -> [1,2,3,4]:[2,3,4]:[3,4]:[4]:tails_loop [] []
tails [] = []
tails (h:t) = tails_loop (h:t) t
    where tails_loop acc [] = [acc,[]]
          tails_loop acc t = acc : tails_loop (tail (h:t)) (tail t) 

isPrefixOf ::  Eq a => [a]-> [a] -> Bool --isPrefixOf [10,20] [10,20,30] -> isPrefixOf [20] [20,30] -> isPrefixOf [] [30] -> True
isPrefixOf [] l = True 
isPrefixOf l [] = False
isPrefixOf (h:t) (x:xs) 
 | h == x = isPrefixOf t xs 
 | otherwise = False

isSuffixOf :: Eq a => [a] -> [a] -> Bool --isSuffixOf [20,30] [10,20,30] -> isSuffixOf [30] [30] -> True
isSuffixOf [] l = False 
isSuffixOf l [] = False
isSuffixOf (h:t) (x:xs) 
 | length (x:xs) == 1 && h==x = True
 | h == (head xs) = isSuffixOf t (tail xs)
 | otherwise = False

isSubsequenceOf ::  Eq a =>[a] -> [a] -> Bool --isSubsequenceOf [20,40] [10,20,30,40] -> isSubsequenceOf [20,40] [20,30,40] -> isSubsequenceOf [40] [30,40] -> iSO [40] [40] -> True
isSubsequenceOf [] l = False                  --iSO [40,20] [10,20,30,40] -> iso [40,20] [20,30,40] -> iso [40,20] [30,40] -> iso [40,20] [40] ->  
isSubsequenceOf l [] = False 
isSubsequenceOf (h:t) (x:xs)     
 | length t >=1 && length (x:xs) == 1 = False             
 | h==x && length (x:xs) == 1 = True
 | h==x && length (x:xs) > 1 = isSubsequenceOf t xs 
 | otherwise = isSubsequenceOf (h:t) xs 

elemIndices ::  Eq a => a ->[a] -> [Int]          --elemIndices 3 [3,2,3] -> eL [0] 3 [2,3] -> eL [0,]
elemIndices a (h:t) = elemIndices_loop 0 a (h:t) 
   where elemIndices_loop acc a [] = []
         elemIndices_loop acc a (y:ys) 
          | a == y = acc : elemIndices_loop (acc + 1) y ys 
          | otherwise = elemIndices_loop (acc + 1) a ys 

nub ::  Eq a => [a] -> [a]
nub [] = []
nub (h:t) = h : nub(naux h t) 
 where 
     naux _ [] = []
     naux a (x:xs) = if a == x 
                     then naux a xs 
                     else x: naux a xs 

delete ::  Eq a => a -> [a]-> [a]
delete a [] = []
delete a (h:t) 
 | a == h = t 
 | otherwise = h : delete a t 

removeL ::  Eq a => [a] -> [a]-> [a] --removeL [1,2,3,4,5,1] [1,5] -> removeL [2,3,4,5,1] [5] -> 2: removeL [3,4,5,1] [5] -> 2:3:4: removeL [5,1] [5] -> 2:3:4: removeL [1] [] -> [2,3,4,1]
removeL l [] = l 
removeL [] l = []
removeL (h:t) (x:xs) 
 | x == h = removeL t xs 
 | otherwise = h : removeL t (x:xs)

union :: Eq a => [a] -> [a]-> [a] --union [1,1,2,3,4] [1,5] -> union [1,1,2,3,4] [5] -> (union [1,1,2,3,4] []) ++ [5] -> [1,1,2,3,4,5]
union l [] = l 
union [] l = l 
union (h:t) (x:xs) 
 | elem x (h:t) = union (h:t) xs
 | otherwise = (union (h:t) xs) ++ [x]

intersect :: Eq a => [a] -> [a] -> [a] --intersect [1,1,2,3,4] [1,3,5] -> 1: intersect [1,2,3,4] [1,3,4] -> 1: 1: intersect [2,3,4] [1,3,4] -> 1:1:intersect x xs
intersect _ [] = []
intersect [] _ = []
intersect (h:t) (x:xs)
 | elem x (h:t) = x : intersect t (x:xs)
 | otherwise = intersect t xs 

insert ::  Ord a => a -> [a]-> [a]
insert a [] = [a]
insert a (h:t)
 | a > h && a < (head t) = h:a:t 
 | a > h = h : insert a t 
 | otherwise = a : h : t

unwords' ::  [String] -> String -- unwords' ["pf","pff"] -> "pf" ++ " " ++ unwords' ["pff"] -> "pf" ++ " " ++ 
unwords' [] = []
unwords' (h:t) 
 | length (h:t) == 1 = h 
 | otherwise = h ++ " " ++ unwords' t 

unlines' ::  [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t 

pMaior ::  Ord a => [a] -> Int
pMaior [] = error "lista vazia"
pMaior (h:t)
 | h == maiorL (h:t) = 0 
 | otherwise = 1 + pMaior t 

maiorL :: Ord a => [a] -> a 
maiorL [] = error "lista vazia"
maiorL (h:t) 
 | length (h:t) == 1 = h
 | h > (head t) = maiorL (h:(tail t))
 | otherwise = maiorL t 

temRepetidos ::  Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = temRepetidos_loop [h] h t 
    where temRepetidos_loop acc c [] = False 
          temRepetidos_loop acc c (y:ys)
           | c == y = True
           | elem y acc = True 
           | otherwise = temRepetidos_loop (acc ++ [c]) y ys  

algarismos ::  [Char] -> [Char]
algarismos [] = []
algarismos (h:t) 
 | ord h >= 48 && ord h <= 57 = h : algarismos t 
 | otherwise = algarismos t 

posImpares ::  [a] -> [a]
posImpares [] = []
posImpares (h:t) = (head t) : posImpares (tail t)

posPares :: [a] -> [a]
posPares [] = []
posPares (h:t) = h : posPares (tail t)          

isSorted ::  Ord a => [a] -> Bool
isSorted (h:t) = isSorted_loop [h] h t 
    where isSorted_loop acc c [] = True 
          isSorted_loop acc c (y:ys)
           | c > y = False 
           | otherwise = isSorted_loop (acc ++ [y]) y ys 

iSort ::  Ord a => [a] -> [a] --iSort [2,1,3,5,4] ->iSort [1,2,3,5,4] -> 1 : iSort [2,3,5,4] -> 1:2: iSort [] 
iSort [] = []
iSort (h:t) 
 | insert h t == (h:t) = [h] ++ iSort t
 | otherwise = iSort (insert h t)

menor ::  String -> String -> Bool
menor _ [] = False 
menor [] _ = True
menor (h:t) (x:xs) 
 | h > x = False 
 | otherwise = menor t xs 

elemMSet ::  Eq a => a -> [(a,Int)] -> Bool --elemMSet 'a' [('b',2),('a',4)] -> elemMSet  'a' [('a',4)] -> True 
elemMSet a [] = False 
elemMSet a ((a1,b):t) 
 | a == a1 = True       
 | otherwise = elemMSet a t 

lengthMSet ::  [(a,Int)] -> Int --lengthMSet [(’b’,2), (’a’,4), (’c’,1)] -> 2 + lengthMSet [a,c] -> ...
lengthMSet [] = 0 
lengthMSet ((a,b):t) = b + lengthMSet t 

converteMSet ::  [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t)
 | b == 1 = a : converteMSet t 
 | otherwise = a : converteMSet ((a,b-1):t) 

insereMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((a1,b):t) 
 | a == a1 = (a1,b+1):t 
 | otherwise = (a1,b) : insereMSet a t 

removeMSet ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((a1,b):t)
 | a == a1 && b == 1 = t
 | a == a1 = (a1,b-1) : t 
 | otherwise = (a1,b) : removeMSet a t 

constroiMSet ::  Ord a => [a] -> [(a,Int)]   --constroiMSet "aaabccc" -> 
constroiMSet (h:t) = constroiMSet_loop [h] h t 
   where constroiMSet_loop acc c [] = [(c,(length acc))]
         constroiMSet_loop acc c (y:ys)
          | c == y = constroiMSet_loop (acc ++ [c]) y ys 
          | otherwise = (c,(length acc)) : constroiMSet_loop [y] y ys 

partitionEithers ::  [Either a b] -> ([a],[b]) 
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of 
                          Left h -> (h:ls,rs) 
                          Right h -> (ls,h:rs) 
                        where (ls,rs) = partitionEithers t 

catMaybes ::  [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (h:t) = case h of 
      Just x -> x : catMaybes t 
      Nothing -> catMaybes t 

data Movimento = Norte | Sul | Este | Oeste deriving Show
posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (h:t) = case h of 
      Norte -> posicao (a,(b+1)) t
      Sul -> posicao (a,(b-1)) t 
      Este -> posicao ((a+1) ,b) t 
      Oeste -> posicao ((a-1),b) t

caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a1,b1) (a2,b2) 
 | a2 > a1 = Norte : caminho (a1,b1) (a2-1,b2)
 | a2 < a1 = Sul : caminho (a1,b1) (a2+1,b2)
 | b2 > b1 = Este : caminho (a1,b1) (a2,b2-1)
 | b2 < b1 = Oeste : caminho (a1,b1) (a2,b2+1)
 | otherwise = []

vertical ::  [Movimento] -> Bool 
vertical [] = False 
vertical (h:[]) = case h of 
       Norte -> True
       Sul -> True 
       _ -> False 
vertical (h:t) = case h of 
      Norte -> vertical t 
      Sul -> vertical t 
      _ -> False

data Posicao = Pos Int Int deriving Show
maisCentral ::  [Posicao] -> Posicao
maisCentral [] = error "lista vazia"
maisCentral (h:[]) = h 
maisCentral (h:t) 
 | dist' h < dist' (head t) = maisCentral (h:(tail t))
 | otherwise = maisCentral t 

dist' (Pos a b) =  (a^2 + b^2) 

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos a1 b1) ((Pos a2 b2):t)
 | a1 == a2 && abs (b2-b1) == 1 = (Pos a2 b2) : vizinhos (Pos a1 b1) t 
 | abs (a2-a1) == 1 && abs (b2-b1) == 1 = (Pos a2 b2) : vizinhos (Pos a1 b1) t 
 | abs (a2-a1) == 1 && b1 == b2 = (Pos a2 b2) : vizinhos (Pos a1 b1) t 
 | otherwise = vizinhos (Pos a1 b1) t 

mesmaOrdenada ::  [Posicao] -> Bool 
mesmaOrdenada [] = True 
mesmaOrdenada ((Pos a1 b1):[]) = True 
mesmaOrdenada ((Pos a1 b1):(Pos a2 b2):t) 
 | b1 == b2 = mesmaOrdenada ((Pos a2 b2):t)
 | otherwise = False 

data Semaforo = Verde | Amarelo | Vermelho
                deriving Show  
interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (h:t) = case h of 
      Vermelho -> case (head t) of 
            Vermelho -> case (head (tail t)) of 
                  Vermelho -> True 
            _ -> case (head (tail t)) of 
                  Vermelho -> case (head (tail (tail t))) of 
                        Vermelho -> True 
                        _ -> False
                  _ -> False
      _ -> case (head t) of 
            Vermelho -> case (head (tail t)) of 
                  Vermelho -> case (head (tail (tail t))) of 
                        Vermelho -> True
                        _ -> False
                  _ -> False 
            _ -> False       