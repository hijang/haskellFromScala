-- Haskell은 기본적으로 lazy evaluation이 지원되므로 따로 Stream을 작성할 필요가 없을것 같지만, 
-- fold 등의 계산을 엄격하지 않게 구현하는 부분은 의미가 있을 것 같다.

module Stream where

bigList = [1..100000000000]

-- 5.2
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x:(take' (n-1) xs)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n org@(x:xs)
    | n <= 0 = org
    | otherwise = drop' (n-1) xs


-- 5.3
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pred (x:xs)
    | pred x = x:(takeWhile' pred xs)
    | otherwise = []



exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists pred (x:xs)
    | pred x = True
    | otherwise = exists pred xs

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ base [] = base
foldRight acc base (x:xs) = acc x (foldRight acc base xs)

exists' :: (a -> Bool) -> [a] -> Bool
exists' pred xs = foldRight (\x y -> (pred x) || y) False xs


product' :: (Eq a, Num a) => [a] -> a
product' xs = foldRight f' 1 xs where f' x y = if x == 0 then x else x * y

-- 5.4
forAll :: (a -> Bool) -> [a] -> Bool
-- forAll _ [] = False ?
forAll pred xs = foldRight (\x y -> (pred x) && y) True xs


-- 5.5
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' pred xs = foldRight acc [] xs where acc x y = if pred x then x:y else []


-- 5.6
headOption :: [a] -> Maybe a
-- headOption [] = Nothing
-- headOption (x:xs) = Just x
headOption xs = foldRight (\x y -> Just x) Nothing xs
-- 근데 이게 왜 어려움..이었을까


-- 5.7
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldRight (\x y -> (f x):y) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs = foldRight (\x y -> if pred x then x:y else y) [] xs

append :: [a] -> [a] -> [a]
append as bs = foldRight (:) bs as

flatMap' :: (a -> [b]) -> [a] -> [b]
flatMap' f xs = foldRight (\x y -> append (f x) y) [] xs