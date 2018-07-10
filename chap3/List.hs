
module List where

data List a = Empty | List { h :: a, t :: List a } deriving (Show)

cons :: (a, List a) -> List a
cons (a, list) = List a list

asList :: [a] -> List a
asList [] = Empty
asList list = cons (head list, asList (tail list))

apply :: (a -> b) -> List a -> List b
apply _ Empty = Empty
apply f list = cons (f (h list), apply f (t list))


sum :: Integral a => List a -> a
sum Empty = 0
-- sum list = h list + List.sum (t list)
sum (List x xs) = x + List.sum xs


product :: Fractional a => List a -> a
product Empty = 1.0
-- product list = h list * List.product (t list)
-- product (List 0 _) = 0.0   왜 안될깡..
product (List x xs) = x * List.product xs

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ base Empty = base
foldRight acc base (List x xs) = acc x (foldRight acc base xs)

foldRight' :: (a -> b -> b) -> b -> List a -> b
-- foldRight' acc base list = foldLeft acc' base (reverse' list) where acc' x y = acc y x
foldRight' acc base (List x xs) = (foldLeft acc' (acc x) xs) base where acc' f x = \y -> f (acc x y)

facc :: Num a => (a -> a) -> a -> (a -> a)
facc f x = \y -> x + f y
-- uf :: List a -> List a
-- uf list = foldLeft acc' Empty list where acc' x y = foldRight List (List y Empty) x

af :: Num a => List a -> a
af (List x xs) = (foldLeft (acc') (x+) xs) 0 where acc' f x = \y -> x + (f y)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ base Empty = base
foldLeft acc base (List x xs) = foldLeft acc (acc base x) xs

foldLeft' :: (b -> a -> b) -> b -> List a -> b
-- foldLeft' acc base list = foldRight acc' base (reverse' list) where acc' x y = acc y x
-- foldLeft' acc base list@(List x xs) = (foldRight acc' (`acc` x) xs) base where acc' x f = \y -> acc (f y) x 
foldLeft' acc base list@(List x xs) = (foldRight acc' (`acc` x) xs) base where acc' x f = \y -> f (acc y x) 

-- foldRight' :: List a -> b -> (a -> b -> b) -> b
-- foldRight' Empty base _ = base
-- foldRight' (List x xs) base acc = acc x (foldRight' xs base acc)

tail' :: List a -> List a
tail' Empty = error "empty..."
tail' (List x xs) = xs


setHead :: a -> List a -> List a
setHead x Empty = cons (x, Empty)
setHead x (List _ xs) = cons (x, xs)


drop' :: Int -> List a -> List a
drop' 0 xs = xs 
drop' _ Empty = error "empty..."
drop' n (List x xs) = drop' (n - 1) xs

dropWhile' :: (a -> Bool) -> List a -> List a
dropWhile' _ Empty = Empty
dropWhile' pred list@(List x xs) = if pred x then dropWhile' pred xs else list

append :: List a -> List a -> List a
append Empty b = b
append (List x xs) b  = List x (append xs b)

append' :: List a -> List a -> List a
append' a b = foldRight List b a

init' :: List a -> List a
init' Empty = Empty
init' (List x Empty) = Empty
init' (List x xs) = cons (x, init' xs)

length' :: List a -> Int
length' list = foldRight counter 0 list where counter _ acc = acc + 1

reverse' :: List a -> List a
reverse' Empty = Empty
reverse' (List x xs) = append (reverse' xs) (List x Empty)

flat' :: List (List a) -> List a
flat' xss = foldRight append Empty xss



--3.16
mapToAddOne :: Num a => List a -> List a
mapToAddOne xs = foldRight incList Empty xs where incList x xs = List (x+1) xs

-- 3.17
mapToString :: (Num a, Show a) => List a -> List String
mapToString xs = foldRight strList Empty xs where strList x xs = List (show x) xs

-- 3.18
map' :: (a -> b) -> List a -> List b
map' f xs = foldRight mapList Empty xs where mapList x xs = List (f x) xs

-- 3.19
filter' :: (a -> Bool) -> List a -> List a
filter' pred xs = foldRight ifList Empty xs where ifList x xs = if pred x then List x xs else xs
--let isEven x = mod x 2 == 0 in filter' isEven (asList [1, 2, 3, 4, 5, 6])

-- 3.20 
flatMap :: (a -> List b) -> List a -> List b
--flatMap f xs = flat' (map' f xs)
flatMap f xs = foldRight mapList Empty xs where mapList x xs = append (f x) xs

-- 3.21
filter'' :: (a -> Bool) -> List a -> List a
filter'' pred xs = flatMap (\x -> if pred x then List x Empty else Empty) xs


-- 3.22
zipAdd :: Num a => List a -> List a -> List a
zipAdd Empty Empty = Empty
zipAdd (List a as) (List b bs) = List ((+) a b) (zipAdd as bs)


-- 3.23
zipWith' :: (a -> b-> c) -> List a -> List b -> List c
zipWith' _ Empty _ = Empty
zipWith' _ _ Empty = Empty
zipWith' f (List a as) (List b bs) = List (f a b) (zipWith' f as bs)


-- 3.24
startWith :: Eq a => List a -> List a -> Bool
startWith as bs = (length' as <= length' bs) && foldLeft ( && ) True (zipWith' (==) as bs)

hasSubsequence :: Eq a => List a -> List a -> Bool
hasSubsequence Empty Empty = True
hasSubsequence list listB@(List b bs) = (startWith list listB) || hasSubsequence list bs 
hasSubsequence _ _ = False


