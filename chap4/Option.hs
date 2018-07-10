module Option where

data Option a = None | Some { value :: a } deriving (Show)


mean :: Fractional a => [a] -> Option a
-- mean xs = if (null xs) then None
--           else Some ((sum xs) / (fromIntegral (length xs)))
mean [] = None
mean xs = Some ((sum xs) / (fromIntegral (length xs)))


-- 4.1 
map' :: (a -> b) -> Option a -> Option b
map' _ None = None
map' f (Some x) = Some (f x)

flatMap' :: (a -> Option b) -> Option a -> Option b
flatMap' f x = getOrElse None (map' f x)

getOrElse :: a -> Option a -> a
getOrElse defaultValue None = defaultValue
getOrElse _ (Some x) = x

orElse :: Option a -> Option a -> Option a
-- orElse None y = y
-- orElse x _ = x
orElse x y = getOrElse y (map' Some x)

filter' :: (a -> Bool) -> Option a -> Option a
filter' pred x = flatMap' (\v -> if pred v then x else None) x


-- flatMap :: (a -> [b]) -> [a] -> [b]
-- flatMap f xs = foldr mapList [] xs where mapList x xs = (++) (f x) xs

-- 4.2
variance :: Fractional a => [a] -> Option a
variance xs = mean (map (\x->(x-m)^2) xs) where m = value (mean xs)

vr m x = (x - m) ^ 2

variance' :: Fractional a => [Option a] -> Option a
variance' = undefined
-- variance' xs = mean (value (traverse' (map2' vr (mean xs)) xs))

-- 4.3
map2' :: (a -> b-> c) -> Option a -> Option b -> Option c
-- map2' _ None _ = None
-- map2' _ _ None = None
-- map2' f (Some x) (Some y) = Some (f x y)
-- map2' f xs ys = lifted ys where lifted = map' (value (map' f xs))
map2' f xs ys = flatMap' (\y -> map' (\x -> f x y) xs) ys


add' :: Num a => Option a -> Option a -> Option a
add' a b = map2' (+) a b

-- 4.4
sequence' :: [Option a] -> Option [a]
-- sequence' [] = None
-- sequence' (x:[]) = map' (:[]) x
-- sequence' (x:xs) = map2' (:) x (sequence' xs)
sequence' xs = traverse' id xs


-- 4.5
traverse' :: (a -> Option b) -> [a] -> Option [b]
traverse' _ [] = None
traverse' f (x:[]) = map' (:[]) (f x)
traverse' f (x:xs) = map2' (:) (f x) (traverse' f xs)