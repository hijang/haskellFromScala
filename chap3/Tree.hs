module Tree where

data Tree a = Leaf { value :: a } | Branch { left :: Tree a, right :: Tree a} deriving (Show)

--
someTree' = Branch (Branch (Leaf "a") (Leaf "b")) (Branch (Leaf "c") (Leaf "d"))
someTree = Branch (Branch (Leaf "aa") someTree') (Branch (Leaf "cc") (Leaf "dd"))
--


size :: Tree a -> Int
-- size (Leaf a) = 1
-- size (Branch left right) = (size left) + (size right) + 1
size xs = fold (\l r -> l + r + 1) (\x->1) xs

maximum' :: Ord a => Tree a -> a
-- maximum' (Leaf a) = a
-- maximum' (Branch a b) = max (maximum' a) (maximum' b)
maximum' xs = fold max id xs

depth :: Tree a -> Int
-- depth (Leaf _) = 1
-- depth (Branch left right) = max (depth left) (depth right) + 1
depth xs = fold (\l r -> max l r + 1) (\x->1) xs


map' :: (a -> b) -> Tree a -> Tree b
-- map' f (Leaf a) = Leaf (f a)
-- map' f (Branch left right) = Branch (map' f left) (map' f right)
-- map' f xs = fold Branch (\x -> Leaf (f x)) xs
map' f xs = fold Branch (Leaf . f) xs


fold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
fold reduce term (Leaf a) = term a
fold reduce term (Branch left right) = reduce (fold reduce term left) (fold reduce term right)


