module Either where

data Either' e v = Left' { error :: e } | Right' { value :: v } deriving (Show)


mean :: Fractional a => [a] -> Either' String a
mean [] = Left' "mean of EMPTY list"
mean xs = Right' ((sum xs) / (fromIntegral (length xs)))



-- 4.6
map' :: (a -> b) -> Either' e a -> Either' e b
map' f a = flatMap' (Right' . f) a

-- getOrElse' :: a -> Either' e a -> a
-- getOrElse' d (Left' e) = d
-- getOrElse' _ (Right' a) = a

flatMap' :: (a -> Either' e b) -> Either' e a -> Either' e b
flatMap' _ (Left' e) = Left' e
flatMap' f (Right' a) = f a

orElse' :: Either' e a -> Either' e a -> Either' e a
orElse' (Left' e) b = b
orElse' a _ = a

map2' :: (a -> b -> c) -> Either' e a -> Either' e b -> Either' e c
map2' f a b = flatMap' (\x -> map' (\y -> f x y) b) a


-- 4.7
traverse' :: (a -> Either' e b) -> [a] -> Either' e [b]
traverse' _ [] = Right' []
traverse' f (x:xs) = map2' (:) (f x) (traverse' f xs)

sequence' :: [Either' e a] -> Either' e [a]
sequence' xs = traverse' id xs


-- 4.8

data Name s = Name { n :: s } deriving (Show)
data Age n = Age { a :: n } deriving (Show)
data Person s n = Person { name :: Name s, age :: Age n} deriving (Show)

makeName :: String -> Either' String (Name String)
makeName [] = Left' "empty name"
makeName str = Right' (Name str)

makeAge :: Int -> Either' String (Age Int)
makeAge n = if n < 1 then Left' "invalid age" else Right' (Age n)

-- Person 타입 기술을 이렇게 하는게 맞는지 잘 모르겠다.
makePerson :: String -> Int -> Either' String (Person String Int) 
makePerson name age = map2' Person (makeName name) (makeAge age)

