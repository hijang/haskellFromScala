-- 이것은 주석!

module MyModule where

myAbs :: Int -> Int  -- myAbs는 정수를 받아서 정수를 돌려준다.
myAbs n = 
    if (n < 0) then (- n)  -- 만약 n이 0보다 작으면 n의 부정(negate, 부호가 반대)을 돌려준다.
    else n

abs' :: Int -> Int
abs' n 
    | n < 0 = (- n)
    | otherwise = n

formatAbs :: Int -> String
formatAbs n = 
    let absOfN = show (abs' n) in
    let strOfN = show n in
        "The absolute value of " ++ (show n) ++ " is " ++ (show absOfN)
    

main = putStrLn (formatAbs (-42))



-- 루프 대신 재귀 이용하기
factorial :: Integer -> Integer
-- factorial n = 
--     let go n acc = if (n <= 0) then acc  -- go를 패턴 매칭으로 구현하려면 let을 어떻게 써야할까? 
--         else go (n - 1) (n * acc) 
--     in go n 1

factorial n = go n 1 where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)


isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ (h:[]) = True
isSorted comp (first:tail@(second:_)) = comp first second && isSorted comp tail


currying :: ((a, b) -> c) -> a -> b -> c
currying func a b = func (a, b)

uncurrying :: (a -> b -> c) -> (a, b) -> c
uncurrying func (a, b) = func a b

uncurriedFunction (a, b) = a + b
curriedFunction a b = a + b

cf = currying uncurriedFunction
uncf = uncurrying curriedFunction


compose' :: (b -> c) -> (a -> b) -> a -> c
compose' f g x = f (g x)

