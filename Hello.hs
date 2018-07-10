
main = putStrLn "hello."

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

composit :: (b -> c) -> (a -> b) -> a -> c
composit f g = \x -> f (g x)


incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times -1) n)



-- myDivMod a b m = if (a < b) then (m, a) else myDivMod (a - b) b (m + 1)
myDivMod a b m 
    | (a < b) = (m, a) 
    | True = myDivMod (a - b) b (m + 1)

divMod' a 0 = error "div by 0"
divMod' a b = myDivMod a b 0