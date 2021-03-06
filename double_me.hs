doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- --------------------------------------------------------------------
-- Using list comprehension
-- length' xs = sum [1 | _ <- xs]

-- Using recursion
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs

-- --------------------------------------------------------------------

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triangles xs = [ (a,b,c) | c <- xs, b <- xs, a <- xs ]

-- factorial :: Integer -> Integer
-- factorial n = product[1..n]

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

capital :: String -> String  
capital [] = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Using where clause
-- calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
-- calcBmis xs = [bmi w h | (w, h) <- xs]  
--     where bmi weight height = weight / height ^ 2

-- Using let and list comprehension
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
-- maximum' (x:xs)   
--     | x > maxTail = x  
--     | otherwise = maxTail  
--     where maxTail = maximum' xs
maximum' (x:xs) = max' x (maximum' xs)

-- Sectioning an infix function
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

-- A function can return a function...
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

-- higher order programming...
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Quicksort using powerful filter...
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

-- fold examples... shows lambdas
-- sum' :: (Num a) => [a] -> a  
-- sum' xs = foldl (\acc x -> acc + x) 0 xs  

-- .. and using currying... because (+) is the same as (\acc x -> acc + x)
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0

-- Function application and composition
-- Plain implementation
-- oddSquareSum :: Integer  
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Using composition
-- oddSquareSum :: Integer  
-- oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- Using let + application
oddSquareSum :: Integer  
oddSquareSum =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit

