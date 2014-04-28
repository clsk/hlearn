import Data.List

myLast :: [a] -> a
myLast [] = error "list is empty"
myLast [x] = x
myLast (_:xs) = myLast xs

myLastButOne :: [a] -> a
myLastButOne (_:x:y:xs) = myLastButOne (x:y:xs)
myLastButOne (x:y:xs) = x
myLastButOne _ = error "list length < 2"

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt xs i
	|  i < 1 || i > length xs = error "Index out of bounds"
	|  otherwise = elementAt (tail xs) (i-1) 

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc+1) 0 

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = all (\ (x,y) -> x == y) (zip xs $ reverse xs)

compress :: (Eq a) => [a] -> [a]
compress (x:ys@(y:_))
	| x == y 	= compress ys
	| otherwise = x : compress ys
compress xs =  xs

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group

packit :: (Eq a) => [a] -> [[a]]
packit (x:xs) = let (first, rest) = span (==x) xs
				in (x:first) : packit rest
packit [] = []

encodeit :: (Eq a) => [a] -> [(Int, a)]
encodeit (x:xs) = let (first, rest) = span (==x) xs
					in (length first + 1, x) : encodeit rest
encodeit [] = []
