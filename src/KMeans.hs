module KMeans () where

type Assignment = [Int]

-- | Cluster data into K clusters
kmeans :: (Floating a, Ord a) => [[a]] -> Int -> Assignment
kmeans xs k = _kmeans xs $ nearestMeans xs (take k xs)

_kmeans :: (Floating a, Ord a) => [[a]] -> Assignment -> Assignment
_kmeans xs asgn
  | newAssign == asgn = asgn
  | otherwise = _kmeans xs newAssign
  where
    newAssign = nearestMeans xs $ calcMeans xs asgn

-- | Recompute means
calcMeans :: (Floating a) => [[a]] -> Assignment -> [[a]]
calcMeans xs asgn = map listMean [[x | (x, i) <- zip xs asgn, i == j] | j <- [0 .. k]]
  where
    k = maximum asgn

-- | mean of a list of lists
listMean :: (Floating a) => [[a]] -> [a]
listMean [] = error "mean of empty list"
listMean xs = map (/ k) $ listSum xs
  where
    k = genericLength xs

-- | sum of a list of lists
listSum :: (Floating a) => [[a]] -> [a]
listSum = foldl1 (\x1 x2 -> zipWith (+) x1 x2)

-- | Find the mean nearest each data point
nearestMeans :: (Floating a, Ord a) => [[a]] -> [[a]] -> [Int]
nearestMeans xs ms = map argmin dists
  where
    dists = [map (sumSquares x) ms | x <- xs]

-- | Calculate the sum of squared error between two lists
sumSquares :: (Floating a) => [a] -> [a] -> a
sumSquares xs ys = sum [(x - y) ^ 2 | (x, y) <- zip xs ys]

-- | Get the index of the smallest element in list
argmin :: (Ord a) => [a] -> Int
argmin (x : xs) = _argmin xs x 0 0

_argmin :: (Ord a) => [a] -> a -> Int -> Int -> Int
_argmin [x] val cix ix
  | x < val = cix + 1
  | otherwise = ix
_argmin (x : xs) val cix ix
  | x < val = _argmin xs x (cix + 1) (cix + 1)
  | otherwise = _argmin xs val (cix + 1) ix
