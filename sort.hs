module Sort where

{-
  | All bad lazy sorting.
  The Lists used to test are : [10,2,1,3,5,4,11] [-10,-2,0,1,5,10,26,40]
-}

-- | Lazy sort where inserting the first list to the second sorted list.
-- P1 (bad version 1)
slow_signleton_insert :: Ord a => a -> [a] -> [a]
slow_signleton_insert x [] = [x]
slow_signleton_insert x (y:ys)
  | x >= y = y : slow_signleton_insert x ys
  | x < y  = x : slow_signleton_insert y ys

slow_insert :: Ord a => [a] -> [a] -> [a]
slow_insert [] lst        = lst
slow_insert (x:xs) lst = slow_insert xs $ slow_signleton_insert x lst

-- | Lazy sort where inserting the first list to the second sorted list.
-- P2 (bad version 2)
slow_signleton_insert_v2 :: Ord a => a -> [a] -> [a]
slow_signleton_insert_v2 x lst = lst1 ++ [x] ++ lst2
 where lst1 = [ z
              | z <- lst
              , z <= x ]
       lst2 = [ z
              | z <- lst
              , z > x ]

slow_insert_v2 :: Ord a => [a] -> [a] -> [a]
--slow_insert_v2 _ []     = error "unexpected sorting error"
slow_insert_v2 [] lst     = lst
slow_insert_v2 (x:xs) lst = slow_insert_v2 xs $ slow_signleton_insert_v2 x lst

-- | Lazy sort of a list. P1 (bad version)
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

lenUpHalf lst@(x:xs) = ceiling $ fromIntegral (len lst) / 2

lenLowHalf lst@(x:xs) = div (len lst) 2

getUpHalf :: [a] -> Int -> Int -> [a]
getUpHalf [] _ _     = []
getUpHalf _ 0 _      = []
getUpHalf (x:xs) n 0 = x : getUpHalf xs (n - 1) 0
getUpHalf (x:xs) n i = getUpHalf xs n (i - 1)

getLowHalf :: [a] -> Int -> [a]
getLowHalf [] _         = []
getLowHalf lst@(x:xs) 0 = []
getLowHalf (x:xs) n     = x : getLowHalf xs (n - 1)

slow_merge_sort :: Ord a => [a] -> [a] -> [a]
slow_merge_sort lst [] = lst
slow_merge_sort [] lst = lst
slow_merge_sort lst1@(x:xs) lst2@(y:ys)
  | x <= y  = x : slow_merge_sort xs lst2
  | x > y   = y : slow_merge_sort lst1 ys

slow_split_sort :: Ord a => [a] -> [a]
slow_split_sort []         = []
slow_split_sort [x]        = [x]
slow_split_sort lst@(x:xs) = let n1   = lenLowHalf lst
                                 n2   = lenUpHalf lst
                                 lst1 = slow_split_sort $ getLowHalf lst n1
                                 lst2 = slow_split_sort $ getUpHalf lst n2 n1
                                 in slow_merge_sort lst1 lst2

-- | Lazy sort of a list. P3 (better bad? version)
getUpHalf_v2 :: [a] -> Int -> [a]
getUpHalf_v2 [] _     = []
getUpHalf_v2 lst 0    = lst
getUpHalf_v2 (x:xs) n = getUpHalf_v2 xs (n - 1)

slow_split_sort_v2 :: Ord a => [a] -> [a]
slow_split_sort_v2 []         = []
slow_split_sort_v2 [x]        = [x]
slow_split_sort_v2 lst@(x:xs) = let n1   = lenLowHalf lst
                                    lst1 = slow_split_sort_v2 $ getLowHalf lst n1
                                    lst2 = slow_split_sort_v2 $ getUpHalf_v2 lst n1
                                    in slow_merge_sort lst1 lst2

-- | Lazy sort of a list. P4 (better clearner bad? version)
slow_split_sort_v3 :: Ord a => [a] -> [a]
slow_split_sort_v3 []         = []
slow_split_sort_v3 [x]        = [x]
slow_split_sort_v3 lst@(x:xs) = let n    = lenLowHalf lst
                                    lst1 = slow_split_sort_v3 $ take n lst
                                    lst2 = slow_split_sort_v3 $ drop n lst
                                    in slow_merge_sort lst1 lst2

-- | Lazy sort of a list. P3 (better-er clearner-er bad? version)
listSplitter :: [a] -> ([a],[a])
listSplitter lst = (take n lst, drop n lst)
 where n = div (len lst) 2

slow_split_sort_v4 :: Ord a => [a] -> [a]
slow_split_sort_v4 []  = []
slow_split_sort_v4 [x] = [x]
slow_split_sort_v4 lst = slow_merge_sort lst1 lst2
 where (left, right) = listSplitter lst
       lst1          = slow_split_sort_v4 left
       lst2          = slow_split_sort_v4 right

-- | Lazy testing for determining if a list is sorted
is_sorted :: (a -> a -> Bool) -> [a] -> Bool
is_sorted f []  = True
is_sorted f [x] = True
is_sorted f (x:y:ys)
  | f x y     = is_sorted f (y:ys)
  | otherwise = False
