module H_99_Problems where

{- | Link to the 99 Haskell Problems:
       https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

   | Throughout all the solved problems here, the general idea was to try to not
   use any pre-defined functions. And to solve as many as possible without
   external assistance (or ripping off snippets from "google"). In some of the
   solved problems, uses the previous problem solution as helper functions. With
   exceptions to functions like: (+), (-), (*), (/), (==), (:), (++), (mod), and
   so on...

   | There might be logical errors due to solving these problems while during
   in-between chaotic sessions.

   | Styling is half-ass at best. Still trying to figure out what the standard
   is for Haskell. (From a quick google search, everyone has their own different
   styling guide for Haskell... Not very helpful...) Ignore any styling
   inconsistencies.

   | Note...
   (Symbol) : Description, <num_of_times_used>
   (?) : Porblems where I couldn't figure the hell out of what's going on, <2>
   (*) : Problems was not able solved, <3>
   (!) : Could not do it without import-ing libraries or using default higher
         level functions, <5>
   ($) : Ripped off somewhere on the net, <1>
   (P) : Partially solved or only works for a subset of inputs, <1>
   (W) : Working on it, <0>
   **The <num_of_times_used> for the corresponding symbol could be wrong, never
     bother to actually double check**
-}

import Data.List
import System.Random

-- | Random functions used for testing
isElem :: Eq a => a -> [a] -> Bool
isElem _ []     = False
isElem e (x:xs)
    | e == x    = True
    | otherwise = isElem e xs

fsort :: (a -> a -> Bool) -> [a] -> [a]
fsort fcn []                   = []
fsort fcn (x:xs)               = f (x:xs) []
  where insertSignleton f e [] = [e]
        insertSignleton f e (t:ts)
            | f e t            = e : insertSignleton f t ts
            | otherwise        = t : insertSignleton f e ts
        f [] accum             = accum
        f (y:ys) accum         = f ys (insertSignleton (fcn) y accum)

lstEq :: Eq a => [a] -> [a] -> Bool
lstEq lst1 lst2
    | length lst1 == length lst2 = isEq lst1 lst2
    | otherwise                  = False
  where isEq [] _                = True
        isEq (x:xs) lst2@(y:ys)
            | isElem x lst2      = isEq xs lst2
            | otherwise          = False

lstOfDiff :: Eq a => [a] -> [a] -> [a]
lstOfDiff lst1 lst2
    | length lst1 >= length lst2 = f lst1 lst2
    | otherwise = f lst2 lst1
  where f [] _ = []
        f (x:xs) lst3
            | isElem x lst3 = f xs lst3
            | otherwise = x : f xs lst3
-- | Problem 1
myLast :: [a] -> a
myLast []     = error "empty list was given"
myLast [x]    = x
myLast (x:xs) = myLast xs

-- | Problem 2
myButLast :: [a] -> a
myButLast []     = error "empty list was given"
myButLast [x]    = error "signleton was given"
myButLast [x,y]  = x
myButLast (x:xs) = myButLast xs

-- | Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _     = error "element at given index does not exists"
elementAt (x:_) 0  = x
elementAt (x:xs) n
    | n >= 0       = elementAt xs (n - 1)
    | n < 0        = error "element at given index does not exists"

-- | Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- | Problem 5
myReverse :: [a] -> [a]
myReverse []           = []
myReverse (x:xs)       = f (x:xs) []
  where f [] accum     = accum
        f (x:xs) accum = f xs (x : accum)

-- | Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome lst = lst == myReverse lst

-- | Problem 7
data NestedList a = Elem a | List [NestedList a]
-- [1,[2,[3,4],[5]]]

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- | Problem 8
compressHelper :: Eq a => [a] -> [a] -> [a]
compressHelper [] accum         = accum
compressHelper (x:xs) accum
    | isUnique x accum == True  = compressHelper xs (accum ++ [x])
    | otherwise                 = compressHelper xs accum
  where isUnique e [] = True
        isUnique e (y:ys)
            | e == y            = False
            | otherwise         = isUnique e ys

compress :: Eq a => [a] -> [a]
compress []         = []
compress lst@(x:xs) = compressHelper lst []

-- | Problem 9
-- Note: That passing the function 'newList' is not necessary, just did it
--       because wasting time and being stupid... don't sue me...
newList :: Eq a => a -> [a] -> [a]
newList e []    = []
newList e (x:xs)
    | e == x    = newList e xs
    | otherwise = (x:xs)

packHelper :: Eq a => (a -> [a] -> [a]) -> [a] -> [[a]]
packHelper fcn []         = []
packHelper fcn lst@(x:xs) = (f x lst []) : packHelper fcn (fcn x lst)
  where f e [] accum = accum
        f e (y:ys) accum
            | e == y       = f e ys (y : accum)
            | otherwise    = accum -- f e ys accum

pack :: Eq a => [a] -> [[a]]
pack []         = []
pack lst@(x:xs) = packHelper (newList) lst

-- | Problem 10
-- encode "aaaabccaadeeee"
encode :: Eq a => [a] -> [(Int, a)]
encode []  = []
encode lst = map (\(x:xs) -> (myLength (x:xs), x)) (pack lst)

-- | Problem 11
data EncodeMod = Signle Char | Multiple Int Char deriving Show

encodeModified :: [Char] -> [EncodeMod]
encodeModified []       = []
encodeModified lst      = map f encoded
  where encoded = map (\(x:xs) -> (myLength (x:xs), x)) (pack lst)
        f (n,c)
            | n > 1     = Multiple n c
            | otherwise = Signle c

-- | Problem 12
flatten2dto1d :: [[a]] -> [a]
flatten2dto1d []     = []
flatten2dto1d (x:xs) = x ++ flatten2dto1d xs

decodeModified :: [EncodeMod] -> [Char]
decodeModified []         = []
decodeModified lst        = flatten2dto1d $ map f lst
   where f (Signle c)     = [c]
         f (Multiple 0 c) = []
         f (Multiple n c) = [c] ++ f (Multiple (n - 1) c)

-- | Problem 13
-- A repeat of Problem 11?
-- Will go back if not.
encodeDirect = encodeModified

-- | Problem 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

-- | Problem 15
repli :: [a] -> Int -> [a]
repli [] _      = []
repli lst 0     = []
repli (x:xs) n  = f x n ++ repli xs n
  where f x 0   = []
        f x n   = x : (f x (n - 1))

-- | Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _          = []
dropEvery lst 0         = []
dropEvery lst n         = f lst n 1
  where f [] _ _        = []
        f (x:xs) n track
           | n == track = f xs n 1
           | otherwise  = x : f xs n (track + 1)

-- | Problem 17
split :: [a] -> Int -> ([a],[a])
split [] _         = ([],[])
split lst n        = ((f lst n), (s lst n))
  where f [] _     = []
        f (x:xs) 0 = []
        f (x:xs) n = x : f xs (n - 1)
        s [] _     = []
        s (x:xs) 0 = xs
        s (x:xs) n = s xs (n - 1)

-- | Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _     = []
slice (x:xs) 1 2 = []
slice (x:xs) 1 k = x : slice xs 1 (k - 1)
slice (x:xs) i k = slice xs (i - 1) k

-- | Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _              = []
rotate lst 0             = lst
rotate lst@(x:xs) n
    | n > 0              = f lst n []
    | otherwise          = rotate lst ((myLength lst) + n)
  where f [] _ _         = []
        f lst 0 _        = lst
        f (x:xs) 1 accum = xs ++ accum ++ [x]
        f (x:xs) n accum
            | n > 1      = f xs (n - 1) (accum ++ [x])
            | otherwise  = []

-- | Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ []            = error "empty list was given"
removeAt n lst@(x:xs)    = (elementAt lst (n - 1), newLst lst n)
  where newLst (x:xs) 1  = xs
        newLst (x:xs) n  = x : newLst xs (n - 1)

-- | Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _     = []
insertAt e lst 1    =  e : lst
insertAt e (x:xs) n = x : insertAt e xs (n - 1)

-- | Problem 22
range :: Int -> Int -> [Int]
range i j
    | i > j     = []
    | otherwise = i : range (i + 1) j

-- | Problem 23 (*) (!)
-- Was never taught how Haskell does IO and was barely taught about Monads...
rnd_select :: [a] -> Int -> IO [a]
rnd_select (x:xs) n = error "too retarded to figure it out"

-- | Problem 24 (*) (!) ($)
-- myNub does not takes advantage of lazy static typing, and has completely
-- trash complexity
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub lst = f lst []
  where f [] z = z
        f (x:xs) z = f xs (g x z)
        g c [] = [c]
        g c (y:ys)
            | c == y = (y:ys)
            | otherwise = y : g c ys

-- Completely ripped off from https://stackoverflow.com/a/27731095
-- All credit goes to Vektorweg
diff_select :: Int -> Int -> IO ()
diff_select i j = do
    g <- newStdGen
    print . take i . nub $ (randomRs (0,j) g :: [Int])

-- | Problem 25 (*)
rnd_permu :: [a] -> [a]
rnd_permu [] = []
rnd_permu (x:xs) = error "too retarded to figure it out"

-- | Problem 26 (P)(!)
-- Solution to this problem, assumes all elements are unique. Don't know how to
-- handle non-uniqueness.
--------------------------------------------------------------------------------
{-                             --REVISION--                                   -}
{-               Just More Cleaner And Simpler And Readable                   -}
--------------------------------------------------------------------------------
{-                                 Note:                                      -}
{-     The reason why I even did this is because, why not. Deal with it.      -}
--------------------------------------------------------------------------------
{-                                 START                                      -}
--------------------------------------------------------------------------------
takeNdrop :: Int -> [a] -> ([a], [a])
takeNdrop 0 lst             = ([], lst)
takeNdrop _ []              = ([], [])
takeNdrop n lst@(x:xs)      = (t2, t1)
  where f 0 (lst, accum)    = (lst, accum)
        f i ([], accum)     = ([], accum)
        f i ((y:ys), accum) = f (i - 1) (ys, accum ++ [y])
        (t1, t2)            = f n (lst, [])

mySort :: Ord a => [a] -> [a]
mySort []                      = []
mySort (x:xs)                  = f (x:xs) []
  where fcn                    = \s t -> s <= t
        insertSignleton f e [] = [e]
        insertSignleton f e (t:ts)
            | f e t            = e : insertSignleton f t ts
            | otherwise        = t : insertSignleton f e ts
        f [] accum             = accum
        f (y:ys) accum         = f ys (insertSignleton (fcn) y accum)

setIdx :: Int -> [a] -> a -> [a]
setIdx _ [] _   = []
setIdx n lst@(l:ls) d
    | n > 1     = l : setIdx (n - 1) ls d
    | n == 1    = d : ls
    | otherwise = []

subIdx :: Ord a => Int -> [[a]] -> a -> [[a]]
subIdx _ [] d = []
subIdx n (l:ls) d
    | n > 0   = mySort (setIdx n l d) : subIdx n ls d
    | n <= 0  = []

sf :: Ord a => Int -> [[a]] -> [a] -> [[a]]
sf 0 _ _               = []
sf n [] _              = []
sf n lst@(l:ls) []     = lst
sf n lst@(l:ls) (d:ds) = sf n (subLst n lst d) ds
  where subLst n2 lst2 q
          | n2 > 0     = (subIdx n2 lst2 q) ++ (subLst (n2 - 1) lst2 q)
          | otherwise  = lst2

recombinations :: (Ord a, Enum a) => Int -> [a] -> [[a]]
recombinations n lst = nub $ sf n [t1] t2
  where (t1, t2)     = takeNdrop n lst
--------------------------------------------------------------------------------
{-                                  END                                       -}
--------------------------------------------------------------------------------

-- | Problem 27 (!)
-- | Hangs like all hell with complexity gone to shit... LOL
repos :: [a] -> (Int, a) -> (Int, a) -> [a]
repos [] _ _    = []
repos lst@(x:xs) (n1, e1) (n2, e2)
    | n1 == n2  = lst
    | n1 == 1   = e1 : repos xs (n1 - 1, e1) (n2 - 1, e2)
    | n2 == 1   = e2 : repos xs (n1 - 1, e1) (n2 - 1, e2)
    | otherwise = x : repos xs (n1 - 1, e1) (n2 - 1, e2)

transition :: [a] -> Int -> [[a]]
transition [] _ = []
transition _ 0 = []
transition lst@(x:xs) n1 = t ++ transition s (n1 - 1)
  where n2 = myLength lst
        f lst2 p1 p2 = repos lst2 (p1, elementAt lst2 (p2 - 1)) (p2, elementAt lst2 (p1 - 1))
        g (lst3, accum) p3 p4
            | p3 == n2 = (lst3, accum)
            | otherwise = g (z, z : accum) (p3 + 1) (p4 + 1)
          where z = f lst3 p3 p4
        (s, t) = g (lst, []) 1 2

--gonnaGetSwifty :: [a] -> [[a]] -> [[a]]
gonnaGetSwifty [] accum                = accum
gonnaGetSwifty (x:xs) []               = gonnaGetSwifty xs [[x]]
gonnaGetSwifty lst1@(x:xs) lst2@(y:ys) = gonnaGetSwifty xs (h lst2 nSub)
  where nSub                           = (myLength y) + 1
        f [] _                         = [x]
        f (z:zs) n
            | n == 1                   = x : z : zs
            | otherwise                = z : f zs (n - 1)
        g lst3 len
            | len == 0                 = []
            | otherwise                = f lst3 len : g lst3 (len - 1)
        h [] _                         = []
        h (a:as) i                     = g a i ++ h as i

swifty :: Eq a => [a] -> Int -> [[a]]
swifty [] _ = []
swifty lst@(x:xs) n = broken
  where brokenElem = elementAt lst
        brokenIdx = [ (i, j)
                    | i <- [0..(n - 1)]
                    , j <- [0..(n - 1)]
                    , i <= j
                    ]
        brokenPair = map (\(s,t) -> ((t + 1, brokenElem s), (s + 1, brokenElem t))) brokenIdx
        broken = nub $ map (\(s, t) -> repos lst s t) brokenPair

--regroup :: [Int] -> [a] -> [[a]]
regroup _ []              = []
regroup [] _              = []
regroup (i:is) lst@(x:xs) = lst1 : regroup is lst2
  where (lst1, lst2)      = takeNdrop i lst

groups1 :: Eq a => [Int] -> [a] -> [[[a]]]
groups1 sizeLst lst = map (\x -> regroup sizeLst x) lst2
  where lst2 = swifty lst (myLength lst)

groups2 :: (Ord a, Enum a) => [Int] -> [a] -> [[[a]]]
groups2 sizeList lst = nub $ map (\x -> map (\y -> mySort y) x) lst2
  where lst2 = map (\s -> regroup sizeList s) $ transition lst (myLength lst)

groups3 :: (Ord a, Enum a) => [Int] -> [[a]] -> [[[[a]]]]
groups3 sizeLst lst  = nub $ map (\x -> map (\y -> mySort y) x) (f lst3)
  where (lst2, lst3) = (gonnaGetSwifty lst [], gonnaGetSwifty sizeLst [])
        f []         = []
        f (i:is)     = (map (\x -> regroup i x) lst2) ++ f is

-- | Problem 27.a
group3 :: (Ord a, Enum a) => [[a]] -> [[[[a]]]]
group3 = groups3 [2,3,4]

-- | Problem 27.b
group' :: (Ord a, Enum a) => [Int] -> [[a]] -> [[[[a]]]]
group' = groups3

--------------------------------------------------------------------------------
{-                        Random Fun / Mental Break                           -}
--------------------------------------------------------------------------------
{-                                 START                                      -}
--------------------------------------------------------------------------------
--qq1 = fromEnum 'A'
--qq2 = toEnum 65 :: Char

-- | Fails to convert negative single digit numbers
myCharToInt :: Char -> Int
myCharToInt c
    | val >= 48 && val <= 57 = val - 48
    | otherwise              = -1    -- error
  where val                  = fromEnum c

-- | Fails to convert string representive decminal based numbers
myStringToNum :: Fractional a => [Char] -> a
myStringToNum []            = 0
myStringToNum s@(x:xs)
    | x == '-'              = (-1) * (fi (f l2 1) + (g r2 1))
    | x == '+'              = fi (f l2 1) + (g r2 1)
    | otherwise             = fi (f l1 1) + (g r1 1)
  where fi                  = fromIntegral
        val                 = myCharToInt
        rev [] (digit, dec) = (digit, dec)
        rev (y:ys) (digit, dec)
            | y == '.'      = (digit, ys)
            | val y /= -1   = rev ys (y : digit, dec)
            | otherwise     = error "given string is not a representive number"
        (l1, r1)            = rev s ([], [])
        (l2, r2)            = rev xs ([], [])
        f [] _              = 0
        f (z:zs) nthDigit   = ((val z) * nthDigit) + (f zs (nthDigit * 10))
        g [] _  = 0
        g (d:ds) nthDec
            | cond          =  (fi (val2) * val3) + (g ds (nthDec + 1))
            | otherwise     = error "given string is not a representive number"
          where val2        = val d
                cond        = val2 >= 0 && val2 <= 9
                val3        = fi (1) / (fi (10) ^^ (nthDec))
--------------------------------------------------------------------------------
{-                                  END                                       -}
--------------------------------------------------------------------------------

-- | Problem 28.a
insertSignleton1 :: ([a] -> [a] -> Bool) -> [a] -> [[a]] -> [[a]]
insertSignleton1 f x [] = [x]
insertSignleton1 f x (y:ys)
    | f x y             = x : insertSignleton1 f y ys
    | otherwise         = y : insertSignleton1 f x ys

lsort :: [[a]] -> [[a]]
lsort []               = []
lsort (x:xs)           = f (x:xs) []
  where fcn            = \s t -> (myLength s) <= (myLength t)
        f [] accum     = accum
        f (y:ys) accum = f ys (insertSignleton1 (fcn) y accum)

-- | Problem 28.b
insertSignleton2 :: Eq b => (([a], b) -> ([a], b) -> Bool) -> ([a], b)
                                      -> [([a], b)] -> [([a], b)]
insertSignleton2 f x [] = [x]
insertSignleton2 f x (y:ys)
    | f x y             = if (freq x) == (freq y)
                              then l : insertSignleton2 f s ys
                              else x : insertSignleton2 f y ys
    | otherwise         = y : insertSignleton2 f x ys
  where freq (_,num)    = num
        len (lst,_)     = myLength lst
        (s,l)           = if (len x) <= (len y)
                              then (x,y)
                              else (y,x)

freqLstLen :: [[a]] -> [([a], Int)]
freqLstLen []           = []
freqLstLen lst          = lst2
  where f n i []        = i
        f n i ((_,t):xs)
            | t == n    = f n (i + 1) xs
            | otherwise = f n i xs
        lst1            = map (\x -> (x, myLength x)) lst
        lst2            = [ (a,(f b 0 lst1)) | (a,b) <- lst1 ]

lfsort :: [[a]] -> [[a]]
lfsort []              = []
lfsort (x:xs)          = map (\(l,_) -> l) $ f (freqLstLen (x:xs)) []
  where fcn            = \(a,b) (c,d) -> (b <= d)
        f [] accum     = accum
        f (y:ys) accum = f ys (insertSignleton2 (fcn) y accum)

-- | Problem 31
isPrime :: Int -> Bool
isPrime p
    | p <= 1                  = False
    | otherwise               = f p (p - 1)
  where f p2 1                = True
        f p2 n
            | p2 `mod` n /= 0 = f p2 (n - 1)
            | otherwise       = False

-- | Problem 32
myGCD :: Int -> Int -> Int
myGCD a b
    | b == 0    = a
    | otherwise = myGCD b (a `mod` b)

-- | Problem 33
coPrime :: Int -> Int -> Bool
coPrime n1 n2
    | n1 <= 0 || n2 <= 0 = False
    | otherwise          = myGCD n1 n2 == 1

-- | Problem 34
totient :: Int -> Int
totient n
    | n <= 0              = 0
    | otherwise           = f n (n - 1)
  where f s 0             = 0
        f s t
            | coPrime s t = 1 + f s (t -1)
            | otherwise   = f s (t - 1)

-- | Problem 35
primeFactors :: Int -> [Int]
primeFactors n
    | n < 1                                = []
    | n == 1                               = [1]
    | otherwise                            = myReverse $ g n n
  where f m l
            | m == 1                       = 1
            | isPrime l && m `mod` l == 0  = l
            | otherwise                    = f m (l - 1)
        g s t
            | val1 == 1                    = []
            | otherwise                    = val1 : g val2 val2
          where val1                       = f s t
                val2                       = s `div` val1

-- | Problem 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n
    | n < 1                = []
    | n == 1               = [(1, 1)]
    | otherwise            = g $ primeFactors n
  where f ([], num, freq)  = ([], num, freq)
        f ((p:ps), num, freq)
            | p == num     = f (ps, num, freq + 1)
            | otherwise    = ((p:ps), num, freq)
        g [] = []
        g lst@(x:xs)       = (x, t) : g s
          where (s, _, t)  = f (lst, x, 0)

-- | Problem 37
totient_phi :: Int -> Int
totient_phi n
    | n < 1             = 0
    | n == 1            = 1
    | otherwise         = f lst
  where lst             = prime_factors_mult n
        pw a 0          = 1
        pw a b          = a * pw a (b - 1)
        f []            = 1
        f ((p, m):rest) = (p - 1) * (pw p (m - 1)) * (f rest)

-- | Problem 38
--
-- Explnation/Answer:
--
-- function1 should make the person running this function to feel like it's
-- overheating their CPU and ripping their electricity bill a new one.
--
-- While function2 should run significantly faster compared to function1 and it
-- DOES NOT, overheat their CPU and ripping their electricity bill a new one.
-- Also, if you can afford to run function1, you can play any AAA game on max
-- settings and sees money as no object as you waste on it.
function1 = totient 10090
function2 = totient_phi 10090

-- | Problem 39
primeR :: Int -> Int -> [Int]
primeR n1 n2
    | n1 > n2 || n1 < 1 || n2 == 1 = []
    | otherwise                    = f n1 n2
  where f s t
            | s > t                = []
            | isPrime s            = s : f (s + 1) t
            | otherwise            = f (s + 1) t

-- | Problem 40
goldbach :: Int -> (Int, Int)
goldbach n
    | n `mod` 2 == 0           = g lst
    | otherwise                = (-1, -1)    -- error
  where lst = primeR 1 n
        f [] _                 = (-1, -1)    -- error
        f (x:xs) y
            | x + y == n       = (y, x)
            | otherwise        = f xs y
        g []                   = (-1, -1)    -- error
        g (p:ps)
            | pair /= (-1, -1) = pair
            | otherwise        = g ps
          where pair           = f lst p

-- | Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n1 n2
    | n1 > n2 || n1 < 2      = []
    | otherwise              = f n1 n2
  where f s t
            | s > t          = []
            | s `mod` 2 == 0 = goldbach s : f (s + 1) t
            | otherwise      = f (s + 1) t

-- | Problems 42 to 45   ( they aren't listed???? )
{- | Currently at the time of attempting to solve as much of the posted Haskell
    Problems, they are not listed.
       => Refer to the above link where the 99 Haskell Problems are posted.

   | My response:    Seriously, where are they???
-}

-- | Problem 46 to Problem 47 (!)(?)
and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = (a == b) && (a == False)

nor' :: Bool -> Bool -> Bool
nor' = (==)

xor' :: Bool -> Bool -> Bool
xor' = (==) . (not)

impl' :: Bool -> Bool -> Bool
impl' a b = not $ (a == True) && (b == False)

equ' :: Bool -> Bool -> Bool
equ' a b = (a == b) && (a == True)

--table :: Monad m => ((Bool, Bool) -> m b) -> m ()
table :: Show a => (Bool -> Bool -> a) -> IO ()
table fcn    = mapM_ f tval
  where tval = [ (x, y)
               | x <- [True, False]
               , y <- [True, False]
               ]
        f    = (\(a, b) -> putStrLn $ show (a) ++ " " ++ show (b) ++ " "
                                                      ++ show (fcn a b)
                                                      )

-- | Problem 46 (?)
problem_46_example_fcn = (\a b -> and' a (or' a b))

-- | Problem 47 (?)
problem_47_example_fcn = (\a b -> a `and'` (a `or'` b))

-- | Problem 48
tablen :: Show a => Int -> (Bool -> Bool -> a) -> IO ()
tablen n fcn = print "WHAT IS GOING ON?!?!??"
