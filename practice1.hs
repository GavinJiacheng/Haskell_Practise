
-- Question 1:

snoc :: a -> [a] -> [a]
snoc n     [] = [n]
snoc n (x:xs) = x: (snoc n xs)

-- Question 2:

myappend :: [a] -> [a] -> [a]
myappend []     ys = ys
myappend (x:xs) ys = x : (myappend xs ys)

-- Question 3:

myreverse :: [a] -> [a]
myreverse []     = []
myreverse (x:xs) = myappend (myreverse xs) [x]

-- Question 4:
-- The functions 'smallest_divisor' and 'is_prime' are from the class note 'Examples of Functions'

smallest_divisor :: Int -> Int
smallest_divisor n
               | n < 0     = error "n must be >= 0"
               | n == 0    = 0
               | n == 1    = 1
               | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

is_prime :: Int -> Bool
is_prime n | n < 2     = False
           | otherwise = (smallest_divisor n) == n

-- get the power of ten for the number

get_digital :: Int -> Int
get_digital n
          | n < 10     = 1
          | otherwise  = 10 * (get_digital (n `div` 10))

-- reverse number

reverse_number :: Int -> Int
reverse_number n
             | n < 10    = n
             | otherwise = ((n `mod` 10) * (get_digital n)) + (reverse_number (n `div` 10))


is_emirps :: Int -> Bool
is_emirps n    = (is_prime n) && (is_prime (reverse_number n)) && (n /= (reverse_number n))

get_count_emirps :: Bool -> Int
get_count_emirps True  = 1
get_count_emirps False = 0

count_emirps :: Int -> Int
count_emirps n
           | n < 13        = 0
           | n == 13       = 1
           | otherwise     = (get_count_emirps (is_emirps n)) + (count_emirps (n - 1))

-- Question 5:

biggest_sum :: [[Int]] -> [Int]
biggest_sum  (x:xs)
            | xs == []                         = x
            | (sum x) > (sum (biggest_sum xs)) = x
            | otherwise                        = biggest_sum xs

-- Question 6:

greatest :: (a -> Int) -> [a] -> a
greatest f (x:xs)
        | null xs                     = x
        | (f x) > (f (greatest f xs)) = x
        | otherwise                   = greatest f xs

-- Question 7:

is_bit :: Int -> Bool
is_bit n = (n == 1) || (n == 0)

-- Question 8:

flip_bit:: Int -> Int
flip_bit n
       | n == 1      = 0
       | n == 0      = 1
       | otherwise   = error "The number is not 1 or 0!"

-- Question 9:

is_bit_seq1:: [Int] -> Bool
is_bit_seq1 []          = True
is_bit_seq1 (x:xs)
           | null xs    = is_bit x
           | otherwise  = (is_bit x) && (is_bit_seq1 xs)

-- 9b:

is_bit_seq2:: [Int] -> Bool
is_bit_seq2 []     = True
is_bit_seq2 (x:xs) = (is_bit x) && (is_bit_seq2 xs)


-- 9c:
checkbit :: Int -> Bool
checkbit n = (n == 1) || (n == 0)

is_bit_seq3:: [Int] -> Bool
is_bit_seq3 []     = True
is_bit_seq3 x      = all is_bit x

-- Question 10:

invert_bits1:: [Int] -> [Int]
invert_bits1 []         = []
invert_bits1 (x:xs)
            | x == 1    = myappend [0] (invert_bits1 xs)
            | x == 0    = myappend [1] (invert_bits1 xs)
            | otherwise = error "The number is not 1 or 0!"

changebits:: Int -> Int
changebits x
         | x == 1    = 0
         | x == 0    = 1
         | otherwise = error "The number is not 1 or 0!"

-- 10b

invert_bits2:: [Int] -> [Int]
invert_bits2 x = map changebits x

-- 10c

invert_bits3:: [Int] -> [Int]
invert_bits3 x = [(changebits n) | n <- x]

-- Question 11:

pairsadd:: Int -> (Int, Int) -> (Int, Int)
pairsadd n (x,y)
       | n == 0    = (x + 1, y)
       | n == 1    = (x, y + 1)

bit_count:: [Int] -> (Int, Int)
bit_count []       = (0, 0)
bit_count (x:xs)
        | x == 0   = pairsadd 0 (bit_count xs)
        | x == 1   = pairsadd 1 (bit_count xs)

-- Question 12:

consall :: Int -> [[Int]] -> [[Int]]
consall n xs = map ((++) [n]) xs

all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n
                 | n < 1        = []
                 | n == 1       = [[1], [0]]
                 | otherwise    = (consall 0 (all_basic_bit_seqs (n - 1))) ++ (consall 1 (all_basic_bit_seqs (n - 1)))

-- Question 13:

data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList []     = Empty
toList (x:xs) = Cons x (toList xs)

-- Question 14:

toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons val rest) = myappend [val] (toHaskellList rest)

-- Question 15:

is_emptyList :: List a -> Bool
is_emptyList Empty           = True
is_emptyList (Cons val rest) = False


append :: List a -> List a -> List a
append (Cons val rest) (Empty) = (Cons val rest)
append (Empty) (Cons val rest) = (Cons val rest)
append (Cons val rest) (Cons val2 rest2)
               | is_emptyList rest   = Cons val (Cons val2 rest2)
               | otherwise           = Cons val (append rest (Cons val2 rest2))

-- Question 16:

removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty            = Empty
removeAll f (Cons val rest)
                | (f val)    = removeAll f rest
                | otherwise  = Cons val (removeAll f rest)

-- Question 17:

getfirstval :: List a -> a
getfirstval Empty              = error "Empty"
getfirstval (Cons val rest)    = val

restofList :: List a -> List a
restofList Empty               = Empty
restofList (Cons val rest)     = rest

length_ofList :: List a -> Int
length_ofList Empty           = 0
length_ofList (Cons val rest) = 1 + (length_ofList rest)

bubblesort :: Ord a => List a -> Int -> List a
bubblesort xs len
         | len == (length_ofList xs)    = xs
         | otherwise                 = bubblesort (sort_loop xs) (len + 1)

sort_loop :: Ord a => List a -> List a
sort_loop Empty                      = Empty
sort_loop (Cons val rest)
         | is_emptyList rest         = Cons val rest
         | val > (getfirstval rest)  = Cons (getfirstval rest) (sort_loop (Cons val (restofList rest)))
         | otherwise                 = Cons val (sort_loop rest)

sort :: Ord a => List a -> List a
sort Empty = Empty
sort xs = bubblesort xs 0
