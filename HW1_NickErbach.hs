module HW1_NickErbach where

-- 1: Write a function count that counts the number of occurrences of a character in a string
count n [] = 0
count n s = (if (last s == n) then 1 else 0) + count n (take (length s - 1) s)

-- 2: Create a function that reverses a list.
--  Put the last element of the list at the front and then call the function on the rest of the list.
rev [] = []
rev l = last l : rev (take (length l - 1) l)

-- 3: create a function that sorts a list.
--  This is selection sort.
--  Put the minimum value at the beginning, sort the rest of the list.
sort [] = []
sort l = minimum l : sort ([x | x <- l, x /= minimum l])


--4: polynomial
type Polynomial = [Double]

--4a: Evaluate a polynomial.
-- Take the last value in the list, and multiply it by x^len(list)-1.
-- So with a list of [1,2,3], recursive step would return (3x^2) + ev([1,2])
-- An empty list evaluates to 0 to create the base case.
ev [] x = 0
ev p x = (x ^ (length p - 1)) * last p + ev (take (length p - 1) p) x

--4b: Add 2 polynomials.
-- Add the first element of the lists, then call the function on the rest of the lists.
-- With lists [1,2,3] and [4,5,6], this would return 5 : addP ([2,3], [5,6])
-- 2 empty lists return an empty list for the base case.
addP [] [] = []
addP u w = (head u + head w) : addP (drop 1 u) (drop 1 w)

--4c: Scale a polynomial.
-- Multiply the first element by the scale, then call the function on the rest of the lists.
-- With a list [1,2,3] and scale 2, return 2 * 1 : [2,3]
-- Empty list, no matter the scale, returns an empty list for the base case.
scale x [] = []
scale x p = head p * x : scale x (drop 1 p)
--4d: Multiply polynomials
multP :: Polynomial -> Polynomial -> Polynomial

-- Append 0 to front of polynomial
multiplyByX p = 0:p

-- Ex: x = [1,2,3] and y = [4,5,6]
-- Evaluate scale 1 y and add that to multP [0,2,3] [4,5,6], repeat
-- recursively
multP [] [] = []
multP x [] = []
multP [] y = []
multP (x1:xs) y = let x1TimesY = scale x1 y
thatTimesX = multiplyByX $ multP xs y
in addP x1TimesY thatTimesX

--4e: Derive a polynomial.
-- Take the last value and multiply it by the len(list)-1. This is because of the power rule.
-- Append that value to the end of the function run on the rest of the list.
-- With a list [1,2,3], this returns der([1,2]) : 6
-- A list of length 1 evaluates to empty to create the base case. This is because the derivative of a constant is 0.
der [x] = []
der p = der (take (length p - 1) p) ++  [(last p * (length p - 1))]
--5
-- factRec 4*0 = 1
--factRec 4*(factRec(3))
-- factRec 4* 3*factRec2
--4*3*2*factRec(1)
--4*3*2*1*factRec(0)
--4*3*2*1*1 = 24
--This makes the most sense because normally when we calculate factorial
--we stop at 1, but in this example we use the base case 0 which makes more sense logically
