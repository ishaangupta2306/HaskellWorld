-- this is a single line commment--
-- myappend1 :: Eq a => [a] -> [a] -> [a]
myappend1 l1 l2 = 
    if l1 == []
        then
            l2
        else
            (head l1) : myappend1 (tail l1) l2

myappend2 :: (Eq a) => [a] -> [a] -> [a]
myappend2 = 
    \l1 l2 ->
        if l1 == []
            then
                l2
            else 
                (head l1): myappend2 (tail l1) l2


myappend3 [] l = l
myappend3 (h:t) l = h : myappend3 t l



squares l = 
    if l == []
        then
            []
        else
            (head l) * (head l) : squares (tail l)

squares2 :: (Eq a, Num a) => [a] -> [a]
squares2 = 
    \l ->
        if l == []
            then 
                []
            else
                (head l) * (head l) : squares2 (tail l)

squares3 [] = []
squares3 (h:t) = h * h : squares3 t


append_cps a b return = 
    if a == []
        then 
            (return b)
        else
            append_cps (tail a) b (\v -> return ((head a) : v))


append_cps2 [] b return = (return b)
append_cps2 (h:t) b return = append_cps2 t b (\v -> return (h : v))

split_cps [] return = return [] []
split_cps (h:t) return = split_cps t (\v1 v2 -> return (h:v2) v1)

ismember _ [] = False
ismember x (h:t)
    | x == h           = True 
    | otherwise        = ismember x t 

{- replaceall 0 9 [0,1,0,0,2]    => [9, 1, 9, 9, 2] -}
replaceall _ _ []   = []
replaceall a b (h:t)
    | a == h         = b : replaceall a b t
    | otherwise      = h: replaceall a b t

replaceall_cps _ _ [] return = (return [])
replaceall_cps a b (h:t) return
    | a == h    = replaceall_cps a b t (\v -> return (b : v))
    | otherwise = replaceall_cps a b t (\v -> return (h : v))


{- merge [1,2,4,5] [1,4,5,7]     => [1,1,2,4,4,5,5,7] -}
merge [] l1 = l1
merge l2 [] = l2
merge (h1:t1) (h2:t2) 
    | h1 > h2   = h2 : merge (h1:t1) t2
    | otherwise = h1 : merge t1 (h2:t2)

{- merge [1,2,4,5] [1,4,5,7]     => [1,1,2,4,4,5,5,7] -}
--merge_cps--
--merge_cps [3,4,7,8] [4,5,7,9] (\v -> v)--
merge_cps [] b return = return b
merge_cps b [] return = return b
merge_cps (h1:t1) (h2:t2) return
    | h1 > h2   = merge_cps (h1:t1) t2 (\v -> return (h2:v))
    | otherwise = merge_cps t1 (h2:t2) (\v -> return (h1:v))

--mergesort_cps [5,3,7,2,6,4,9] (\v -> v)
--[2,3,4,5,6,7,9]

mergesort_cps l return = mergesort_cps l (\v -> return (merge_cps (head (split_cps l (\a b -> [a,b]))) (head (tail (split_cps l (\a b -> [a,b])))) (\v -> v)))



{- reverse a list -}
myreverse [] = []
myreverse (h:t) = (myreverse t) ++ [h]

myreverse2 [] = []
myreverse2 (h:t) =  ((++) . myreverse2) t [h]

--f(g(x)) = (f . g) x
len [] = 0
len (h:t) = ((+) .(len)) t 1

-- Use merge_cps function and 
--the split_cps example from lecture to 
--create a cps version of mergesort that 
--takes a list and returns the list sorted.
--mergesort_cps [5,3,7,2,6,4,9] (\v -> v)

type Scale = Double
data Coordinate t = Zero | Coord1 t | Coord2 t t | Coord3 t t t deriving (Show)

getX Zero = 0
getX (Coord1 a) = a
getX (Coord2 a b) = a
getX (Coord3 a b c) = a

getY Zero = 0
getY (Coord1 a) = 0
getY (Coord2 a b) = b
getY (Coord3 a b c) = b

getZ Zero = 0
getZ (Coord1 a) = 0
getZ (Coord2 a b) = 0
getZ (Coord3 a b c) = c

(-|-) (Coord1 a) (Coord2  c d) = (Coord3(a+c) d 0)

instance (Floating t, Eq t) => Num (Coordinate t) where
    c1 + c2 = (-|-) c1 c2


data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving(Show)

height Empty = 0
height (Leaf t) = 1
-- heightL (InnerNode a l r) = 1 + (heightL l)
-- heightR (InnerNode a l r) = 1 + (heightR r)
height (InnerNode a l r) 
    | (1 + (height l) ) > (1 + (height r))        = 1 + (height l)
    | otherwise                                   = 1 + (height r)

--addToRight 10 (InnerNode 1 (InnerNode 2 (Leaf 3) (Leaf 4)) (Leaf 5))
addToRight v Empty = (Leaf v)
addToRight v (Leaf a) = (InnerNode a Empty (Leaf v))
addToRight v (InnerNode a l r) = (InnerNode a l (addToRight v r))

--applytotree (\v -> v*v (InnerNode 1 (InnerNode 2 (Leaf 3) (Leaf 4)) (Leaf 5))
applytotree _ Empty = Empty
applytotree f (Leaf v) = (Leaf (f v))
applytotree f (InnerNode a l r) = InnerNode (f a) (applytotree f l ) (applytotree f r )

data Value t = Value t | NoValue deriving (Show)
myreturn t = Value t
mybind (Value x) f = (f x)
mybind NoValue _ = NoValue

(+++) vx vy = mybind vx (\x -> mybind vy (\y -> myreturn (y + x)))
(///) vx vy = mybind vx (\x -> mybind vy (\y -> if y == 0 then NoValue else myreturn (x / y)))

(~~~~) mx my = do    
     x <- mx    
     y <- my    
     return (x - y)

--- mapply (Just [1,2,3,4,5]) (myappend) (Just [6,7,8])   ==>  Just 
mapply mx my = do
    x <- mx
    y <- my
    return (x (++) y)

-- vsqrt mb = mybind mb (\a -> if a < 0 then NoValue else myreturn (sqrt a))

-- msqrt mb = do
--     b <- mb
--     if b < 0 then Nothing else return (sqrt b)

(++++) mx my = mx >>= (\x -> my >>= (\y -> return (x + y)))

vqrt mx = do    
    x <- mx    
    if x < 0 then Nothing else return (sqrt x)

mapply2 mx f my = do
    x <- mx
    y <- my
    return (f x y)

is_inorder [] = Just []
is_inorder [a] = Just [a]
is_inorder (a:(b:t)) 
    | a <= b        =  (is_inorder (b:t)) >>= (\v -> return (a:v) )
    | otherwise     = Nothing


-- Create a Haskell type that represents functions of a single variable. 
-- You can assume the variable is always x or always z or some letter of your choice.  
-- For this question, we will limit our functions to: integer constants, a single variable, 
-- the summation of two functions, the product of two functions, and a function raised to an integer power.
-- data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving(Show)

data Function f =  Integer f | SingleVariable f | Sum f f | Product f f  | Exponent f Integer deriving (Show)

getFunctionValue (Integer v) = v
getFunctionValue (SingleVariable v) = v
getFunctionValue (Sum v f) = v+f
getFunctionValue (Product v f) = v*f
getFunctionValue ( Exponent f v) = f^v


-- value should take a function and an integer value as input and give the value of the 
-- input function at that integer.  For example value(x3, 2) should output 8
value (Integer f) i = f
value (SingleVariable f) i = i
value (Sum f g) i = getFunctionValue(f g)
value (Product f g) i = getFunctionValue(f g) 
