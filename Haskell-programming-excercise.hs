--Author: Ishaan Gupta--
{--Takes two vectors (lists of numbers) and computes the dot product of the vectors --}
dotproduct a b = dotproduct_cps a b (\v -> v)
dotproduct_cps [] a return = return 0
dotproduct_cps a [] return = return 0
dotproduct_cps (h1:t1) (h2:t2) return = dotproduct_cps t1 t2 (\v -> return ((h1 * h2) + v)) 

{--Takes a mapping variable, list of numbers and Continuation function 
and returns the modified list according to mapping variable --}
mymap _ [] return = return []
mymap f (h:t) return = mymap f t (\v -> return ((f h) : v))

 
{-- Takes a row vector (a list of numbers) and matrix and CPS (a list of lists of numbers)
    and multiplies the vector times the matrix. --}
vectormult_cps v m return
  | head m == []   = return []
  | otherwise      = dotproduct_cps v (mymap head m (\v1 -> v1)) (\v2 -> vectormult_cps v (mymap tail m (\v3 -> v3))  (\v4 -> return (v2:v4)))

{-- Takes a row vector (a list of numbers) and matrix and and calls the helper method to perform the multiplication. --}
vectormult v m = vectormult_cps v m (\v -> v)

{-- Takes two matrices (a list of lists of numbers) and CPS and multiplies them. --}
matrixmultiply_cps [] b return = (return [])
matrixmultiply_cps (h:t) b return = vectormult_cps h b (\v1 -> matrixmultiply_cps t b (\v2 -> return (v1:v2)))

matrixmultiply a b = matrixmultiply_cps a b (\v -> v)

{--- Takes a BinaryTree as input. 
    If the element stored in the root is larger than either children, swap the element 
    with the smaller child, and recurse on the child you swapped the element with. 
    The recursion stops when either you reach a leaf or 
    when the element of the node is smaller than both its children. --}
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Show)
bubbledown Empty = Empty
bubbledown (Leaf a) = (Leaf a)
-- bubbledown (InnerNode a l r) 
--   | a > (head l)         = InnerNode a l r
--   | otherwise            = InnerNode a l r 


{-- Type that allows nested list. Takes two kinds of values: elements and sublists.--}
data NestedList t = Element t | SubList [NestedList t] deriving (Show, Eq)

{--Takes a list and returns a list with just the elements.--}
flatten [] = []
flatten ((Element e):sl) = Element e: flatten sl
flatten ((SubList sl): ssl) = flatten sl ++ flatten ssl

{--Helper Method to check if a string is a number or not--}
isDigit x 
  | (x == '1' || x == '2'  || x == '3'|| x == '4'|| x == '5'|| x == '6'|| x == '7'|| x == '8'|| x == '9'|| x == '0')   = True
  | otherwise                                                                                                          = False

{--Helper Method to convert a String to a list using CPS--}
string2list_cps [] return = (return [])
string2list_cps (h:t) return
  | (isDigit h)             = string2list_cps t (\v -> return ((Element h):v))
  | h == '('                = string2list_cps t (\v -> return ((SubList []):v))
  | otherwise               = string2list_cps t return

{--Takes a string containing single digits and parentheses and create a list.--}
string2list ls = string2list_cps ls (\v -> v)


  
-- yourfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
yourfunction v ml f = do  
  l <- ml
  if f v then return (v:l) else Nothing

{--Helper method that takes a list and a function and CPS and returns Nothing if the elements in the list fail to past the function 
  and the list (embedded in a Maybe) if all the elements pass.--}
checklist_cps [] _ return = Just (return [])
checklist_cps (h:t) f return
  |yourfunction h (Just []) f /= Nothing = checklist_cps t f (\v -> return (h:v))
  |otherwise                            = Nothing

{--Helper method that takes a list and a function and CPS and returns Nothing if the elements in the list fail to past the function 
  and the list (embedded in a Maybe) if all the elements pass.--}  
checklist l f = checklist_cps l f (\v -> v) 


{-- Helper method that takes a nested list & CPS and returns a Maybe of the list if sublists are in non-decreasing order from left to right, 
  otherwise return Nothing --}
inorder_cps [] return  =  return []
inorder_cps [a] return  = return [a]
inorder_cps (a: (b:t)) return 
  | a <= b                = inorder_cps (b:t) (\v -> return (a:v)) 
  | a > b                 = Nothing
  | otherwise             = inorder_cps (b:t) (\v -> return (a:v)) 


{-- Takes a nested list of numbers and returns a Maybe of the list if sublists are in non-decreasing order from left to right, 
  otherwise return Nothing --}  
inorder ls = inorder_cps (head ls) (\mv -> (inorder_cps (tail ls) (\ma -> if (head (tail mv)) <= (head (head ma)) then return (mv:ma) else Nothing)))


 {-- List monad that generalizes a list. --}
data List t = Null | Pair t (List t) deriving (Show)
lreturn h = (Pair h)  
lbind (Pair a b) f = (f a) (lbind b f)
lbind Null _ = Null

