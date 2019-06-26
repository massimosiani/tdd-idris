import Data.Vect

total createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

total transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

total transposeHelper2 : Vect n elem -> Vect n (Vect len elem) -> Vect n (Vect (S len) elem)
transposeHelper2 = zipWith (::)

total transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper2 x xsTrans

total addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys

innerProd : Num a => Vect m a -> Vect m a -> a
innerProd xs ys = sum (zipWith (*) xs ys)

multMatrix_rhs_2 : Num a => Vect m a -> Vect len (Vect m a) -> Vect p (Vect m a) -> Vect (S len) (Vect p a)
multMatrix_rhs_2 x xs [] = createEmpties
multMatrix_rhs_2 x xs (y :: ys) = (innerProd x y :: ?multMatrix_rhs_2_rhs_4) :: ?multMatrix_rhs_2_rhs_5

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] ys = []
multMatrix (x :: xs) ys = let ysTrans = transposeMat ys in
                          multMatrix_rhs_2 x xs ysTrans
