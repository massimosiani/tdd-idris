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
