module Main

import Data.Vect

-- Define a type synonym: Matrix : Nat -> Nat -> Type.
Matrix : Nat -> Nat -> Type
Matrix x y = Vect x (Vect y Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

-- implement a vector as nested pairs, with the nesting calculated from the length
TupleVect 0 ty = ()
TupleVect 1 ty = (ty, ())
TupleVect 2 ty = (ty, (ty, ()))

TupleVect : Nat -> Type -> TuplerType 