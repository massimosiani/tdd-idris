import Data.Vect

insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

total insSort: Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

total my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

total my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

total my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
