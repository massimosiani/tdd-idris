data Expr num = Val num
    | Add (Expr num) (Expr num)
    | Sub (Expr num) (Expr num)
    | Mul (Expr num) (Expr num)
    | Div (Expr num) (Expr num)
    | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger
Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
Abs ty => Abs (Expr ty) where
    abs = Abs

Show ty => Show (Expr ty) where
    show (Val x) = show x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (Abs x) = "|" ++ show x ++ "|"

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)


data Vect: Nat -> Type -> Type where
    Nil: Vect 0 a
    (::): (x : a) -> (xs : Vect k a) -> Vect (S k) a

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && (xs == ys)

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)
    