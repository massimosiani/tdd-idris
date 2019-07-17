data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

total insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

data BSTree : Type -> Type where
  BSEmpty : Ord elem => BSTree elem
  BSNode : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

total bsinsert : elem -> BSTree elem -> BSTree elem
bsinsert x BSEmpty = BSNode BSEmpty x BSEmpty
bsinsert x orig@(BSNode left val right) = case compare x val of
                                               LT => BSNode (bsinsert x left) val right
                                               EQ => orig
                                               GT => BSNode left val (bsinsert x right)

total listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

total treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node tree x tree1) = (treeToList tree) ++ (x :: (treeToList tree1))

data Expr : Type where
     Val : Int -> Expr
     Add : Expr -> Expr -> Expr
     Sub : Expr -> Expr -> Expr
     Mult : Expr -> Expr -> Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)
