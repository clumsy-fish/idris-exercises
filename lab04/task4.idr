data Tree : Type -> Type where
  Empty : Tree elem
  Node : (left : Tree elem)
                      -> (val : elem)
                      -> (right : Tree elem) -> Tree elem


insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left y right) = case compare x y of
                LT => Node (insert x left) y right
                EQ => Node left y right
                GT => Node left y (insert x right)

||| Inserts elements of a list into a binary search tree


listToTree : Ord a => List a -> Tree a
listToTree Nil = Empty
listToTree (y::ys) = insert x (listToTree xs) where 
	x = last (y::ys)
	xs = init (y::ys)

treeToList : Tree a -> List a
treeToList Empty = Nil
treeToList (Node left x right) =  x :: (treeToList left) ++ (treeToList right )

tr : Tree Nat
tr = Node (Node  Empty 1 (Node  Empty 3  Empty)) 5 Empty

--ура!!!!
