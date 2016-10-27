data Tree a = Leaf | Node (Tree a) a (Tree a)

using (x: a, y: a, left: Tree a, right: Tree a)
data ElemTree : a -> Tree a -> Type where
        Here  : ElemTree x (Node left x right)
        Left  : ElemTree x left -> ElemTree x (Node left  y right)
        Right : ElemTree x right -> ElemTree x (Node left y right)

elemInTree : DecEq a => (x : a) -> (t : Tree a) -> Maybe (ElemTree x t)
elemInTree _ Leaf = Nothing
elemInTree x (Node left y right) with (decEq x y)
    elemInTree _ _                    | (Yes Refl) = Just Here
    elemInTree x (Node left y right)  | (No _) =
{-        case elemInTree x left of
            Just way => Just (Left way)
            Nothing => case elemInTree x right of
                Just way => Just (Right way)
                Nothing => Nothing
-}
        Left <$> elemInTree x left <|>
        Right <$> elemInTree x right


-- короче нужно было просто (Node left y right) взять в скобки и все заработало Ужас!!!!!!!!!!!!!!!!!!

tr : Tree Nat
tr = Node (Node Leaf 5 Leaf) 1 Leaf

--  еееееееееееееее работает
-- Примерчики?!?!
