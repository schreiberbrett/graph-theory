unriffle :: [a] -> [([a], [a])]
unriffle [] = [([], [])]
unriffle (x:xs) = do
    (l, r) <- unriffle xs
    result <- [(x:l, r), (l, x:r)]
    return result
data Expr a =
	Val a |
	Op (Expr a) (Expr a) deriving (Eq, Ord, Show)


solve :: Set a -> (a -> a -> a) -> a -> Maybe (Expr a)
solve q op y = undefined			

