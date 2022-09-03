unriffle :: [a] -> [([a], [a])]
unriffle [] = [([], [])]
unriffle (x:xs) = do
    (l, r) <- unriffle xs
    result <- [(x:l, r), (l, x:r)]
    return result

