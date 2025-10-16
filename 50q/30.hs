--intersect.hs
--resolução recursiva da função intersect

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) ys
  | x `elem` ys = x : intersect xs ys
  | otherwise   = intersect xs ys

