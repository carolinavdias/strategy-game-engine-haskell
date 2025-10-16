--intersect.hs
--resolução recursiva da função intersect

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) ys
  | x `elem` ys = x : intersect xs ys
  | otherwise   = intersect xs ys

teste :: [Bool]
teste = [
	intersect [1,1,2,3,4] [1,3,5] == [1,1,3],
	intersect [1,3,5,6] [2,4,6] == [6],
	intersect [4,5,6] [5,5,7] == [5],
	intersect [] [1,2,3,4] == [],
	intersect [3,8,9] [] == []
    ]
