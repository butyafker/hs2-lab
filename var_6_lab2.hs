import Lab2
more [] [] = True
more [] y = True
more x [] = False
more x y 
	| (head x) == (head y) = more (tail x) (tail y)
	| otherwise = head x < head y
bub [] = []
bub (x:s) = sort x (bub s)
sort k [] = [k]
sort k list@(x:s) 
	| more (surname k) (surname x)   = k:list -- k < x
    | otherwise  = x:(sort k s)
main = do
putStrLn "—писок студентов, отсортированный по фамилии:"
mapM_ putStrLn $ map (\(pos, st) -> show pos ++ ") " ++ surname st ++ " " ++ name st) $ zip [1..] (bub students)