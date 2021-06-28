--MINAS ELEFTHERIOU AM:4245
-----------------------------------------------------------------------------------------

-- ASKHSH 1

statistics :: [(Int,Int)]->(Int,Int,Int,Int,Int)
statistics s = (matchesplayed s ,points s,goals_s s, goals_a s, goal_d s) 
matchesplayed ::[(Int,Int)]->Int
matchesplayed [] = 0
matchesplayed (h:t) = 1 + matchesplayed t
points :: [(Int,Int)]->Int
points [] = 0 
points ((h1,h2):t)
	| h1>h2 = 3 + points t
	| h1 == h2 = 1 + points t 
	| otherwise = points t 
goals_s :: [(Int,Int)]->Int
goals_s [] = 0
goals_s ((h1,h2):t) = h1 + goals_s t
goals_a :: [(Int,Int)]->Int
goals_a [] = 0
goals_a ((h1,h2):t) = h2 + goals_a t
goal_d :: [(Int,Int)]->Int
goal_d [] = 0
goal_d ((h1,h2):t) = find ((h1,h2):t) (h1-h2)
find :: [(Int,Int)]->Int->Int
find [] x = x
find ((h1,h2):t) x
    | (h1-h2) > x = find t (h1-h2)
    | otherwise = find t x

-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

partition :: String->[[String]]

partition w = [["-1821"]] 



