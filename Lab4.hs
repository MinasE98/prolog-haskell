-----------------------------------------------------------------------------------------
--Minas Eleftheriou AM:4245
-- ASKHSH 1
move :: Eq u => [u]->u->Int->[u]
move s x n = []
-----------------------------------------------------------------------------------------
-- ASKHSH 2
combine :: [u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->[w]
combine s t f g h = find s t f g h 1
find :: [u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->Int->[w]
find [] s f g h i = []
find s [] f g h i = []
find (h1:t1) (h2:t2) f g h i
    | h i = (f h1 h2) : find t1 t2 f g h (i+1)
    | otherwise = (g h1 h2) : find t1 t2 f g h (i+1)


