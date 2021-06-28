-----------------------------------------------------------------------------------------
--MINAS ELEFTHERIOU AM:4245
-- ASKHSH 1
ab :: Int->(Int,Int) 
ab n = find n 1 1
find :: Int->Int->Int->(Int,Int)
find a b c
   | b*c > a = find a 1 (c-b)
   | b*c == a = (c,b)
   | otherwise = find a (b+1) (c+1)
-----------------------------------------------------------------------------------------
     
-- ASKHSH 2
sum2021 :: Integer->Integer->Integer             
sum2021 m n = calculate m n m
calculate :: Integer->Integer->Integer->Integer
calculate m n i 
		|i > n = 0
		|otherwise = (n+i)^m + calculate m n (i+1)