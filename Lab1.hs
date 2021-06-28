
-----------------------------------------------------------------------------------------
--Minas Eleftheriou AM:4245
-- ASKHSH 1


grade :: Int->Int->Int                           

grade a b = if a>=0 && a<=100 && b >=0 && b<=20 then result a b ((8*a) `div` 10 + b )
			else -1
					
result :: Int->Int->Int->Int
result a b c 
			| c>100 || c<0 = -1
			| c>47 && a<47 = 47
			| c>47 && a>47 && c<50 = 50
			|otherwise = c






-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

digits :: Int->Int->Int                         

digits x y 
		  | x<10000000 || x>99999999 || y<10000000 || y>99999999 = -1                                  
		  |(check x y < 2) = 0
		  |(check x y == 2) = 1
		  |(check x y == 3) = 5
		  |(check x y == 4) = 20
		  |(check x y == 5)= 300
		  |(check x y == 6) = 8000
		  |(check x y == 7) = 100000
		  |otherwise = 1000000
check :: Int->Int->Int
check 0 0 = 0
check x y = if  ((x`mod`10) == (y`mod`10)) then 1+(check (x `div` 10)(y `div` 10))
			else check (x`div`10) (y`div`10)



