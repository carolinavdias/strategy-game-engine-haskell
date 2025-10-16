isPrime :: Int -> Bool
isPrime x = if x<= 2 then True
	 else isPrimeAux x 2

isPrimeAux n m = if m*m > n then True 
		else if mod n m == 0 then False 
		else isPrimeAux n (m+1) 
