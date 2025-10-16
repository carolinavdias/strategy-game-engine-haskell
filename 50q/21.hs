isPrime :: Int -> Bool
isPrime x = if x == 2 then True
            else if x < 2 then False
            else isPrimeAux x 2

isPrimeAux :: Int -> Int -> Bool
isPrimeAux n m = if m*m > n then True 
                 else if mod n m == 0 then False 
                 else isPrimeAux n (m+1)

teste_isP :: [Bool]
teste_isP = [
    isPrime 27 == False,   
    isPrime 3 == True,     
    isPrime 5 == True,   
    isPrime 8 == False,    
    isPrime 144 == False   
    ]
