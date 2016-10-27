-- 1) Doubling given number (add implementation)
double : Num a => a -> a
double a = a*2

-- 2) Should return True if only both arguments are True
bothTrue : Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _  _ = False

-- 3) Should return True if only one of its arguments is True
oneTrue : Bool -> Bool -> Bool
oneTrue True _ = True
oneTrue _ True  = True
oneTrue _ _ = False

-- 4) Sign of a number
sign : (Num a, Ord a) => a -> Int
sign a = if a > 0 then 1 else -1

-- 5) Maximum of three arguments
max3 : Ord a => a -> a -> a -> a
max3 x y  z = max x (max y z)

-- 6) Fahrenheit to Celsius conversion
f2c : Double -> Double
f2c  x = (x - 32)*5/9

-- 7) Temperature description (like "cold", "warm" or "hot")
describeTemperature : Double -> String
describeTemperature x = if x < 0 then "cold" else if x > 100 then "hot" else "warm"

-- 8) Square of circle with given radius
square : Double -> Double 
square r = pi * r * r

-- 9) Numbers of days in a year
nDays' : Int -> Int
nDays' year = if ( isLeap year ) then 366 else 365 where
    isLeap : Int -> Bool
    isLeap x= if ( mod x 4  == 0) then True else False 
    
-- 10) Write a function palindrome , which returns whether the input reads
--     the same backwards as forwards. (Hint: You may find the
--     function reverse : String -> String useful.)

palindrome : String -> Bool
palindrome str = str == reverse str
