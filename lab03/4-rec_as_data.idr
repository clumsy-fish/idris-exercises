{-

Define datatype Person using general syntax and provide implementations
for accessors and updating functions:

record Person where
    constructor MkPerson
    firstName, middleName, lastName : String
    age : Int
    
-}
 
data Person  = MkPerson  String String String Int 

firstName : Person -> String
firstName p = let MkPerson firstN _  _  _ = p in  firstN 

middleName : Person -> String
middleName p = let MkPerson _ middleN   _  _ = p in middleN 
	
lastName : Person -> String
lastName p = let MkPerson  _  _  lastN  _ = p in  lastN 

age : Person -> Int
age p = let MkPerson  _  _  _  ag= p in  ag 

updateFirstN : Person -> String -> Person
updateFirstN p firstN = let MkPerson first middle last age = p in MkPerson firstN middle last age

updateMiddleN : Person -> String -> Person
updateMiddleN p middleN = let MkPerson first middle last age = p in MkPerson first middleN last age

updateLastN : Person -> String -> Person
updateLastN p lastN = let MkPerson first middle last age = p in MkPerson first middle lastN age

updateAge: Person -> Int -> Person
updateAge p ag = let MkPerson first middle last age = p in MkPerson first middle last ag

{-
tests:
 
  > :let p = MkPerson "John" "R." "Doe" 30
  > age p
  30
  > lastName p
  Doe : String
  > updateAge p 31
  MkPerson "John" "R." "Doe" 31 : Person
-}
