{-
   This is the program for interactive maintaining datastore of strings.
   We can use three commands:
     1) Adding string to the store:

        add somestring
        
       Added string receives a numeric id.
     2) Extracting string from a store by its numeric id
   
        get id
        
     3) Finishing interaction:
   
        quit
   
   Extend this program to support storing values corresponded to some schema
   as defined at the lecture:
   1) add datatypes and utility functions for schema support
   2) update DataStore record for schema support
   3) correct compilation errors
   4) implement displaying values from the store according to the schema
   5) implement parsing input values according to the schema
   6) implement updating schema (only when the store is empty)
-}

module Main
import Data.Vect

record DataStore where
  constructor MkData
  size : Nat
  items : Vect size String

addToStore : String -> DataStore -> DataStore
addToStore newitem (MkData size store) = MkData _ (addToData store)
  where
   addToData : Vect oldsize String -> Vect (S oldsize) String
   addToData [] = [newitem]
   addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = 
  case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id (items store) ++ "\n", store)
      
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                             Nothing => Just ("Invalid command\n", store)
                             Just (Add item) =>
                             Just ("ID " ++ show (size store) ++ "\n", addToStore item store)
                             Just (Get pos) => getEntry pos store
                             Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
