module Main

{-
  Write a program which operates with some B-device executing user commands:
  1) Define data type UserCommand for commands transforming BDevice
  2) Define function for parsing user input: 
     parseCommand : String -> UserCommand
  3) Define function for processing user input
  4) Modify program in a way that supports operating several devices at once
-}

import BDevice

-- can be changed
defbdev : BDevice Maybe Additive
defbdev = defaultBDevice

main : IO ()
main = replWith defbdev "Enter command: " ?process
