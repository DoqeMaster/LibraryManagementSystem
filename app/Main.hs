module Main where

import Types (initialState, LibraryState)
import Commands (parseCommand, Command(..))

main :: IO ()
main = do
  putStrLn "Library Management System (type 'help' for commands)"
  loop initialState

loop :: LibraryState -> IO ()
loop state = do
  putStr "> "
  input <- getLine
  case parseCommand input of
    Nothing -> do
      putStrLn "Unknown command. Type 'help' for a list of commands."
      loop state
    Just Help -> do
      putStrLn "Commands: addBook, removeBook, addUser, removeUser, borrow, return, listBooks, listUsers, help, exit"
      loop state
    Just Exit -> do
      putStrLn "Goodbye!"
    Just _ -> do
      putStrLn "Command recognised (implementation coming next)."
      loop state