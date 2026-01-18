module Main where

import Types (initialState, LibraryState)
import Commands (parseCommand, Command(..))
import Library (addBook, availableBooks, addUser, listUsers)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
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

    Just AddBook -> do
      putStr "Title: "
      t <- getLine
      putStr "Author: "
      a <- getLine
      let (newState, newId) = addBook t a state
      putStrLn ("Book added with ID " ++ show newId)
      loop newState

    Just ListBooks -> do
      putStrLn "Available books:"
      mapM_ print (availableBooks state)
      loop state

    Just AddUser -> do
      putStr "User name: "
      name <- getLine
      let (newState, newId) = addUser name state
      putStrLn ("User added with ID " ++ show newId)
      loop newState

    Just ListUsers -> do
      putStrLn "Registered users:"
      mapM_ print (listUsers state)
      loop state

    Just _ -> do
      putStrLn "Command recognised (implementation coming next)."
      loop state
