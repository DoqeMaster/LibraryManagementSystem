module Main where

import Types (initialState, LibraryState(..), User(..), Book(..), BookStatus(..), Error(..))
import Commands (parseCommand, Command(..))
import Library
  ( addBook
  , availableBooks
  , borrowedBooks
  , addUser
  , listUsers
  , removeBook
  , removeUser
  , borrowBook
  , returnBook
  )
import Data.List (find)
import Text.Read (readMaybe)
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
      mapM_ (putStrLn . formatBook) (availableBooks state)
      putStrLn "Borrowed books:"
      mapM_ (putStrLn . formatBorrowedBook state) (borrowedBooks state)
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

    Just RemoveBook -> do
      mBookId <- promptInt "Book ID: "
      case mBookId of
        Nothing -> do
          putStrLn (renderError InvalidInput)
          loop state
        Just bid ->
          case removeBook bid state of
            Left err -> do
              putStrLn (renderError err)
              loop state
            Right newState -> do
              putStrLn "Book removed."
              loop newState

    Just RemoveUser -> do
      mUserId <- promptInt "User ID: "
      case mUserId of
        Nothing -> do
          putStrLn (renderError InvalidInput)
          loop state
        Just uid ->
          case removeUser uid state of
            Left err -> do
              putStrLn (renderError err)
              loop state
            Right newState -> do
              putStrLn "User removed."
              loop newState

    Just Borrow -> do
      mUserId <- promptInt "User ID: "
      mBookId <- promptInt "Book ID: "
      case (mUserId, mBookId) of
        (Just uid, Just bid) ->
          case borrowBook uid bid state of
            Left err -> do
              putStrLn (renderError err)
              loop state
            Right newState -> do
              putStrLn "Book borrowed."
              loop newState
        _ -> do
          putStrLn (renderError InvalidInput)
          loop state

    Just Return -> do
      mUserId <- promptInt "User ID: "
      mBookId <- promptInt "Book ID: "
      case (mUserId, mBookId) of
        (Just uid, Just bid) ->
          case returnBook uid bid state of
            Left err -> do
              putStrLn (renderError err)
              loop state
            Right newState -> do
              putStrLn "Book returned."
              loop newState
        _ -> do
          putStrLn (renderError InvalidInput)
          loop state

    Just _ -> do
      putStrLn "Command recognised (implementation coming next)."
      loop state
      
promptInt :: String -> IO (Maybe Int)
promptInt label = do
  putStr label
  input <- getLine
  pure (readMaybe input)

formatBook :: Book -> String
formatBook book =
  show (bookId book) ++ ": " ++ title book ++ " by " ++ author book

formatBorrowedBook :: LibraryState -> Book -> String
formatBorrowedBook state book =
  case status book of
    Borrowed uid ->
      let nameSuffix = case find (\u -> userId u == uid) (users state) of
            Just user -> " (" ++ userName user ++ ")"
            Nothing -> ""
      in formatBook book ++ " - borrowed by user " ++ show uid ++ nameSuffix
    Available -> formatBook book

renderError :: Error -> String
renderError err =
  case err of
    BookNotFound -> "Error: Book not found."
    UserNotFound -> "Error: User not found."
    BookAlreadyBorrowed -> "Error: Book is already borrowed."
    BookNotBorrowed -> "Error: Book is not currently borrowed."
    BorrowedByDifferentUser -> "Error: Book was borrowed by a different user."
    CannotRemoveBorrowedBook -> "Error: Cannot remove a borrowed book."
    CannotRemoveUserWithBorrowedBooks -> "Error: Cannot remove user with borrowed books."
    InvalidInput -> "Error: Invalid input."