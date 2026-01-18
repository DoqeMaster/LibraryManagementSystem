module Library
  ( addBook
  , removeBook
  , addUser
  , removeUser
  , borrowBook
  , returnBook
  , availableBooks
  , borrowedBooks
  , listUsers
  ) where

import Types

addBook :: String -> String -> LibraryState -> (LibraryState, BookId)
addBook = undefined

removeBook :: BookId -> LibraryState -> Either Error LibraryState
removeBook = undefined

addUser :: String -> LibraryState -> (LibraryState, UserId)
addUser = undefined

removeUser :: UserId -> LibraryState -> Either Error LibraryState
removeUser = undefined

borrowBook :: UserId -> BookId -> LibraryState -> Either Error LibraryState
borrowBook = undefined

returnBook :: UserId -> BookId -> LibraryState -> Either Error LibraryState
returnBook = undefined

availableBooks :: LibraryState -> [Book]
availableBooks = undefined

borrowedBooks :: LibraryState -> [Book]
borrowedBooks = undefined

listUsers :: LibraryState -> [User]
listUsers = undefined