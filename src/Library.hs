module Library
  ( addBook
  , availableBooks
  , borrowedBooks
  , addUser
  , listUsers
  , removeBook
  , removeUser
  , borrowBook
  , returnBook
  ) where

import Types

-- BOOKS

addBook :: String -> String -> LibraryState -> (LibraryState, BookId)
addBook t a state =
  let newBook = Book
        { bookId = nextBookId state
        , title  = t
        , author = a
        , status = Available
        }
      newState = state
        { books = newBook : books state
        , nextBookId = nextBookId state + 1
        }
  in (newState, bookId newBook)

availableBooks :: LibraryState -> [Book]
availableBooks state =
  filter (\b -> status b == Available) (books state)

borrowedBooks :: LibraryState -> [Book]
borrowedBooks state =
  filter (\b -> status b /= Available) (books state)

-- USERS

addUser :: String -> LibraryState -> (LibraryState, UserId)
addUser name state =
  let newUser = User
        { userId = nextUserId state
        , userName = name
        }
      newState = state
        { users = newUser : users state
        , nextUserId = nextUserId state + 1
        }
  in (newState, userId newUser)

listUsers :: LibraryState -> [User]
listUsers state = users state

-- PLACEHOLDERS (not implemented yet)

removeBook :: BookId -> LibraryState -> Either Error LibraryState
removeBook _ _ = error "removeBook not implemented yet"

removeUser :: UserId -> LibraryState -> Either Error LibraryState
removeUser _ _ = error "removeUser not implemented yet"

borrowBook :: UserId -> BookId -> LibraryState -> Either Error LibraryState
borrowBook _ _ _ = error "borrowBook not implemented yet"

returnBook :: UserId -> BookId -> LibraryState -> Either Error LibraryState
returnBook _ _ _ = error "returnBook not implemented yet"
