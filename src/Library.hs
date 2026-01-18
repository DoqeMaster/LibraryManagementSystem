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

findBook :: BookId -> [Book] -> Maybe Book
findBook bid = foldr (\b acc -> if bookId b == bid then Just b else acc) Nothing

findUser :: UserId -> [User] -> Maybe User
findUser uid = foldr (\u acc -> if userId u == uid then Just u else acc) Nothing

updateBook :: Book -> [Book] -> [Book]
updateBook updated =
  map (\b -> if bookId b == bookId updated then updated else b)

userHasBorrowedBooks :: UserId -> [Book] -> Bool
userHasBorrowedBooks uid =
  any (\b -> status b == Borrowed uid)

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

removeBook :: BookId -> LibraryState -> Either Error LibraryState
removeBook bid state =
  case findBook bid (books state) of
    Nothing -> Left BookNotFound
    Just book ->
      case status book of
        Available ->
          Right state { books = filter (\b -> bookId b /= bid) (books state) }
        Borrowed _ -> Left CannotRemoveBorrowedBook

removeUser :: UserId -> LibraryState -> Either Error LibraryState
removeUser uid state =
  case findUser uid (users state) of
    Nothing -> Left UserNotFound
    Just _ ->
      if userHasBorrowedBooks uid (books state)
        then Left CannotRemoveUserWithBorrowedBooks
        else Right state { users = filter (\u -> userId u /= uid) (users state) }

borrowBook :: UserId -> BookId -> LibraryState -> Either Error LibraryState
borrowBook uid bid state =
  case findUser uid (users state) of
    Nothing -> Left UserNotFound
    Just _ ->
      case findBook bid (books state) of
        Nothing -> Left BookNotFound
        Just book ->
          case status book of
            Available ->
              let updated = book { status = Borrowed uid }
              in Right state { books = updateBook updated (books state) }
            Borrowed _ -> Left BookAlreadyBorrowed

returnBook :: UserId -> BookId -> LibraryState -> Either Error LibraryState
returnBook uid bid state =
  case findBook bid (books state) of
    Nothing -> Left BookNotFound
    Just book ->
      case status book of
        Available -> Left BookNotBorrowed
        Borrowed borrowerId ->
          if borrowerId == uid
            then
              let updated = book { status = Available }
              in Right state { books = updateBook updated (books state) }
            else Left BorrowedByDifferentUser
