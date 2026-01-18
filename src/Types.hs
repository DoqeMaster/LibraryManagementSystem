module Types where

type BookId = Int
type UserId = Int

data BookStatus
  = Available
  | Borrowed UserId
  deriving (Eq, Show)

data Book = Book
  { bookId :: BookId
  , title  :: String
  , author :: String
  , status :: BookStatus
  }
  deriving (Eq, Show)

data User = User
  { userId   :: UserId
  , userName :: String
  }
  deriving (Eq, Show)

data LibraryState = LibraryState
  { books      :: [Book]
  , users      :: [User]
  , nextBookId :: BookId
  , nextUserId :: UserId
  }
  deriving (Eq, Show)

data Error
  = BookNotFound
  | UserNotFound
  | BookAlreadyBorrowed
  | BookNotBorrowed
  | BorrowedByDifferentUser
  | CannotRemoveBorrowedBook
  | CannotRemoveUserWithBorrowedBooks
  | InvalidInput
  deriving (Eq, Show)

initialState :: LibraryState
initialState = LibraryState
  { books = []
  , users = []
  , nextBookId = 1
  , nextUserId = 1
  }