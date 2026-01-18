module Commands
  ( Command(..)
  , parseCommand
  ) where

data Command
  = AddBook
  | RemoveBook
  | AddUser
  | RemoveUser
  | Borrow
  | Return
  | ListBooks
  | ListUsers
  | Help
  | Exit
  deriving (Eq, Show)

parseCommand :: String -> Maybe Command
parseCommand input =
  case input of
    "addBook"    -> Just AddBook
    "removeBook" -> Just RemoveBook
    "addUser"    -> Just AddUser
    "removeUser" -> Just RemoveUser
    "borrow"     -> Just Borrow
    "return"     -> Just Return
    "listBooks"  -> Just ListBooks
    "listUsers"  -> Just ListUsers
    "help"       -> Just Help
    "exit"       -> Just Exit
    _            -> Nothing