module Main where

import Test.HUnit
import Test.QuickCheck
import Types
import Library

testAddUser :: Test
testAddUser =
  TestCase $
    let (st1, uid) = addUser "Alice" initialState
    in assertEqual "User ID should be 1" 1 uid

testAddBook :: Test
testAddBook =
  TestCase $
    let (st1, bid) = addBook "1984" "Orwell" initialState
    in assertEqual "Book ID should be 1" 1 bid

hunitTests :: Test
hunitTests = TestList
  [ TestLabel "addUser" testAddUser
  , TestLabel "addBook" testAddBook
  ]

prop_borrowReturnRestoresAvailable :: String -> String -> String -> Bool
prop_borrowReturnRestoresAvailable userName title author =
  let (st1, uid) = addUser userName initialState
      (st2, bid) = addBook title author st1
      Right st3  = borrowBook uid bid st2
      Right st4  = returnBook uid bid st3
  in all (\b -> status b == Available) (availableBooks st4)

main :: IO ()
main = do
  putStrLn "Running HUnit tests..."
  _ <- runTestTT hunitTests

  putStrLn "Running QuickCheck properties..."
  quickCheck prop_borrowReturnRestoresAvailable
