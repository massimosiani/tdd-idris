module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : Nat -> (guesses: Nat) -> IO ()
guess target guesses = do
    putStrLn ("You have done " ++ (cast guesses) ++ " guesses")
    putStrLn "Guess the number: "
    Just x <- readNumber
        | Nothing => do
            putStrLn "Bad input"
            guess target (guesses + 1)
    case compare x target of
        LT => putStrLn "Too low" >>= \_ => guess target (guesses + 1)
        EQ => putStrLn "Good job!" >>= \_ => pure ()
        GT => putStrLn "Too high" >>= \_ => guess target (guesses + 1)

main : IO ()
main = do
    t <- time
    guess (cast t) 0
