module Main

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : Nat -> IO ()
guess target = do
    putStrLn "Guess the number: "
    Just x <- readNumber
        | Nothing => do
            putStrLn "Bad input"
            guess target
    case compare x target of
        LT => do
            putStrLn "Too low"
            guess target
        EQ => do
            putStrLn "Good job!"
            pure ()
        GT => do
            putStrLn "Too high"
            guess target
