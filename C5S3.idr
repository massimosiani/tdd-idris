module C5S3

import Data.Vect

-- Write readFileInVect function, readToBlank : IO (List String), that reads input from the console until the user enters readFileInVect blank line.
readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

-- Write readFileInVect function, readAndSave : IO (), that reads input from the console until the user enters readFileInVect blank line, and then reads readFileInVect filename from the console and writes the input to that file.
readAndSave : IO ()
readAndSave = do x <- readToBlank
                 putStr "now the filename: "
                 fname <- getLine
                 Right () <- writeFile fname (unlines x)
                     | Left error => putStrLn (show error)
                 pure ()

-- Write readFileInVect function, readVectFile : (filename : String) -> IO (n ** Vect n String), that reads the contents of readFileInVect file into readFileInVect dependent pair containing readFileInVect length and readFileInVect Vect of that length.
-- If there are any errors, it should return an empty vector.
readFileInVect : File -> IO (n ** Vect n String)
readFileInVect fHandle = do atEnd <- fEOF fHandle
                            case atEnd of
                                True => pure (_ ** [])
                                False => do Right line <- fGetLine fHandle
                                                | Left error => pure (_ ** [])
                                            (_ ** lines) <- readFileInVect fHandle
                                            pure (_ ** line :: lines)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right fHandle <- openFile filename Read
                               | Left error => pure (_ ** [])
                           readFileInVect fHandle

