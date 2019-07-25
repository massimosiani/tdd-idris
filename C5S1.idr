module Main

-- Using do notation, write a printLonger program that reads two strings and then dis- plays the length of the longer string.
printLonger : IO ()
printLonger = do putStr "First string: "
                 s1 <- getLine
                 let len1 = length s1
                 putStr "Second string: "
                 s2 <- getLine
                 let len2 = length s2
                 case len1 > len2 of
                    True => putStrLn $ cast len1
                    False => putStrLn $ cast len2

printLongerBind : IO ()
printLongerBind = putStr "First string: " >>=
    \_ => getLine >>=
        \s1 => putStr "Second string: " >>=
            \_ => getLine >>=
        (\s2 => let len1 = length s1
                    len2 = length s2 in
            case len1 > len2 of
                True => putStrLn $ cast len1
                False => putStrLn $ cast len2
        )