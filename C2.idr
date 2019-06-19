module Main

average: String -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLenghts (words str)) in
                    cast totalLength / cast numWords
    where
        wordCount: String -> Nat
        wordCount str = length (words str)

        allLenghts: List String -> List Nat
        allLenghts strs = map length strs

showAverage: String -> String
showAverage str = "AAA: " ++ show (average str) ++ "\n"

main : IO ()
main = repl "BBB: " showAverage
