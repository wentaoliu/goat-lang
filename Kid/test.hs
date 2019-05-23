import System.Exit


data MyData = Program [Int] | Nogram

myFunc :: MyData -> Int
myFunc Nogram = 0
myFunc (Program []) = 0
myFunc (Program (x:xs)) = x + myFunc (Program xs)

checkArgs :: String -> [String] -> IO ()
checkArgs "Goat" ["-p",filename]
    = return ()
checkArgs "Goat" [filename]
    = do { putStrLn ("Sorry, code cannot be generated yet\n")
        ; exitWith (ExitFailure 1)
        }
checkArgs "Goat" _
    = do { putStrLn ("Usage: " ++ "Goat" ++ " filename\n\n")
        ; exitWith (ExitFailure 1)
        }



add :: Int -> Int -> Int -> Int
add x y z = x+y+z

arrayF :: [String] -> String
arrayF [] = ""
arrayF (x:xs) = x ++ arrayF xs


