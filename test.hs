import System.Exit


add :: Int -> Int -> Int
add x y = x+y


checkArgs :: String -> [String] -> IO ()
checkArgs _ ["-p", _]
    = return ()
checkArgs "Goat" [_,filename]
    = do { putStrLn ("Sorry, code cannot be generated yet\n")
        ; exitWith (ExitFailure 1)
        }
checkArgs progname _
    = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
        ; exitWith (ExitFailure 1)
        }



add :: Int -> Int -> Int -> Int
add x y z = x+y+z

