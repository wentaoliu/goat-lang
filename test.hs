import System.Exit



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

