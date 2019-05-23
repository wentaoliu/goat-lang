import System.Environment

pmat :: [String] -> String
pmat [file] = file
pmat [arg, file] = arg ++ ", " ++ file
