generateProgram :: String -> String -> String -> String
generateProgram header decl stmt 
    = "proc" ++ "(" ++ header ++ ")" ++ decl ++ "begin" ++ body ++ "end"

genHeader :: [String] -> String
genHeader (arg:[]) = arg
genHeader (arg:args) = arg ++ ", " ++ genHeader args

genParameter :: String -> String -> String -> String
genParameter passing idType ident = passing ++ " " ++ pType ++ " " ++ ident

combineDecl :: [String] -> String 
combineDecl [] = ""
combineDecl (arg:args) = arg ++ "\n" ++ genHeader args

genDecl :: String -> String -> String
genDecl idType arrayMod ident = idType  ++ " " ++ ident ++ arrayMod ++ ";"

genArrayMod :: [Int] -> String
genArrayMod [] = ""
genArrayMod (index:indicies) = "[" ++ index ++ "]" ++ genArrayMod indicies

genBody :: [String] -> String 
genBody [] = ""
genBody (stmt:stmts) = stmt ++ "\n" ++ genBody stmts

genStmt :: 