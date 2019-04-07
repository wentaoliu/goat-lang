module GoatPrinter where

import GoatAST
import Data.List

pretty :: GoatProgram -> String
pretty ast = formatProgram ast

--data GoatProgram = Program [Proc]
formatProgram :: GoatProgram -> String
formatProgram (Program []) = ""
--rule 2: two consecutive procedures should be separated by a 
--single blank line => "\n\n". 
--this assumes formatProc does not put a new line onto the end of each proc
formatProgram (Program (proc:procs)) 
    = formatProc proc ++ "\n\n" ++ formatProgram (Program procs)

--data Proc = Proc Ident [Param] [Decl] [Stmt]
formatProc :: Proc -> String
formatProc (Proc id param decl stmt) 
    = "proc " ++ id ++ "(" ++ formatParam param ++ ")" 
        ++ "\n" ++ formatDecl decl ++ "begin\n" ++ formatStmts stmt ++ "end" 


formatParam :: [Param] -> String
formatParam _ = ""

--needs to be indented
formatDecl :: [Decl] -> String
formatDecl _ = ""

--needs to be indented. if block and while block need extra indentation
formatStmts :: [Stmt] -> String
formatStmts s = formatStmtsI indentStep s


data Indent = BySpace Int 
            -- | ByTab Int

indentStep = BySpace 4

formatIndent :: Indent -> String
formatIndent (BySpace n) = replicate n ' '

furtherIndent :: Indent -> Indent
furtherIndent = addIndent indentStep

addIndent :: Indent -> Indent -> Indent
addIndent (BySpace x) (BySpace y) = BySpace (x + y)

formatStmtsI :: Indent -> [Stmt] -> String
formatStmtsI i stmts = concat $ map (formatStmtI i) stmts

formatStmtI :: Indent -> Stmt -> String
formatStmtI i stmt = case stmt of 
    Assign lval expr -> (formatIndent i) ++
        (show lval) ++ " := " ++ (show expr) ++ ";\n"
    Read lval -> (formatIndent i) ++
        "read " ++ (show lval) ++ ";\n"
    Write expr -> (formatIndent i) ++
        "write " ++ (show expr) ++ ";\n"
    Call id exprs -> (formatIndent i) ++
        "call " ++ (show id) ++  
        "(" ++ (joinStrings ", " $ map show exprs) ++ ")\n"
    If expr stmts -> 
        (formatIndent i) ++ "if " ++ (show expr) ++ " then\n" ++
        formatStmtsI (furtherIndent i) stmts ++
        (formatIndent i) ++ "fi\n"
    IfElse expr stmtIf stmtElse -> 
        (formatIndent i) ++ "if " ++ (show expr) ++ " then\n" ++
        formatStmtsI (furtherIndent i) stmtIf ++
        (formatIndent i) ++ "else\n" ++
        formatStmtsI (furtherIndent i) stmtElse ++
        (formatIndent i) ++ "fi\n"
    While expr stmts -> 
        (formatIndent i) ++ "while " ++ (show expr) ++ " do\n" ++
        formatStmtsI (furtherIndent i) stmts ++
        (formatIndent i) ++ "od\n"



                     

-- This function might be in the library already.
joinStrings :: String -> [String] -> String
joinStrings _ [] = ""
joinStrings _ [x] = x
joinStrings sep (x:y:ys) = x ++ sep ++ y ++ (joinStrings sep ys)




