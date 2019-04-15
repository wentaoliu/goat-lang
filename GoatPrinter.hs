
{-
File: GoatPrinter.hs
Author: Wumpus-Killers (Wentao Liu, Raymond Sun, Zeyu Huang, Yiqun Wang)
Origin: Thu 4 Apr 2019
Purpose: Pretty printer for GoatAST data
    - This program deals with printing GoatAST objects across multiple lines:
      GoatProgram, Proc, [Decl], [Stmt], etc.
    - For formating elements within a single line (expr, stmt, etc.), the
      logic is implemented in GoatAST.hs by instantiating Show for these types
    - The consideration behind this design is that bigger AST objects needs
      to be indented properly based on its "context", which is not accessible
      in the GoatAST module.
-}

module GoatPrinter where

import GoatAST
import Data.List

pretty :: GoatProgram -> String
pretty ast = formatProgram ast

--AST specifies: GoatProgram = Program [Proc]
formatProgram :: GoatProgram -> String
formatProgram (Program procs) = intercalate "\n" (map formatProc procs)
--note that formatProc *puts a new line* onto the end of each proc.


--AST spec: data Proc = Proc Ident [Param] [Decl] [Stmt]
formatProc :: Proc -> String
formatProc (Proc id param decl stmt) 
    = "proc " ++ id ++ " (" ++ formatParam param ++ ")" 
        ++ "\n" ++ formatDecl decl ++ "begin\n" ++ formatStmts stmt ++ "end\n" 

--AST spec: Param ParamType BaseType Ident
formatParam :: [Param] -> String
--no params = no text
formatParam [] = ""
--the last param is printed without comma afterwards
formatParam ((Param pType bType id):[]) 
    = (intercalate " " [show pType, show bType, id])
--non last param is paramtype, basetype and id separated by space, then comma into next param
formatParam ((Param pType bType id): params)
    = (intercalate " " [show pType, show bType, id]) ++ ", " ++ formatParam params

-- A set of functions for formatting indentation
data Indent = BySpace Int 

indentStep = BySpace 4

formatIndent :: Indent -> String
formatIndent (BySpace n) = replicate n ' '

furtherIndent :: Indent -> Indent
furtherIndent = addIndent indentStep

addIndent :: Indent -> Indent -> Indent
addIndent (BySpace x) (BySpace y) = BySpace (x + y)

-- A list of variable declarations is indented by one indentStep
formatDecl :: [Decl] -> String
formatDecl [] = ""
formatDecl ((BaseDecl id bType):decls) 
    = (formatIndent indentStep) ++ (show bType) 
        ++ " " ++ (id) ++ ";\n" ++ formatDecl decls
formatDecl ((ArrayDecl id aSize bType):decls) 
    = (formatIndent indentStep) ++ (show bType) 
        ++ " " ++ (id) ++ (show aSize) ++ ";\n" ++ formatDecl decls

formatStmts :: [Stmt] -> String
formatStmts s = formatStmtsI indentStep s

-- This method formats statements with specified indentation.
formatStmtsI :: Indent -> [Stmt] -> String
formatStmtsI i stmts = concat $ map (formatStmtI i) stmts

-- A statement is formatted recursively where each recursive call
-- furthers the indentation by one indentStep.
formatStmtI :: Indent -> Stmt -> String
formatStmtI i stmt = case stmt of 
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
    atomicStmt ->
        -- For (Assign|Read|Write|Call) statements,
        -- show is implemented in GoatAST.hs
        (formatIndent i) ++ show atomicStmt






