{-
Generate oz code from AST
-}

module Codegen where

import GoatAST
import GoatParser
import GoatPrinter

import Data.List
import Data.Either.Utils

import Text.Parsec
import Text.Parsec.Language (emptyDef)

import System.Environment
import System.Exit

veryShortProgram = "proc main () begin a := 3; write a; end\n"

veryShortParsed :: Either ParseError GoatProgram
veryShortParsed = runParser pMain 0 "" veryShortProgram

veryShortPrint :: Either ParseError GoatProgram -> IO ()
veryShortPrint output =
    case output of
        Right ast -> putStr $ GoatPrinter.formatProgram ast
        Left  err -> do { putStr "Parse error at "
                        ; print err
                        ; exitWith (ExitFailure 2)
                        }   

data OzStmt = OzWrite String
instance Show OzStmt where
    show (OzWrite s) = "string_const r0, " ++ s ++ "\n" ++
                       "call_builtin print_string\n"


type OzProgram = [OzProc]
data OzProc = OzProc Ident [OzStmt]

transMain :: GoatProgram -> OzProgram
transMain (Program procs) = map transProc procs

transProc :: Proc -> OzProc
transProc (Proc id params decls stmts) =
    OzProc id ozstmts where
        ozstmts = 
            -- [OzWrite (show id ++ ": ")] ++
            map transStmt stmts

transStmt :: Stmt -> OzStmt
transStmt (Write e) = OzWrite (show e ++ "\n")
transStmt _ = OzWrite "<unsupported statement;>\n"

genMain :: OzProgram -> String
genMain = (intercalate "\n") . (map genProc) 

genProc :: OzProc -> String
genProc (OzProc id ozstmts) =
    concatMap genStmt ((OzWrite (id ++ ":\n")):ozstmts)

genStmt :: OzStmt -> String
genStmt (OzWrite s) = 
    "string_const r0, " ++ show s ++
    "\ncall_builtin print_string\n"

printOz :: IO ()
printOz = do
    putStr output 
    putStr "halt\n"
    return ()
    where
        output = genMain (transMain $ fromRight veryShortParsed)
