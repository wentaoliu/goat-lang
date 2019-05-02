{-
File: GoatParser.hs
Author: Wumpus-Killers (Wentao Liu, Raymond Sun, Zeyu Huang, Yiqun Wang)
        - Based on skeleton code provided by Harald Søndergaard
Origin: Thu 4 Apr 2019
Purpose: Converts a Goat program (text) to an AST as specified in GoatAST.hs
    - Pretty prints the result into a "standard" Goat program layout 
    - For more insight into Goat program syntax, check GoatAST.hs
-}

module Main where

import GoatAST
import GoatPrinter
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import Text.Parsec.Expr
import System.Environment
import System.Exit

type Parser a
    = Parsec String Int a


-----------------------------------------------------------------
--    Main
-----------------------------------------------------------------

-- Represents a task that is desired of the GoatParser program
data Task = Compile | PrettyPrint deriving(Eq, Show)

-- Read the command line inputs and do the desired action
-- Currently we can only parse and then pretty print the result
main :: IO ()
main = do 
    progname <- getProgName
    args <- getArgs
    task <- checkArgs progname args
    if task == Compile then do
        putStrLn "Sorry, cannot generate code yet"
        exitWith ExitSuccess
    else do 
        input <- readFile (last args)
        let output = runParser pMain 0 "" input
        case output of
            Right ast -> putStr $ GoatPrinter.formatProgram ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            ; exitWith (ExitFailure 2)
                            }        

-- Check the command line args to determine what the program should do
checkArgs :: String -> [String] -> IO Task
checkArgs _ ["-p",filename] = 
    return PrettyPrint 
checkArgs _ [filename] =
    return Compile
checkArgs progname args = do 
    putStrLn ("\nUsage: " ++ progname ++ "[-p] filename\n\n")
    exitWith (ExitFailure 1)



-----------------------------------------------------------------
--    Parsec scanner functions
-----------------------------------------------------------------
lexer :: Q.TokenParser Int
lexer = 
    Q.makeTokenParser
        (emptyDef
        { Q.commentLine     = "#"
        , Q.nestedComments  = True
        , Q.identStart      = letter
        , Q.opStart         = oneOf "+-*/:|&<=>!"
        , Q.opLetter        = oneOf "+-*/:|&<=>!"
        , Q.reservedNames   = myReserved
        , Q.reservedOpNames = myOpnames
        })

whiteSpace      = Q.whiteSpace lexer
lexeme          = Q.lexeme lexer
natural         = Q.natural lexer
float           = Q.float lexer
identifier      = Q.identifier lexer
colon           = Q.colon lexer
semi            = Q.semi lexer
comma           = Q.comma lexer
parens          = Q.parens lexer
squares         = Q.squares lexer
reserved        = Q.reserved lexer
reservedOp      = Q.reservedOp lexer
naturalOrFloat  = Q.naturalOrFloat lexer

myReserved, myOpnames :: [String]

myReserved = 
    ["begin", "bool", "call", "do", "else", "end", "false", "fi", "float", "if", 
    "int", "od", "proc", "read", "ref", "then", "true", "val", "while", "write"]
      
myOpnames = 
    ["+", "-", "*", "/", ":=", "||", "&&", "!", "=", "!=", "<", "<=", ">", ">="]


-----------------------------------------------------------------
--    Parsing the goat program
-----------------------------------------------------------------

-- Parses the entire goat program which consists of one or more procedures 
pMain :: Parser GoatProgram
pMain = do
    whiteSpace
    p <- many1 pProc
    eof
    return (Program p)

pProc :: Parser Proc
pProc = do
    reserved "proc"
    ident <- identifier 
    params <- parens pProcHeader 
    (decls,stmts) <- pProcBody
    return (Proc ident params decls stmts)


-----------------------------------------------------------------
--   Parsing the header of a goat procedure
-----------------------------------------------------------------

pProcHeader :: Parser [Param]
pProcHeader = 
    sepBy pProcParam comma

pProcParam :: Parser Param
pProcParam = do
    passType <- pPassType
    basetype <- pBaseType
    ident <- identifier
    return (Param passType basetype ident)

pPassType :: Parser PassType
pPassType =
    do { reserved "ref"; return Ref }
    <|>
    do { reserved "val"; return Val }


-----------------------------------------------------------------
--  Parsing the body of a goat procedure
-----------------------------------------------------------------

-- The body is just everything other than the procedure id and header
-- Contains variable declarations and statments.

pProcBody :: Parser ([Decl],[Stmt])
pProcBody = do
    decls <- many pDecl
    reserved "begin"
    stmts <- many1 pStmt
    reserved "end"
    return (decls,stmts)

pDecl :: Parser Decl
pDecl = do
    basetype <- pBaseType
    ident <- identifier
    maybeSize <- optionMaybe $ squares pArraySize
    whiteSpace
    semi
    case maybeSize of 
        Nothing   -> return (BaseDecl ident basetype)
        Just size -> return (ArrayDecl ident size basetype)

pArraySize :: Parser ArraySize
pArraySize = do 
    x <- pInt
    maybeY <- optionMaybe $ comma *> pInt
    case maybeY of 
        Nothing -> return (OneDimen x)
        Just y  -> return (Matrix x y)

pInt :: Parser Int
pInt = do 
    n <- natural
    return (fromInteger n::Int)
  
pBaseType :: Parser BaseType
pBaseType = 
    do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType}
      
-----------------------------------------------------------------
--  Parsing statements
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pIfElse, pWhile, pCall :: Parser Stmt

pStmt = choice [pRead, pWrite, pAsg, pIfElse, pWhile, pCall]

pRead = do 
    reserved "read"
    var <- pVar
    semi
    return (Read var)

pWrite = do 
    reserved "write"
    exp <- (pString <|> pExp)
    semi
    return (Write exp)

pCall = do
    reserved "call"
    ident <- identifier
    exprlist <- parens (sepBy pExp comma)
    semi
    return (Call ident exprlist)

pAsg = do
    var <- pVar
    reservedOp ":="
    rvalue <- pExp
    semi
    return (Assign var rvalue)

-- Handles both (if..then) and (if..then..else) stmts
pIfElse = do
    reserved "if"
    exp <- pExp
    reserved "then"
    stmts <- many1 pStmt
    maybeElseStmts <- optionMaybe elseStmts
    reserved "fi"
    case maybeElseStmts of 
        Nothing        -> return (If exp stmts)
        Just elseStmts -> return (IfElse exp stmts elseStmts)
    where elseStmts = reserved "else" *> (many1 pStmt)

pWhile = do
    reserved "while"
    exp <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return (While exp stmts)
  

-----------------------------------------------------------------
--  Parsing expressions
-----------------------------------------------------------------

pExp, pNum, pIdent, pString, pBool :: Parser Expr

--buildExpressionParser automatically builds a parser for expressions (duh)
-- based on the table of operations (table) and any provided terms (pFac) 
pExp = 
    buildExpressionParser table pFac 
    <?> 
    "expression"

pFac = choice [parens pExp, pNum, pIdent, pString, pBool]

table = [ [ prefix "-" (UnaryExpr Op_umin) ]
        , [ binary "*" (BinExpr Op_mul), binary "/" (BinExpr Op_div) ] 
        , [ binary "+" (BinExpr Op_add), binary "-" (BinExpr Op_sub) ] 
        , [ relation "=" (BinExpr Op_eq), relation "!=" (BinExpr Op_ne)
          , relation "<" (BinExpr Op_lt), relation "<=" (BinExpr Op_lte)
          , relation ">" (BinExpr Op_gt), relation ">=" (BinExpr Op_gte) ] 
        , [ prefix "!" (UnaryExpr Op_uneg) ]
        , [ binary "&&" (BinExpr Op_and) ]
        , [ binary "||" (BinExpr Op_or) ]
        ]

prefix name fun = Prefix (do { reservedOp name; return fun })

binary name op = Infix (do { reservedOp name; return op }) AssocLeft

relation name rel = Infix (do { reservedOp name; return rel }) AssocNone

pString = do
    char '"'
    str <- many (satisfy (\x -> (x /= '"') 
                             && (x /= '\t') 
                             && (x /= '\n')))
    char '"'
    whiteSpace
    return (StrConst str)
    <?>
    "string"

pBool = 
    do { reserved "true"; return (BoolConst True) }
    <|>
    do { reserved "false"; return (BoolConst False) }

pNum = do
    iOrF <- naturalOrFloat;
    case iOrF of 
        Left i  -> return (IntConst (fromInteger i :: Int))
        Right f -> return (FloatConst (realToFrac f :: Float))
    <?>
    "number"

pIdent = do
    ident <- identifier 
    maybeIndex <- optionMaybe $ squares pArrayIndex
    case maybeIndex of
        Nothing    -> return (Id ident)  
        Just index -> return (Array ident index)

-----------------------------------------------------------------
--  Parsing variables
-----------------------------------------------------------------   
pVar :: Parser Var
pVar = do 
    ident <- identifier
    maybeIndex <- optionMaybe $ squares pArrayIndex
    case maybeIndex of 
        Nothing    -> return (VId ident)
        Just index -> return (VArray ident index)
    <?>
    "variable"

pArrayIndex :: Parser ArrayIndex
pArrayIndex = do 
    x <- pExp
    maybeY <- optionMaybe $ comma *> pExp
    case maybeY of 
        Nothing -> return (OneDimenIndex x)
        Just y  -> return (MatrixIndex x y)
 


