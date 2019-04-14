-------------------------------------------------------------------
-- GoatParser parses a Goat program 
-- Authors: Zeyu Huang, Yiqun Wang, Wentao Liu, Raymond Sun
-- Based on skeleton code provided by Harald Søndergaard
-- 
-- Parses a goat program according to the AST specified in GoatAST
-- Pretty prints the result into a "standard" goat program layout 
-------------------------------------------------------------------
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

lexer :: Q.TokenParser Int
lexer
    = Q.makeTokenParser
      (emptyDef
      { Q.commentLine     = "#"
      , Q.nestedComments  = True
      , Q.identStart      = letter
      , Q.opStart         = oneOf "+-*:"
      , Q.opLetter        = oneOf "+-*:"
      , Q.reservedNames   = myReserved
      , Q.reservedOpNames = myOpnames
      })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
float      = Q.float lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved
  = ["begin", "bool", "call", "do", "else", "end", "false", "fi", "float", "if", 
      "int", "od", "proc", "read", "ref", "then", "true", "val", "while", "write"]
      
myOpnames 
  = ["+", "-", "*", "/", ":=", "||", "&&", "!", "=", "!=", "<", "<=", ">", ">="]


-----------------------------------------------------------------
--     Parsing the goat program
-----------------------------------------------------------------

-- pProc parses a function (aka process) in a goat program
pProc :: Parser Proc
pProc
  = do
      reserved "proc"
      ident <- identifier 
      params <- parens pProcHeader 
      (decls,stmts) <- pProcBody
      return (Proc ident params decls stmts)

-----------------------------------------------------------------
--     Parsing the header of a goat program
-----------------------------------------------------------------

-- pProcHeader parses the function arguments (aka parameters)
pProcHeader :: Parser [Param]
pProcHeader
  = sepBy pProcParam comma

-- pProcParam parses the individual parameters 
pProcParam :: Parser Param
pProcParam
  = do
      passType <- pPassType
      basetype <- pBaseType
      ident <- identifier
      return (Param passType basetype ident)

-- pPassype separates the parameters which are passed by reference or value
pPassType :: Parser PassType
pPassType
  = do { reserved "ref"; return Ref }
    <|>
    do { reserved "val"; return Val }

-----------------------------------------------------------------
--  Parsing the body of a goat program
-----------------------------------------------------------------

-- Body aka non header sections of the function

-- Parse the body of the program
-- Body consists of some variable declarations, then some statememts
pProcBody :: Parser ([Decl],[Stmt])
pProcBody
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (decls,stmts)

-- Parse a single declaration which can be declaring a 
-- single var or an array 
pDecl :: Parser Decl
pDecl
  = do
    try pBaseDecl <|>  pArrayDecl

-- Parse a single variable. Consists of the type (int/bool/float) and the identifier
pBaseDecl :: Parser Decl
pBaseDecl
  = do
    basetype <- pBaseType
    ident <- identifier
    whiteSpace
    semi
    return (BaseDecl ident basetype)

-- Parse an array declaration.
-- Consists of the type, identifier and the characteristics of 
-- the array (i.e one or two dimensions, size of each dimension)
pArrayDecl :: Parser Decl
pArrayDecl 
  = do
    basetype <- pBaseType
    ident <- identifier
    size <- squares pArraySize
    whiteSpace
    semi
    return (ArrayDecl ident size basetype)

-- Parses the array size and dimension
pArraySize :: Parser ArraySize
pArraySize
  = do 
    x <- pInt
    optional comma
    y <- optionMaybe pInt
    case y of 
      Nothing -> return (OneDimen x)
      Just i -> return (Matrix x i)

pInt :: Parser Int
pInt 
  = do 
    n <- natural
    return (fromInteger n::Int)
  
-- Parses a data type in goat. can be boolean, integer or floating point number.
pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType}
      
-----------------------------------------------------------------
--  Parsing statements
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pIfElse, pWhile, pCall :: Parser Stmt
-- Statement can be either a 'read' statement, 'write' statement,
-- assignment to a value, if statment, if .. else statement, while statement 
--  or a 'call' statement
pStmt 
  = choice [pRead, pWrite, pAsg, pIfElse, pWhile, pCall]

-- read statment begins with a 'read' keyword and some value
pRead
  = do 
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read lvalue)
-- write stmt begins with a 'write' keyword and some value or a string
pWrite
  = do 
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return (Write exp)
-- call stmt begins with 'call' keyword and some procedure id and its arguments
pCall
  = do
    reserved "call"
    ident <- identifier
    exprlist <- parens (sepBy1 pExp comma)
    semi
    return (Call ident exprlist)

-- assignment stmt is a variable followed by the assignment symbol ':=' and the some expression
pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign lvalue rvalue)

-- Handles both (if..then) and (if..then..else stmts
pIfElse 
  = do
    reserved "if"
    exp <- pExp
    reserved "then"
    stmts1 <- many1 pStmt
    optional (reserved "else")
    stmts2 <- optionMaybe (many1 pStmt)
    reserved "fi"
    case stmts2 of 
      Nothing -> return (If exp stmts1)
      Just stmts -> return (IfElse exp stmts1 stmts)

pWhile
  = do
    reserved "while"
    exp <- pExp
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return (While exp stmts)
  

-----------------------------------------------------------------
--  Parsing expressions
-----------------------------------------------------------------

pExp, pIntConst, pFloatConst, pIdent, pString, pBool :: Parser Expr

pExp = buildExpressionParser table pFac 
        <?> "expression"

pFac = choice [parens pExp, (try pFloatConst <|> pIntConst), pIdent, pString, pBool]

table = [ [ prefix "-" (UnaryExpr Op_umin) ]
        , [ binary "*" (BinExpr Op_mul), binary "/" (BinExpr Op_div) ] 
        , [ binary "+" (BinExpr Op_add), binary "-" (BinExpr Op_sub) ] 
        , [ relation "=" (BinExpr Op_eq), relation "!=" (BinExpr Op_ne)
          , relation "<" (BinExpr Op_lt), relation "<=" (BinExpr Op_lte)
          , relation ">" (BinExpr Op_gt), relation ">=" (BinExpr Op_gte) ] 
        , [ prefix "!" (UnaryExpr Op_uneg) ]
        , [ binary "&&" (BinExpr Op_add) ]
        , [ binary "||" (BinExpr Op_or) ]
        ]

prefix name fun = Prefix (do { reservedOp name; return fun })

binary name op = Infix (do { reservedOp name; return op }) AssocLeft

relation name rel = Infix (do { reservedOp name; return rel }) AssocNone

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      return (StrConst str)
    <?>
    "string"

pBool
  = do { reserved "true"; return (BoolConst True) }
    <|>
    do { reserved "false"; return (BoolConst False) }

pIntConst
  = do
      n <- natural <?> ""
      return (IntConst (fromInteger n :: Int))
    <?>
    "number"

pFloatConst
  = do
      n <- float <?> ""
      return (FloatConst (realToFrac n :: Float))
    <?>
    "float"

pIdent 
  = do
    ident <- identifier 
    aindex <- optionMaybe (squares pArrayIndex)
    case aindex of
      Nothing -> return (Id ident)  
      Just i -> return (Array ident i)

pLvalue :: Parser Lvalue
pLvalue
  = do 
    try pLarray <|> pLatom
    <?>
    "lvalue"

pLatom :: Parser Lvalue
pLatom 
  = do
    ident <- identifier
    return (LId ident)

pLarray :: Parser Lvalue
pLarray 
  = do
      ident <- identifier
      aindex <- squares pArrayIndex
      return (LArray ident aindex)
  

pArrayIndex :: Parser ArrayIndex
pArrayIndex
  = do 
    x <- pExp
    optional comma
    y <- optionMaybe pExp
    case y of 
      Nothing -> return (OneDimenIndex x)
      Just i -> return (MatrixIndex x i)
 
-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- many1 pProc
      eof
      return (Program p)

main :: IO ()
main
  = do 
        progname <- getProgName
        args <- getArgs
        task <- checkArgs progname args
        if task == Compile then
            do
                putStrLn "Sorry, cannot generate code yet"
                exitWith ExitSuccess
        else 
            do 
                input <- readFile (last args)
                let output = runParser pMain 0 "" input
                case output of
                    Right ast -> putStr $ GoatPrinter.formatProgram ast
                    Left  err -> do { putStr "Parse error at "
                                    ; print err
                                    ; exitWith (ExitFailure 2)
                                    }        

-- Check the arguments to determine what the program should do
checkArgs :: String -> [String] -> IO Task
checkArgs _ ["-p",filename]
    = return Pprint 
checkArgs _ [filename]
    = return Compile
checkArgs progname args
    = do 
        putStrLn ("\nUsage: " ++ progname ++ " filename\n\n")
        exitWith (ExitFailure 1)

-- Represents a command that is desired of the GoatParser program
data Task = Compile | Pprint deriving(Eq, Show)