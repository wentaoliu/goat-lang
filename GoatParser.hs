-------------------------------------------------------------------
-- GoatParser parses a Goat program 
-- Authors: Zeyu Huang, Yiqun Wang, Wentao Liu, Raymond Sun
-- Based on skeleton code provided by Harald Søndergaard
-- 
-- Converts a Goat program in text to an AST as specified in GoatAST.hs
-- Pretty prints the result into a "standard" goat program layout 
-- 
-- For more insight on how a Goat program syntax, check GoatAST.hs
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

-- Parsec provides some scanning functionality 
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
--    Parsing the goat program
-----------------------------------------------------------------

-- Parses the entire goat program which consists of one or more procedures
pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- many1 pProc
      eof
      return (Program p)

-- proc aka procedure
pProc :: Parser Proc
pProc
  = do
      reserved "proc"
      ident <- identifier 
      params <- parens pProcHeader 
      (decls,stmts) <- pProcBody
      return (Proc ident params decls stmts)


-----------------------------------------------------------------
--   Parsing the header of a goat procedure
-----------------------------------------------------------------

pProcHeader :: Parser [Param]
pProcHeader
  = sepBy pProcParam comma

pProcParam :: Parser Param
pProcParam
  = do
      passType <- pPassType
      basetype <- pBaseType
      ident <- identifier
      return (Param passType basetype ident)

pPassType :: Parser PassType
pPassType
  = do { reserved "ref"; return Ref }
    <|>
    do { reserved "val"; return Val }


-----------------------------------------------------------------
--  Parsing the body of a goat program
-----------------------------------------------------------------

pProcBody :: Parser ([Decl],[Stmt])
pProcBody
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (decls,stmts)

pDecl :: Parser Decl
pDecl
  = do
    try pBaseDecl <|>  pArrayDecl

pBaseDecl :: Parser Decl
pBaseDecl
  = do
    basetype <- pBaseType
    ident <- identifier
    whiteSpace
    semi
    return (BaseDecl ident basetype)

pArrayDecl :: Parser Decl
pArrayDecl 
  = do
    basetype <- pBaseType
    ident <- identifier
    size <- squares pArraySize
    whiteSpace
    semi
    return (ArrayDecl ident size basetype)

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

pStmt 
  = choice [pRead, pWrite, pAsg, pIfElse, pWhile, pCall]

pRead
  = do 
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read lvalue)

pWrite
  = do 
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return (Write exp)

pCall
  = do
    reserved "call"
    ident <- identifier
    exprlist <- parens (sepBy1 pExp comma)
    semi
    return (Call ident exprlist)

pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign lvalue rvalue)

-- Handles both (if..then) and (if..then..else) stmts
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
--    Main
-----------------------------------------------------------------

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

-- Check the command line args to determine what the program should do
checkArgs :: String -> [String] -> IO Task
checkArgs _ ["-p",filename]
    = return Pprint 
checkArgs _ [filename]
    = return Compile
checkArgs progname args
    = do 
        putStrLn ("\nUsage: " ++ progname ++ " filename\n\n")
        exitWith (ExitFailure 1)

-- Represents a task that is desired of the GoatParser program
data Task = Compile | Pprint deriving(Eq, Show)