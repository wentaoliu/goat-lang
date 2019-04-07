module Main where

import GoatAST
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
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
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------

pProg :: Parser Proc
pProg
  = do
      reserved "proc"
      -- reserved "main"
      ident <- identifier 
      -- parens (return ())
      params <- parens pProgHeader 
      (decls,stmts) <- pProgBody
      return (Proc ident params decls stmts)


pProgHeader :: Parser [Param]
pProgHeader
  = sepBy pProgParam comma

pProgParam :: Parser Param
pProgParam
  = do
      paramtype <- pParamType
      basetype <- pBaseType
      ident <- identifier
      return (Param paramtype basetype ident)

pParamType :: Parser ParamType
pParamType
  = do { reserved "ref"; return Ref }
    <|>
    do { reserved "val"; return Val }
-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody
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
  = do { x <- pInt; return (OneDimen x) }
    <|>
    do { x <- pInt; comma; y <- pInt; return (Matrix x y) }
    
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
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pIfOrElse, pIf, pIfelse, pWhile, pCall :: Parser Stmt

pStmt 
  = choice [pRead, pWrite, pAsg, pIfOrElse, pWhile, pCall]

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

pIfOrElse
  = do
    try pIf <|> pIfelse

pIf
  = do
    reserved "if"
    exp <- pExp
    reserved "then"
    stmts <- many pStmt
    reserved "fi"
    return (If exp stmts)

pIfelse 
  = do
    reserved "if"
    exp <- pExp
    reserved "then"
    stmts1 <- many pStmt
    reserved "else"
    stmts2 <- many pStmt
    reserved "fi"
    return (IfElse exp stmts1 stmts2)

pWhile
  = do
    reserved "while"
    exp <- pExp
    reserved "do"
    stmts <- many pStmt
    reserved "od"
    return (While exp stmts)
  

-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------

pExp, pTerm, pFactor, pUminus, pIntConst, pFloatConst, pIdent, pString, pArray, pBool :: Parser Expr

pExp 
  = pString 
    <|> (chainl1 pTerm (pAddOp <|> pSubOp <|> pGt <|> pGte <|> pLt <|> pLte <|> pEq <|> pNe))
    <?>
    "expression"

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

pAddOp, pSubOp, pMulOp, pDivOp, pGt, pGte, pLt, pLte, pEq, pNe :: Parser (Expr -> Expr -> Expr)

pAddOp
  = do 
      reservedOp "+"
      return (BinExpr Op_add)

pSubOp
  = do
    reservedOp "-"
    return (BinExpr Op_sub)

pTerm 
  = (chainl1 pFactor pMulOp)
    <|>
    (chainl1 pFactor pDivOp)
    <?>
    "\"term\""

pMulOp
  = do 
      reservedOp "*"
      return (BinExpr Op_mul)

pDivOp
  = do
    reservedOp "/"
    return (BinExpr Op_div)

pGt
  = do
    reservedOp ">"
    return (BinExpr Op_gt)
    
pGte
  = do
    reservedOp ">="
    return (BinExpr Op_gte)

pLt 
  = do 
    reservedOp "<"
    return (BinExpr Op_lt)
    
pLte
  = do
    reservedOp "<="
    return (BinExpr Op_lte)

pEq
  = do
    reservedOp "="
    return (BinExpr Op_eq)

pNe
  = do 
    reservedOp "!="
    return (BinExpr Op_ne)

pFactor
  = choice [pUminus, parens pExp, pIntConst, pFloatConst, pIdent, pArray, pBool]
    <?> 
    "\"factor\""

pUminus
  = do 
      reservedOp "-"
      exp <- pFactor
      return (UnaryExpr Op_umin exp)

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
      return (Id ident)
    <?>
    "identifier"

pArray 
  = do 
      ident <- identifier
      aindex <- squares pArrayIndex
      return (Array ident aindex)

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
  = do { x <- pExp; return (OneDimenIndex x) }
    <|>
    do { x <- pExp; comma; y <- pExp; return (MatrixIndex x y) }
 
-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- many1 pProg
      eof
      return (Program p)

main :: IO ()
main
  = do { progname <- getProgName
        ; args <- getArgs
        ; checkArgs progname args
        ; input <- readFile (last args)
        ; let output = runParser pMain 0 "" input
        ; case output of
            Right ast -> putStr $ pretty ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            }
        }



checkArgs :: String -> [String] -> IO ()
--need to conform filename is a .gt file
checkArgs _ ["-p",filename]
    = return ()
checkArgs progname [filename]
    = do { putStrLn (progname ++ "\n" ++ (concat [filename]) ++ "\nSorry, code cannot be generated yet\n")
        ; exitWith (ExitFailure 1)
        }
checkArgs progname args
    = do { putStrLn ((Data.List.intercalate " " args) ++ "\nUsage: " ++ progname ++ " filename\n\n")
        ; exitWith (ExitFailure 1)
        }




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

formatDecl :: [Decl] -> String
formatDecl _ = ""

formatStmts :: [Stmt] -> String
formatStmts _ = ""
        