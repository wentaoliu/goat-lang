module GoatParser (ast)
where

-------------------------------------------------------------------------
--  A parser for Goat, poorly commented.
--  It should still be fairly easy to read, as it uses the Parsec parser
--  library.
--
--  Harald Sondergaard, April 2019
-------------------------------------------------------------------------

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

data MaybeOneOrTwo a
  = None
  | One a
  | Two a a
  | TooMany
    deriving (Eq, Show)

type Parser a
   = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
   = Q.makeTokenParser
     (emptyDef
     { Q.commentLine     = "#"
     , Q.nestedComments  = True
     , Q.identStart      = letter
     , Q.opStart         = oneOf "+-*/=!<>&|:"
     , Q.opLetter        = oneOf "=&|"
     , Q.reservedNames   = myReserved
     , Q.reservedOpNames = myOpnames
     })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
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
  = [ "begin", "bool", "call", "do", "else", "end", "false"
    , "fi", "float", "if", "int", "od", "proc", "read"
    , "ref", "then", "true", "val", "while", "write"
    ]

myOpnames 
  = [ "+", "-", "*", "/"
    , "=", "!=", "<", ">", "<=", ">="
    , "&&", "||", "!", ":="
    ]

-- Translate a source code position to a pair of integers (line, column)

comps :: SourcePos -> (Int,Int)
comps pos
  = (sourceLine pos, sourceColumn pos)

pExp :: Parser Expr
pExp
  = buildExpressionParser table pFac
    <?> "expression"

pFac
  = choice [parens pExp, pNum, pBool, pIdent]
    <|> do
          pos <- getPosition
          reservedOp "-"
          exp <- pFac
          return (UnaryMinus (comps pos) exp)
    <?> "simple expression"

table   
  = [ [ prefix "-" UnaryMinus ]
    , [ binary "*" Op_mul
      , binary "/" Op_div
      ]
    , [ binary "+" Op_add
      , binary "-" Op_sub
      ]
    , [ relation "="  Op_eq
      , relation "!=" Op_ne
      , relation "<"  Op_lt
      , relation "<=" Op_le
      , relation ">"  Op_gt
      , relation ">=" Op_ge
      ]
    , [ prefix "!" Not ]
    , [ binLogic "&&" And ]
    , [ binLogic "||" Or ]
    ]

binary name op 
  = Infix (do { pos <- getPosition
              ; reservedOp name
              ; return (BinOpExp (comps pos) op) 
              }
          ) AssocLeft

binLogic name op 
  = Infix (do { pos <- getPosition
              ; reservedOp name 
              ; return (op (comps pos))
              }
          ) AssocLeft

relation name rel
  = Infix (do { pos <- getPosition
              ; reservedOp name
              ; return (Rel (comps pos) rel) 
              }
          ) AssocNone

prefix name fun 
  = Prefix (do { pos <- getPosition
               ; reservedOp name
               ; return (fun (comps pos))
               }
           )

pProgram :: Parser Program
pProgram 
  = do
      procs <- many1 pProc
      return (Program procs)

pProc :: Parser Procedure
pProc
  = do
      reserved "proc"
      pos <- getPosition
      (ident,argspecs) <- pProcHead
      (decls,stmts) <- pProcbody
      reserved "end"
      return (Procedure (comps pos) ident argspecs decls stmts)
      
pProcHead :: Parser (Ident, [FormalArgSpec])
pProcHead
  = do
      ident <- identifier
      argspecs <- parens (sepBy pArg comma)
      pos <- getPosition
      let lineCol = comps pos
      return (ident,argspecs)

pArg :: Parser FormalArgSpec
pArg
  = do
      mode <- pParmode
      basetype <- pBaseType
      pos <- getPosition
      ident <- identifier
      return (FormalArgSpec (comps pos) mode basetype ident)

pParmode :: Parser ParMode
pParmode
  = do { reserved "val"; return Val }
    <|> 
    do { reserved "ref"; return Ref }

pBaseType :: Parser BaseType

pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

pProcbody :: Parser ([Decl],[Stmt])
pProcbody
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      return (decls,stmts)

pDecl :: Parser Decl
pDecl
  = do
      basetype <- pBaseType
      pos <- getPosition
      ident <- identifier
      whiteSpace
      rest <- pMaybeIndices
      if rest == TooMany then
        unexpected "extra dimension(s)"
      else 
        do
          semi
          let 
            typespec
              = case rest of
                  None -> Base basetype
                  One n -> Array basetype n
                  Two m n -> Matrix basetype m n
          return (Decl (comps pos) ident typespec)

pMaybeIndices :: Parser (MaybeOneOrTwo Int)
pMaybeIndices
  = do { indices <- squares (sepBy1 natural comma)
       ; case indices of
           [n] -> return (One (fromInteger n))
           [m,n] -> return (Two (fromInteger m) (fromInteger n))
           _ -> return TooMany
       }
    <|>
    return None

pStmt, pRead, pWrite, pCall, pCond, pWhile, pAsg :: Parser Stmt

pStmt 
  = choice [pRead, pWrite, pCall, pCond, pWhile, pAsg]

pRead
  = do 
      pos <- getPosition
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read (comps pos) lvalue)

pWrite
  = do 
      pos <- getPosition
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return (Write (comps pos) exp)

pCall
  = do 
      reserved "call"
      pos <- getPosition
      ident <- identifier
      exps <- parens (sepBy pExp comma)
      semi
      return (ProcCall (comps pos) ident exps)

pCond
  = do
      reserved "if"
      pos <- getPosition
      cond <- pExp
      reserved "then"
      stmts1 <- many1 pStmt
      stmts2 <- pIftail
      let result = if null stmts2 
          then If (comps pos) cond stmts1 
          else IfElse (comps pos) cond stmts1 stmts2
      return result
      
pWhile
  = do
      reserved "while"
      pos <- getPosition
      cond <- pExp
      reserved "do"
      stmts <- many1 pStmt
      reserved "od"
      return (While (comps pos) cond stmts)

pAsg
  = do
      lvalue <- pLvalue
      pos <- getPosition
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign (comps pos) lvalue rvalue)

pIftail :: Parser [Stmt]
pIftail
  = do { reserved "else"
       ; stmts <- many1 pStmt
       ; reserved "fi"
       ; return stmts
       }
    <|> 
    do { reserved "fi"; return [] }
 
pString 
  = do
      pos <- getPosition
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      whiteSpace
      return (StrCon (comps pos) str)
    <?>
    "string"

pNum
  = do
      pos <- getPosition
      whole <- many1 digit
      rest <- pNumtail
      let val = case rest of 
                Nothing   
                  -> IntCon (comps pos) (read whole :: Int)
                Just frac 
                  -> FloatCon (comps pos) ((read (whole++frac)) :: Float)
      return val
    <?> 
    "number"

pNumtail :: Parser (Maybe String)
pNumtail
  = do { char '.'; frac <- many1 digit; whiteSpace; return (Just ('.':frac)) }
    <|> 
    do { whiteSpace; return Nothing }
      
pBool
  = do { pos <- getPosition
       ; reserved "true"
       ; return (BoolCon (comps pos) True) 
       }
    <|>
    do { pos <- getPosition
       ; reserved "false"
       ; return (BoolCon (comps pos) False) 
       }

pIdent 
  = do
      pos <- getPosition
      ident <- identifier
      expressions <- pMaybeIndexExps
      if expressions == TooMany then
        unexpected "extra dimension(s)"
      else
        do
          case expressions of 
            None -> return (Id (comps pos) ident)
            One e -> return (ArrayRef (comps pos) ident e)
            Two e1 e2 -> return (MatrixRef (comps pos) ident e1 e2)
    <?>
    "identifier"

pLvalue :: Parser Lvalue
pLvalue
  = do
      pos <- getPosition
      ident <- identifier
      expressions <- pMaybeIndexExps
      if expressions == TooMany then
        unexpected "extra dimension(s)"
      else
        do
          case expressions of 
            None -> return (LId (comps pos) ident)
            One e -> return (LArrayRef (comps pos) ident e)
            Two e1 e2 -> return (LMatrixRef (comps pos) ident e1 e2)
    <?>
    "lvalue"
      
pMaybeIndexExps :: Parser (MaybeOneOrTwo Expr)
pMaybeIndexExps
  = do { expressions <- squares (sepBy1 pExp comma)
       ; case expressions of
           [e] -> return (One e)
           [e1,e2] -> return (Two e1 e2)
           _ -> return TooMany
       }
    <|>
    return None

-------------------------------------------------------------------------
-- 
--  A function main, to run the parser and pretty-printer.  
--  Later modules will simply use the exported function ast.
-- 
-------------------------------------------------------------------------

goatParse :: Parser Program
goatParse
  = do
      whiteSpace
      p <- pProgram
      eof
      return p

ast :: String -> Either ParseError Program
ast input
  =  runParser goatParse 0 "" input

main :: IO ()
main
  = do { progname <- getProgName
       ; args <- getArgs
       ; checkArgs progname args
       ; input <- readFile (head args)
       ; let output = runParser goatParse 0 "" input
       ; case output of
           Right ast -> print ast
           Left  err -> do { putStr "Parse error at "
                           ; print err
                           }
       }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
   = return ()
checkArgs progname _
   = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
       ; exitWith (ExitFailure 1)
       }

