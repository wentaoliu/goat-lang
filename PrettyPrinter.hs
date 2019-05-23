module PrettyPrinter ( prettyPrint
                     , ppStmt
                     , ppExp
                     ) 
where


-------------------------------------------------------------------------
--  A pretty-printer module for Goat.
--
--  Harald Sondergaard, April 2019
-------------------------------------------------------------------------

import GoatAST
import Data.Char
import Numeric (showFFloatAlt)
import Data.List (intercalate)
import GoatParser (ast)


--  Indent an entire block of lines by four spaces:

indent :: [String] -> [String]
indent 
  = map ("    " ++)


--  Pretty print each procedure in a program:

prettyPrint :: Program -> String
prettyPrint (Program procedures)
  = intercalate "\n\n" (map ppProcedure procedures)

ppProcedure :: Procedure -> String
ppProcedure (Procedure _ name formals declarations statements)
  = if null decls then 
      intercalate "\n" [header, "begin", body, "end"]
    else 
      intercalate "\n" [header, decls, "begin", body, "end"]
    where
      header = concat ["proc ", name, " ", ppFormals formals]
      decls = intercalate "\n" (ppDeclarations declarations)
      body = intercalate "\n" (ppStatements statements)


--  Pretty print the list of formal parameters:

ppFormals :: [FormalArgSpec] -> String
ppFormals formals
  = '(' : (intercalate ", " (map ppFormal formals) ++ ")")

ppFormal :: FormalArgSpec -> String
ppFormal (FormalArgSpec _ passingMode argType name)
  = concat [mode, " ", baseType, " ", name]
    where
      mode = ppMode passingMode
      baseType = ppType argType
    
ppMode :: ParMode -> String
ppMode mode
  = case mode of
      Val -> "val"
      Ref -> "ref"

ppType :: BaseType -> String
ppType baseType
  = case baseType of
      BoolType  -> "bool"
      IntType   -> "int"
      FloatType -> "float"


--  Pretty print a list of declarations:

ppDeclarations :: [Decl] -> [String]
ppDeclarations 
  = indent . (map ppDecl)

ppDecl :: Decl -> String
ppDecl (Decl _ name goatType)
  = let
      (baseType, suffix) = typeDetails goatType
    in
      concat [ppType baseType, " ", name, suffix, ";"]

typeDetails :: GoatType -> (BaseType, String)
typeDetails goatType
  = case goatType of
      Base t       -> (t, "")
      Array t n    -> (t, concat ["[", show n, "]"])
      Matrix t m n -> (t, concat ["[", show m, ", ", show n,"]"])


--  Pretty printing statements:

ppStatements :: [Stmt] -> [String]
ppStatements 
  = indent . concat . (map ppStmt)

ppStmt :: Stmt -> [String]
ppStmt stmt
  = case stmt of
      Assign _ lval exp    
        -> [concat [ppLvalue lval, " := ", ppExp False exp, ";"]]

      Read _ lval          
        -> [concat ["read ", ppLvalue lval, ";"]]

      Write _ exp          
        -> [concat ["write ", ppExp False exp, ";"]]

      ProcCall _ p exps        
        -> [concat ["call ", p, "(", ppExpList exps, ");"]]

      If _ exp stmts       
        -> concat ["if ", ppExp False exp, " then"] :
             (ppStatements stmts) ++ ["fi"]

      IfElse _ exp s1s s2s 
        -> concat ["if ", ppExp False exp, " then"] :
             (ppStatements s1s) ++ ["else"] ++
             (ppStatements s2s) ++ ["fi"]

      While _ exp stmts    
        -> concat ["while ", ppExp False exp, " do"] :
             (ppStatements stmts) ++ ["od"]

ppLvalue :: Lvalue -> String
ppLvalue lvalue
  = case lvalue of
      LId _ name              
        -> name
      LArrayRef _ name exp    
        -> concat [name, "[", ppExp False exp, "]"]
      LMatrixRef _ name e1 e2 
        -> concat [name, "[", ppExp False e1, ", ", ppExp False e2, "]"]


-- Pretty printing expressions:

ppExpList :: [Expr] -> String
ppExpList exps
  = intercalate ", " (map (ppExp False) exps)

--  'ppExp nested e' prints e with surrounding parentheses iff
--  e is the application of a binary operator and 'nested' is true

ppExp :: Bool -> Expr -> String
ppExp nested exp
  = case exp of
      BoolCon _ False      
        -> "false"
      BoolCon _ True       
        -> "true"
      And _ e1 e2            
        -> concat [left, ppExp True e1, " && ", ppExp True e2, right]
           where
             left  = if nested then "(" else ""
             right = if nested then ")" else ""
      Or _ e1 e2             
        -> concat [left, ppExp True e1, " || ", ppExp True e2, right]
           where
             left  = if nested then "(" else ""
             right = if nested then ")" else ""
      Not _ e                
        -> '!' : ppExp True e
      Rel _ rel e1 e2        
        -> concat [left, ppExp True e1, ppRelop rel, ppExp True e2, right]
           where
             left  = if nested then "(" else ""
             right = if nested then ")" else ""
      IntCon _ n           
        -> show n
      FloatCon _ n         
        -> showFFloatAlt Nothing n ""
      StrCon _ str           
        -> '\"' : (str ++ "\"")
      Id _ name              
        -> name
      ArrayRef _ name exp    
        -> concat [name, "[", ppExp False exp, "]"]
      MatrixRef _ name e1 e2 
        -> concat [name, "[", ppExp False e1, ", ", ppExp False e2, "]"]
      BinOpExp _ op e1 e2    
        -> concat [left, ppExp True e1, ppBinop op, ppExp True e2, right]
           where
             left  = if nested then "(" else ""
             right = if nested then ")" else ""
      UnaryMinus _ exp       
        -> '-' : ppExp True exp

ppRelop :: Relop -> String
ppRelop rel
  = case rel of
      Op_eq -> " = "
      Op_ne -> " != "
      Op_ge -> " >= "
      Op_le -> " <= "
      Op_gt -> " > "
      Op_lt -> " < "
     
ppBinop :: Binop -> String
ppBinop op
  = case op of
      Op_add -> " + "
      Op_sub -> " - "
      Op_mul -> " * "
      Op_div -> " / "
      
