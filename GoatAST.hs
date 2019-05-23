module GoatAST where

---------------------------------------------------------------------------
--  Specification of an AST for Goat 
--
--  Harald Sondergaard, April 2019
-- 
--  The constructors for the AST are mainly self-explanatory. We leave 
--  open the possibility of an expression 'ToFloat e', in preparation for 
--  static semantic analysis; that is, we intend to let the analyzer 
--  change the AST when it sees a need for integer-to-float conversion.
--
--  We also decorate the tree with source code position information. 
--  A 'pos' of type Pos is a pair (line, column). This way the semantic
--  analysis can give meaningful error messages, without the parser
--  having to construct a symbol table - that can be left to the 
--  analysis stage.
---------------------------------------------------------------------------

type Ident = String

type Pos
  = (Int,Int)
 
data BaseType 
  = BoolType | IntType | FloatType | StringType
    deriving (Show,Eq)

data GoatType 
  = Base BaseType
  | Array BaseType Int
  | Matrix BaseType Int Int
    deriving (Show,Eq)

data Lvalue 
  = LId Pos Ident 
  | LArrayRef Pos Ident Expr 
  | LMatrixRef Pos Ident Expr Expr 
    deriving (Show,Eq)

data Binop 
  = Op_add | Op_sub | Op_mul | Op_div 
    deriving (Show,Eq)

data Relop
  = Op_eq | Op_ne | Op_ge | Op_le | Op_gt | Op_lt
    deriving (Show,Eq)

data Expr
  = BoolCon Pos Bool
  | And Pos Expr Expr
  | Or Pos Expr Expr
  | Not Pos Expr
  | Rel Pos Relop Expr Expr
  | IntCon Pos Int
  | FloatCon Pos Float
  | StrCon Pos String
  | Id Pos Ident
  | ArrayRef Pos Ident Expr
  | MatrixRef Pos Ident Expr Expr
  | BinOpExp Pos Binop Expr Expr
  | UnaryMinus Pos Expr
    deriving (Show,Eq)

data Decl 
  = Decl Pos Ident GoatType
    deriving (Show,Eq)

data Stmt 
  = Assign Pos Lvalue Expr
  | Read Pos Lvalue
  | Write Pos Expr
  | ProcCall Pos Ident [Expr]
  | If Pos Expr [Stmt]
  | IfElse Pos Expr [Stmt] [Stmt]
  | While Pos Expr [Stmt]
    deriving (Show,Eq)

data ParMode 
  = Val | Ref
    deriving (Show,Eq)

data FormalArgSpec
  = FormalArgSpec Pos ParMode BaseType Ident
    deriving (Show,Eq)

data Procedure 
  = Procedure Pos Ident [FormalArgSpec] [Decl] [Stmt]
    deriving (Show,Eq)

data Program 
  = Program [Procedure]
    deriving (Show,Eq)
 
