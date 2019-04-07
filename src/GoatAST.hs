module GoatAST where

-----------------------------------
-- Specification of an AST for Goat
-----------------------------------

type Ident = String

data ArraySize
  = OneDimen Int
  | Matrix Int Int 
    deriving (Show, Eq)

data ArrayIndex
  = OneDimenIndex Expr
  | MatrixIndex Expr Expr
    deriving (Show, Eq)

data BaseType
    = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue
    = LId Ident
    | LArray Ident ArrayIndex
    deriving (Show, Eq)

data Binop
    = Op_or | Op_and | Op_add | Op_mul | Op_sub | Op_div | Op_eq |     
    Op_ne | Op_gt | Op_gte | Op_lt | Op_lte
    -- deriving (Show, Enum, Eq)
    deriving (Enum, Eq)

data UnaryOp
    = Op_uneg | Op_umin
    -- deriving (Show, Enum, Eq)
    deriving (Enum, Eq)

data Expr
    = BoolConst Bool
    | IntConst Int
    | FloatConst Float
    | StrConst String
    | Id Ident
    | Array Ident ArrayIndex
    | BinExpr Binop Expr Expr
    | UnaryExpr UnaryOp Expr
    deriving (Show, Eq)

data Decl
    = BaseDecl Ident BaseType
    | ArrayDecl Ident ArraySize BaseType
    deriving (Show, Eq)

data Stmt
    = Assign Lvalue Expr
 -- | Assign Lvalue ArrayIndex Expr
    | Read Lvalue
 -- | Read Lvalue ArrayIndex
    | Write Expr
    | Call Ident [Expr]
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    deriving (Show, Eq)

data Proc
    = Proc Ident [Param] [Decl] [Stmt]
    deriving (Show, Eq)

data ParamType 
  = Ref | Val
    deriving (Show, Eq)
    
data Param
  = Param ParamType BaseType Ident
    deriving (Show, Eq)

data GoatProgram
  = Program [Proc]
    deriving (Show, Eq)


instance Show Binop where
    show Op_or  = "||"
    show Op_and = "&&"
    show Op_add = "+"
    show Op_sub = "-"
    show Op_div = "/"
    show Op_mul = "*"
    show Op_eq  = "="
    show Op_ne  = "!="
    show Op_gt  = ">"
    show Op_gte = ">="
    show Op_lt  = "<"
    show Op_lte = "<="

instance Show UnaryOp where
    show Op_uneg = "!"
    show Op_umin = "-"