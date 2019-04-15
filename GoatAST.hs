{-
File: GoatAST.hs
Origin: Thu 4 Apr 2019
Purpose: Data structure of an AST for Goat
    - This program defines the data types for a Goat AST.
    - It also specifies the instantiation of the show method for "atomic" types,
      which can be utilized by GoatPrinter.hs.
-}

module GoatAST where
import Numeric
import Data.List

-----------------------------------
-- Specification of an AST for Goat
-----------------------------------

-- Identifier
type Ident = String

-- Array size of shape [n] or [m, n]
-- This differs from ArrayIndex as it is only used in declaration.
data ArraySize
    = OneDimen Int
    | Matrix Int Int 
    deriving (Eq)

-- Array index of shape [expr] or [expr1, expr2]
data ArrayIndex
    = OneDimenIndex Expr
    | MatrixIndex Expr Expr
    deriving (Eq)

data BaseType
    = BoolType | IntType | FloatType
    deriving (Eq)

data Var
    = VId Ident
    | VArray Ident ArrayIndex
    deriving (Eq)

-- Binary operators
data Binop
    = Op_or | Op_and | Op_add | Op_mul | Op_sub | Op_div | Op_eq |     
    Op_ne | Op_gt | Op_gte | Op_lt | Op_lte
    deriving (Enum, Eq)

-- Unary operators
data UnaryOp
    = Op_uneg | Op_umin
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
    deriving (Eq)

data Decl
    = BaseDecl Ident BaseType
    | ArrayDecl Ident ArraySize BaseType
    deriving (Show, Eq)

data Stmt
    = Assign Var Expr
    | Read Var
    | Write Expr
    | Call Ident [Expr]
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    deriving (Eq)

data Proc
    = Proc Ident [Param] [Decl] [Stmt]
    deriving (Show, Eq)

data PassType 
    = Ref | Val
    deriving (Eq)
    
data Param
    = Param PassType BaseType Ident
    deriving (Show, Eq)

data GoatProgram
    = Program [Proc]
    deriving (Show, Eq)


----------------------------------------------------------------------
--  Instantiate Show for formating smaller components of an Goat AST.
--  These Show instances can be utilized in GoatPrinter where smaller 
--  components are organized into statements, declarations, etc.
----------------------------------------------------------------------

instance Show Expr where
    show (BoolConst b)
        | b == True = "true"    
        | b == False = "false"
    show (IntConst i) = show i
    -- showFFloat to print floats not as exponents 
    show (FloatConst f) = showFFloat Nothing f ""
    -- String literals are wrapped with quotation marks
    show (StrConst s) = "\"" ++ s ++ "\""
    show (Id id) = id
    show (Array id aindex) = id ++ show aindex
    show (BinExpr b e1 e2) = 
        intercalate " " [showWrap e1, show b, showWrap e2]
    show (UnaryExpr u e) = show u ++ showWrap e

-- Wrap operands of an expression with parentheses if it is a BinExpr
showWrap :: Expr -> String
showWrap expr = case expr of
    BinExpr b e1 e2 -> "(" ++ show expr ++ ")"
    _ -> show expr
    
-- Format (Assign|Read|Write|Call) statements which does not recursively
-- contain other statements.
instance Show Stmt where
    show (Assign lval e) = show lval ++ " := " ++ show e ++ ";\n"
    show (Read lval) = "read " ++ show lval ++ ";\n"
    show (Write e) = "write " ++ show e  ++ ";\n"
    show (Call id exprs) = 
        "call " ++ id ++ 
        "(" ++ intercalate ", " (map show exprs) ++ ");\n"
    -- Other cases should be handled by GoatPrinter.formatStmtI
    -- Thus the code below is only for debug purpose.
    show _ = "<Stub for a (If|IfElse|While) block>"

instance Show ArraySize where
    show (OneDimen len) = "[" ++ show len ++ "]"
    show (Matrix h w) = "[" ++ show h ++ ", " ++ show w ++ "]"

instance Show ArrayIndex where
    show (OneDimenIndex e) = "[" ++ show e ++ "]"
    show (MatrixIndex e1 e2) = "[" ++ show e1 ++ ", " ++ show e2 ++ "]"

instance Show Var where
    show (VId id) = id
    show (VArray id aindex) = id ++ show aindex

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
                        
instance Show BaseType where
    show BoolType = "bool"
    show IntType = "int"
    show FloatType = "float"

instance Show PassType where
    show Ref = "ref"
    show Val = "val"
