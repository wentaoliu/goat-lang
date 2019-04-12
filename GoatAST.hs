--  File     : GoatAST.hs
--  Author   : Wentao Liu, Raymond Sun, Zeyu Huang, Yiqun Wang
--  Origin   : Thu 4 Apr 2019
--  Purpose  : Specification of an AST for Goat

module GoatAST where
import Numeric
import Data.List
-----------------------------------
-- Specification of an AST for Goat
-----------------------------------

type Ident = String

data ArraySize
    = OneDimen Int
    | Matrix Int Int 
    deriving (Eq)

data ArrayIndex
    = OneDimenIndex Expr
    | MatrixIndex Expr Expr
    deriving (Eq)

data BaseType
    = BoolType | IntType | FloatType
    deriving (Eq)

data Lvalue
    = LId Ident
    | LArray Ident ArrayIndex
    deriving (Eq)

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
    deriving (Eq)

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
    deriving (Eq)

data Proc
    = Proc Ident [Param] [Decl] [Stmt]
    deriving (Show, Eq)

data ParamType 
    = Ref | Val
    deriving (Eq)
    
data Param
    = Param ParamType BaseType Ident
    deriving (Show, Eq)

data GoatProgram
    = Program [Proc]
    deriving (Show, Eq)


-----------------------------------------------------------------
--  Instantiate show for formating smaller components of a stmt
-----------------------------------------------------------------

instance Show Expr where
    show (BoolConst b)
        | b == True = "true"    
        | b == False = "false"
    show (IntConst i) = show i
    --showFFloat to print floats not as exponents 
    show (FloatConst f) = showFFloat Nothing f ""
    show (StrConst s) = "\"" ++ s ++ "\""
    show (Id id) = id
    show (Array id aindex) = id ++ show aindex
    show (BinExpr b e1 e2) = 
        intercalate " " [showWrap e1, show b, showWrap e2]
    show (UnaryExpr u e) = show u ++ show e

-- Wrap operands of an expression with parentheses
showWrap :: Expr -> String
showWrap expr = case expr of
    BinExpr b e1 e2 -> "(" ++ show expr ++ ")"
    _ -> show expr
    

instance Show Stmt where
    show (Assign lval e) = show lval ++ " := " ++ show e
    show (Read lval) = "read " ++ show lval
    show (Write e) = "write " ++ show e
    show (Call id exprs) = 
        "call " ++ id ++ 
        "(" ++ intercalate ", " (map show exprs) ++ ")"
    -- other cases should be handled by GoatPrinter.formatStmtI
    show _ = "<Stub for a (If|IfElse|While) block>"

instance Show ArraySize where
    show (OneDimen len) = "[" ++ show len ++ "]"
    show (Matrix h w) = "[" ++ show h ++ ", " ++ show w ++ "]"
    -- bug fix: space after comma

instance Show ArrayIndex where
    show (OneDimenIndex e) = "[" ++ show e ++ "]"
    show (MatrixIndex e1 e2) = "[" ++ show e1 ++ ", " ++ show e2 ++ "]"
    -- bug fix: space after comma

instance Show Lvalue where
    show (LId id) = id
    show (LArray id aindex) = id ++ show aindex

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

instance Show ParamType where
    show Ref = "ref"
    show Val = "val"
