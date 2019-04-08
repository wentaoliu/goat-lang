module GoatAST where

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
    deriving (Show, Eq)

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
    deriving (Show, Eq)
    
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
    show (BoolConst b) = show b
    show (IntConst i) = show i
    show (FloatConst f) = show f
    show (StrConst s) = show s
    show (Id id) = id
    show (Array id aindex) = id ++ show aindex
    show (BinExpr b e1 e2) = 
        joinStrings " " [showWrap e1, show b, showWrap e2]
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
        "(" ++ joinStrings ", " (map show exprs) ++ ")"
    -- other cases should be handled by GoatPrinter.formatStmtI
    show _ = "<Stub for a (If|IfElse|While) block>"

instance Show ArraySize where
    show (OneDimen len) = "[" ++ show len ++ "]"
    show (Matrix h w) = "[" ++ show h ++ "," ++ show w ++ "]"

instance Show ArrayIndex where
    show (OneDimenIndex e) = "[" ++ show e ++ "]"
    show (MatrixIndex e1 e2) = "[" ++ show e1 ++ "," ++ show e2 ++ "]"

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
                     

-- This function might be in the library already.
joinStrings :: String -> [String] -> String
joinStrings _ [] = ""
joinStrings sep (x:xs) = case xs of 
    []       -> x
    (y:ys)   -> x ++ sep ++ joinStrings sep xs