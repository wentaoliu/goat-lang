module GoatAST where

    -----------------------------------
    -- Specification of an AST for Goat 
    -----------------------------------
    
    type Ident = String
    type Index = Int

    data BaseType 
      = BoolType | IntType | FloatType --can we just use haskell basic types
        deriving (Show, Eq)
    
    data Lvalue 
      = LId Ident
        deriving (Show, Eq)
    
    data Binop 
      =  Op_or | Op_and |  Op_eq | Op_neq | Op_less 
        | OOp_lessq | Op_greater | Op_greaterq 
        | Op_add | Op_minus | Op_mul | Op_div 
        deriving (Show, Eq)
    
    data Unop
      = Op_not | Op_neg --do we need a separate type for unary ops? i just did this because the other type was called binop
        deriving (Show, Eq)

    data Expr
      = BoolConst Bool  
      | IntConst Int
      | StrConst String
      | FloatConst Float
      | Id Ident        --Does Id Array Ident work?
      | Binop Expr Expr
      | Unop Expr 
        deriving (Show, Eq)
    
    data Array a
      = a | a Index | a (Index, Index)

    data Decl 
      = Decl Ident BaseType
        deriving (Show, Eq)
    
    data Stmt 
      = Assign Lvalue Expr
      | Assign Lvalue Index Expr        --is it better as (Int, Int) or Int Int
      | Assign Lvalue (Index,Index) Expr  --could replace with Array Expr? does this even work?
      | Read Lvalue
      | Read Lvalue Index       
      | Read Lvalue (Index, Index)
      | Write Expr
      | Call Ident [Expr]
      | If Expr [Stmt] 
      | IfElse Expr [Stmt] [Stmt]
      | While Expr [Stmt]
        deriving (Show, Eq)
    
    data GoatProgram
      = Program [Decl] [Stmt]
        deriving (Show, Eq)
    
    