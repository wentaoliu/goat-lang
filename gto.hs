import GoatAST
import Numeric

convertExpr :: Expr -> String
convertExpr (BoolConst b) = "int_const r0, 0\n"
convertExpr (IntConst i) = "int_const r0, " ++ show i ++ "\n"
    -- showFFloat to print floats not as exponents 
convertExpr (FloatConst f) = "real_const r0, " ++ (showFFloat Nothing f "") ++ "\n"
convertExpr (StrConst s) = "string_const r0, " ++ s ++ "\n"
--some way to resolve this identifier -> a symbol table?
--  convertExpr (Id id) = id
--some way to evaluate this binary expression
--  convertExpr (BinExpr b e1 e2) = 

-- Format (Assign|Read|Write|Call) statements which does not recursively
-- contain other statements.
convertStmt :: Stmt -> String
convertStmt (Write (BoolConst e)) = (convertExpr (BoolConst e)) ++ "call_builtin print_bool"
convertStmt (Write (IntConst e)) = (convertExpr (IntConst e)) ++ "call_builtin print_int"
convertStmt (Write (FloatConst e)) = (convertExpr (FloatConst e)) ++ "call_builtin print_float"
