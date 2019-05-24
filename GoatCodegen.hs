module GoatCodegen where

import GoatAST
import GoatSymTable
import qualified Data.Map as Map
import Control.Monad

type StackSlot = Int
type Label = String
type LabelCounter = Int
type MemSize = Int
type Code = [Instruction]
type Instruction = String
type Stack = [Int]

data State = State Reg Code Symbols StackSlot LabelCounter
data Codegen a = Codegen (State -> (a, State))
instance Monad Codegen where
    return x = Codegen (\s -> (x, s))
    Codegen f >>= fn = Codegen (\s ->
        let
            (x, s') = f s
            Codegen f' = fn x
        in
            f' s')

instance Functor Codegen where
    fmap = Control.Monad.liftM
instance Applicative Codegen where
    pure = return
    (<*>) = Control.Monad.ap


-- helper functions for operations on monad
initState :: State
initState = State 0 [] initSymbols (-1) (-1)

setRegister :: Reg -> Codegen ()
setRegister r' = Codegen (\(State r c s sl l)
    -> ((), State r' c s sl l))

resetRegister :: Codegen Reg
resetRegister = Codegen (\(State r c s sl l)
    -> (0, State 0 c s sl l))

resetStack :: Codegen StackSlot
resetStack = Codegen (\(State r c s sl l)
    -> ((-1), State r c s (-1) l))

regZero :: Reg
regZero = 0

nextRegister :: Codegen Reg
nextRegister = Codegen (\(State r c s sl l)
    -> (r + 1, State (r + 1) c s sl l))

nextSlot :: Codegen StackSlot
nextSlot = Codegen (\(State r c s sl l)
    -> (sl + 1, State r c s (sl + 1) l))

-- "allocate" a chunk of stackslots given size
-- return the final stack slot position
nextSlotMulti :: MemSize -> Codegen StackSlot
nextSlotMulti n
    | n <= 0 = error ""
    | n == 1 = nextSlot
    | n > 1 = do
        sl <- nextSlot
        nextSlotMulti $ n - 1
        return sl
    
nextLabelCounter :: Codegen LabelCounter
nextLabelCounter = Codegen (\(State r c s sl l)
    -> (l + 1, State r c s sl (l + 1)))

nextLabel :: Codegen Label
nextLabel = nextLabelCounter >>= (\l -> return ("label" ++ show l))


writeCode :: Instruction -> Codegen ()
writeCode inst = Codegen (\(State r code s sl l)
    -> ((), State r (code ++ [inst]) s sl l))

writeComment :: String -> Codegen ()
writeComment str = writeCode $ "  # " ++ str

writeLabel :: Label -> Codegen ()
writeLabel str = writeCode $ str ++ ":"

writeInstruction :: String -> [String] -> Codegen ()
writeInstruction name [] = writeCode name
writeInstruction name args =
    writeCode $ "    " ++ name ++ " " ++ (strJoin ", " args)


showReg :: Reg -> String
showReg r = "r" ++ show r

getRegType :: Reg -> Codegen (GoatType)
getRegType reg = Codegen (\(State r c symbols sl l) ->
    (lookupRegType reg symbols, State r c symbols sl l))

putRegType :: Reg -> GoatType -> Codegen ()
putRegType reg typ = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertRegType reg typ symbols) sl l))

resetVariables :: Codegen ()
resetVariables = Codegen (\(State r c symbols sl l) ->
    ((), State r c (clearVariables symbols) sl l))

getVariable :: String -> Codegen (Bool, GoatType, Int)
getVariable name = Codegen (\(State r c symbols sl l) ->
    (lookupVariable name symbols, State r c symbols sl l))

putVariable :: String -> (Bool, GoatType, Int) -> Codegen ()
putVariable name val = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertVariable name val symbols) sl l))

getArrayBounds :: String -> Codegen (Int, Int)
getArrayBounds name = Codegen (\(State r c symbols sl l) ->
    (lookupArrayBounds name symbols, State r c symbols sl l))

putArrayBounds :: String -> (Int, Int) -> Codegen ()
putArrayBounds name val = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertArrayBounds name val symbols) sl l))

putProcedure :: String -> [(Bool, GoatType)] -> Codegen ()
putProcedure name params = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertProcedure name params symbols) sl l))

getProcedure :: String -> Codegen ([(Bool, GoatType)])
getProcedure name = Codegen (\(State r c symbols sl l) ->
    (lookupProcedure name symbols, State r c symbols sl l))

strJoin :: String -> [String] -> String
strJoin _ [] = ""
strJoin _ [x] = x
strJoin sep (x:y:zs) = x ++ sep ++ (strJoin sep (y:zs))

cgJoin :: [Codegen ()] -> Codegen ()
cgJoin [] = return ()
cgJoin (x:xs) = x >> (cgJoin xs)

cgFoldr :: (a -> b -> b) -> b -> [Codegen (a)] -> Codegen (b)
cgFoldr _ val [] = return val
cgFoldr fn val (x:xs) = do
    v <- x
    let val' = fn v val
    cgFoldr fn val' xs

printSepBy :: IO () -> [IO ()] -> IO ()
printSepBy _ [] = return ()
printSepBy _ [x] = x
printSepBy sep (x:y:zs) =
    x >> sep >> (printSepBy sep (y:zs))

cgPushStackFrame :: MemSize -> Codegen ()
cgPushStackFrame size =
    writeInstruction "push_stack_frame" [show size]

cgPopStackFrame :: MemSize -> Codegen ()
cgPopStackFrame size =
    writeInstruction "pop_stack_frame" [show size]

generateCode :: Program -> IO ()
generateCode prog = do
    let Codegen fn = cgProgram prog
    let (_, finalState) = fn initState
    let State _ instructions _ _ _ = finalState
    printSepBy (putStr "\n") (map putStr instructions)


-- here starts functions for code generation
cgProgram :: Program -> Codegen ()
cgProgram (Program procs) = do
    writeCode "    call proc_main"
    writeCode "    halt"
    -- put all procedures into symbol table
    -- cgJoin $ map cgPrepareProcedure procs
    -- -- all procedures
    cgJoin $ map cgProcedure procs
    writeCode "    return"

-- generate code for each procedure
cgProcedure :: Procedure -> Codegen()
cgProcedure (Procedure pos ident args decls stmts) = do
    writeComment ("procedure " ++ ident)
    writeLabel ("proc_" ++ ident)
    resetVariables
    resetStack
    cgProcedure' args decls stmts

cgProcedure' :: [FormalArgSpec] -> [Decl] -> [Stmt] -> Codegen ()
cgProcedure' [] decls stmts = do
    size <- cgDeclarationPart decls
    -- generate function body
    cgPushStackFrame size
    cgCompoundStatement stmts
    cgPopStackFrame size


-- generate code for variable declaration part, return
-- number of stack slots required
cgDeclarationPart :: [Decl] -> Codegen (MemSize)
cgDeclarationPart decls = do
    writeComment "declaration part"
    cgFoldr (+) 0 $ map (cgDeclaration False) decls

-- generate code for a variable declaration, also used
-- to handle parameters in a procedure
cgDeclaration :: Bool -> Decl -> Codegen (MemSize)
cgDeclaration varness (Decl _ ident typ) = do
    writeComment ("declaration " ++ ident)
    case typ of
        -- ArrayTypeDeno`ter arrayType -> cgArrayType i arrayType
        Base baseType -> do
            sl <- nextSlot
            putVariable ident (varness, typ, sl) 
            return 1 -- all primitives have size 1


cgCompoundStatement :: [Stmt] -> Codegen ()
cgCompoundStatement stmt = do
    writeComment "compound statement"
    cgCompoundStatement' stmt

cgCompoundStatement' :: [Stmt] -> Codegen ()
cgCompoundStatement' [] = return ()
cgCompoundStatement' (x:xs) = do
    r <- resetRegister
    cgStatement x
    cgCompoundStatement' xs

cgStatement :: Stmt -> Codegen ()
cgStatement (Write _ expr) = cgWriteStatement expr
cgStatement (If _ expr stmts) = cgIfStatement expr stmts
cgStatement (IfElse _ expr stmts1 stmts2) = cgIfElseStatement expr stmts1 stmts2
cgStatement (While _ expr stmts) = cgWhileStatement expr stmts
cgStatement otherExpr = error ("do not use this statement " ++ (show otherExpr))

cgWriteStatement :: Expr -> Codegen ()
cgWriteStatement expr = do
    writeComment "write"
    (reg, typ) <- cgExpression expr
    func <- case typ of
        IntType     -> return "print_int"
        FloatType   -> return "print_real"
        BoolType    -> return "print_bool"
        StringType  -> return "print_string"
    if showReg reg /= "r0"
        then writeInstruction "move" ["r0", showReg reg]
        else return ()
    writeInstruction "call_builtin" [func]


cgIfStatement :: Expr -> [Stmt] -> Codegen ()
cgIfStatement expr stmts = do
    writeComment "if statement"
    afterLabel <- nextLabel
    r <- nextRegister
    -- expression
    cgExpression expr
    writeInstruction "branch_on_false" [showReg r, afterLabel]
    -- if part
    cgCompoundStatement stmts
    writeInstruction "branch_uncond" [afterLabel]
    writeLabel afterLabel


cgIfElseStatement :: Expr -> [Stmt] -> [Stmt] -> Codegen()
cgIfElseStatement expr stmts1 stmts2 = do
    writeComment "if-else statement"
    elseLabel <- nextLabel
    afterLabel <- nextLabel
    r <- nextRegister
    -- expression
    cgExpression expr
    writeInstruction "branch_on_false" [showReg r, elseLabel]
    -- if part
    cgCompoundStatement stmts1
    writeInstruction "branch_uncond" [afterLabel]
    -- else part
    writeLabel elseLabel
    cgCompoundStatement stmts2
    writeLabel afterLabel


cgWhileStatement :: Expr -> [Stmt] -> Codegen ()
cgWhileStatement expr stmts = do
    writeComment "while statement"
    beginLabel <- nextLabel
    afterLabel <- nextLabel
    writeLabel beginLabel
    r <- nextRegister
    -- while expression
    cgExpression expr
    writeInstruction "branch_on_false" [showReg r, afterLabel]
    -- while body
    cgCompoundStatement stmts
    writeInstruction "branch_uncond" [beginLabel]
    writeLabel afterLabel



---------------------------
-- zeyu's part: expressions
---------------------------

-- data ExprType = SimpleExprType
--               | LogicExprType 
--               | RelExprType 
--               | IdExprType
--               | ArithExprType

cgExpression :: Expr -> Codegen (Reg, BaseType)
-- const access
cgExpression (BoolCon _ bool) = do
    reg <- nextRegister
    case bool of
        True      -> writeInstruction "int_const"  [showReg reg, "1"]
        False     -> writeInstruction "int_const"  [showReg reg, "0"]
    putRegType reg (Base BoolType)
    return (reg, BoolType)
cgExpression (IntCon _ int) = do
    reg <- nextRegister
    writeInstruction "int_const"  [showReg reg, show int]
    putRegType reg (Base IntType)
    return (reg, IntType)
cgExpression (FloatCon _ float) = do
    reg <- nextRegister
    writeInstruction "real_const" [showReg reg, show float]
    putRegType reg (Base FloatType)
    return (reg, FloatType)
cgExpression (StrCon _ str) = do
    reg <- nextRegister
    writeInstruction "string_const" [showReg reg, show str]
    putRegType reg (Base StringType)
    return (reg, StringType)

-- unary operators
-- UnaryMinus Pos Expr
cgExpression (UnaryMinus _ expr) = do
    (reg, typ) <- cgExpression expr
    func <- case typ of
        IntType -> return "neg_int"
        FloatType -> return "neg_real"
        BoolType -> error $ "expected integer or real, found boolean"
    writeInstruction func [showReg reg, showReg reg]
    return (reg, typ)

-- Not Pos Expr
cgExpression (Not _ expr) = do
    (reg, typ) <- cgExpression expr
    case typ of
        BoolType -> writeInstruction "not" [showReg reg, showReg reg]
        otherwise -> error $ "expected bool, found " ++ show typ
    return (reg, BoolType)

-- And | Or
cgExpression (And _ expr1 expr2) = do
    (reg1, typ1) <- cgExpression expr1
    (reg2, typ2) <- cgExpression expr2
    cgPrepareLogical reg1 reg2
    writeInstruction "and" [showReg reg1, showReg reg1, showReg reg2]
    return (reg1, BoolType) -- (typ1 == typ2 == BoolType)

cgExpression (Or _ expr1 expr2) = do
    (reg1, typ1) <- cgExpression expr1
    (reg2, typ2) <- cgExpression expr2
    cgPrepareLogical reg1 reg2
    writeInstruction "or" [showReg reg1, showReg reg1, showReg reg2]
    return (reg1, BoolType) -- (typ1 == typ2 == BoolType)

-- Rel Pos Relop Expr Expr
cgExpression (Rel _ relop expr1 expr2) = do
    (reg1, typ1) <- cgExpression expr1
    (reg2, typ2) <- cgExpression expr2
    optype <- cgPrepareComparison reg1 reg2
    let relopInstruction = 
            case relop of
            Op_eq -> "cmp_eq_"
            Op_ne -> "cmp_ne_"
            Op_ge -> "cmp_ge_"
            Op_le -> "cmp_le_"
            Op_gt -> "cmp_gt_"
            Op_lt -> "cmp_lt_"
    let relopType = 
            case optype of 
            OpType IntType -> "int"
            OpType FloatType -> "real"
    writeInstruction (relopInstruction ++ relopType)
                     [showReg reg1, showReg reg1, showReg reg2]
    -- return (reg1, fromOpType optype)
    return (reg1, BoolType)

-- Id Pos Ident
-- getVariable :: String -> Codegen (Bool, BaseType, Int)
cgExpression (Id _ ident) = do
    
    (isRef, goattype, addr) <- getVariable ident
    case (isRef, goattype) of 
        (False, Base btype) -> 
            do
                reg <- nextRegister
                writeInstruction "load" [showReg reg, show addr]
                putRegType reg (Base btype)
                return (reg, btype)
        (True, Base btype)  -> 
            do
                reg <- nextRegister
                writeInstruction "load_indirect" [showReg reg, show addr]
                putRegType reg (Base btype)
                return (reg, btype)
        _                   -> 
            error ("variable " ++ show ident ++ " cannot be loaded.") 

-- Arithmetic expressions
-- BinOpExp Pos Binop Expr Expr
cgExpression (BinOpExp _ binop expr1 expr2) = do
    (reg1, typ1) <- cgExpression expr1
    (reg2, typ2) <- cgExpression expr2
    optype <- cgPrepareArithmetic reg1 reg2
    let binopInstruction = 
            case binop of
            Op_add -> "add_"
            Op_sub -> "sub_"
            Op_mul -> "mul_"
            Op_div -> "div_"
    let binopType = 
            case optype of 
            OpType IntType -> "int"
            OpType FloatType -> "real"
    writeInstruction (binopInstruction ++ binopType)
                     [showReg reg1, showReg reg1, showReg reg2]
    return (reg1, fromOpType optype)

-- Arrays and Matrices
-- As specified in Goat language description
--     ident denotes a local variable (it cannot come from parameter passing)
--     expr should have GoatType (Base IntType)
cgExpression (ArrayRef _ ident expr) = do
    (regRowIndex, btype) <- cgExpression expr
    (isRef, gtype, addr) <- getVariable ident
    case (btype, gtype) of
        (IntType, Array abtype _) ->
            do
            regAddr <- nextRegister
            writeInstruction "load_address" [showReg regAddr, show addr]
            writeInstruction "sub_offset" [showReg regAddr, showReg regAddr,
                                           showReg regRowIndex]
            writeInstruction "load_indirect" [showReg regAddr, showReg regAddr]
            putRegType regAddr (Base abtype)
            return (regAddr, abtype)
        _  ->
            error ("Index [" ++ (show expr) ++ "] of array " ++ (show ident) ++
                  " cannot be loaded.")
            
cgExpression (MatrixRef _ ident rowExpr colExpr) = do
    (regRowIndex, rowbtype) <- cgExpression rowExpr
    (regColIndex, colbtype) <- cgExpression colExpr
    (isRef, gtype, addr) <- getVariable ident -- isRef is always false
    case (rowbtype, colbtype, gtype) of
        (IntType, IntType, Matrix mbtype _ _) ->
            do
            regAddr <- nextRegister
            writeInstruction "load_address" [showReg regAddr, show addr]
            flattenMatrixIndex regRowIndex regColIndex gtype
            writeInstruction "sub_offset" [showReg regAddr, showReg regAddr,
                                           showReg regRowIndex]
            writeInstruction "load_indirect" [showReg regAddr, showReg regAddr]
            putRegType regAddr (Base mbtype)
            return (regAddr, mbtype)
        _  ->
            error ("Index [" ++ (show rowExpr) ++ ", " ++ (show colExpr) ++
                   "] of matrix " ++ (show ident) ++ " cannot be loaded.")

flattenMatrixIndex :: Reg -> Reg -> GoatType -> Codegen ()
flattenMatrixIndex regRowIndex regColIndex (Matrix mbtype rows cols) = do
    -- flattened index = rowIndex * #cols + colIndex
    regCols <- nextRegister -- (Base IntType)
    writeInstruction "int_const" [show regCols, show cols]
    putRegType regCols (Base IntType)
    writeInstruction "mul_int" [show regRowIndex, show regRowIndex,
                                show regCols]
    writeInstruction "add_int" [show regRowIndex, show regRowIndex,
                                show regColIndex]
    -- return ()

-- data OperatorType = IntOp | RealOp
data OpType = OpType BaseType

-- deconstruct OpType to get BaseType
fromOpType :: OpType -> BaseType
fromOpType (OpType b) = b

-- check types of both operands, do type casting if necessary, report final type
cgPrepareArithmetic :: Reg -> Reg -> Codegen (OpType)
cgPrepareArithmetic r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (Base FloatType, Base FloatType) -> return $ OpType FloatType
        (Base IntType,   Base FloatType) -> do
                                            cgIntToReal r1
                                            return $ OpType FloatType
        (Base FloatType, Base IntType  ) -> do
                                            cgIntToReal r2
                                            return $ OpType FloatType
        (Base IntType,   Base IntType  ) -> return $ OpType IntType
        _ -> error $ "arithmetic/comparision cannot be done between " ++
                     (show t1) ++ " and " ++ (show t2)

-- helper function for cgPrepareArithmetic
cgIntToReal :: Reg -> Codegen ()
cgIntToReal reg = do
    writeInstruction "int_to_real" [showReg reg, showReg reg]
    putRegType reg (Base FloatType)


-- check that logical expressions involve two booleans
cgPrepareLogical :: Reg -> Reg -> Codegen ()
cgPrepareLogical r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (Base BoolType, Base BoolType) -> return ()
        _ -> error $ "logical operation cannot be done between " ++
                     (show t1) ++ " and " ++ (show t2)

cgPrepareComparison :: Reg -> Reg -> Codegen (OpType)
cgPrepareComparison = cgPrepareArithmetic



-- reserve registers for arguments to be passed
-- cgAllocateRegs :: [ASTExpression] -> Codegen ()
-- cgAllocateRegs [] = return ()
-- cgAllocateRegs (_:xs) = nextRegister >> (cgAllocateRegs xs)

-- -- generate code to pass variables according to varnesses that callee specifies
-- cgPassArgument
--     :: Reg -> [ASTExpression] -> [(Bool, ASTTypeDenoter)] -> Codegen ()
-- cgPassArgument _ (_:_) [] = error "num of arguments incorrect"
-- cgPassArgument _ [] (_:_) = error "num of arguments incorrect"
-- cgPassArgument _ [] [] = return ()
-- cgPassArgument r (a:as) ((v, vt):ps) = do
--     if v then
--         -- pass by reference
--         cgVariableReference a vt r
--     else do
--         -- pass by value
--         cgExpression a r
--         at <- getRegType r
--         cgPrepareAssignment vt (r, at)
--     cgPassArgument (r+1) as ps


