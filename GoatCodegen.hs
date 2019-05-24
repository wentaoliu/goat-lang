module GoatCodegen where

import GoatAST
import GoatSymTable
import PrettyPrinter
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
initState = State (-1) [] initSymbols (-1) (-1)

setRegister :: Reg -> Codegen ()
setRegister r' = Codegen (\(State r c s sl l)
    -> ((), State r' c s sl l))

resetRegister :: Codegen Reg
resetRegister = Codegen (\(State r c s sl l)
    -> (0, State (-1) c s sl l))

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
nextLabel = nextLabelCounter >>= (\l -> return ("label_" ++ show l))


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
    cgJoin $ map cgPrepareProcedure procs
    -- -- all procedures
    cgJoin $ map cgProcedure procs

--robin
cgPrepareProcedure :: Procedure -> Codegen ()
cgPrepareProcedure (Procedure _ ident args _ _) = do
    putProcedure ident (bareParameters args)

    
bareParameters :: [FormalArgSpec] -> [(Bool, GoatType)]
bareParameters args = map bareParameters' args

bareParameters' :: FormalArgSpec -> (Bool, GoatType)
bareParameters' (FormalArgSpec _ Val typ _) = (False, Base typ)
bareParameters' (FormalArgSpec _ Ref typ _) = (True, Base typ)
            
            
-- generate code to put procedure arguments into stack slots
cgStoreArg :: Reg -> StackSlot -> [FormalArgSpec] -> Codegen ()
cgStoreArg _ _ [] = return ()
cgStoreArg r sl (_:xs) = do
    writeInstruction "store" [show sl, showReg r]
    cgStoreArg (r+1) (sl+1) xs

cgFormalParameterList :: [FormalArgSpec] -> Codegen (MemSize)
cgFormalParameterList args = do
    writeComment "formal parameter section"

    cgFoldr (+) 0 $ map cgProcessSection args

cgProcessSection :: FormalArgSpec -> Codegen (MemSize)
cgProcessSection (FormalArgSpec pos Val baseType ident) = cgDeclaration False (Decl pos ident (Base baseType))
cgProcessSection (FormalArgSpec pos Ref baseType ident) = cgDeclaration True (Decl pos ident (Base baseType))
--robin


-- generate code for each procedure
cgProcedure :: Procedure -> Codegen()
cgProcedure (Procedure pos ident args decls stmts) = do
    writeLabel ("proc_" ++ ident)
    writeComment ("procedure " ++ ident)
    resetVariables
    resetStack
    cgProcedure' args decls stmts
    

cgProcedure' :: [FormalArgSpec] -> [Decl] -> [Stmt] -> Codegen ()
cgProcedure' [] decls stmts = do
    size <- cgDeclarationPart decls
    -- generate function body
    cgPushStackFrame size
    cgInitVariablePart decls
    cgCompoundStatement stmts
    cgPopStackFrame size
    writeCode "    return"
--robin    
cgProcedure' args decls stmts = do
    -- prepare variables and params
    size <- cgFormalParameterList args
    size2 <- cgDeclarationPart decls
    -- generate function body
    cgPushStackFrame (size + size2)
    cgStoreArg 0 0 args
    cgInitVariablePart decls
    cgCompoundStatement stmts
    cgPopStackFrame (size + size2)
    writeCode "    return"
--robin  


-- generate code for variable declaration part, return
-- number of stack slots required
cgDeclarationPart :: [Decl] -> Codegen (MemSize)
cgDeclarationPart decls = do
    cgFoldr (+) 0 $ map (cgDeclaration False) decls

-- generate code for a variable declaration, also used
-- to handle parameters in a procedure
cgDeclaration :: Bool -> Decl -> Codegen (MemSize)
cgDeclaration varness (Decl _ ident typ) = do
    case typ of
        -- ArrayTypeDeno`ter arrayType -> cgArrayType i arrayType
        Base baseType -> do
            sl <- nextSlot
            putVariable ident (varness, typ, sl) 
            return 1 -- all primitives have size 1
                --robin
        Array baseType n -> do
            sl <- nextSlotMulti n
            -- putVariable stores the slot number of the beginning of array
            putVariable ident (varness, typ, sl)
            return n
        Matrix baseType m n -> do
            let size = m*n
            sl <- nextSlotMulti size
            -- putVariable stores the slot number of the beginning of matrix
            putVariable ident (varness, typ, sl)
            return size
        --robin

-- initialize all variables in declaration part
cgInitVariablePart  :: [Decl] -> Codegen()
cgInitVariablePart  decls = do
    cgJoin $ map cgInitVariable decls
    
cgInitVariable :: Decl -> Codegen()
cgInitVariable (Decl _ ident typ) = do
    (_, _, sl) <- getVariable ident
    case typ of
        Base baseType -> do
            case baseType of
                BoolType -> do 
                    writeComment ("initialize bool val " ++ ident)
                    cgInitInt sl 1
                IntType -> do
                    writeComment ("initialize int val " ++ ident)
                    cgInitInt sl 1
                FloatType -> do
                    writeComment ("initialize float val " ++ ident)
                    cgInitFloat sl 1
        Array baseType n -> do
            case baseType of
                BoolType -> do
                    writeComment ("initialize bool val " ++ ident ++ "[" ++ show n ++ "]")
                    cgInitInt sl n
                IntType -> do
                    writeComment ("initialize int val " ++ ident ++ "[" ++ show n ++ "]")
                    cgInitInt sl n
                FloatType -> do
                    writeComment ("initialize float val " ++ ident ++ "[" ++ show n ++ "]")
                    cgInitFloat sl n
        Matrix baseType m n -> do
            let size = m*n
            case baseType of
                BoolType -> do
                    writeComment ("initialize bool val "  ++ ident ++ "[" ++ show m ++ "," ++ show n ++ "]")
                    cgInitInt sl size
                IntType -> do
                    writeComment ("initialize int val "  ++ ident ++ "[" ++ show m ++ "," ++ show n ++ "]")
                    cgInitInt sl size
                FloatType -> do
                    writeComment ("initialize float val "  ++ ident ++ "[" ++ show m ++ "," ++ show n ++ "]")
                    cgInitFloat sl size


-- initialize int and bool variable, array and matrix
cgInitInt :: Int -> Int -> Codegen()
cgInitInt sl 0 = return ()
cgInitInt sl size = do
    writeInstruction "int_const"  ["r0", "0"]
    writeInstruction "store" [show sl, "r0"]
    cgInitInt (sl+1) (size-1)

-- initialize float variable, array and matrix
cgInitFloat :: Int -> Int -> Codegen()
cgInitFloat sl 0 = return ()
cgInitFloat sl size = do
    writeInstruction "real_const"  ["r0", "0.0"]
    writeInstruction "store" [show sl, "r0"]
    cgInitFloat (sl+1) (size-1)


-- generate code to handle parameters in a procedure
cgFormalArgDeclaration :: Bool -> Decl -> Codegen (MemSize)
cgFormalArgDeclaration varness (Decl _ ident typ) = do
    case typ of
        Base baseType -> do
            sl <- nextSlot
            putVariable ident (varness, typ, sl) 
            return 1 -- all primitives have size 1


cgCompoundStatement :: [Stmt] -> Codegen ()
cgCompoundStatement stmt = do
    cgCompoundStatement' stmt

cgCompoundStatement' :: [Stmt] -> Codegen ()
cgCompoundStatement' [] = return ()
cgCompoundStatement' (x:xs) = do
    resetRegister
    writeComment $ (ppStmt x)!!0
    cgStatement x
    cgCompoundStatement' xs

cgStatement :: Stmt -> Codegen ()
cgStatement (Write _ expr) = cgWriteStatement expr
cgStatement (If _ expr stmts) = cgIfStatement expr stmts
cgStatement (IfElse _ expr stmts1 stmts2) = cgIfElseStatement expr stmts1 stmts2
cgStatement (While _ expr stmts) = cgWhileStatement expr stmts
cgStatement (Assign _ lvalue expr) = cgAssignmentStatement lvalue expr
cgStatement (Read _ lvalue) = cgReadStatement lvalue
cgStatement (ProcCall _ ident exprs) = cgProcCallStatement ident exprs

-- generate code to call a procedure. Put arguments in registers starting at r0
cgProcCallStatement :: Ident -> [Expr] -> Codegen ()
cgProcCallStatement ident argExprs = do
    params <- getProcedure ident
    if length params /= length argExprs
        then error $ "expected " ++ (show $ length params) ++ " parameter(s), found " ++ (show $ length argExprs)
        else cgArgumentList 0 params argExprs
    writeInstruction "call" ["proc_" ++ ident]

-- evaluate and move parameters to registers
cgArgumentList :: Reg ->[(Bool, GoatType)] -> [Expr] -> Codegen ()
cgArgumentList _ _ [] = return ()
cgArgumentList reg ((isRef, typ):ps) (arg: args) = do
    setRegister reg
    let basetype = cgGetBaseType typ
    if isRef
        then do
            -- pass by reference
            case arg of
                Id _ ident -> do cgRefParameter basetype reg ident arg
                ArrayRef _ ident _ -> do cgRefParameter basetype reg ident arg
                MatrixRef _ ident _ _ -> do cgRefParameter basetype reg ident arg
                otherwise -> error "expected id"
        else do
            -- pass by value
            (reg', typ') <- cgExpression arg
            case needCastType (cgGetBaseType typ) typ' of
                CastLeft  -> error "expected integer, found real"
                CastRight -> writeInstruction "int_to_real" [showReg reg, showReg reg']
                NoNeed    -> if reg == reg' 
                                then return ()
                                else writeInstruction "move" [showReg reg, showReg reg']
    cgArgumentList (reg+1) ps args


cgRefParameter :: BaseType -> Reg -> Ident -> Expr -> Codegen()
cgRefParameter typ reg ident arg = do
    (_, goattype, _) <- getVariable ident
    let typ' = cgGetBaseType goattype
    if typ' == typ then cgLoadAddress reg arg
    else error $ "expected " ++ show typ ++ ", found " ++ show typ'

cgLoadAddress :: Reg -> Expr -> Codegen ()
cgLoadAddress reg (Id _ id) = do
    (isRef, _, slot) <- getVariable id
    if isRef
        then writeInstruction "load" [showReg reg, show slot]
        else writeInstruction "load_address" [showReg reg, show slot]
cgLoadAddress reg (ArrayRef _ id expr) = do
    (reg', typ') <- cgExpression expr
    if typ' /= IntType
        then error $ "expected int as array index"
        else return ()
    (isRef, typ, slot) <- getVariable id
    case typ of
        Array _ _ -> do
            writeInstruction "load_address" [showReg reg, show slot]
            writeInstruction "sub_offset" [showReg reg, showReg reg,
                                           showReg reg']
        otherwise -> error $ "expected arrary variable"
cgLoadAddress reg (MatrixRef _ id expr1 expr2) = do
    (reg1, typ1) <- cgExpression expr1
    (reg2, typ2) <- cgExpression expr2
    if typ1 /= IntType || typ2 /= IntType
        then error $ "expected int as matrix index"
        else return ()
    (isRef, typ, slot) <- getVariable id
    case typ of
        Matrix _ _ _ -> do
            writeInstruction "load_address" [showReg reg, show slot]
            flattenMatrixIndex reg1 reg2 typ
            writeInstruction "sub_offset" [showReg reg, showReg reg,
                                           showReg reg1]
        otherwise -> error $ "expected matrix variable"



-- generate code to read input from console
cgReadStatement :: Lvalue -> Codegen ()
cgReadStatement var = do
    (ref, t, sl) <- cgVariableAccess var
    let name = case t of
            IntType     -> "read_int"
            FloatType   -> "read_real"
            BoolType    -> "read_bool"
    writeInstruction "call_builtin" [name]
    case ref of
        False -> writeInstruction "store" [show sl, showReg regZero]
        True -> writeInstruction "store_indirect" [showReg sl, showReg regZero]


cgAssignmentStatement :: Lvalue -> Expr -> Codegen ()
cgAssignmentStatement var expr = do
    (isRef, ltype, addr) <- cgVariableAccess var
    (rvalue, rtype) <- cgExpression expr
    case needCastType ltype rtype of
        CastRight -> writeInstruction "int_to_real" [showReg rvalue, showReg rvalue]
        CastLeft  -> error $ "expected integer, found real"
        NoNeed    -> return ()
    -- writeInstruction "store" [show addr, showReg rvalue]
    if isRef 
        then do
            writeInstruction "store_indirect" [showReg addr, showReg rvalue]
        else do
            writeInstruction "store" [show addr, showReg rvalue]


cgGetVariableType :: Lvalue -> Codegen (BaseType)
cgGetVariableType (LId pos id) = do
    (_,goatType,_) <- getVariable id
    return (cgGetBaseType goatType)
cgGetVariableType (LArrayRef pos id expr) = do
    (_,goatType,_) <- getVariable id
    return (cgGetBaseType goatType)
cgGetVariableType (LMatrixRef pos id expr1 expr2) = do
    (_,goatType,_) <- getVariable id
    return (cgGetBaseType goatType)

cgGetBaseType :: GoatType -> BaseType
cgGetBaseType (Base bt) = bt
cgGetBaseType (Array bt num) = bt
cgGetBaseType (Matrix  bt num1 num2) = bt

--returns the reference type, data type and location of a named variable
-- location is slotnum if its a value (bool=false) and register num if its a reference (bool=true)
cgVariableAccess :: Lvalue -> Codegen (Bool, BaseType, Int)
cgVariableAccess (LId pos id) = do
    (isReference, goatType, slot) <- getVariable id
    if not isReference then do
        -- var is a value and can be read directly from
        return (isReference, cgGetBaseType goatType, slot)
    else do
        -- var is a reference, the returned 
        r <- nextRegister
        writeInstruction "load" [showReg r, show slot]
        return (isReference, cgGetBaseType goatType, r)
cgVariableAccess (LArrayRef pos id expr) = do
    (isReference, goatType, slot) <- getVariable id
    --stack slot chnges depending on array index. assumes getVariable returns the stacknum of index 0 in array
    --maybe the expression should be evaluated. do we have any function for that?
    (reg, baseType) <- cgExpression expr
    r <- nextRegister
    -- put address of array[0] into r
    writeInstruction "load_address" [showReg r, show slot]
    -- increment address of array[0] by the value in reg (the expression)
    writeInstruction "sub_offset" [showReg r, showReg r, show reg]
    return (True, cgGetBaseType goatType, r)
cgVariableAccess (LMatrixRef pos id expr1 expr2) = do
    --assumes accessing array[8][3] from array[10][10] means the stack slot is &a[0] + (8*10)+3 
    (isReference, goatType, slot) <- getVariable id
    r1 <- nextRegister
    writeInstruction "load_address" [showReg r1, show slot]
    --get the size of the first dimension of the array
    r2 <- nextRegister
    writeComment "size of array fst dimension"
    writeInstruction "int_const" [showReg r2, show $ getFstDimen goatType]
    --evaluate the expressions. TODO check if they are int
    (reg1, bt1) <- cgExpression expr1
    (reg2, bt2) <- cgExpression expr2
    --multiply and add to get the offset, apply the offset and return the address in the register
    writeInstruction "mul_int" [showReg r2, showReg r2, showReg reg1]
    writeInstruction "add_int" [showReg r2, showReg r2, showReg reg2]
    writeInstruction "sub_offset" [showReg r1, showReg r1, showReg r2]
    return (True, cgGetBaseType goatType, r1)

getFstDimen :: GoatType -> Int
getFstDimen (Matrix bt s1 s2) = s1
getFstDimen _ = error "getFstDimen error"

cgIntToReal :: Reg -> Codegen ()
cgIntToReal r = do
    writeInstruction "int_to_real" [showReg r, showReg r]
    putRegType r (Base FloatType) 



cgWriteStatement :: Expr -> Codegen ()
cgWriteStatement expr = do
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
    beginLabel <- nextLabel
    afterLabel <- nextLabel
    -- expression
    (reg, typ) <- cgExpression expr
    writeInstruction "branch_on_true" [showReg reg, beginLabel]
    writeInstruction "branch_uncond" [afterLabel]
    -- if part
    writeLabel beginLabel
    cgCompoundStatement stmts
    writeLabel afterLabel
    writeComment "fi"


cgIfElseStatement :: Expr -> [Stmt] -> [Stmt] -> Codegen()
cgIfElseStatement expr stmts1 stmts2 = do
    beginLabel <- nextLabel
    elseLabel <- nextLabel
    afterLabel <- nextLabel
    -- expression
    (reg, typ) <- cgExpression expr
    writeInstruction "branch_on_true" [showReg reg, beginLabel]
    writeInstruction "branch_uncond" [elseLabel]
    -- if part
    writeLabel beginLabel
    cgCompoundStatement stmts1
    writeInstruction "branch_uncond" [afterLabel]
    -- else part
    writeComment "else"
    writeLabel elseLabel
    cgCompoundStatement stmts2
    writeLabel afterLabel
    writeComment "fi"


cgWhileStatement :: Expr -> [Stmt] -> Codegen ()
cgWhileStatement expr stmts = do
    beginLabel <- nextLabel
    whileLabel <- nextLabel
    afterLabel <- nextLabel
    writeLabel beginLabel
    -- while expression
    (reg, typ) <- cgExpression expr
    writeInstruction "branch_on_true" [showReg reg, whileLabel]
    writeInstruction "branch_uncond" [afterLabel]
    -- while body
    writeLabel whileLabel
    cgCompoundStatement stmts
    writeInstruction "branch_uncond" [beginLabel]
    writeLabel afterLabel
    writeComment "od"

data Cast = NoNeed | CastLeft | CastRight
needCastType :: BaseType -> BaseType -> Cast
needCastType IntType    IntType   = NoNeed
needCastType FloatType  FloatType = NoNeed
needCastType BoolType   BoolType  = NoNeed
needCastType IntType    FloatType = CastLeft
needCastType FloatType  IntType   = CastRight
needCastType FloatType  BoolType  = error $ "expected real, found boolean"
needCastType BoolType   typ       = error $ "expected boolean, found " ++ show typ
needCastType IntType    typ       = error $ "expected integer, found " ++ show typ

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
    writeInstruction "string_const" [showReg reg, "\"" ++ str ++ "\""]
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
    -- (typ1 == typ2 == BoolType)
    putRegType reg1 (Base BoolType)
    return (reg1, BoolType)

cgExpression (Or _ expr1 expr2) = do
    (reg1, typ1) <- cgExpression expr1
    (reg2, typ2) <- cgExpression expr2
    cgPrepareLogical reg1 reg2
    writeInstruction "or" [showReg reg1, showReg reg1, showReg reg2]
    -- (typ1 == typ2 == BoolType)
    putRegType reg1 (Base BoolType)
    return (reg1, BoolType) 

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
            OpType BoolType  -> "int" 
            OpType IntType   -> "int"
            OpType FloatType -> "real"
    writeInstruction (relopInstruction ++ relopType)
                     [showReg reg1, showReg reg1, showReg reg2]
    putRegType reg1 (Base BoolType)
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
cgPrepareComparison r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    if (t1, t2) == (Base BoolType, Base BoolType)
    then do return (OpType BoolType)
    else do cgPrepareArithmetic r1 r2



