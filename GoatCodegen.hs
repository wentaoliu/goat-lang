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


cgExpression :: Expr -> Codegen (Reg, BaseType)
-- const access
cgExpression (BoolCon _ bool) = do
    reg <- nextRegister
    case bool of
        True      -> writeInstruction "int_const"  [showReg reg, "1"]
        False     -> writeInstruction "int_const"  [showReg reg, "0"]
    return (reg, BoolType)
cgExpression (IntCon _ int) = do
    reg <- nextRegister
    writeInstruction "int_const"  [showReg reg, show int]
    return (reg, IntType)
cgExpression (FloatCon _ float) = do
    reg <- nextRegister
    writeInstruction "real_const" [showReg reg, show float]
    return (reg, FloatType)
cgExpression (StrCon _ str) = do
    reg <- nextRegister
    writeInstruction "string_const" [showReg reg, show str]
    return (reg, StringType)
-- -- var access
-- cgExpression (Id _ ident) = do
--     typ <- variableType ident
--     reg <- nextRegister
--     loadVariable reg ident
--     return (reg, typ)
-- sign op
cgExpression (UnaryMinus _ expr) = do
    (reg, typ) <- cgExpression expr
    func <- case typ of
        IntType -> return "neg_int"
        FloatType -> return "neg_real"
        BoolType -> error $ "expected integer or real, found boolean"
    writeInstruction func [showReg reg, showReg reg]
    return (reg, typ)
-- not op
cgExpression (Not _ expr) = do
    (reg, typ) <- cgExpression expr
    case typ of
        BoolType -> writeInstruction "not" [showReg reg, showReg reg]
        otherwise -> error $ "expected bool, found " ++ show typ
    return (reg, typ)


---------------------------------------------------------------------

cgGetVariableType :: Lvalue -> CodeGen (BaseType)
cgGetVariableType LId pos id = do
    (_,goatType,_) <- getVariable id
    return cgGetBaseType goatType
cgGetVariableType LArrayRef pos id expr = do
    (_,goatType,_) <- getVariable id
    return cgGetBaseType goatType
cgGetVariableType LMatrixRef pos id expr1 expr2 = do
    (_,goatType,_) <- getVariable id
    return cgGetBaseType goatType

cgGetBaseType :: GoatType -> BaseType
cgGetBaseType BaseType bt = bt
cgGetBaseType Array bt num = bt
cgGetBaseType Matrix  bt num1 num2 = bt

--returns the reference type, data type of the variable and stacknum to load it from. 
--jack9966 wrapped refType and stacknum together in 'data VarAddress' but i didnt
cgVariableAccess :: Lvalue -> CodeGen (Bool, BaseType, Int)
cgGetVariableType LId pos id = do
    (isReference, goatType, slot) <- getVariable id
    if isReference then do
        r <- nextRegister
        writeInstruction "load" [showReg r, show slot]
        return (isReference, goatType, r)
    return (isReference, goatType, slot)
cgGetVariableType LArrayRef pos id expr = do
    (isReference, goatType, slot) <- getVariable id
    --stack slot chnges depending on array index. assumes getVariable returns the stacknum of index 0 in array
    --maybe the expression should be evaluated. do we have any function for that?
    return (isReference, goatType, slot + expr - 1)
cgGetVariableType LMatrixRef pos id expr1 expr2 = do
    --assumes accessing array[10][3] from array[10][10] means  the stack slot is +93 or something like that 
    (isReference, goatType, slot) <- getVariable id
    return (isReference, goatType, slot + (expr1 * expr2) + expr2 - 1)

cgWriteStringStatement :: Expr -> CodeGen()
cgWriteStringStatement StrCon pos string = do
    cgCharracterString string regZero
    writeInstruction "call_builtin" ["print_string"]

cgWriteStatement :: Expr -> Codegen ()
cgWriteStatement expr = do
    cgExpression expr regZero
    t <- cgGetBaseType getRegType regZero
    let name = case t of
            IntType -> "print_int"
            FloatType -> "print_real"
            BoolType -> "print_bool"
            _ -> error "unwritable type"
writeInstruction "call_builtin" [name]


cgIntToReal :: Reg -> Codegen ()
cgIntToReal r = do
    writeInstruction "int_to_real" [showReg r, showReg r]
    putRegType r astTypeReal