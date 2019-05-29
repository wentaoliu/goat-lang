module GoatCodegen where

---------------------------------------------------------------------------
--  A compiler for Goat
--
--  Author: Wumpus-Killers (Wentao Liu, Raymond Sun, Zeyu Huang, Yiqun Wang)
-- 
--  Structure of the module:
--  * helper functions and monads
--  * procedure generation
--  * decleartion generation
--  * statement generation
--  * variable access (array and matrix)
--  * expression generation
---------------------------------------------------------------------------

import GoatAST
import GoatSymTable
import PrettyPrinter
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad

-- additional lib (bob-3)
-- import  Data.Bool

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
    writeCode $ "    " ++ name ++ " " ++ (intercalate ", " args)

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

putProcedure :: String -> [(Bool, GoatType)] -> Codegen ()
putProcedure name params = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertProcedure name params symbols) sl l))

getProcedure :: String -> Codegen ([(Bool, GoatType)])
getProcedure name = Codegen (\(State r c symbols sl l) ->
    (lookupProcedure name symbols, State r c symbols sl l))

variableExists :: String -> Codegen (Bool)
variableExists name = Codegen (\(State r c symbols sl l) ->
    (varMembership name symbols, State r c symbols sl l))

procExists :: String -> Codegen (Bool)
procExists name = Codegen (\(State r c symbols sl l) ->
    (procMembership name symbols, State r c symbols sl l))

generateJoin :: [Codegen ()] -> Codegen ()
generateJoin [] = return ()
generateJoin (x:xs) = x >> (generateJoin xs)

generateFoldr :: (a -> b -> b) -> b -> [Codegen (a)] -> Codegen (b)
generateFoldr _ val [] = return val
generateFoldr fn val (x:xs) = do
    v <- x
    let val' = fn v val
    generateFoldr fn val' xs

printSepBy :: IO () -> [IO ()] -> IO ()
printSepBy _ [] = return ()
printSepBy _ [x] = x
printSepBy sep (x:y:zs) =
    x >> sep >> (printSepBy sep (y:zs))

generatePushStackFrame :: MemSize -> Codegen ()
generatePushStackFrame size =
    writeInstruction "push_stack_frame" [show size]

generatePopStackFrame :: MemSize -> Codegen ()
generatePopStackFrame size =
    writeInstruction "pop_stack_frame" [show size]

generateCode :: Program -> IO ()
generateCode prog = do
    let Codegen fn = generateProgram prog
    let (_, finalState) = fn initState
    let State _ instructions _ _ _ = finalState
    printSepBy (putStr "\n") (map putStr instructions)


-- here starts functions for code generation
generateProgram :: Program -> Codegen ()
generateProgram (Program procs) = do
    writeCode "    call proc_main"
    writeCode "    halt"
    -- put all procedures into symbol table
    generateJoin $ map storeProcedureInfo procs
    mainExists <- procExists "main"
    case mainExists of 
        True -> generateJoin $ map generateProcedure procs
        False -> error "program must contain a 0 arg procedure \"main\""
    

--For each procedure, put it into a map of ident->[(reference Type, base Type)]
storeProcedureInfo :: Procedure -> Codegen ()
storeProcedureInfo (Procedure _ ident args _ _) = do
    duplicate <- procExists ident
    case duplicate of
        True -> error ("duplicate procedure detected!!: " ++ show ident)
        False -> putProcedure ident (extractArgInfo args)

--extracts the refType and BaseType from a FormalArgSpec type (see AST)
extractArgInfo :: [FormalArgSpec] -> [(Bool, GoatType)]
extractArgInfo args = map extractArgInfo' args
extractArgInfo' :: FormalArgSpec -> (Bool, GoatType)
extractArgInfo' (FormalArgSpec _ Val typ _) = (False, Base typ)
extractArgInfo' (FormalArgSpec _ Ref typ _) = (True, Base typ)

-- Add procedure args to the symbol table and returns the amount of slots needed for all args
analyseProcArgs :: [FormalArgSpec] -> Codegen (MemSize)
analyseProcArgs args = do
    generateFoldr (+) 0 $ map analyseProcArg args

analyseProcArg :: FormalArgSpec -> Codegen (MemSize)
analyseProcArg (FormalArgSpec pos Val baseType ident) = analyseDeclaration False (Decl pos ident (Base baseType))
analyseProcArg (FormalArgSpec pos Ref baseType ident) = analyseDeclaration True (Decl pos ident (Base baseType))

-- generate code for each procedure
generateProcedure :: Procedure -> Codegen()
generateProcedure (Procedure pos ident args decls stmts) = do
    writeLabel ("proc_" ++ ident)
    writeComment ("procedure " ++ ident)
    resetVariables
    resetStack
    -- write instructions to create all the variables in scope and get the size for memory allocation
    size <- analyseProcArgs args
    size2 <- analyseDeclarations decls
    -- generate function body
    generatePushStackFrame (size + size2)
    writeComment "formal parameter section"
    generateProcArgs 0 0 args
    generateDeclarations decls
    generateStatements stmts
    generatePopStackFrame (size + size2)
    writeCode "    return"

-- Generate instructions to store procedure arguments into stack slots
generateProcArgs :: Reg -> StackSlot -> [FormalArgSpec] -> Codegen ()
generateProcArgs _ _ [] = return ()
generateProcArgs r sl (_:xs) = do
    writeInstruction "store" [show sl, showReg r]
    generateProcArgs (r+1) (sl+1) xs

-- Look over all declarations, do some processing, return
-- number of stack slots required
analyseDeclarations :: [Decl] -> Codegen (MemSize)
analyseDeclarations decls = do
    generateFoldr (+) 0 $ map (analyseDeclaration False) decls

-- Add variable to symbol table and calculate required memory to store it. Also check for duplicates
-- Includes procedure arguments as well, so the Declaration part is a bit of a misnomer
analyseDeclaration :: Bool -> Decl -> Codegen (MemSize)
analyseDeclaration varness (Decl _ ident typ) = do
    duplicate <- variableExists ident
    case (typ, duplicate) of
        (_, True) -> error ("Duplicate variable declaration detected!!: " ++ show ident)
        -- ArrayTypeDenoter arrayType -> generateArrayType i arrayType
        (Base baseType, False) -> do
            sl <- nextSlot
            putVariable ident (varness, typ, sl) 
            return 1 -- all primitives have size 1
                --robin
        (Array baseType n, False) -> do
            sl <- nextSlotMulti n
            -- putVariable stores the slot number of the beginning of array
            putVariable ident (varness, typ, sl)
            return n
        (Matrix baseType m n, False) -> do
            let size = m*n
            sl <- nextSlotMulti size
            -- putVariable stores the slot number of the beginning of matrix
            putVariable ident (varness, typ, sl)
            return size
        --robin

-- Generate instructions for declarations
generateDeclarations  :: [Decl] -> Codegen()
generateDeclarations  decls = do
    generateJoin $ map generateDeclaration decls
    
generateDeclaration :: Decl -> Codegen()
generateDeclaration (Decl _ ident typ) = do
    (_, _, sl) <- getVariable ident
    case typ of
        Base baseType -> do
            case baseType of
                BoolType -> do 
                    writeComment ("initialize bool val " ++ ident)
                    generateInitInt sl 1
                IntType -> do
                    writeComment ("initialize int val " ++ ident)
                    generateInitInt sl 1
                FloatType -> do
                    writeComment ("initialize float val " ++ ident)
                    generateInitFloat sl 1
        Array baseType n -> do
            case baseType of
                BoolType -> do
                    writeComment ("initialize bool val " ++ ident ++ "[" ++ show n ++ "]")
                    generateInitInt sl n
                IntType -> do
                    writeComment ("initialize int val " ++ ident ++ "[" ++ show n ++ "]")
                    generateInitInt sl n
                FloatType -> do
                    writeComment ("initialize float val " ++ ident ++ "[" ++ show n ++ "]")
                    generateInitFloat sl n
        Matrix baseType m n -> do
            let size = m*n
            case baseType of
                BoolType -> do
                    writeComment ("initialize bool val "  ++ ident ++ "[" ++ show m ++ "," ++ show n ++ "]")
                    generateInitInt sl size
                IntType -> do
                    writeComment ("initialize int val "  ++ ident ++ "[" ++ show m ++ "," ++ show n ++ "]")
                    generateInitInt sl size
                FloatType -> do
                    writeComment ("initialize float val "  ++ ident ++ "[" ++ show m ++ "," ++ show n ++ "]")
                    generateInitFloat sl size


-- Instructions for initialising an integer variable (or boolean bcoz bools are stored as int)
generateInitInt :: Int -> Int -> Codegen()
generateInitInt sl 0 = return ()
generateInitInt sl size = do
    writeInstruction "int_const"  ["r0", "0"]
    writeInstruction "store" [show sl, "r0"]
    generateInitInt (sl+1) (size-1)

-- Instructions for initialising a float variable
generateInitFloat :: Int -> Int -> Codegen()
generateInitFloat sl 0 = return ()
generateInitFloat sl size = do
    writeInstruction "real_const"  ["r0", "0.0"]
    writeInstruction "store" [show sl, "r0"]
    generateInitFloat (sl+1) (size-1)


-- generate code to handle parameters in a procedure
generateFormalArgDeclaration :: Bool -> Decl -> Codegen (MemSize)
generateFormalArgDeclaration varness (Decl _ ident typ) = do
    case typ of
        Base baseType -> do
            sl <- nextSlot
            putVariable ident (varness, typ, sl) 
            return 1 

-- Generate instructions for a given block of statements
generateStatements :: [Stmt] -> Codegen ()
generateStatements [] = return ()
generateStatements (x:xs) = do
    resetRegister
    writeComment $ (ppStmt x)!!0
    generateStatement x
    generateStatements xs

generateStatement :: Stmt -> Codegen ()
generateStatement (Write _ expr) = generateWriteStatement expr
generateStatement (If _ expr stmts) = generateIfStatement expr stmts
generateStatement (IfElse _ expr stmts1 stmts2) = generateIfElseStatement expr stmts1 stmts2
generateStatement (While _ expr stmts) = generateWhileStatement expr stmts
generateStatement (Assign _ lvalue expr) = generateAssignmentStatement lvalue expr
generateStatement (Read _ lvalue) = generateReadStatement lvalue
generateStatement (ProcCall _ ident exprs) = generateProcCallStatement ident exprs

-- generate code to call a procedure. Put arguments in registers starting at r0
generateProcCallStatement :: Ident -> [Expr] -> Codegen ()
generateProcCallStatement ident argExprs = do
    params <- getProcedure ident
    if length params /= length argExprs
        then error $ "expected " ++ (show $ length params) ++ " parameter(s), found " ++ (show $ length argExprs)
        else generateArgumentList 0 params argExprs
    writeInstruction "call" ["proc_" ++ ident]

-- Generate instr to evaluate and move parameter values to registers for the called proc to read
-- Put arguments in registers starting at r0
generateArgumentList :: Reg ->[(Bool, GoatType)] -> [Expr] -> Codegen ()
generateArgumentList _ _ [] = return ()
generateArgumentList reg ((isRef, typ):ps) (arg: args) = do
    setRegister reg
    let basetype = goatType2BaseType typ
    if isRef
        then do
            -- pass by reference
            case arg of
                Id _ ident -> do generateRefParameter basetype reg ident arg
                ArrayRef _ ident _ -> do generateRefParameter basetype reg ident arg
                MatrixRef _ ident _ _ -> do generateRefParameter basetype reg ident arg
                otherwise -> error "expected ident when passing ref arg, got some other expr"
        else do
            -- pass by value
            (reg', typ') <- generateExpression arg
            case needCastType basetype typ' of
                CastLeft  -> error "expected integer, found real"
                CastRight -> writeInstruction "int_to_real" [showReg reg, showReg reg']
                NoNeed    -> if reg == reg' 
                                then return ()
                                else writeInstruction "move" [showReg reg, showReg reg']
    generateArgumentList (reg+1) ps args

-- Generate instructions for putting the address of the parameter in some (correct) register
generateRefParameter :: BaseType -> Reg -> Ident -> Expr -> Codegen()
generateRefParameter typ reg ident arg = do
    (_, goattype, _) <- getVariable ident
    let typ' = goatType2BaseType goattype
    if typ' == typ then loadAddress reg arg  --TODO loadAddress overlap with VariableAccess but the arg types are different
    else error $ "expected " ++ show typ ++ ", found " ++ show typ'

-- Instructins for putting the Expr (guaranteed to be a Id, Array or Matrix) into the register
loadAddress :: Reg -> Expr -> Codegen ()
loadAddress reg (Id _ id) = do
    (isRef, _, slot) <- getVariable id
    if isRef
        then writeInstruction "load" [showReg reg, show slot]
        else writeInstruction "load_address" [showReg reg, show slot]
loadAddress reg (ArrayRef _ id expr) = do
    (reg', typ') <- generateExpression expr
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
loadAddress reg (MatrixRef _ id expr1 expr2) = do
    (reg1, typ1) <- generateExpression expr1
    (reg2, typ2) <- generateExpression expr2
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
generateReadStatement :: Lvalue -> Codegen ()
generateReadStatement var = do
    t <- getVariableType var
    let name = case t of
            IntType     -> "read_int"
            FloatType   -> "read_real"
            BoolType    -> "read_bool"
    writeInstruction "call_builtin" [name]
    nextRegister -- otherwise the r0 might be overwritten
    (ref, _, sl) <- variableLocation var
    case ref of
        False -> writeInstruction "store" [show sl, showReg regZero]
        True -> writeInstruction "store_indirect" [showReg sl, showReg regZero]

-- Generate instr for a given assignment of Expr to Lvalue
generateAssignmentStatement :: Lvalue -> Expr -> Codegen ()
generateAssignmentStatement var expr = do
    (isRef, varType, addr) <- variableLocation var
    (exprReg, exprType) <- generateExpression expr
    case needCastType varType exprType of
        CastRight -> writeInstruction "int_to_real" [showReg exprReg, showReg exprReg]
        CastLeft  -> error $ "expected integer, found real"
        NoNeed    -> return ()
    -- Store the expression at the location of the variable (Lvalue)
    case isRef of
        False -> writeInstruction "store" [show addr, showReg exprReg]
        True -> writeInstruction "store_indirect" [showReg addr, showReg exprReg]


getVariableType :: Lvalue -> Codegen (BaseType)
getVariableType (LId pos id) = do
    (_,goatType,_) <- getVariable id
    return (goatType2BaseType goatType)
getVariableType (LArrayRef pos id expr) = do
    (_,goatType,_) <- getVariable id
    return (goatType2BaseType goatType)
getVariableType (LMatrixRef pos id expr1 expr2) = do
    (_,goatType,_) <- getVariable id
    return (goatType2BaseType goatType)

goatType2BaseType :: GoatType -> BaseType
goatType2BaseType (Base bt) = bt
goatType2BaseType (Array bt num) = bt
goatType2BaseType (Matrix  bt num1 num2) = bt

matchGoatTypeLvalueType :: GoatType -> Lvalue -> Codegen()
matchGoatTypeLvalueType (Base _) (LId _ _) = do return ()
matchGoatTypeLvalueType (Array _ _) (LArrayRef _ _ _) = do return ()
matchGoatTypeLvalueType (Matrix _ _ _) (LMatrixRef _ _ _ _) = do return ()
matchGoatTypeLvalueType _ _ = error "Variable not properly indexed"

--returns the reference type, data type and location of a named variable
-- location is slotnum if its a value (bool=false) and register num if its a reference (bool=true)
variableLocation :: Lvalue -> Codegen (Bool, BaseType, Int)
variableLocation (LId pos id) = do
-- Single variable info
    (isReference, goatType, slot) <- getVariable id
    matchGoatTypeLvalueType goatType (LId pos id)
    if not isReference then do
        return (isReference, goatType2BaseType goatType, slot)
    else do
        r <- nextRegister
        writeInstruction "load" [showReg r, show slot]
        return (isReference, goatType2BaseType goatType, r)
-- Array element info
variableLocation (LArrayRef pos id expr) = do
    (isReference, goatType, slot) <- getVariable id
    matchGoatTypeLvalueType goatType (LArrayRef pos id expr)
    (reg, baseType) <- generateExpression expr
    exprType <- getRegType reg
    case (goatType2BaseType exprType) of
        (IntType) -> do
            r <- nextRegister
            writeInstruction "load_address" [showReg r, show slot]
            writeInstruction "sub_offset" [showReg r, showReg r, showReg reg]
            return (True, goatType2BaseType goatType, r)
        (_) -> error "Array indicies must be integers"
-- Matrix element info
variableLocation (LMatrixRef pos id expr1 expr2) = do
    (isReference, goatType, slot) <- getVariable id
    matchGoatTypeLvalueType goatType (LMatrixRef pos id expr1 expr2)
    r1 <- nextRegister
    writeInstruction "load_address" [showReg r1, show slot]
    r2 <- nextRegister
    writeComment "size of array fst dimension"
    writeInstruction "int_const" [showReg r2, show $ getFstDimen goatType]
    (reg1, bt1) <- generateExpression expr1
    (reg2, bt2) <- generateExpression expr2
    expr1Type <- getRegType reg1
    expr2Type <- getRegType reg2
    case (goatType2BaseType expr1Type, goatType2BaseType expr2Type) of
        (IntType, IntType) -> do
            --multiply and add to get the offset, apply the offset and return the address in the register
            writeInstruction "mul_int" [showReg r2, showReg r2, showReg reg1]
            writeInstruction "add_int" [showReg r2, showReg r2, showReg reg2]
            writeInstruction "sub_offset" [showReg r1, showReg r1, showReg r2]
            return (True, goatType2BaseType goatType, r1)
        (_,_) -> error "Matrix indicies must be integers"
    

getFstDimen :: GoatType -> Int
getFstDimen (Matrix bt s1 s2) = s1
getFstDimen _ = error "getFstDimen error"

-- Generate instr to convert int to real (wow)
generateIntToReal :: Reg -> Codegen ()
generateIntToReal r = do
    writeInstruction "int_to_real" [showReg r, showReg r]
    putRegType r (Base FloatType) 

<<<<<<< HEAD


cgWriteStatement :: Expr -> Codegen ()
cgWriteStatement expr = do
    writeComment "write"
    (reg, typ) <- cgExpression expr
=======
generateWriteStatement :: Expr -> Codegen ()
generateWriteStatement expr = do
    (reg, typ) <- generateExpression expr
>>>>>>> master
    func <- case typ of
        IntType     -> return "print_int"
        FloatType   -> return "print_real"
        BoolType    -> return "print_bool"
        StringType  -> return "print_string"
    if showReg reg /= "r0"
        then writeInstruction "move" ["r0", showReg reg]
        else return ()
    writeInstruction "call_builtin" [func]


generateIfStatement :: Expr -> [Stmt] -> Codegen ()
generateIfStatement expr stmts = do
    beginLabel <- nextLabel
    afterLabel <- nextLabel
    -- expression
    (reg, typ) <- generateExpression expr
    checkBoolean typ
    writeInstruction "branch_on_true" [showReg reg, beginLabel]
    writeInstruction "branch_uncond" [afterLabel]
    -- if part
    writeLabel beginLabel
    generateStatements stmts
    writeLabel afterLabel
    writeComment "fi"


generateIfElseStatement :: Expr -> [Stmt] -> [Stmt] -> Codegen()
generateIfElseStatement expr stmts1 stmts2 = do
    beginLabel <- nextLabel
    elseLabel <- nextLabel
    afterLabel <- nextLabel
    -- expression
    (reg, typ) <- generateExpression expr
    checkBoolean typ
    writeInstruction "branch_on_true" [showReg reg, beginLabel]
    writeInstruction "branch_uncond" [elseLabel]
    -- if part
    writeLabel beginLabel
    generateStatements stmts1
    writeInstruction "branch_uncond" [afterLabel]
    -- else part
    writeComment "else"
    writeLabel elseLabel
    generateStatements stmts2
    writeLabel afterLabel
    writeComment "fi"


generateWhileStatement :: Expr -> [Stmt] -> Codegen ()
generateWhileStatement expr stmts = do
    beginLabel <- nextLabel
    whileLabel <- nextLabel
    afterLabel <- nextLabel
    writeLabel beginLabel
    -- while expression
    (reg, typ) <- generateExpression expr
    checkBoolean typ
    writeInstruction "branch_on_true" [showReg reg, whileLabel]
    writeInstruction "branch_uncond" [afterLabel]
    -- while body
    writeLabel whileLabel
    generateStatements stmts
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

---------------
--  expressions
---------------

-- data ExprType = SimpleExprType
--               | LogicExprType 
--               | RelExprType 
--               | IdExprType
--               | ArithExprType


-- Instructions for evaluating expressions. The end value of the expr is stored in 
-- the returned Reg. also returns the type of the expression evaluated
generateExpression :: Expr -> Codegen (Reg, BaseType)
-- const access
generateExpression (BoolCon _ bool) = do
    reg <- nextRegister
    case bool of
        True      -> writeInstruction "int_const"  [showReg reg, "1"]
        False     -> writeInstruction "int_const"  [showReg reg, "0"]
    putRegType reg (Base BoolType)
    return (reg, BoolType)
generateExpression (IntCon _ int) = do
    reg <- nextRegister
    writeInstruction "int_const"  [showReg reg, show int]
    putRegType reg (Base IntType)
    return (reg, IntType)
generateExpression (FloatCon _ float) = do
    reg <- nextRegister
    writeInstruction "real_const" [showReg reg, show float]
    putRegType reg (Base FloatType)
    return (reg, FloatType)
generateExpression (StrCon _ str) = do
    reg <- nextRegister
    writeInstruction "string_const" [showReg reg, "\"" ++ str ++ "\""]
    putRegType reg (Base StringType)
    return (reg, StringType)

-- unary operators
-- UnaryMinus Pos Expr
generateExpression (UnaryMinus _ expr) = do
    (reg, typ) <- generateExpression expr
    func <- case typ of
        IntType -> return "neg_int"
        FloatType -> return "neg_real"
        BoolType -> error $ "expected integer or real, found boolean"
    writeInstruction func [showReg reg, showReg reg]
    return (reg, typ)

-- Not Pos Expr
generateExpression (Not _ expr) = do
    (reg, typ) <- generateExpression expr
    case typ of
        BoolType -> writeInstruction "not" [showReg reg, showReg reg]
        otherwise -> error $ "expected bool, found " ++ show typ
    return (reg, BoolType)

-- And operator with nonstrictness. If only i could use the generateIfStmt but I think its slightly different
-- expr2 evaluated only if expr1 is true, basically:
-- if expr1
--     if expr2
--         true
-- else false
generateExpression (And _ expr1 expr2) = do
    expr2Label <- nextLabel
    endlabel <- nextLabel

    (reg1, typ1) <- generateExpression expr1
    checkBoolean typ1
    -- if true the go to the label for checking expr2
    writeInstruction "branch_on_true" [showReg reg1, expr2Label]
    -- otherwise reg1 already contains 0 (false) and we just skip the rest of the logical expression
    writeInstruction "branch_uncond" [endlabel]

    -- the second arg of the && expression
    writeLabel expr2Label
    (reg2, typ2) <- generateExpression expr2
    checkBoolean typ2
    -- move the value of expr2 into reg1 because we are returning reg1 
    -- dont worry, this won't happen if expr1 branches past this block so reg1 is guaranteed to hold the desired result
    writeInstruction "move" [showReg reg1, showReg reg2]
    -- at this point, expr1 is true so the overall value of the expression depends on this expr2
    -- so we can leave the value in reg1 as the final result
    writeInstruction "branch_uncond" [endlabel]
    
    writeLabel endlabel
    putRegType reg1 (Base BoolType)
    return (reg1, BoolType)

-- Or operator with nonstrictness
-- approximately -> if expr1 then true, elif expr2 then true
generateExpression (Or _ expr1 expr2) = do
    endLabel <- nextLabel
    falseLabel <- nextLabel
    (reg1, typ1) <- generateExpression expr1
    checkBoolean typ1
    -- if expr1 evaluated to true just skip the rest of the logical expr, we're done
    writeInstruction "branch_on_true" [showReg reg1, endLabel]
    -- if the branch instruction didnt send us away, the instructions to evaluate expr2 now execute
    (reg2, typ2) <- generateExpression expr2
    checkBoolean typ2
    -- move the value of expr2 into reg1 because we are returning reg1 
    -- dont worry, this won't happen if expr1 branches past this block so reg1 is guaranteed to hold the desired result
    writeInstruction "move" [showReg reg1, showReg reg2]
    -- if expr2 is true, then we finish the logical expr 
    writeInstruction "branch_on_true" [showReg reg1, endLabel]
    -- if that branch instruction didnt send us off, a value of 0 (false) is put in the output register
    writeInstruction "int_const" [showReg reg1, show 0]

    writeLabel endLabel
    putRegType reg1 (Base BoolType)
    return (reg1, BoolType) 

-- Rel Pos Relop Expr Expr
generateExpression (Rel _ relop expr1 expr2) = do
    (reg1, typ1) <- generateExpression expr1
    (reg2, typ2) <- generateExpression expr2
    optype <- generatePrepareComparison reg1 reg2
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
    return (reg1, BoolType)

generateExpression (Id _ ident) = do
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
                writeInstruction "load" [showReg reg, show addr]
                writeInstruction "load_indirect" [showReg reg, showReg reg]
                putRegType reg (Base btype)
                return (reg, btype)
        _                   -> 
            error ("variable " ++ show ident ++ " cannot be loaded.") 

-- Arithmetic expressions
-- BinOpExp Pos Binop Expr Expr
generateExpression (BinOpExp _ binop expr1 expr2) = do
    (reg1, typ1) <- generateExpression expr1
    (reg2, typ2) <- generateExpression expr2
    optype <- generateTypeCasting reg1 reg2
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
generateExpression (ArrayRef _ ident expr) = do
    (regRowIndex, btype) <- generateExpression expr
    (isRef, gtype, addr) <- getVariable ident --can use variableAccess here
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
            
generateExpression (MatrixRef _ ident rowExpr colExpr) = do
    (regRowIndex, rowbtype) <- generateExpression rowExpr
    (regColIndex, colbtype) <- generateExpression colExpr
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
    writeInstruction "int_const" [showReg regCols, show cols]
    putRegType regCols (Base IntType)
    writeInstruction "mul_int" [showReg regRowIndex, showReg regRowIndex,
                                showReg regCols]
    writeInstruction "add_int" [showReg regRowIndex, showReg regRowIndex,
                                showReg regColIndex]
    -- return ()

-- data OperatorType = IntOp | RealOp
data OpType = OpType BaseType
            -- | IllegalOp BaseType BaseType

-- deconstruct OpType to get BaseType
fromOpType :: OpType -> BaseType
fromOpType (OpType b) = b

-- Generate instructions for casting number types (for two args of a binary op)
-- Check types of both operands, do type casting if necessary, return result type
generateTypeCasting :: Reg -> Reg -> Codegen (OpType)
generateTypeCasting r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (Base FloatType, Base FloatType) -> return $ OpType FloatType
        (Base IntType,   Base FloatType) -> do
                                            generateIntToReal r1
                                            return $ OpType FloatType
        (Base FloatType, Base IntType  ) -> do
                                            generateIntToReal r2
                                            return $ OpType FloatType
        (Base IntType,   Base IntType  ) -> return $ OpType IntType
        _ -> error $ "arithmetic/comparision cannot be done between " ++
                     (show t1) ++ " and " ++ (show t2)

-- check that logical expressions involve two booleans
analyseLogicalOp :: Reg -> Reg -> Codegen ()
analyseLogicalOp r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (Base BoolType, Base BoolType) -> return ()
        _ -> error $ "logical operation cannot be done between " ++
                     (show t1) ++ " and " ++ (show t2)

-- Generate instructions for casting for comparison operations
-- Also does checking types for comparison operations
generatePrepareComparison :: Reg -> Reg -> Codegen (OpType)
generatePrepareComparison r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    if (t1, t2) == (Base BoolType, Base BoolType)
    then do return (OpType BoolType)
    else do generateTypeCasting r1 r2


checkBoolean :: BaseType -> Codegen (Bool)
checkBoolean BoolType = do return True
checkBoolean otherType = error ("Expected boolean, got " ++ (show otherType))
