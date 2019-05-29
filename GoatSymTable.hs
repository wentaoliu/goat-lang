module GoatSymTable where

---------------------------------------------------------------------------
--  Symbol table, for the Goat compiler
--
--  Author: Wumpus-Killers (Wentao Liu, Raymond Sun, Zeyu Huang, Yiqun Wang)
-- 
--  A symbol table and helper functions 
---------------------------------------------------------------------------

import Data.Map
import GoatAST ( GoatType )

type Reg = Int

type Symbols = (
    -- for each procedure, for each formal parameter, its refernce type and basetype
    Map String [(Bool, GoatType)],
    -- for each variable, its reference type, base type, and starting slot number
    Map String (Bool, GoatType, Int),
    -- type for each value in register
    Map Reg GoatType
    )

-- helper functions initialising/getting/setting the symbol table
initSymbols :: Symbols
initSymbols = (empty, empty, empty)

insertRegType :: Reg -> GoatType -> Symbols -> Symbols
insertRegType r t (a, b, map) = (a, b, insert r t map)

lookupRegType :: Reg -> Symbols -> GoatType
lookupRegType r (_, _, map) = (map ! r)

insertVariable :: String -> (Bool, GoatType, Int) -> Symbols -> Symbols
insertVariable name val (a, map, c) =
    (a, insert name val map, c)

lookupVariable :: String -> Symbols -> (Bool, GoatType, Int)
lookupVariable name (_, map, _) = (map ! name)

insertProcedure :: String -> [(Bool, GoatType)] -> Symbols -> Symbols
insertProcedure name vals (map, b, c) = (insert name vals map, b, c)

lookupProcedure :: String -> Symbols -> [(Bool, GoatType)]
lookupProcedure name (map, _, _) = (map ! name)

clearVariables :: Symbols -> Symbols
clearVariables (a, _, c) = (a, empty, c)

varMembership :: String -> Symbols -> Bool
varMembership ident (_,map,_) = member ident map 

procMembership :: String -> Symbols -> Bool
procMembership ident (map,_,_) = member ident map 
