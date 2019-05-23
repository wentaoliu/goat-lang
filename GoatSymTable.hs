module GoatSymTable where

import Data.Map (
    Map,
    (!),
    insert
    )
import qualified Data.Map as Map
import GoatAST ( GoatType )

type Reg = Int

type Symbols = (
    -- for each procedure, for each formal parameter, its varness and type
    Map String [(Bool, GoatType)],
    -- for each variable, its varness, type, and starting slot number
    Map String (Bool, GoatType, Int),
    -- type for each value in register
    Map Reg GoatType,
    -- for each array variable, its lower and upper bound
    Map String (Int, Int)
    )

-- helper functions initialising/getting/setting the symbol table
initSymbols :: Symbols
initSymbols = (Map.empty, Map.empty, Map.empty, Map.empty)

insertRegType :: Reg -> GoatType -> Symbols -> Symbols
insertRegType r t (a, b, map, d) = (a, b, insert r t map, d)

lookupRegType :: Reg -> Symbols -> GoatType
lookupRegType r (_, _, map, _) = (map ! r)

insertVariable :: String -> (Bool, GoatType, Int) -> Symbols -> Symbols
insertVariable name val (a, map, c, d) =
    (a, insert name val map, c, d)

lookupVariable :: String -> Symbols -> (Bool, GoatType, Int)
lookupVariable name (_, map, _, _) = (map ! name)

insertArrayBounds :: String -> (Int, Int) -> Symbols -> Symbols
insertArrayBounds name val (a, b, c, map) = (a, b, c, insert name val map)

lookupArrayBounds :: String -> Symbols -> (Int, Int)
lookupArrayBounds name (_, _, _, map) = (map ! name)

insertProcedure :: String -> [(Bool, GoatType)] -> Symbols -> Symbols
insertProcedure name vals (map, b, c, d) = (insert name vals map, b, c, d)

lookupProcedure :: String -> Symbols -> [(Bool, GoatType)]
lookupProcedure name (map, _, _, _) = (map ! name)

clearVariables :: Symbols -> Symbols
clearVariables (a, _, c, d) = (a, Map.empty, c, d)