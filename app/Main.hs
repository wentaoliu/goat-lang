module Main where

-- Goat project
import GoatAST
import GoatParser
import GoatPrinter

main :: IO ()
main = GoatParser.exportedMain
