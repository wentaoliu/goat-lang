# Goat Parser

Parser for the Goat language. Programming project for COMP90045. 


## Authors

Zeyu, Yiqun, Wentao, Raymond

Skeleton code by Harald

## Issues
Todo:

* check mytest.gt and search BUG for all unfixed bugs 

* read statemtnts when expecint int and got float >>>> not sure if we did anything wrong for this. for the input 0.99, looks like the scanner reads 0 for the int and leaves .99 in the buffer, which can be read by a float stmt.


Fixed:

* nonstricness for logical ops

* if/while conditions must be bool

* array indices must be ints

* array variables must be fully indexed

* duplicated declarations of variables, procedures

* program must contain main

* generateExpr Ident isRef bug
