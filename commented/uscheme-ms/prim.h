/*
 * Arithmetic primitives
 * 
 * The primitives that implement arithmetic are
 * specified as follows: [*]
 * <prim.h>=
 */
xx("+", PLUS,  arith)
xx("-", MINUS, arith)
xx("*", TIMES, arith)
xx("/", DIV,   arith)
xx("<", LT,    arith)
xx(">", GT,    arith)
/*
 * Other binary primitives
 * 
 * The non-arithmetic binary primitives are [[cons]]
 * and [[=]].
 * <prim.h>=
 */
xx("cons", CONS, binary)
xx("=",    EQ,   binary)
/*
 * Unary primitives
 * 
 * The unary primitives include the type predicates,
 * list operations [[car]] and [[cdr]], printing
 * primitives, and [[error]].
 * <prim.h>=
 */
xx("boolean?",   BOOLEANP,   unary)
xx("null?",      NULLP,      unary)
xx("number?",    NUMBERP,    unary)
xx("pair?",      PAIRP,      unary)
xx("function?",  FUNCTIONP,  unary)
xx("symbol?",    SYMBOLP,    unary)
xx("car",        CAR,        unary)
xx("cdr",        CDR,        unary)
xx("println",    PRINTLN,    unary)
xx("print",      PRINT,      unary)
xx("printu",     PRINTU,     unary)
xx("error",      ERROR,      unary)
