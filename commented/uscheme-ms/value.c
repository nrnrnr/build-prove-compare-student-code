#include "all.h"
/*
 * Implementation of micro-Scheme's value \
 * chaptocsplitinterface
 * 
 * The value interface has special support for Booleans
 * and for unspecified values. As usual, the value
 * interface also has support for printing.
 * 
 * Boolean values and Boolean testing
 * 
 * The first part of the value interface supports
 * Booleans.
 * <value.c>=
 */
bool istrue(Value v) {
    return v.alt != BOOLV || v.boolv;
}

Value truev, falsev;

void initvalue(void) {
    truev  = mkBoolv(true);
    falsev = mkBoolv(false);
}
/*
 * Unspecified values
 * 
 * The interface defines a function to return an
 * unspecified value. ``Unspecified'' means the
 * evaluator can pick any value it likes. For example,
 * it could just always use \vnil. Unfortunately, if the
 * evaluator always returns \vnil, careless persons will
 * grow to rely on finding \vnil, and they shouldn't.
 * (Ask me how I know.) To foil such carelessness, the
 * evaluator chooses an unhelpful value at random. [*]
 * <value.c>=
 */
Value unspecified (void) {
    switch ((rand()>>4) & 0x3) {
        case 0:  return truev;
        case 1:  return mkNum(rand());
        case 2:  return mkSym(strtoname("this value is unspecified"));
        case 3:  return mkPrimitive(ERROR, unary);
        default: return mkNil();
    }
}
