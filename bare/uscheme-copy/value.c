#include "all.h"
/* value.c S327b */
bool istrue(Value v) {
    return v.alt != BOOLV || v.boolv;
}

Value truev, falsev;

void initvalue(void) {
    truev  = mkBoolv(true);
    falsev = mkBoolv(false);
}
/* value.c S328a */
Value unspecified (void) {
    switch ((rand()>>4) & 0x3) {
        case 0:  return truev;
        case 1:  return mkNum(rand());
        case 2:  return mkSym(strtoname("this value is unspecified"));
        case 3:  return mkPrimitive(ERROR, unary);
        default: return mkNil();
    }
}
