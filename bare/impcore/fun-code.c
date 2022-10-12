#include "all.h"
Func mkUserdef(Userfun userdef) {
    Func n;
    
    n.alt = USERDEF;
    n.userdef = userdef;
    return n;
}

Func mkPrimitive(Name primitive) {
    Func n;
    
    n.alt = PRIMITIVE;
    n.primitive = primitive;
    return n;
}

