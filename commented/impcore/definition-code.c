#include "all.h"
Userfun mkUserfun(Namelist formals, Exp body) {
    Userfun n;
    
    n.formals = formals;
    n.body = body;
    return n;
}

Def mkVal(Name name, Exp exp) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = VAL;
    n->val.name = name;
    n->val.exp = exp;
    return n;
}

Def mkExp(Exp exp) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = EXP;
    n->exp = exp;
    return n;
}

Def mkDefine(Name name, Userfun userfun) {
    Def n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = DEFINE;
    n->define.name = name;
    n->define.userfun = userfun;
    return n;
}

struct Def mkValStruct(Name name, Exp exp) {
    struct Def n;
    
    n.alt = VAL;
    n.val.name = name;
    n.val.exp = exp;
    return n;
}

struct Def mkExpStruct(Exp exp) {
    struct Def n;
    
    n.alt = EXP;
    n.exp = exp;
    return n;
}

struct Def mkDefineStruct(Name name, Userfun userfun) {
    struct Def n;
    
    n.alt = DEFINE;
    n.define.name = name;
    n.define.userfun = userfun;
    return n;
}

