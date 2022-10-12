#include "all.h"
Exp mkLiteral(Value literal) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = LITERAL;
    n->literal = literal;
    return n;
}

Exp mkVar(Name var) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = VAR;
    n->var = var;
    return n;
}

Exp mkSet(Name name, Exp exp) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = SET;
    n->set.name = name;
    n->set.exp = exp;
    return n;
}

Exp mkIfx(Exp cond, Exp truex, Exp falsex) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = IFX;
    n->ifx.cond = cond;
    n->ifx.truex = truex;
    n->ifx.falsex = falsex;
    return n;
}

Exp mkWhilex(Exp cond, Exp exp) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = WHILEX;
    n->whilex.cond = cond;
    n->whilex.exp = exp;
    return n;
}

Exp mkBegin(Explist begin) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = BEGIN;
    n->begin = begin;
    return n;
}

Exp mkApply(Name name, Explist actuals) {
    Exp n;
    n = malloc(sizeof(*n));
    assert(n != NULL);
    
    n->alt = APPLY;
    n->apply.name = name;
    n->apply.actuals = actuals;
    return n;
}

struct Exp mkLiteralStruct(Value literal) {
    struct Exp n;
    
    n.alt = LITERAL;
    n.literal = literal;
    return n;
}

struct Exp mkVarStruct(Name var) {
    struct Exp n;
    
    n.alt = VAR;
    n.var = var;
    return n;
}

struct Exp mkSetStruct(Name name, Exp exp) {
    struct Exp n;
    
    n.alt = SET;
    n.set.name = name;
    n.set.exp = exp;
    return n;
}

struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex) {
    struct Exp n;
    
    n.alt = IFX;
    n.ifx.cond = cond;
    n.ifx.truex = truex;
    n.ifx.falsex = falsex;
    return n;
}

struct Exp mkWhilexStruct(Exp cond, Exp exp) {
    struct Exp n;
    
    n.alt = WHILEX;
    n.whilex.cond = cond;
    n.whilex.exp = exp;
    return n;
}

struct Exp mkBeginStruct(Explist begin) {
    struct Exp n;
    
    n.alt = BEGIN;
    n.begin = begin;
    return n;
}

struct Exp mkApplyStruct(Name name, Explist actuals) {
    struct Exp n;
    
    n.alt = APPLY;
    n.apply.name = name;
    n.apply.actuals = actuals;
    return n;
}

