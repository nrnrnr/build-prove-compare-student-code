#include "all.h"
Lambda mkLambda(Namelist formals, Exp body) {
    Lambda n;
    
    n.formals = formals;
    n.body = body;
    return n;
}

Value mkSym(Name sym) {
    Value n;
    
    n.alt = SYM;
    n.sym = sym;
    return n;
}

Value mkNum(int32_t num) {
    Value n;
    
    n.alt = NUM;
    n.num = num;
    return n;
}

Value mkBoolv(bool boolv) {
    Value n;
    
    n.alt = BOOLV;
    n.boolv = boolv;
    return n;
}

Value mkNil(void) {
    Value n;
    
    n.alt = NIL;
    
    return n;
}

Value mkPair(Value *car, Value *cdr) {
    Value n;
    
    n.alt = PAIR;
    n.pair.car = car;
    n.pair.cdr = cdr;
    return n;
}

Value mkClosure(Lambda lambda, Env env) {
    Value n;
    
    n.alt = CLOSURE;
    n.closure.lambda = lambda;
    n.closure.env = env;
    return n;
}

Value mkPrimitive(int tag, Primitive *function) {
    Value n;
    
    n.alt = PRIMITIVE;
    n.primitive.tag = tag;
    n.primitive.function = function;
    return n;
}

