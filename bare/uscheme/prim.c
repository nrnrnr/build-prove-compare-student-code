#include "all.h"
/* prim.c ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) */
static int32_t divide(int32_t n, int32_t m);
/* prim.c 161a */
static int32_t projectint32(Exp e, Value v) {
    if (v.alt != NUM)
        runerror("in %e, expected an integer, but got %v", e, v);
    return v.num;
}
/* prim.c 161b */
Value arith(Exp e, int tag, Valuelist args) {
    checkargc(e, 2, lengthVL(args));
    int32_t n = projectint32(e, nthVL(args, 0));
    int32_t m = projectint32(e, nthVL(args, 1));

    switch (tag) {                                       // OMIT
    case PLUS:                                           // OMIT
        checkarith('+', n, m, 32); // OMIT               // OMIT
        break;                                           // OMIT
    case MINUS:                                          // OMIT
        checkarith('-', n, m, 32); // OMIT               // OMIT
        break;                                           // OMIT
    case TIMES:                                          // OMIT
        checkarith('*', n, m, 32); // OMIT               // OMIT
        break;                                           // OMIT
    case DIV:                                            // OMIT
        if (m != 0) checkarith('/', n, m, 32); // OMIT   // OMIT
        break;                                           // OMIT
    default:                                             // OMIT
        break;                                           // OMIT
    }                                                    // OMIT
    switch (tag) {
    case PLUS:  return mkNum(n + m);
    case MINUS: return mkNum(n - m);
    case TIMES: return mkNum(n * m);
    case DIV:   if (m==0) runerror("division by zero");
                else return mkNum(divide(n, m));  // round to minus infinity
    case LT:    return mkBoolv(n < m);
    case GT:    return mkBoolv(n > m);
    default:    assert(0);
    }
}
/* prim.c 161c */
Value cons(Value v, Value w) {
    return mkPair(allocate(v), allocate(w));
}
/* prim.c 162a */
Value unary(Exp e, int tag, Valuelist args) {
    checkargc(e, 1, lengthVL(args));
    Value v = nthVL(args, 0);
    switch (tag) {
    case NULLP:
        return mkBoolv(v.alt == NIL);
    case CAR:
        if (v.alt == NIL)
            runerror("in %e, car applied to empty list", e);
        else if (v.alt != PAIR)
            runerror("car applied to non-pair %v in %e", v, e);
        return *v.pair.car;
    case PRINTU:
        if (v.alt != NUM)
            runerror("printu applied to non-number %v in %e", v, e);
        print_utf8(v.num);
        return v;
    case ERROR:
        runerror("%v", v);
        return v;
    /* other cases for unary primitives S314c */
    case BOOLEANP:
        return mkBoolv(v.alt == BOOLV);
    case NUMBERP:
        return mkBoolv(v.alt == NUM);
    case SYMBOLP:
        return mkBoolv(v.alt == SYM);
    case PAIRP:
        return mkBoolv(v.alt == PAIR);
    case FUNCTIONP:
        return mkBoolv(v.alt == CLOSURE || v.alt == PRIMITIVE);
    /* other cases for unary primitives S315a */
    case CDR:
        if (v.alt == NIL)
            runerror("in %e, cdr applied to empty list", e);
        else if (v.alt != PAIR)
            runerror("cdr applied to non-pair %v in %e", v, e);
        return *v.pair.cdr;
    case PRINTLN:
        print("%v\n", v);
        return v;
    case PRINT:
        print("%v", v);
        return v;
    default:
        assert(0);
    }
}
/* prim.c S313a */
static int32_t divide(int32_t n, int32_t m) {
    if (n >= 0)
        if (m >= 0)
            return n / m;
        else
            return -(( n - m - 1) / -m);
    else
        if (m >= 0)
            return -((-n + m - 1) /  m);
        else
            return -n / -m;
}
/* prim.c S313d */
Value binary(Exp e, int tag, Valuelist args) {
    checkargc(e, 2, lengthVL(args));
    Value v = nthVL(args, 0);
    Value w = nthVL(args, 1);

    switch (tag) {
    case CONS: 
        return cons(v, w);
    case EQ:   
        return equalatoms(v, w);
    default:
        assert(0);
    }
}
/* prim.c S314a */
Value equalatoms(Value v, Value w) {
    if (v.alt != w.alt)
        return falsev;

    switch (v.alt) {
    case NUM:
        return mkBoolv(v.num   == w.num);
    case BOOLV:
        return mkBoolv(v.boolv == w.boolv);
    case SYM:
        return mkBoolv(v.sym   == w.sym);
    case NIL:
        return truev;
    default:
        return falsev;
    }
}
