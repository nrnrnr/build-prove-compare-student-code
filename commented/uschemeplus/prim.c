#include "all.h"
/*
 * \qbreak
 * 
 * Arithmetic primitives
 * 
 * Each arithmetic primitive expects two integer
 * arguments, which are obtained by projecting
 * micro-Scheme values. The projection function
 * [[projectint32]] takes not only a value but also an
 * expression, so if its argument is not an integer, it
 * can issue an informative error message.
 * <prim.c ((elided))>=
 */
static int32_t divide(int32_t n, int32_t m);
/*
 * <prim.c>=
 */
static int32_t projectint32(Exp e, Value v) {
    if (v.alt != NUM)
        runerror("in %e, expected an integer, but got %v", e, v);
    return v.num;
}
/*
 * Function [[arith]] first converts its arguments to
 * integers, then consults the tag to decide what to do.
 * [*] In each case, it computes a number or a Boolean,
 * which is converted a micro-Scheme value by either
 * [[mkNum]] or [[mkBool]], both of which are generated
 * automatically from the definition of [[Value]] in
 * code chunk [->]. Checks for arithmetic overflow are
 * not shown. [*]
 * <prim.c>=
 */
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
/*
 * Other binary primitives
 * 
 * micro-Scheme has two other binary primitives, which
 * don't require integer arguments: [[cons]] and [[=]].
 * The implementation of [[=]] is relegated to the
 * Supplement, but the implementation of [[cons]] is
 * shown here. Because S-expressions are a recursive
 * type, a cons cell must contain pointers to
 * S-expressions, not S-expressions themselves. Every
 * [[cons]] must therefore allocate fresh locations for
 * the pointers. This behavior makes [[cons]] a major
 * source of allocation in micro-Scheme programs. [
 * In~full \scheme, a cons cell is typically represented
 * by a pointer to an object allocated on the heap, so
 * [[cons]] requires only one allocation, not two.] [*]
 * <prim.c>=
 */
Value cons(Value v, Value w) {
    return mkPair(allocate(v), allocate(w));
}
/*
 * \qbreak
 * 
 * Unary primitives
 * 
 * Unary primitives are implemented here. Most of the
 * cases are relegated to the Supplement.[*] [*]
 * <prim.c>=
 */
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
    /*
     * Some of these primitives are implemented in \cref
     * scheme.chap (\chunkrefscheme.chunk.unary).
     * The remaining primitives are implemented by these
     * cases:
     * <other cases for unary primitives>=
     */
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
    /*
     * <other cases for unary primitives>=
     */
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
/*
 * \qbreak Except for division, each of micro-Scheme's
 * arithmetic primitives can be implemented by the
 * corresponding C operator. Division requires a more
 * involved implementation; micro-Scheme's division must
 * always round toward minus infinity, but C's division
 * guarantees a rounding direction only when dividing
 * positive operands.
 * <prim.c>=
 */
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
/*
 * <prim.c>=
 */
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
/*
 * \qbreak The implementation of micro-Scheme's
 * primitive equality is not completely trivial. Two
 * values are [[=]] only if they are the same number,
 * the same boolean, the same symbol, or both the empty
 * list. Because all these values are atoms, I have
 * named the C function [[equalatoms]]. A different
 * C function, [[equalpairs]], is used in \cref
 * schemea.equalpairs to implement the equality used to
 * implement [[check-expect]]. [*]
 * <prim.c>=
 */
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
