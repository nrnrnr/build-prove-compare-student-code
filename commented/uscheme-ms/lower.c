#include "all.h"
/*
 * Whether to lower [[return]] is your choice.
 * The choice is communicated to the compiler via this
 * [[LOWER_RETURN]] macro:
 * <lower.c>=
 */
#define LOWER_RETURN false // to do return-lowering exercise, change me
/*
 * After that it's just one damn lowering function after
 * another.
 * <lower.c>=
 */
static inline Exp lowerLet1(Name x, Exp e, Exp body) {
    return mkLetx(LET, mkNL(x, NULL), mkEL(e, NULL), body);
}
/*
 * In [[lowerSequence]], the name ``\monoboxignore me''
 * cannot be written in the source code, so there is no
 * danger of variable capture.
 * <lower.c>=
 */
static Exp lowerSequence(Exp e1, Exp e2) {
    return lowerLet1(strtoname("ignore me"), e1, e2);
}
/*
 * <lower.c>=
 */
static Exp lowerBegin(Explist es) {
    if (es == NULL)
        return mkLiteral(falsev);
    else if (es->tl == NULL) 
        return es->hd;
    else
        return lowerSequence(es->hd, lowerBegin(es->tl));
}
/*
 * <lower.c>=
 */
static Exp lower(LoweringContext c, Exp e);
static void lowerAll(LoweringContext c, Explist es) {
    if (es) {
        lowerAll(c, es->tl);
        es->hd = lower(c, es->hd);
    }
}
/*
 * A frame is pushed, whether by [[pushframe]] or
 * [[pushenv_opt]], using the private function [[push]].
 * Function [[push]] returns a pointer to the frame just
 * pushed.
 * <lower.c>=
 */
static Exp lowerLetstar(Namelist xs, Explist es, Exp body) {
    if (xs == NULL) {
        assert(es == NULL);
        return body;
    } else {
        assert(es != NULL);
        return lowerLet1(xs->hd, es->hd, lowerLetstar(xs->tl, es->tl, body));
    }
}
/*
 * Once lowering functions for individual forms are
 * defined, the definition of [[lower]] can be emitted.
 * <lower.c>=
 */
/*
 * A lowering function is defined for every kind of
 * syntactic form that can contain an expression: true
 * definitions, tests, extended definitions, and so on.
 * All these functions call [[lower]], which lowers an
 * expression. Calling \monoboxlower(context, e)
 * recursively lowers every subexpression of e. And if
 * the form of e calls for it to be lowered, [[lower]]
 * returns e's [[LOWERED]] form; otherwise it returns e.
 * The code is repetitive, and it just implements the
 * rules shown in \crefschemes.tab.lower, so only two
 * cases are shown here. The rest are relegated to \cref
 * schemesa.chap.
 * <definition of private function [[lower]]>=
 */
static Exp lower(LoweringContext c, Exp e) {
    switch (e->alt) {
    case SET:
        e->set.exp = lower(c, e->set.exp);
        return e;
    case BREAKX:
        if (c & LOOPCONTEXT) 
            return mkLowered(e, mkLongGoto(strtoname(":break"),
                             mkLiteral(falsev)));
        else
            othererror("Lowering error: %e appeared outside of any loop", e);
    /*
     * While \crefschemes.chap shows the rules for lowering
     * all the expression forms, it shows code for lowering
     * only [[set]] and [[break]]. Code for the other forms
     * is shown here.
     * <other cases for lowering expression [[e]]>=
     */
    case LITERAL: return e;
    case VAR:     return e;
    case IFX:     e->ifx.cond   = lower(c, e->ifx.cond);
                  e->ifx.truex  = lower(c, e->ifx.truex);
                  e->ifx.falsex = lower(c, e->ifx.falsex);
                  return e;
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case WHILEX: {
        LoweringContext nc = c | LOOPCONTEXT;
        Exp body = mkLongLabel(strtoname(":continue"), lower(nc, e->whilex.body)
                                                                              );
        Exp cond = lower(c, e->whilex.cond);
        Exp placeholder   = mkLiteral(falsev); // unique pointer
        Exp loop          = mkIfx(cond, placeholder, mkLiteral(falsev));
        loop->ifx.truex = lowerSequence(body, mkLowered(e, mkLoopback(loop)));
        Exp lowered       = mkLongLabel(strtoname(":break"), loop);
        return mkLowered(e, lowered);
    }
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case BEGIN: 
        lowerAll(c, e->begin);
        return mkLowered(e, lowerBegin(e->begin));
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case LETX:
        lowerAll(c, e->letx.es);
        e->letx.body = lower(c, e->letx.body);
        switch (e->letx.let) {
        case LET: case LETREC:
            return e;
        case LETSTAR:
            return mkLowered(e, lowerLetstar(e->letx.xs, e->letx.es,
                                               e->letx.body));
        default:
            assert(0);
        }
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case LAMBDAX: {
        LoweringContext nc = FUNCONTEXT;  // no loop!
        Exp body   = lower(nc, e->lambdax.body);
        e->lambdax.body =
            LOWER_RETURN ? mkLowered(e->lambdax.body, mkLongLabel(strtoname(
                                                              ":return"), body))
                         : body;
        return e;
    }
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case APPLY:
        lowerAll(c, e->apply.actuals);
        e->apply.fn = lower(c, e->apply.fn);
        return e;
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case CONTINUEX:
        if (c & LOOPCONTEXT) 
            return mkLowered(e, mkLongGoto(strtoname(":continue"), mkLiteral(
                                                                      falsev)));
        else
            othererror("Lowering error: %e appeared outside of any loop", e);
    case RETURNX:
        e->returnx = lower(c, e->returnx);
        if (c & FUNCONTEXT) 
            return LOWER_RETURN ? mkLowered(e, mkLongGoto(strtoname(":return"),
                                                                    e->returnx))
                                : e;
        else
            othererror("Lowering error: %e appeared outside of any function", e)
                                                                               ;
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case TRY_CATCH: {
        Exp body    = lower(c, e->try_catch.body);
        Exp handler = lower(c, e->try_catch.handler);
        Name h = strtoname("the-;-handler");
        Name x = strtoname("the-;-answer");
        Exp lbody = lowerLet1(x, body, 
                              mkLambdax(mkLambda(mkNL(strtoname("_"), NULL),
                                                 mkVar(x))));
        Exp labeled = mkLongLabel(e->try_catch.label, lbody);
        Exp lowered = lowerLet1(h, handler, mkApply(labeled, mkEL(mkVar(h), NULL
                                                                            )));
        return mkLowered(e, lowered);
    }
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case THROW: {
        Name h = strtoname("the-;-handler");
        Name x = strtoname("the-;-answer");
        Lambda thrown =
            mkLambda(mkNL(h, NULL), mkApply(mkVar(h), mkEL(mkVar(x), NULL)));
        Exp throw = mkLongGoto(e->throw.label, mkLambdax(thrown));
        Exp lowered = lowerLet1(x, lower(c, e->throw.exp), throw);
        return mkLowered(e, lowered);
    }
    /*
     * <other cases for lowering expression [[e]]>=
     */
    case LONG_LABEL:
        e->long_label.body = lower(c, e->long_label.body);
        return e;
    case LONG_GOTO:
        e->long_goto.exp = lower(c, e->long_goto.exp);
        return e;
    case LOWERED: case LOOPBACK:
        assert(0); // never expect to lower twice
    default:
        assert(0);
    }
}
/*
 * Code in \crefschemes.chap is simplified by using
 * [[pushframe]], which pushes syntax.
 * <lower.c>=
 */
static void lowerDef(Def d) {
    switch (d->alt) {
    case VAL:    d->val.exp = lower(0, d->val.exp);     break;
    case EXP:    d->exp     = lower(0, d->exp);         break;
    case DEFINE: {
        LoweringContext c = FUNCONTEXT;
        Exp body = lower(c, d->define.lambda.body);
        if (LOWER_RETURN)
            body = mkLowered(d->define.lambda.body,
                               mkLongLabel(strtoname(":return"), body));
        d->define.lambda.body = body;
        break;
    }
    case DEFS:   break;  /*OMIT*/
    default:     assert(0);
    }
}
/*
 * <lower.c>=
 */
void lowerXdef(XDef d) {
    switch (d->alt) {
    case DEF: lowerDef(d->def); break;
    case USE: break;
    case TEST: break;  // lowering is delayed until testing time
    default: assert(0);
    }
}
/*
 * Printing the stack
 * 
 * Frames, stacks, and environments are printed by
 * conversion specifiers [[ and [[ Function
 * [[printnoenv]] prints the current environment as a
 * C pointer, not as a list of (name, value) pairs.
 * <lower.c>=
 */
Exp testexp(Exp e) {
    return lower(0, e);
}
