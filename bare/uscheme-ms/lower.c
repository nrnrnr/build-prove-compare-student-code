#include "all.h"
/* lower.c S350a */
#define LOWER_RETURN false // to do return-lowering exercise, change me
/* lower.c S350b */
static inline Exp lowerLet1(Name x, Exp e, Exp body) {
    return mkLetx(LET, mkNL(x, NULL), mkEL(e, NULL), body);
}
/* lower.c S350c */
static Exp lowerSequence(Exp e1, Exp e2) {
    return lowerLet1(strtoname("ignore me"), e1, e2);
}
/* lower.c S350d */
static Exp lowerBegin(Explist es) {
    if (es == NULL)
        return mkLiteral(falsev);
    else if (es->tl == NULL) 
        return es->hd;
    else
        return lowerSequence(es->hd, lowerBegin(es->tl));
}
/* lower.c S350e */
static Exp lower(LoweringContext c, Exp e);
static void lowerAll(LoweringContext c, Explist es) {
    if (es) {
        lowerAll(c, es->tl);
        es->hd = lower(c, es->hd);
    }
}
/* lower.c S350f */
static Exp lowerLetstar(Namelist xs, Explist es, Exp body) {
    if (xs == NULL) {
        assert(es == NULL);
        return body;
    } else {
        assert(es != NULL);
        return lowerLet1(xs->hd, es->hd, lowerLetstar(xs->tl, es->tl, body));
    }
}
/* lower.c S350g */
/* definition of private function [[lower]] 226e */
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
    /* other cases for lowering expression [[e]] S351f */
    case LITERAL: return e;
    case VAR:     return e;
    case IFX:     e->ifx.cond   = lower(c, e->ifx.cond);
                  e->ifx.truex  = lower(c, e->ifx.truex);
                  e->ifx.falsex = lower(c, e->ifx.falsex);
                  return e;
    /* other cases for lowering expression [[e]] S352a */
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
    /* other cases for lowering expression [[e]] S352b */
    case BEGIN: 
        lowerAll(c, e->begin);
        return mkLowered(e, lowerBegin(e->begin));
    /* other cases for lowering expression [[e]] S352c */
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
    /* other cases for lowering expression [[e]] S352d */
    case LAMBDAX: {
        LoweringContext nc = FUNCONTEXT;  // no loop!
        Exp body   = lower(nc, e->lambdax.body);
        e->lambdax.body =
            LOWER_RETURN ? mkLowered(e->lambdax.body, mkLongLabel(strtoname(
                                                              ":return"), body))
                         : body;
        return e;
    }
    /* other cases for lowering expression [[e]] S352e */
    case APPLY:
        lowerAll(c, e->apply.actuals);
        e->apply.fn = lower(c, e->apply.fn);
        return e;
    /* other cases for lowering expression [[e]] S352f */
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
    /* other cases for lowering expression [[e]] S353a */
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
    /* other cases for lowering expression [[e]] S353b */
    case THROW: {
        Name h = strtoname("the-;-handler");
        Name x = strtoname("the-;-answer");
        Lambda thrown =
            mkLambda(mkNL(h, NULL), mkApply(mkVar(h), mkEL(mkVar(x), NULL)));
        Exp throw = mkLongGoto(e->throw.label, mkLambdax(thrown));
        Exp lowered = lowerLet1(x, lower(c, e->throw.exp), throw);
        return mkLowered(e, lowered);
    }
    /* other cases for lowering expression [[e]] S353c */
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
/* lower.c S351a */
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
/* lower.c S351c */
void lowerXdef(XDef d) {
    switch (d->alt) {
    case DEF: lowerDef(d->def); break;
    case USE: break;
    case TEST: break;  // lowering is delayed until testing time
    default: assert(0);
    }
}
/* lower.c S351d */
Exp testexp(Exp e) {
    return lower(0, e);
}
