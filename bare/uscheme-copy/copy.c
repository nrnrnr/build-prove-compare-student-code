#include "all.h"
/* copy.c ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) */
/* private declarations for copying collection 276a */
static Value *fromspace, *tospace; // used only at GC time
static int semispacesize;          // # of objects in fromspace or tospace
/* private declarations for copying collection 276b */
static Value *hp, *heaplimit;      // used for every allocation
/* private declarations for copying collection 277a */
static void scanenv      (Env env);
static void scanexp      (Exp exp);
static void scanexplist  (Explist es);
static void scanframe    (Frame *fr);
static void scantest     (UnitTest t);
static void scantests    (UnitTestlist ts);
static void scanloc      (Value *vp);
/* private declarations for copying collection 278b */
static inline bool isinspace(Value *loc, Value *space) {
    return space <= loc && loc < space + semispacesize;
}
static Value *forward(Value *p);
/* private declarations for copying collection S377e */
static void collect(void);
/* copy.c ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) */
/* representation of [[struct Stack]] S343a */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};
/* copy.c 276c */
int nalloc;   /* OMIT */
Value* allocloc(void) {
    if (hp == heaplimit)
        collect();
    assert(hp < heaplimit);
    assert(isinspace(hp, fromspace)); /*runs after spaces are swapped*/ /*OMIT*/
    nalloc++;   /* OMIT */
    /* tell the debugging interface that [[hp]] is about to be allocated 282f */
    gc_debug_pre_allocate(hp);
    return hp++;
}
/* copy.c 277b */
static void scanenv(Env env) {
    for (; env; env = env->tl)
      { /*OMIT*/
        env->loc = forward(env->loc);
        assert(isinspace(env->loc, tospace)); /*OMIT*/
      } /*OMIT*/
}
/* copy.c 277c */
static void scanloc(Value *vp) {
    switch (vp->alt) {
    case NIL:
    case BOOLV:
    case NUM:
    case SYM:
        return;
    case PAIR:
        vp->pair.car = forward(vp->pair.car);
        vp->pair.cdr = forward(vp->pair.cdr);
        return;
    case CLOSURE:
        scanexp(vp->closure.lambda.body);
        scanenv(vp->closure.env);
        return;
    case PRIMITIVE:
        return;
    default:
        assert(0);
        return;
    }
}
/* copy.c 278a */
static Value* forward(Value *p) {
    if (isinspace(p, tospace)) {
        /* already in to space; must belong to scanned root */
        return p;
    } else {
        assert(isinspace(p, fromspace));
        /* forward pointer [[p]] and return the result 272b */
        if (p->alt == FORWARD) {
            assert(isinspace(p->forward, tospace));   /* OMIT */
            return p->forward;
        } else {
            assert(isinspace(hp, tospace)); /* there is room */   /* OMIT */

    /* tell the debugging interface that [[hp]] is about to be allocated 282f */
            gc_debug_pre_allocate(hp);
            *hp = *p;
            *p  = mkForward(hp);
                                /* overwrite *p with a new forwarding pointer */
            assert(isinspace(p->forward, tospace)); /*extra*/   /* OMIT */
            return hp++;
        }
    }
    return NULL; /* appease a stupid compiler */  /*OMIT*/
}
/* copy.c S366f */
static void scanexp(Exp e) {
    switch (e->alt) {
    /* cases for [[scanexp]] S366g */
    case LITERAL:
        scanloc(&e->literal);
        return;
    case VAR:
        return;
    case IFX:
        scanexp(e->ifx.cond);
        scanexp(e->ifx.truex);
        scanexp(e->ifx.falsex);
        return;
    /* cases for [[scanexp]] S367a */
    case WHILEX:
        scanexp(e->whilex.cond);
        scanexp(e->whilex.body);
        return;
    case BEGIN:
        scanexplist(e->begin);
        return;
    case SET:
        scanexp(e->set.exp);
        return;
    case LETX:
        scanexplist(e->letx.es);
        scanexp(e->letx.body);
        return;
    case LAMBDAX:
        scanexp(e->lambdax.body);
        return;
    case APPLY:
        scanexp(e->apply.fn);
        scanexplist(e->apply.actuals);
        return;
    /* cases for [[scanexp]] S367b */
    case BREAKX:
        return;
    case CONTINUEX:
        return;
    case RETURNX:
        scanexp(e->returnx);
        return;
    case THROW:
        scanexp(e->throw.exp);
        return;
    case TRY_CATCH:
        scanexp(e->try_catch.handler);
        scanexp(e->try_catch.body);
        return;
    case LONG_GOTO:
        scanexp(e->long_goto.exp);
        return;
    case LONG_LABEL:
        scanexp(e->long_label.body);
        return;
    case LOWERED:
        scanexp(e->lowered.before);
        scanexp(e->lowered.after);
        return;
    case LOOPBACK:
        return;
    /* cases for [[scanexp]] S368a */
    case HOLE:
        return;
    case ENV:
        scanenv(e->env.contents);
        return;
    }
    assert(0);
}
/* copy.c S368b */
static void scanframe(Frame *fr) {
    scanexp(&fr->form);
    if (fr->syntax != NULL)
        scanexp(fr->syntax);
}
/* copy.c S368c */
static void scanexplist(Explist es) {
    for (; es; es = es->tl)
        scanexp(es->hd);
}
/* copy.c S368d */
static void scantests(UnitTestlist tests) {
    for (; tests; tests = tests->tl)
        scantest(tests->hd);
}
/* copy.c S368e */
static void scantest(UnitTest t) {
    switch (t->alt) {
    case CHECK_EXPECT:
        scanexp(t->check_expect.check);
        scanexp(t->check_expect.expect);
        return;
    case CHECK_ASSERT:
        scanexp(t->check_assert);
        return;
    case CHECK_ERROR:
        scanexp(t->check_error);
        return;
    }
    assert(0);
}
/* copy.c ((prototype)) S377f */
/* you need to redefine these functions */
static void collect(void) { (void)scanframe; (void)scantests; assert(0); }
void printfinalstats(void) { assert(0); }
/* you need to initialize this variable */
bool gc_uses_mark_bits;
