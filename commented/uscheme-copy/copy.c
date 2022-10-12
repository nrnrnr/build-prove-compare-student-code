#include "all.h"
/*
 * \tikzsetx=0.9cm,y=0.9cm
 * 
 * A brief example
 * 
 * I illustrate copying collection using a heap of
 * size 20. Suppose there are three roots, and the heap
 * looks like this: \pssetarrowsize=2pt 3 \xeheapdemo0
 * The reachable S-expressions are all part of one list;
 * two cons cells point to the same symbol [[b]]:
 * 
 *  \pssetlevelsep=24pt,treesep=40pt \pstree\Tp \
 *  rnode[t]blog\constree\TR\heapsymboxa \constree\Tn
 *  \constree\TR\heapsymboxb\niltree \nccurve[angleA=
 *  225,angleB=135]->T-0-0-1carT-0-0-1-1-0 \ncline->
 *  T-0blog
 * 
 * The collector begins by forwarding the roots. After
 * the first root is forwarded, the heap looks like
 * this: \xeheapdemo1 The first object in from-space has
 * been copied into to-space and replaced with a
 * forwarding pointer, which is shown with a
 * dotted line. The root now points to the copy, with
 * the pointer passing ``behind'' from-space so as not
 * to clutter the diagram. The copied object, as shown
 * by its thick border, is now gray. (Objects in the top
 * row are white.)
 * 
 * \qbreak After all three roots have been forwarded,
 * that is, after the execution of the first loop in
 * [[]], the
 * heap looks like this: \xeheapdemo3 Now the collector
 * starts scanning the gray objects located between
 * [[scanp]] and [[hp]]. Gray objects point to white
 * objects, which in a copying collector means that the
 * internal pointers in these objects point back to
 * from-space.
 * 
 * Scanning the first two gray objects does not change
 * the heap, because these objects have no internal
 * pointers. (But once [[scanp]] moves past them, the
 * first two gray objects are considered black.)
 * Scanning the third gray object (the pair) forwards
 * its two internal pointers. The symbol [['a]] has
 * already been copied into to-space, so forwarding the
 * [[car]] doesn't copy any data; it just adjusts a
 * pointer: \xeheapdemo4 Forwarding the [[cdr]] requires
 * copying another pair into to-space, however. After
 * this copy, the collector increments [[scanp]], and
 * the newly copied pair is now the only gray object—the
 * first three objects in to-space are black. \
 * xeheapdemo5
 * 
 * The collector continues copying objects pointed to by
 * [[*scanp]] until eventually [[scanp]] catches up with
 * [[hp]]. Now to-space holds only black objects \qbreak
 * and from-space holds only white objects. From-space
 * can be discarded, and the mutator can resume
 * execution. Its next allocation requests will be
 * satisfied using the four locations recovered in
 * to-space. \xeheapdemo10 By using forwarding pointers,
 * the collector has preserved the sharing of the
 * symbol [[b]] by two pairs. The same technique also
 * preserves cycles.
 * 
 * Prototype of a copying system for micro-Scheme
 * 
 * Although conceptually more elaborate than a
 * mark-and-sweep system, a copying system is easier to
 * build. You'll build one (\crefrange
 * gc.ex.copy-firstgc.ex.copy-last) based on the data
 * structures and supporting functions described in this
 * section.
 * <copy.c ((elided))>=
 */
/*
 * The semispaces [[fromspace]] and [[tospace]] each
 * have size [[semispacesize]].
 * <private declarations for copying collection>=
 */
static Value *fromspace, *tospace; // used only at GC time
static int semispacesize;          // # of objects in fromspace or tospace
/*
 * The system always allocates from [[fromspace]]. The
 * next location available to be allocated is at the
 * heap pointer [[hp]], and the end of the available
 * space is marked by [[heaplimit]]. The number of
 * locations that can be allocated before the next
 * collection is \monoboxheaplimit - hp.
 * <private declarations for copying collection>=
 */
static Value *hp, *heaplimit;      // used for every allocation
/*
 * The assertion can help detect bugs in a heap-growth
 * algorithm.
 * 
 * Tracing roots
 * 
 * [*] Just as the mark-and-sweep system has a visiting
 * procedure for each type of potential root, the
 * copying system has a scanning procedure for each type
 * of potential root. These procedures implement the
 * chunks of the form \makenowebnotdef [[<<scan...,
 * forwarding all internal pointers>>]] in \chunkref
 * gc.chunk.uses-of-scans.
 * <private declarations for copying collection>=
 */
static void scanenv      (Env env);
static void scanexp      (Exp exp);
static void scanexplist  (Explist es);
static void scanframe    (Frame *fr);
static void scantest     (UnitTest t);
static void scantests    (UnitTestlist ts);
static void scanloc      (Value *vp);
/*
 * The [[isinspace]] test contributes significantly to
 * garbage-collection time, and inlining it results in a
 * measurable improvement. [*]
 * <private declarations for copying collection>=
 */
static inline bool isinspace(Value *loc, Value *space) {
    return space <= loc && loc < space + semispacesize;
}
static Value *forward(Value *p);
/*
 * Placeholders for \chaptocsplitexercises
 * 
 * The rest of this section includes the placeholder
 * code that you are meant to replace when you do the
 * implementation exercises.
 * <private declarations for copying collection>=
 */
static void collect(void);
/*
 * <copy.c ((elided))>=
 */
/*
 * <representation of [[struct Stack]]>=
 */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};
/*
 * Allocation
 * 
 * The allocator tests for heap exhaustion, increments 
 * [[hp]], and returns the prior value of [[hp]]. In
 * real systems, [[hp]] is kept in a register and
 * [[allocloc]] is inlined. [*]
 * <copy.c>=
 */
int nalloc;   /* OMIT */
Value* allocloc(void) {
    if (hp == heaplimit)
        collect();
    assert(hp < heaplimit);
    assert(isinspace(hp, fromspace)); /*runs after spaces are swapped*/ /*OMIT*/
    nalloc++;   /* OMIT */
    /*
     * <tell the debugging interface that [[hp]] is about to be allocated>=
     */
    gc_debug_pre_allocate(hp);
    return hp++;
}
/*
 * [*] The implementations of the scanning procedures
 * are more complicated than they would be in a real
 * system. In a real system, scanning procedures would
 * simply forward internal pointers. In our system,
 * because only [[Value]] objects are allocated on the
 * heap, scanning procedures forward pointers to
 * [[Value]] objects but traverse pointers to other
 * types of objects. For example, to scan an
 * environment, the collector forwards the [[loc]]
 * pointer and traverses the [[tl]] pointer (by
 * advancing [[env]]).
 * <copy.c>=
 */
static void scanenv(Env env) {
    for (; env; env = env->tl)
      { /*OMIT*/
        env->loc = forward(env->loc);
        assert(isinspace(env->loc, tospace)); /*OMIT*/
      } /*OMIT*/
}
/*
 * The code that scans an object forwards the pointers
 * of type \monoboxValue * but traverses the pointers of
 * types [[Exp]] and [[Env]]. [*]
 * <copy.c>=
 */
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
/*
 * The remaining scanning procedures appear in Section 
 * [->]. [*]
 * 
 * Forwarding
 * 
 * The scanning procedures above closely resemble the
 * visiting procedures used by a mark-and sweep
 * collector (Sections [->] and [->]). One difference is
 * that the [[forward]] operation, as shown in chunk
 * [[]],
 * never makes a recursive call.
 * 
 * The complete implementation of [[forward]] suffers
 * from one more subtlety, which arises because a root
 * can appear on the context stack more than once.
 * For example, an evaluation stack might contain two \
 * astenv frames whose environments share a \monobox
 * Value * pointer associated with the name [[foldr]].
 * When the second such frame is scanned, the [[loc]]
 * field associated with [[foldr]] already points into
 * to-space. Such a pointer must not be forwarded.
 * <copy.c>=
 */
static Value* forward(Value *p) {
    if (isinspace(p, tospace)) {
        /* already in to space; must belong to scanned root */
        return p;
    } else {
        assert(isinspace(p, fromspace));
        /*
         * A copying collection adjusts every pointer to every
         * live object so that it points into to-space; the
         * adjustment is called forwarding the pointer. When 
         * [[p]] points to an object in from-space and [[*p]]
         * has not yet been copied, the collector copies [[*p]]
         * to [[*hp]]. When [[*p]] has already been copied, tag
         * [[p->alt]] identifies [[p->forward]] as a forwarding
         * pointer, which the collector returns without copying 
         * [[*p]] a second time.
         * <forward pointer [[p]] and return the result>=
         */
        if (p->alt == FORWARD) {
            assert(isinspace(p->forward, tospace));   /* OMIT */
            return p->forward;
        } else {
            assert(isinspace(hp, tospace)); /* there is room */   /* OMIT */
            /*
             * <tell the debugging interface that [[hp]] is about to be
                                                                     allocated>=
             */
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
/*
 * Expressions are scanned by scanning internal values
 * or subexpressions.
 * <copy.c>=
 */
static void scanexp(Exp e) {
    switch (e->alt) {
    /*
     * First, micro-Scheme expressions:
     * <cases for [[scanexp]]>=
     */
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
    /*
     * More micro-Scheme expressions.
     * <cases for [[scanexp]]>=
     */
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
    /*
     * Next, \uschemeplus expressions:
     * <cases for [[scanexp]]>=
     */
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
    /*
     * \qbreak Last, \uschemeplus evaluation contexts.
     * <cases for [[scanexp]]>=
     */
    case HOLE:
        return;
    case ENV:
        scanenv(e->env.contents);
        return;
    }
    assert(0);
}
/*
 * A frame is scanned by scanning its expressions.
 * <copy.c>=
 */
static void scanframe(Frame *fr) {
    scanexp(&fr->form);
    if (fr->syntax != NULL)
        scanexp(fr->syntax);
}
/*
 * A list of expressions is scanned by [[scanexplist]].
 * <copy.c>=
 */
static void scanexplist(Explist es) {
    for (; es; es = es->tl)
        scanexp(es->hd);
}
/*
 * <copy.c>=
 */
static void scantests(UnitTestlist tests) {
    for (; tests; tests = tests->tl)
        scantest(tests->hd);
}
/*
 * A test is scanned by scanning its expressions.
 * <copy.c>=
 */
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
/*
 * <copy.c ((prototype))>=
 */
/* you need to redefine these functions */
static void collect(void) { (void)scanframe; (void)scantests; assert(0); }
void printfinalstats(void) { assert(0); }
/* you need to initialize this variable */
bool gc_uses_mark_bits;
