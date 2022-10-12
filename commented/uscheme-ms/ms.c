#include "all.h"
/*
 * What's new here are the calls to [[pushreg]] and
 * [[popreg]]. When [[v]] is a [[cons]] cell and
 * [[allocloc]] happens to call the garbage collector,
 * [[pushreg]] and [[popreg]] prevent the garbage
 * collector from reclaiming the locations pointed to by
 * [[v.pair.car]] and [[v.pair.cdr]]. The call to
 * [[pushreg]] makes [[v]] a ``machine register'' and
 * ensures that the collector treats it as a root. And
 * if the collector happens to move [[v]]'s [[car]] and
 * [[cdr]], it updates [[v]]'s internal pointers to
 * point to the new locations.
 * 
 * Mark-and-sweep \chaptocsplitcollection
 * 
 * [*]
 * 
 * The original garbage-collection technique, invented
 * by \citetmccarthy:recursive for \lisp, is called
 * mark-and-sweep. It allocates available objects from a
 * data structure called the free list. To track when
 * objects become available, it uses an extra bit for
 * each object, called the mark bit. These data
 * structures support conceptually simple algorithms for
 * allocating and recovering objects:
 * 
 *  1. When an object is requested, look for a suitable
 *  object is on the free list. If there isn't one,
 *  ask the collector to recover some objects.
 *  2. Remove an object from the free list and return
 *  it.
 * 
 * The collector recovers objects in two phases:
 * 
 *  1. Mark (i.e., set the mark bit associated with)
 *  every reachable object. This phase traverses the
 *  heap starting from the roots.
 *  2. Sweep every object in the heap. Unmarked objects
 *  are unreachable. Place each unreachable object on
 *  the free list, and clear the mark bit associated
 *  with each reachable object.
 * 
 * If these algorithms are implemented \naively, the
 * sweep phase visits the entire heap. That's a lot of
 * objects, and they probably don't all fit in the
 * cache. The mutator might pause for a long time. Or
 * the allocator could do the sweeping. It too must
 * visit the entire heap, but the visit is spread out
 * over many allocation requests. Since the collector
 * only has to mark, its running time drops. In this
 * variant, called lazy sweeping, the allocator keeps a
 * pointer into the managed heap, advancing the pointer
 * one or more objects until it encounters one that can
 * be reused. Such an allocator is sketched below; in \
 * crefgc.ex.ms-allocate, you complete the sketch.
 * 
 * Prototype mark-and-sweep allocator for micro-Scheme
 * 
 * A mark-and-sweep system associates a mark bit with
 * each heap location. To keep things simple, I don't
 * try to pack mark bits densely; I just wrap each
 * [[Value]] in another structure, which holds a single
 * mark bit, [[live]]. By placing the [[Value]] at the
 * beginning, I ensure that it is safe to cast between
 * values of type [[Value*]] and type [[Mvalue*]].
 * <ms.c ((elided))>=
 */
/*
 * <private declarations for mark-and-sweep collection>=
 */
typedef struct Mvalue Mvalue;
struct Mvalue {
    Value v;
    unsigned live:1;
};
/*
 * The use of mark bits has to be announced to my
 * debugging interface (\secrefgc.gc-debug).
 */

/*
 * The [[MValue]] structures are grouped into pages. A
 * single page holds a contiguous array of objects;
 * pages are linked together into a list, which forms
 * the heap. The page is the unit of heap growth; when
 * the heap is too small, the collector calls [[malloc]]
 * to add one or more pages to the heap.
 * <private declarations for mark-and-sweep collection>=
 */
#ifndef GCHYPERDEBUG /*OMIT*/
#define GROWTH_UNIT 24 /* increment in which the heap grows, in objects */
#else /*OMIT*/
#define GROWTH_UNIT 3 /*OMIT*/
#endif /*OMIT*/
typedef struct Page Page;
struct Page {
    Mvalue pool[GROWTH_UNIT];
    Page *tl;
};
/*
 * The [[tl]] field links pages into a list that is
 * referred to by multiple pointers. Pointer
 * [[pagelist]] points to the head of the list, that is,
 * the entire heap. The ``heap pointer'' [[hp]] points
 * to the next [[Mvalue]] to be allocated. And
 * [[heaplimit]] points to the first [[Mvalue]] after
 * the current page, [[curpage]].
 * <private declarations for mark-and-sweep collection>=
 */
Page *pagelist, *curpage;
Mvalue *hp, *heaplimit;
/*
 * <private declarations for mark-and-sweep collection>=
 */
static void visitloc          (Value *loc);
static void visitvalue        (Value v);
static void visitenv          (Env env);
static void visitexp          (Exp exp);
static void visitexplist      (Explist es);
static void visitframe        (Frame *fr);
static void visitstack        (Stack s);
static void visittest         (UnitTest t);
static void visittestlists    (UnitTestlistlist uss);
static void visitregister     (Register reg);
static void visitregisterlist (Registerlist regs);
static void visitroots        (void);
/*
 * <private declarations for mark-and-sweep collection>=
 */
static int nalloc;              /* total number of allocations */
static int ncollections;        /* total number of collections */
static int nmarks;              /* total number of cells marked */
/*
 * <ms.c>=
 */
bool gc_uses_mark_bits = true;
/*
 * The pointers and page list look like this:
 * 
 *  {tikzpicture}[x=14pt,y=14pt, get units, heappage
 *  /.style=anchor=south west,minimum height=\yunit,
 *  minimum width=5\xunit, shape=rectangle, square
 *  /.style=draw,minimum height=14pt, minimum width=
 *  14pt, shape=rectangle, ]
 * 
 *  \node[draw=none,fill=none] (pl) at (-1,1.7)
 *  pagelist; \node[draw=none,fill=none] (cp) at (12,
 *  1.7) curpage; \node[draw=none,fill=none] (hp) at
 *  (12, -.7) hp; \node[draw=none,fill=none] (hl) at
 *  (12, -1.7) heaplimit;
 * 
 *  \nodeat (1.5,0) [heappage,draw] (page1) ;
 * 
 *  \node[square,anchor=east] at (page1.east) (dot1)
 *  \bullet;
 * 
 *  \nodeat (8,0) [heappage,draw] (page2) ;
 * 
 *  \node[square,anchor=east] at (page2.east) (dot2)
 *  \bullet;
 * 
 *  \nodeat (14.5,0) [heappage,draw] (page3) ;
 * 
 *  \draw[->] (dot1.center) – (page2.west);
 * 
 *  \draw[->] (dot2.center) – (page3.west);
 * 
 *  \node[pattern=fromspace,rectangle,draw,minimum
 *  width=2.5\xunit,minimum height=\yunit,anchor=
 *  east] (crossed) at ((page3.east)-(1,0)) ;
 * 
 *  \draw(crossed.south east) – (page3.north east);
 * 
 *  \draw[->] (pl.east) to[out=0,in=110] (page1.north
 *  west); \draw[->] (cp) to[out=0,in=110]
 *  (page3.north west); \draw[->] (hp) .. controls +
 *  (2,0.0) and +(-0.35,-0.8) .. (crossed.south
 *  west); \draw[->] (hl) .. controls +(4,0) and +
 *  (0,-1.5) .. (crossed.south east);
 * 
 *  {tikzpicture}
 * 
 * White areas have been allocated; areas marked in gray
 * diamonds are available for allocation. Pages except
 * the current one are entirely used. The number of
 * unallocated cells in the current page is \monobox
 * heaplimit - hp.
 * 
 * A fresh page is made current by [[makecurrent]].
 * <ms.c>=
 */
static void makecurrent(Page *page) {
    assert(page != NULL);
    curpage = page;
    hp = &page->pool[0];
    heaplimit = &page->pool[GROWTH_UNIT];
}
/*
 * When the heap grows, it grows by one page at a time.
 * Each new page is allocated with [[calloc]], so its
 * mark bits are zeroed.
 * <ms.c>=
 */
static int heapsize;            /* OMIT */
static void addpage(void) {
    Page *page = calloc(1, sizeof(*page));
    assert(page != NULL);
    /*
     * Some of the debugging functions are used in some of
     * the prototype code above:
     * <tell the debugging interface that each object on [[page]] has been
                                                                      acquired>=
     */
    {   unsigned i;
        for (i = 0; i < sizeof(page->pool)/sizeof(page->pool[0]); i++)
            gc_debug_post_acquire(&page->pool[i].v, 1);
    }

    if (pagelist == NULL) {
        pagelist = page;
    } else {
        assert(curpage != NULL && curpage->tl == NULL);
        curpage->tl = page;
    }
    makecurrent(page);
    heapsize += GROWTH_UNIT;   /* OMIT */
}
/*
 * It is a checked run-time error to call [[addpage]]
 * except when [[pagelist]] is [[NULL]] or when
 * [[curpage]] points to the last page in the list.
 * 
 * Writing the allocator is your job (\cref
 * gc.ex.ms-alloc). But I provide a prototype that does
 * not collect garbage; when it runs out of space,
 * it adds a new page. [*]
 * <ms.c ((prototype))>=
 */
Value* allocloc(void) {
    if (hp == heaplimit)
        addpage();
    assert(hp < heaplimit);
    /*
     * <tell the debugging interface that [[&hp->v]] is about to be allocated>=
     */
    gc_debug_pre_allocate(&hp->v);
    return &(hp++)->v;
}
/*
 * Most ``visit'' procedures are easy to write. As an
 * example, the visit procedure for an environment
 * visits all of its [[loc]] pointers.
 * <ms.c>=
 */
static void visitenv(Env env) {
    for (; env; env = env->tl)
        visitloc(env->loc);
}
/*
 * The most important such procedure visits a location
 * and sets its mark bit. Unless the location has been
 * visited already, its value is also visited.
 * <ms.c ((prototype))>=
 */
static void visitloc(Value *loc) {
    Mvalue *m = (Mvalue*) loc;
    if (!m->live) {
        m->live = 1;
        visitvalue(m->v);
    }
}
/*
 * A register is different from a heap location:
 * a register has no mark bit.
 * <ms.c>=
 */
static void visitregister(Value *reg) {
    visitvalue(*reg);
}
/*
 * <ms.c>=
 */
static void visitvalue(Value v) {
    switch (v.alt) {
    case NIL:
    case BOOLV:
    case NUM:
    case SYM:
    case PRIMITIVE:
        return;
    case PAIR:
        visitloc(v.pair.car);
        visitloc(v.pair.cdr);
        return;
    case CLOSURE:
        visitexp(v.closure.lambda.body);
        visitenv(v.closure.env);
        return;
    default:
        assert(0);
        return;
    }
    assert(0);
}
/*
 * <ms.c>=
 */
static void visitexp(Exp e) {
    switch (e->alt) {
    /*
     * <cases for [[visitexp]]>=
     */
    case LITERAL:
        visitvalue(e->literal);
        return;
    case VAR:
        return;
    case IFX:
        visitexp(e->ifx.cond);
        visitexp(e->ifx.truex);
        visitexp(e->ifx.falsex);
        return;
    case WHILEX:
        visitexp(e->whilex.cond);
        visitexp(e->whilex.body);
        return;
    case BEGIN:
        visitexplist(e->begin);
        return;
    /*
     * <cases for [[visitexp]]>=
     */
    case SET:
        visitexp(e->set.exp);
        return;
    case LETX:
        visitexplist(e->letx.es);
        visitexp(e->letx.body);
        return;
    case LAMBDAX:
        visitexp(e->lambdax.body);
        return;
    case APPLY:
        visitexp(e->apply.fn);
        visitexplist(e->apply.actuals);
        return;
    /*
     * Next, the \uschemeplus expressions:
     * <cases for [[visitexp]]>=
     */
    case BREAKX:
        return;
    case CONTINUEX:
        return;
    case RETURNX:
        visitexp(e->returnx);
        return;
    case THROW:
        visitexp(e->throw.exp);
        return;
    case TRY_CATCH:
        visitexp(e->try_catch.handler);
        visitexp(e->try_catch.body);
        return;
    case LONG_GOTO:
        visitexp(e->long_goto.exp);
        return;
    case LONG_LABEL:
        visitexp(e->long_label.body);
        return;
    case LOWERED:
        visitexp(e->lowered.before);
        return;
    case LOOPBACK:
        return;
    /*
     * Last, \uschemeplus evaluation contexts:
     * <cases for [[visitexp]]>=
     */
    case ENV:
        visitenv(e->env.contents);
        return;
    case HOLE:
        return;
    }
    assert(0);
}
/*
 * A list of expressions is visited by [[visitexplist]].
 * <ms.c>=
 */
static void visitexplist(Explist es) {
    for (; es; es = es->tl)
        visitexp(es->hd);
}
/*
 * A list of registers is visited by
 * [[visitregisterlist]].
 * <ms.c>=
 */
static void visitregisterlist(Registerlist regs) {
    for ( ; regs != NULL; regs = regs->tl)
        visitregister(regs->hd);
}
/*
 * A [[Stack]] is visited by visiting every frame.
 * A frame can be seen only if the representation is
 * exposed, so [[<<representation of [[struct
 * Stack]]>>]] is defined here.
 * <ms.c>=
 */
/*
 * <representation of [[struct Stack]]>=
 */
struct Stack {
    int size;
    Frame *frames;  // memory for 'size' frames
    Frame *sp;      // points to first unused frame
};
static void visitstack(Stack s) {
    Frame *fr;
    for (fr = s->frames; fr < s->sp; fr++) {
        visitframe(fr);
    }
}
/*
 * Visiting a frame means visiting both of its
 * expressions.
 * <ms.c>=
 */
static void visitframe(Frame *fr) {
    visitexp(&fr->form);
    if (fr->syntax != NULL)
        visitexp(fr->syntax);
}
/*
 * Visiting lists of pending unit tests means visiting
 * all tests on the list.
 * <ms.c>=
 */
static void visittestlists(UnitTestlistlist uss) {
    UnitTestlist ul;

    for ( ; uss != NULL; uss = uss->tl)
        for (ul = uss->hd; ul; ul = ul->tl)
            visittest(ul->hd);
}
/*
 * Visiting a unit test means visiting its component
 * expressions.
 * <ms.c>=
 */
static void visittest(UnitTest t) {
    switch (t->alt) {
    case CHECK_EXPECT:
        visitexp(t->check_expect.check);
        visitexp(t->check_expect.expect);
        return;
    case CHECK_ASSERT:
        visitexp(t->check_assert);
        return;
    case CHECK_ERROR:
        visitexp(t->check_error);
        return;
    }
    assert(0);
}
/*
 * \qbreak Visiting roots means visiting the global
 * variables, the stack, and any machine registers.
 * <ms.c>=
 */
static void visitroots(void) {
    visitenv(*roots.globals.user);
    visittestlists(roots.globals.internal.pending_tests);
    visitstack(roots.stack);
    visitregisterlist(roots.registers);
}
/*
 * <ms.c ((prototype))>=
 */
/* you need to redefine these functions */
void printfinalstats(void) { 
  (void)nalloc; (void)ncollections; (void)nmarks;
  assert(0); 
}
/*
 * <ms.c ((prototype))>=
 */
void avoid_unpleasant_compiler_warnings(void) {
    (void)visitroots;
}
/*
 * Completed garbage collectors
 * 
 * [Table of contents]
 * 
 * [*]
 * 
 * Mark and sweep
 * 
 * These variables help us accumulate statistical
 * information; they are totals for the lifetime of the
 * program.
 */

