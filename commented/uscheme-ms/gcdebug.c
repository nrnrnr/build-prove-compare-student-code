#include "all.h"
/*
 * GC debugging, with or without Valgrind
 * 
 * [*] As I write, the gold standard for debugging
 * memory errors is Valgrind (or a similar tool like
 * Pin). Valgrind's Memcheck tool tracks the status of
 * every bit of every word of memory; it knows what bits
 * have been initialized and what bits haven't been
 * initialized, and it knows which parts of memory the
 * program has permission to read and write. Although
 * Memcheck was designed to find general memory errors
 * in unsafe programs, not specifically to debug garbage
 * collectors, it is still an invaluable asset. And with
 * the help of the code below, Valgrind can be used
 * implement the debugging interface described in \cref
 * gc.gc-debug. (If you don't have Valgrind, you can
 * turn it off with one C macro.)
 * 
 * This code implements the debugging interface
 * described in \secrefgc.gc-debug. It finds bugs in
 * three ways:
 * 
 *   • When memory belongs to the collector and not the
 *  interpreter, the [[alt]] field is set to
 *  [[INVALID]]. If [[validate]] is called with an
 *  [[INVALID]] expression, it dies.
 *   • When memory belongs to the collector and not the
 *  interpreter, Valgrind is informed that nobody
 *  must read or write it. If your collector
 *  mistakenly reclaims memory that the interpreter
 *  still has access to, when the interpreter tries
 *  to read or write that memory, Valgrind will
 *  bleat. (Valgrind is discussed briefly in \
 *  crefpage,gc.valgrind.)
 * 
 *   • When memory is given from the collector to the
 *  interpreter, Valgrind is informed that it is OK
 *  to write but not OK to read until it has been
 *  initialized.
 * 
 * If you don't have Valgrind, you can [[#define
 * NOVALGRIND]], and you'll still have the [[INVALID]]
 * thing in the [[alt]] field to help you.
 * <gcdebug.c>=
 */
#ifndef NOVALGRIND
  #include <valgrind/memcheck.h>
#else
  /*
   * <define do-nothing replacements for Valgrind macros>=
   */
  #define VALGRIND_CREATE_BLOCK(p, n, s)     ((void)(p),(void)(n),(void)(s))
  #define VALGRIND_CREATE_MEMPOOL(p, n, z)   ((void)(p),(void)(n),(void)(z))
  #define VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(p, n) \
                                             ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_DEFINED(p, n)    ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_UNDEFINED(p, n)  ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_NOACCESS(p, n)   ((void)(p),(void)(n))
  #define VALGRIND_MEMPOOL_ALLOC(p1, p2, n)  ((void)(p1),(void)(p2),(void)(n))
  #define VALGRIND_MEMPOOL_FREE(p1, p2)      ((void)(p1),(void)(p2))
#endif
/*
 * <gcdebug.c>=
 */
static int gc_pool_object;
static void *gc_pool = &gc_pool_object;  /* valgrind needs this */
static int gcverbose;  /* GCVERBOSE tells gcprintf & gcprint to make noise */

void gc_debug_init(void) {
    VALGRIND_CREATE_MEMPOOL(gc_pool, 0, gc_uses_mark_bits);
    gcverbose = getenv("GCVERBOSE") != NULL;
}
/*
 * When new objects are acquired from the operating
 * system, each one is marked invalid and is made known
 * to Valgrind. Because the objects' memory belongs to
 * the collector, it is marked as inaccessible.
 * <gcdebug.c>=
 */
void gc_debug_post_acquire(Value *mem, unsigned nvalues) {
    unsigned i;
    for (i = 0; i < nvalues; i++) {
        gcprintf("ACQUIRE %p\n", (void*)&mem[i]);
        mem[i] = mkInvalid("memory acquired from OS");
        VALGRIND_CREATE_BLOCK(&mem[i], sizeof(*mem), "managed Value");
    }
    /*
     * <when using mark bits, barf unless [[nvalues]] is 1>=
     */
    if (gc_uses_mark_bits) /* mark and sweep */
        assert(nvalues == 1);
    VALGRIND_MAKE_MEM_NOACCESS(mem, nvalues * sizeof(*mem));
}
/*
 * Before memory is released, the code below checks that
 * each object is invalid. Valgrind has to be told that
 * it's temporarily OK to look at the object.
 * <gcdebug.c>=
 */
void gc_debug_pre_release(Value *mem, unsigned nvalues) {
    unsigned i;
    for (i = 0; i < nvalues; i++) {
        gcprintf("RELEASE %p\n", (void*)&mem[i]);
        VALGRIND_MAKE_MEM_DEFINED(&mem[i].alt, sizeof(mem[i].alt));
        assert(mem[i].alt == INVALID);
    }
    VALGRIND_MAKE_MEM_NOACCESS(mem, nvalues * sizeof(*mem));
}
/*
 * Before an object is handed to the interpreter,
 * Valgrind is told that the object has been allocated,
 * then the object is made invalid, and finally Valgrind
 * is told that the object is writable but
 * uninitialized.
 * <gcdebug.c>=
 */
void gc_debug_pre_allocate(Value *mem) {
    gcprintf("ALLOC %p\n", (void*)mem);
    VALGRIND_MEMPOOL_ALLOC(gc_pool, mem, sizeof(*mem));
    VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(&mem->alt, sizeof(mem->alt));
    assert(mem->alt == INVALID);
    *mem = mkInvalid("allocated but uninitialized");
    VALGRIND_MAKE_MEM_UNDEFINED(mem, sizeof(*mem));    
}
/*
 * \qbreak When an object is reclaimed, it should not be
 * invalid—because it should have been initialized to a
 * valid value immediately after it was allocated. After
 * the object's validity is confirmed, the object is
 * marked invalid, and Valgrind is told that the object
 * has been freed.
 * <gcdebug.c>=
 */
void gc_debug_post_reclaim(Value *mem) {
    gcprintf("FREE %p\n", (void*)mem);
    assert(mem->alt != INVALID);
    *mem = mkInvalid("memory reclaimed by the collector");
    VALGRIND_MEMPOOL_FREE(gc_pool, mem);
}
/*
 * Objects in a block can be reclaimed in one call,
 * provided the block is an array of [[Value]], not an
 * array of [[Mvalue]].
 * <gcdebug.c>=
 */
void gc_debug_post_reclaim_block(Value *mem, unsigned nvalues) {
    unsigned i;
    /*
     * <when using mark bits, barf unless [[nvalues]] is 1>=
     */
    if (gc_uses_mark_bits) /* mark and sweep */
        assert(nvalues == 1);
    for (i = 0; i < nvalues; i++)
        gc_debug_post_reclaim(&mem[i]);
}
/*
 * \qbreak Functions that print GC diagnostics are
 * defined here.
 * <gcdebug.c>=
 */
void gcprint(const char *fmt, ...) {
  if (gcverbose) {
    va_list_box box;
    Printbuf buf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(buf, fmt, &box);
    va_end(box.ap);
    fwritebuf(buf, stderr);
    fflush(stderr);
    freebuf(&buf);
  }
}
/*
 * <gcdebug.c>=
 */
void gcprintf(const char *fmt, ...) {
  if (gcverbose) {
    va_list args;

    assert(fmt);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fflush(stderr);
  }
}
/*
 * The code uses depth-first search to make sure no
 * value is ever its own ancestor.
 * <gcdebug.c>=
 */
struct va { /* value ancestors */
    Value *l;
    struct va *parent;
};
/*
 * <gcdebug.c>=
 */
static void check(Value *l, struct va *ancestors) {
    struct va *c;
    for (c = ancestors; c; c = c->parent)
        if (l == c->l) {
            fprintf(stderr, "%p is involved in a cycle\n", (void *)l);
            if (c == ancestors) {
                fprintf(stderr, "%p -> %p\n", (void *)l, (void *)l);
            } else {
                fprintf(stderr, "%p -> %p\n", (void *)l, (void *)ancestors->l);
                while (ancestors->l != l) {
                    fprintf(stderr, "%p -> %p\n",
                            (void *)ancestors->l, (void *)ancestors->parent->l);
                    ancestors = ancestors->parent;
                }
            }
            runerror("cycle of cons cells");
        }
}
/*
 * \qbreak
 * <gcdebug.c>=
 */
static void search(Value *v, struct va *ancestors) {
    if (v->alt == PAIR) {
        struct va na;  // new ancestors
        check(v->pair.car, ancestors);
        check(v->pair.cdr, ancestors);
        na.l = v;
        na.parent = ancestors;
        search(v->pair.car, &na);
        search(v->pair.cdr, &na);
    }
}

void cyclecheck(Value *l) {
    search(l, NULL);
}
