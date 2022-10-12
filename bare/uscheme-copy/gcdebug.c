#include "all.h"
/* gcdebug.c S370a */
#ifndef NOVALGRIND
  #include <valgrind/memcheck.h>
#else
  /* define do-nothing replacements for Valgrind macros S370b */
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
/* gcdebug.c S371a */
static int gc_pool_object;
static void *gc_pool = &gc_pool_object;  /* valgrind needs this */
static int gcverbose;  /* GCVERBOSE tells gcprintf & gcprint to make noise */

void gc_debug_init(void) {
    VALGRIND_CREATE_MEMPOOL(gc_pool, 0, gc_uses_mark_bits);
    gcverbose = getenv("GCVERBOSE") != NULL;
}
/* gcdebug.c S371b */
void gc_debug_post_acquire(Value *mem, unsigned nvalues) {
    unsigned i;
    for (i = 0; i < nvalues; i++) {
        gcprintf("ACQUIRE %p\n", (void*)&mem[i]);
        mem[i] = mkInvalid("memory acquired from OS");
        VALGRIND_CREATE_BLOCK(&mem[i], sizeof(*mem), "managed Value");
    }
    /* when using mark bits, barf unless [[nvalues]] is 1 S372c */
    if (gc_uses_mark_bits) /* mark and sweep */
        assert(nvalues == 1);
    VALGRIND_MAKE_MEM_NOACCESS(mem, nvalues * sizeof(*mem));
}
/* gcdebug.c S371c */
void gc_debug_pre_release(Value *mem, unsigned nvalues) {
    unsigned i;
    for (i = 0; i < nvalues; i++) {
        gcprintf("RELEASE %p\n", (void*)&mem[i]);
        VALGRIND_MAKE_MEM_DEFINED(&mem[i].alt, sizeof(mem[i].alt));
        assert(mem[i].alt == INVALID);
    }
    VALGRIND_MAKE_MEM_NOACCESS(mem, nvalues * sizeof(*mem));
}
/* gcdebug.c S371d */
void gc_debug_pre_allocate(Value *mem) {
    gcprintf("ALLOC %p\n", (void*)mem);
    VALGRIND_MEMPOOL_ALLOC(gc_pool, mem, sizeof(*mem));
    VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(&mem->alt, sizeof(mem->alt));
    assert(mem->alt == INVALID);
    *mem = mkInvalid("allocated but uninitialized");
    VALGRIND_MAKE_MEM_UNDEFINED(mem, sizeof(*mem));    
}
/* gcdebug.c S372a */
void gc_debug_post_reclaim(Value *mem) {
    gcprintf("FREE %p\n", (void*)mem);
    assert(mem->alt != INVALID);
    *mem = mkInvalid("memory reclaimed by the collector");
    VALGRIND_MEMPOOL_FREE(gc_pool, mem);
}
/* gcdebug.c S372b */
void gc_debug_post_reclaim_block(Value *mem, unsigned nvalues) {
    unsigned i;
    /* when using mark bits, barf unless [[nvalues]] is 1 S372c */
    if (gc_uses_mark_bits) /* mark and sweep */
        assert(nvalues == 1);
    for (i = 0; i < nvalues; i++)
        gc_debug_post_reclaim(&mem[i]);
}
/* gcdebug.c S373a */
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
/* gcdebug.c S373b */
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
/* gcdebug.c S376c */
struct va { /* value ancestors */
    Value *l;
    struct va *parent;
};
/* gcdebug.c S376d */
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
/* gcdebug.c S377a */
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
