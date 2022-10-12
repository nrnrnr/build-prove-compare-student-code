#include "all.h"
/*
 * Memory allocation
 * 
 * In this chapter, a new location is allocated with
 * [[malloc]]. [*]
 * <loc.c>=
 */
Value* allocate(Value v) {
    Value *loc = malloc(sizeof(*loc));
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/*
 * Memory allocation
 * 
 * The micro-Scheme interpreter in \crefscheme.chap
 * allocates new objects using [[malloc]], which
 * requires no special initialization or resetting.
 * A more interesting implementation of [[initallocate]]
 * is found in \crefgcsa.chap, which supports the
 * garbage-collecting interpreters described in \cref
 * gc.chap.
 * <loc.c>=
 */
void initallocate(Env *globals) {
    (void)globals;
}
