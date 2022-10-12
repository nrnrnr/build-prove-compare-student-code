#include "all.h"
/*
 * <loc.c>=
 */
Value* allocate(Value v) {
    pushreg(&v);
    Value *loc = allocloc();
    popreg(&v);
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/*
 * \qbreak
 * 
 * Access to the desired size of the heap
 * 
 * The size of the garbage-collected heap can be
 * controlled by setting the micro-Scheme variable [[
 * --- gamma-desired]], as described in \cref
 * gc.ex.gammadesired-ms,gc.ex.gammadesired-copy in \
 * crefgc.chap. The value of that variable, if any, is
 * fetched by function [[gammadesired]]. [*]
 * <loc.c>=
 */
int gammadesired(int defaultval, int minimum) {
    assert(roots.globals.user != NULL);
    Value *gammaloc = find(strtoname("&gamma-desired"), *roots.globals.user);
    if (gammaloc && gammaloc->alt == NUM)
        return gammaloc->num > minimum ? gammaloc->num : minimum;
    else
        return defaultval;
}
/*
 * When the collector is initialized, function
 * [[initallocate]] uses the ANSI C function [[atexit]]
 * to make sure that before the program exits, final
 * garbage-collection statistics are printed.
 * <loc.c>=
 */
extern void printfinalstats(void);
void initallocate(Env *globals) {
    gc_debug_init();
    roots.globals.user                   = globals;
    roots.globals.internal.pending_tests = NULL;
    roots.stack     = emptystack();
    roots.registers = NULL;
    atexit(printfinalstats);
}
