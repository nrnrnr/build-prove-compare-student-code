#include "all.h"
/*
 * Options and other leftover bits
 * 
 * This section shows bits of interpreter code that
 * don't fit anywhere else.
 * 
 * The \uschemeplus interpreter has two internal options
 * that can be changed during execution: whether to
 * optimize tail calls and whether to show the high
 * stack mark after each evaluation. These options are
 * controlled by micro-Scheme variables, the values of
 * which are obtained from the global environment by
 * function [[getoption]].
 * <options.c>=
 */
Value getoption(Name name, Env env, Value defaultval) {
    Value *p = find(name, env);
    if (p)
        return *p;
    else
        return defaultval;
}
