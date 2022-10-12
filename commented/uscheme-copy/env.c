#include "all.h"
/*
 * A name is looked up by following [[tl]] pointers. [*]
 * <env.c>=
 */
Value* find(Name name, Env env) {
    for (; env; env = env->tl)
        if (env->name == name)
            return env->loc;
    return NULL;
}
/*
 * An environment can be printed, which can be useful if
 * you need to debug your code.
 * <env.c>=
 */
void printenv(Printbuf output, va_list_box *box) {
    char *prefix = " ";

    bprint(output, "{");
    for (Env env = va_arg(box->ap, Env); env; env = env->tl) {
        bprint(output, "%s%n -> %v", prefix, env->name, *env->loc);
        prefix = ", ";
    }
    bprint(output, " }");
}
/*
 * And an environment's names can be printed, which is
 * what the -names and -primitives options to the
 * interpreter do.
 * <env.c>=
 */
void dump_env_names(Env env) {
    for ( ; env; env = env->tl)
        fprint(stdout, "%n\n", env->name);
}
/*
 * \qbreak
 * 
 * Code that is changed to support garbage \
 * chaptocsplitcollection
 * 
 * When the garbage collector is deployed, most parts of
 * the \uschemeplus interpreter are either replaced
 * completely or are used without change. But a few
 * parts are modified versions of the originals. The
 * modifications help to keep track of the root set:
 * any code that can allocate is modified to make sure
 * that before [[allocloc]] is called, the root set is
 * up to date.
 * 
 * To keep the root set up to date, my code abuses the
 * stack of evaluation contexts. If it needs to save an
 * [[Exp]] or an [[Env]], for example, my code pushes an
 * appropriate context. Because the context is popped
 * immediately after the allocation, these abusive
 * contexts are never seen by the evaluator, and
 * therefore they don't interfere with it. (If my code
 * needs to save a [[Value]], it simply uses [[pushreg]]
 * or [[pushregs]] as intended.)
 * 
 * Code that is modified or added to support garbage
 * collection is shown in typewriter italics.
 * 
 * Revised environment-extension routines
 * 
 * To be sure that the current environment is always
 * visible to the garbage collector, \usp needs a new
 * version of [[bindalloc]]. When [[bindalloc]] is
 * called, its [[env]] argument contains bindings to
 * heap-allocated locations. \qbreak And because [[env]]
 * is a local variable in [[eval]], it doesn't appear on
 * the stack of evaluation contexts. It gets put on the
 * stack so that when [[allocate]] is called, the
 * bindings in [[env]] are kept live.
 * <env.c>=
 */
Env bindalloc(Name name, Value val, Env env) {
    Env newenv = malloc(sizeof(*newenv));
    assert(newenv != NULL);

    newenv->name = name;
    pushframe(mkEnvStruct(env, NONCALL), roots.stack);
    newenv->loc  = allocate(val);
    popframe(roots.stack);
    newenv->tl   = env;
    return newenv;
}
/*
 * Please also observe that [[val]] is a parameter
 * passed by value, so [[bindalloc]] has a fresh copy of
 * it. Because [[val]] contains [[Value*]] pointers, you
 * might think it needs to be on the root stack for the
 * copying collector (so that the pointers can be
 * updated if necessary). But by the time [[allocate]]
 * is called, this copy of [[val]] is dead—only
 * [[allocate]]'s private copy matters.
 * 
 * In [[bindalloclist]], by contrast, when [[bindalloc]]
 * is called with [[vs->hd]], not everything is dead.
 * The value [[vs->hd]] is dead, as is everything that
 * precedes it on list [[vs]]. But values reachable from
 * [[vs->tl]] are still live. To make them visible to
 * the garbage collector, [[bindalloclist]] treats the
 * entire list [[vs]] as ``machine registers.''
 */

/*
 * <env.c>=
 */
Env bindalloclist(Namelist xs, Valuelist vs, Env env) {
    Valuelist oldvals = vs;
    pushregs(oldvals);
    for (; xs && vs; xs = xs->tl, vs = vs->tl)
        env = bindalloc(xs->hd, vs->hd, env);
    popregs(oldvals);
    return env;
}
