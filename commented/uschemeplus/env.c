#include "all.h"
/*
 * Implementation of micro-Scheme environments
 * 
 * micro-Scheme's environments are significantly
 * different from Impcore environments, but not so
 * dramatically different that it's worth showing their
 * implementation in \crefscheme.chap. The big
 * difference in a micro-Scheme environment is that
 * evaluating a [[lambda]] expression copies an
 * environment, and that copy can be extended.
 * The possibility of copying rules out the
 * mutate-in-place optimization I use in Impcore's
 * environments, and it militates toward a different
 * representation.
 * 
 * First, and most important, environments are
 * immutable, as you can tell by analyzing the interface
 * in \crefscheme.chap (\cpagerefscheme.env-interface).
 * The operational semantics never mutates an
 * environment, and there is really no need, because
 * only locations are mutated. Moreover, if environments
 * could be mutated then it wouldn't be safe to copy
 * them just by copying pointers; using mutable
 * environments would make the evaluation of [[lambda]]
 * expressions very expensive.
 * 
 * \qtrim1
 * 
 * I choose a representation of environments that makes
 * it easy to share and extend them: an environment
 * contains a single binding and a pointer to the rest
 * of the bindings in the environment. [*]
 * <env.c>=
 */
struct Env {
    Name name;
    Value *loc;
    Env tl;
};
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
 * Function [[bindalloc]] always creates a new
 * environment with a new binding. The existing
 * environment is not mutated.
 * <env.c>=
 */
Env bindalloc(Name name, Value val, Env env) {
    Env newenv = malloc(sizeof(*newenv));
    assert(newenv != NULL);

    newenv->name = name;
    newenv->loc  = allocate(val);
    newenv->tl   = env;
    return newenv;
}
/*
 * Function [[bindalloclist]] binds names to values in
 * sequence.
 * <env.c>=
 */
Env bindalloclist(Namelist xs, Valuelist vs, Env env) {
    for (; xs && vs; xs = xs->tl, vs = vs->tl)
        env = bindalloc(xs->hd, vs->hd, env);
    assert(xs == NULL && vs == NULL);
    return env;
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
