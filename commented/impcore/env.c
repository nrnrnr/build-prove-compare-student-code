#include "all.h"
/*
 * Implementation of environments
 * 
 * An environment is represented by a pair of lists; one
 * holds names and the other holds the corresponding
 * values. The lists have the same length. (A search
 * tree or hash table would be enable faster search but
 * would be more complicated.) \iimplabelValenv
 * <env.c>=
 */
struct Valenv {
    Namelist  xs;
    Valuelist vs;
    // invariant: lists have the same length
};
/*
 * Given the representation, creating an environment is
 * simple. To prevent the invariant from being violated,
 * the code asserts that [[xs]] and [[vs]] have equal
 * length.
 * <env.c>=
 */
Valenv mkValenv(Namelist xs, Valuelist vs) {
    Valenv env = malloc(sizeof(*env));
    assert(env != NULL);
    assert(lengthNL(xs) == lengthVL(vs));
    env->xs = xs;
    env->vs = vs;
    return env;
}
/*
 * The list of names [[xs]] is searched by three
 * functions: [[fetchval]], [[isvalbound]], and
 * [[bindval]]. Behind the scenes, that search is
 * implemented just once, in private function
 * [[findval]]. Given a name [[x]], it searches the
 * environment. If it doesn't find [[x]], it returns
 * [[NULL]]. If it does find [[x]], it returns a pointer
 * to the value associated with [[x]]. The pointer can
 * be used to test for binding ([[isvalbound]]), to
 * fetch a bound value ([[fetchval]]), or to change an
 * existing binding ([[bindval]]).
 * <env.c>=
 */
static Value* findval(Name x, Valenv env) {
    Namelist  xs;
    Valuelist vs;

    for (xs=env->xs, vs=env->vs; xs && vs; xs=xs->tl, vs=vs->tl)
        if (x == xs->hd)
            return &vs->hd;
    return NULL;
}
/*
 * A name is bound if there is a value associated
 * with it.
 * <env.c>=
 */
bool isvalbound(Name name, Valenv env) {
    return findval(name, env) != NULL;
}
/*
 * A value is fetched through the pointer returned by
 * [[findval]], if any.
 * <env.c>=
 */
Value fetchval(Name name, Valenv env) {
    Value *vp = findval(name, env);
    assert(vp != NULL);
    return *vp;
}
/*
 * A new binding could be added to an environment by
 * inserting a new name and value at the beginning of
 * [[xs]] and [[vs]]. But I can get away with an
 * optimization. If \nomathbreakx in dom rho, instead of
 * extending rho by making rho{x |->v}, I overwrite the
 * old binding of x. This optimization is safe only
 * because no program written in Impcore can tell that
 * it is there. Proving that the optimization is safe
 * requires reasoning about the rules of the operational
 * semantics, which show that in any context where rho{x
 * |->v} appears, the old rho(x) can't affect any
 * evaluations (\impexpagesafe-overwrite). [*]
 * <env.c>=
 */
void bindval(Name name, Value val, Valenv env) {
    Value *vp = findval(name, env);
    if (vp != NULL)
        *vp = val;              // safe optimization
    else {
        env->xs = mkNL(name, env->xs);
        env->vs = mkVL(val,  env->vs);
    }
}
/*
 * Implementations of other abstractions
 * 
 * Implementation of function environments
 * 
 * This code is continued from Chapter [->], which gives
 * the implementation of value environments. Except for
 * the types, the implementation of function
 * environments is identical to code in \crefpage
 * (impcore.Valenv.imp. \iimplabelFunenv
 * <env.c>=
 */
struct Funenv {
    Namelist xs;
    Funclist funs;
    // invariant: both lists are the same length
};
/*
 * <env.c>=
 */
Funenv mkFunenv(Namelist xs, Funclist funs) {
    Funenv env = malloc(sizeof *env);
    assert(env != NULL);
    assert(lengthNL(xs) == lengthFL(funs));
    env->xs = xs;
    env->funs = funs;
    return env;
}
/*
 * <env.c>=
 */
static Func* findfun(Name name, Funenv env) {
    Namelist xs   = env->xs;
    Funclist funs = env->funs;

    for ( ; xs && funs; xs = xs->tl, funs = funs->tl)
        if (name == xs->hd)
            return &funs->hd;
    return NULL;
}
/*
 * <env.c>=
 */
bool isfunbound(Name name, Funenv env) {
    return findfun(name, env) != NULL;
}
/*
 * <env.c>=
 */
Func fetchfun(Name name, Funenv env) {
    Func *fp = findfun(name, env);
    assert(fp != NULL);
    return *fp;
}
/*
 * \qbreak
 * <env.c>=
 */
void bindfun(Name name, Func fun, Funenv env) {
    Func *fp = findfun(name, env);
    if (fp != NULL)
        *fp = fun;              // safe optimization
    else {
        env->xs   = mkNL(name, env->xs);
        env->funs = mkFL(fun,  env->funs);
    }
}
/*
 * <env.c>=
 */
void dump_fenv_names(Funenv env) {
    Namelist xs;
    if (env)
        for (xs = env->xs; xs; xs = xs->tl)
            print("%n\n", xs->hd);
}
