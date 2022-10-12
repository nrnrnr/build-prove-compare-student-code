#include "all.h"
/* env.c 54c */
struct Valenv {
    Namelist  xs;
    Valuelist vs;
    // invariant: lists have the same length
};
/* env.c 54d */
Valenv mkValenv(Namelist xs, Valuelist vs) {
    Valenv env = malloc(sizeof(*env));
    assert(env != NULL);
    assert(lengthNL(xs) == lengthVL(vs));
    env->xs = xs;
    env->vs = vs;
    return env;
}
/* env.c 55a */
static Value* findval(Name x, Valenv env) {
    Namelist  xs;
    Valuelist vs;

    for (xs=env->xs, vs=env->vs; xs && vs; xs=xs->tl, vs=vs->tl)
        if (x == xs->hd)
            return &vs->hd;
    return NULL;
}
/* env.c 55b */
bool isvalbound(Name name, Valenv env) {
    return findval(name, env) != NULL;
}
/* env.c 55c */
Value fetchval(Name name, Valenv env) {
    Value *vp = findval(name, env);
    assert(vp != NULL);
    return *vp;
}
/* env.c 55d */
void bindval(Name name, Value val, Valenv env) {
    Value *vp = findval(name, env);
    if (vp != NULL)
        *vp = val;              // safe optimization
    else {
        env->xs = mkNL(name, env->xs);
        env->vs = mkVL(val,  env->vs);
    }
}
/* env.c S303b */
struct Funenv {
    Namelist xs;
    Funclist funs;
    // invariant: both lists are the same length
};
/* env.c S303c */
Funenv mkFunenv(Namelist xs, Funclist funs) {
    Funenv env = malloc(sizeof *env);
    assert(env != NULL);
    assert(lengthNL(xs) == lengthFL(funs));
    env->xs = xs;
    env->funs = funs;
    return env;
}
/* env.c S303d */
static Func* findfun(Name name, Funenv env) {
    Namelist xs   = env->xs;
    Funclist funs = env->funs;

    for ( ; xs && funs; xs = xs->tl, funs = funs->tl)
        if (name == xs->hd)
            return &funs->hd;
    return NULL;
}
/* env.c S303e */
bool isfunbound(Name name, Funenv env) {
    return findfun(name, env) != NULL;
}
/* env.c S303f */
Func fetchfun(Name name, Funenv env) {
    Func *fp = findfun(name, env);
    assert(fp != NULL);
    return *fp;
}
/* env.c S304a */
void bindfun(Name name, Func fun, Funenv env) {
    Func *fp = findfun(name, env);
    if (fp != NULL)
        *fp = fun;              // safe optimization
    else {
        env->xs   = mkNL(name, env->xs);
        env->funs = mkFL(fun,  env->funs);
    }
}
/* env.c S304b */
void dump_fenv_names(Funenv env) {
    Namelist xs;
    if (env)
        for (xs = env->xs; xs; xs = xs->tl)
            print("%n\n", xs->hd);
}
