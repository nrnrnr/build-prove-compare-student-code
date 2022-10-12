#include "all.h"
/* eval.c ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) */
/* eval.c declarations S310b */
static Valuelist evallist(Explist es, Env env);
/* eval.c 155b */
Value eval(Exp e, Env env) {
    checkoverflow(1000000 * sizeof(char *)); // OMIT
    switch (e->alt) {
    case LITERAL: /* evaluate [[e->literal]] and return the result 156a */
                  return e->literal;
    case VAR:     /* evaluate [[e->var]] and return the result 156b */
                  if (find(e->var, env) == NULL)
                      runerror("name %n not found", e->var);
                  return *find(e->var, env);
    case SET:     /* evaluate [[e->set]] and return the result 156c */
                  if (find(e->set.name, env) == NULL)
                      runerror("set unbound variable %n in %e", e->set.name, e);
                  return *find(e->set.name, env) = eval(e->set.exp, env);
    case IFX:     /* evaluate [[e->ifx]] and return the result 159b */
                  if (istrue(eval(e->ifx.cond, env)))
                      return eval(e->ifx.truex, env);
                  else
                      return eval(e->ifx.falsex, env);
    case WHILEX:  /* evaluate [[e->whilex]] and return the result 159c */
                  while (istrue(eval(e->whilex.cond, env)))
                      eval(e->whilex.body, env);
                  return falsev;
    case BEGIN:   /* evaluate [[e->begin]] and return the result 159d */
                  {
                      Value lastval = falsev;
                      for (Explist es = e->begin; es; es = es->tl)
                          lastval = eval(es->hd, env);
                      return lastval;
                  }
    case APPLY:   /* evaluate [[e->apply]] and return the result 156e */
                  {
                      Value     f  = eval    (e->apply.fn,      env);
                      Valuelist vs = evallist(e->apply.actuals, env);

                      switch (f.alt) {
                      case PRIMITIVE:

                /* apply [[f.primitive]] to [[vs]] and return the result 157a */
                          return f.primitive.function(e, f.primitive.tag, vs);
                      case CLOSURE:

                  /* apply [[f.closure]] to [[vs]] and return the result 157b */
                          {
                              Namelist xs = f.closure.lambda.formals;
                              checkargc(e, lengthNL(xs), lengthVL(vs));
                              return eval(f.closure.lambda.body,
                                          bindalloclist(xs, vs, f.closure.env));
                          }
                      default:
                          runerror("%e evaluates to non-function %v in %e",
                                   e->apply.fn, f, e);
                      }
                  }
    case LETX:    /* evaluate [[e->letx]] and return the result 157d */
                  switch (e->letx.let) {
                  case LET:
            /* extend [[env]] by simultaneously binding [[es]] to [[xs]] 158a */
                                env = bindalloclist(e->letx.xs, evallist(e->
                                                            letx.es, env), env);
                                break;
                  case LETSTAR:
              /* extend [[env]] by sequentially binding [[es]] to [[xs]] 158b */
                                {
                                    Namelist xs;
                                    Explist es;

                                    for (xs = e->letx.xs, es = e->letx.es;
                                         xs && es;
                                         xs = xs->tl, es = es->tl)
                                        env = bindalloc(xs->hd, eval(es->hd, env
                                                                        ), env);
                                    assert(xs == NULL && es == NULL);
                                }
                                break;
                  case LETREC:
               /* extend [[env]] by recursively binding [[es]] to [[xs]] 159a */
                                {
                                    Namelist xs;

                                    for (xs = e->letx.xs; xs; xs = xs->tl)    
                                        env = bindalloc(xs->hd, unspecified(),
                                                                           env);
                                    Valuelist vs = evallist(e->letx.es, env);
                                    for (xs = e->letx.xs;
                                         xs && vs;
                                         xs = xs->tl, vs = vs->tl)
                                        *find(xs->hd, env) = vs->hd;
                                    assert(xs == NULL && vs == NULL);
                                }
                                break;
                  default:      assert(0);
                  }
                  return eval(e->letx.body, env);
    case LAMBDAX: /* evaluate [[e->lambdax]] and return the result 156d */
                  return mkClosure(e->lambdax, env);
    }
    assert(0);
}
/* eval.c 157c */
static Valuelist evallist(Explist es, Env env) {
    if (es == NULL) {
        return NULL;
    } else {
        Value v = eval(es->hd, env);   // enforce uScheme's order of evaluation
        return mkVL(v, evallist(es->tl, env));
    }
}
/* eval.c S336b */
Exp testexp(Exp e) {
    return e;
}
