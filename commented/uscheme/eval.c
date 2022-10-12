#include "all.h"
/*
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 *  [[ [[ [[ [[ [[ proxy for lambda)
 *  [[ [[ [[ [[ [[ [[ [[ [[ [[
 * 
 * Specifications used in [[print]] and [[fprint]] [*]
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 * Printing
 * 
 * Just like the Impcore interpreter, the micro-Scheme
 * interpreter uses functions [[print]] and [[fprint]],
 * but the micro-Scheme interpreter knows how to print
 * more kinds of things. The alternatives are shown in \
 * crefscheme.tab.print-specs. Most of these
 * specifications are used only to debug the
 * interpreter.
 * <eval.c ((elided))>=
 */
/*
 * Evaluation
 * 
 * The evaluator for a list of expressions has to be
 * declared.
 * <eval.c declarations>=
 */
static Valuelist evallist(Explist es, Env env);
/*
 * Implementation \chaptocsplitof the evaluator
 * 
 * \qtrim0.5
 * 
 * [*] As in Impcore, the evaluator starts with
 * [[switch]], which chooses how to evaluate [[e]] based
 * on its syntactic form:\scmflabeleval
 * <eval.c>=
 */
Value eval(Exp e, Env env) {
    checkoverflow(1000000 * sizeof(char *)); // OMIT
    switch (e->alt) {
    case LITERAL: /*
                   * Literals
                   * 
                   * As in Impcore, a literal evaluates to itself.
                   * <evaluate [[e->literal]] and return the result>=
                   */
                  return e->literal;
    case VAR:     /*
                   * Variables and assignment
                   * 
                   * Variable lookup and assignment are simpler than in
                   * Impcore, because micro-Scheme has only one rule for
                   * each form. In the code, rho(x) is implemented by find
                   * (x, rho), and sigma(\aloc) is implemented by [[*]]\
                   * aloc. [*]
                   * <evaluate [[e->var]] and return the result>=
                   */
                  if (find(e->var, env) == NULL)
                      runerror("name %n not found", e->var);
                  return *find(e->var, env);
    case SET:     /*
                   * In an assignment, the store is set to sigma'{\aloc|->
                   * v} by assigning to [[*]]\aloc. [*] [*]
                   * <evaluate [[e->set]] and return the result>=
                   */
                  if (find(e->set.name, env) == NULL)
                      runerror("set unbound variable %n in %e", e->set.name, e);
                  return *find(e->set.name, env) = eval(e->set.exp, env);
    case IFX:     /*
                   * Conditional, iteration, and sequence
                   * 
                   * The control-flow operations are implemented much as
                   * they are in Impcore. The semantic rules are not worth
                   * repeating.
                   * <evaluate [[e->ifx]] and return the result>=
                   */
                  if (istrue(eval(e->ifx.cond, env)))
                      return eval(e->ifx.truex, env);
                  else
                      return eval(e->ifx.falsex, env);
    case WHILEX:  /*
                   * <evaluate [[e->whilex]] and return the result>=
                   */
                  while (istrue(eval(e->whilex.cond, env)))
                      eval(e->whilex.body, env);
                  return falsev;
    case BEGIN:   /*
                   * <evaluate [[e->begin]] and return the result>=
                   */
                  {
                      Value lastval = falsev;
                      for (Explist es = e->begin; es; es = es->tl)
                          lastval = eval(es->hd, env);
                      return lastval;
                  }
    case APPLY:   /*
                   * When a function is applied, its actual parameters are
                   * evaluated and stored in [[vs]]. The next step depends
                   * on whether the function is a primitive or a closure.
                   * <evaluate [[e->apply]] and return the result>=
                   */
                  {
                      Value     f  = eval    (e->apply.fn,      env);
                      Valuelist vs = evallist(e->apply.actuals, env);

                      switch (f.alt) {
                      case PRIMITIVE:
                          /*
                           * Because a primitive is represented by a pair
                           * containing a function pointer and a tag, its
                           * application is simpler than in Impcore. Function
                           * [[f.primitive.function]] gets the tag, the
                                                                       arguments
                           * [[vs]], and the abstract syntax [[e]]. (The syntax
                                                                              is
                           * used in error messages.)
                           * <apply [[f.primitive]] to [[vs]] and return the
                                                                        result>=
                           */
                          return f.primitive.function(e, f.primitive.tag, vs);
                      case CLOSURE:
                          /*
                           * A closure is applied by extending its stored
                           * environment (rho_c in the operational semantics)
                                                                            with
                           * the bindings for the formal variables, then
                           * evaluating the body in that environment. [*]
                           * <apply [[f.closure]] to [[vs]] and return the
                                                                        result>=
                           */
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
    case LETX:    /*
                   * \qtrim0.3
                   * 
                   * Let, let*, and letrec
                   * 
                   * Each expression in the [[let]] family uses its
                   * internal name-expression pairs to create a new
                   * environment, then evaluates the body in that
                   * environment. Each form creates the new environment in
                   * a different way.
                   * <evaluate [[e->letx]] and return the result>=
                   */
                  switch (e->letx.let) {
                  case LET:     /*
                                 * A \xlet expression evaluates its right-hand
                                                                          sides,
                                 * then binds them all at once. All the work is
                                                                         done by
                                 * functions [[evallist]] and [[bindalloclist]].
                                 * <extend [[env]] by simultaneously binding
                                                              [[es]] to [[xs]]>=
                                 */
                                env = bindalloclist(e->letx.xs, evallist(e->
                                                            letx.es, env), env);
                                break;
                  case LETSTAR: /*
                                 * A \xletstar expression binds a new name as
                                                                            each
                                 * expression is evaluated.
                                 * 
                                 * <extend [[env]] by sequentially binding
                                                              [[es]] to [[xs]]>=
                                 */
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
                  case LETREC:  /*
                                 * Finally, before evaluating any expressions, \
                                                                         xletrec
                                 * binds each name to a fresh location. The
                                                                      locations'
                                 * initial contents are unspecified, and they
                                                                          remain
                                 * unspecified until all the values are
                                                                     computed. \
                                 * qbreak The right-hand sides are confirmed to
                                                                            be \
                                 * xlambdas at parse time, by the same function
                                                                            that
                                 * confirms the x_i's are distinct, so they
                                                                      needn't be
                                 * checked here.
                                 * <extend [[env]] by recursively binding [[es]]
                                                                     to [[xs]]>=
                                 */
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
    case LAMBDAX: /*
                   * Closures and function application
                   * 
                   * Wrapping a closure is simple: \punctrule.
                   * <evaluate [[e->lambdax]] and return the result>=
                   */
                  return mkClosure(e->lambdax, env);
    }
    assert(0);
}
/*
 * As in Impcore's interpreter, [[evallist]] evaluates a
 * list of arguments in turn, returning a list of
 * values.
 * <eval.c>=
 */
static Valuelist evallist(Explist es, Env env) {
    if (es == NULL) {
        return NULL;
    } else {
        Value v = eval(es->hd, env);   // enforce uScheme's order of evaluation
        return mkVL(v, evallist(es->tl, env));
    }
}
/*
 * <eval.c>=
 */
Exp testexp(Exp e) {
    return e;
}
