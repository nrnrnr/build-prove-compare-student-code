#include "all.h"
/*
 * Implementation of \chaptocsplitthe evaluator
 * 
 * [*] [*]
 * 
 * The evaluator implements Impcore's semantics.
 * It comprises functions [[eval]] and [[evaldef]],
 * which evaluate expressions and true definitions.
 * 
 * Evaluating expressions
 * 
 * Function [[eval]] implements the ==> relation from
 * the operational semantics. Calling eval(e, xi, phi, 
 * rho) finds a v, xi', and rho' such that \evale ==>\
 * eval[']v, assigns rho := rho' and xi := xi', and
 * returns v. Because Greek letters aren't customary in
 * C code, I use these English names:
 * 
 *  xi  [[globals]]
 *  phi [[functions]]
 *  rho [[formals]]
 * 
 * Function [[eval]] is mutually recursive with a
 * private helper function, [[evallist]]: [*]
 * <eval.c>=
 */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions,
                          Valenv formals);
/*
 * Evaluation of an expression [[e]] begins by
 * discovering its syntactic form, using a [[switch]] on
 * the tag [[e->alt]]. \iiflabeleval\iimplabeleval [*]
 * <eval.c>=
 */
Value eval(Exp e, Valenv globals, Funenv functions, Valenv formals) {
    checkoverflow(1000000 * sizeof(char *));
                                        // see last section of Appendix A (OMIT)
    switch (e->alt) {
    case LITERAL: /*
                   * The \xliteral form appears in the conclusion of just
                   * one rule. The implementation returns the literal
                   * value.
                   * <evaluate [[e->literal]] and return the result>=
                   */
                  return e->literal;
    case VAR:     /*
                   * The \xvar form appears in the conclusions of two
                   * rules. A rule can be used only if its premises hold,
                   * so the interpreter checks \nomathbreakx in dom rho,
                   * which is implemented by calling \monoboxisvalbound
                   * (e->var, formals). If x \notindom rho and x \notindom
                   * xi, the operational semantics gets stuck—so the
                   * interpreter issues an error message. Less formally,
                   * the interpreter looks for x in the formal-parameter
                   * environment first, then the global environment.
                   * <evaluate [[e->var]] and return the result>=
                   */
                  if (isvalbound(e->var, formals))
                      return fetchval(e->var, formals);
                  else if (isvalbound(e->var, globals))
                      return fetchval(e->var, globals);
                  else
                      runerror("unbound variable %n", e->var);
    case SET:     /*
                   * The call to [[runerror]] illustrates the convenience
                   * of the extensible printer; it uses [[ needing to
                   * convert the [[Name]] to a string.
                   * 
                   * The \xset form is very similar. Again there are two
                   * rules, and again they are distinguished by testing \
                   * mathboxx in dom rho. Because both rules require the
                   * premise \evale ==>\eval[']v, the code evaluates e
                   * ([[e->set.exp]]) first, then puts its value in [[v]].
                   * <evaluate [[e->set]] and return the result>=
                   */
                  {
                      Value v = eval(e->set.exp, globals, functions, formals);

                      if (isvalbound(e->set.name, formals))
                          bindval(e->set.name, v, formals);
                      else if (isvalbound(e->set.name, globals))
                          bindval(e->set.name, v, globals);
                      else
                          runerror("tried to set unbound variable %n in %e", e->
                                                                   set.name, e);
                      return v;
                  }
    case IFX:     /*
                   * The \xif form appears in the conclusions of two
                   * rules. Both rules have the same first premise: \eval
                   * e_1 ==>\eval[']v_1. To get v_1, xi', and rho', the
                   * code calls \monoboxeval(e->ifx.cond, globals,
                   * functions, formals) recursively. This call may mutate
                   * the [[globals]] and [[formals]] environments, but
                   * regardless of whether v_1 = 0, the mutation is safe,
                   * because the third premises of both rules use the new
                   * environments xi' and rho'. Comparing v_1 with zero
                   * determines which rule should be used: the
                   * implementation ends with a recursive call to evaluate
                   * either e_2 ([[e->ifx.truex]]) or e_3 ([[e->
                   * ifx.falsex]]). [*]
                   * <evaluate [[e->ifx]] and return the result>=
                   */
                  if (eval(e->ifx.cond, globals, functions, formals) != 0)
                      return eval(e->ifx.truex, globals, functions, formals);
                  else
                      return eval(e->ifx.falsex, globals, functions, formals);
    case WHILEX:  /*
                   * The \xwhile form appears in the conclusions of two
                   * rules. In the first rule, the premise \eval['']\while
                   * (e_1, e_2) ==>\eval[''']v_3 could be implemented as a
                   * recursive call to \monoeval(e, ...). But [[e]] is
                   * always a while loop, so I have optimized the code by
                   * turning the recursion into iteration. This
                   * optimization prevents a long \xwhile loop from
                   * overflowing the C stack.
                   * <evaluate [[e->whilex]] and return the result>=
                   */
                  while (eval(e->whilex.cond, globals, functions, formals) != 0)
                      eval(e->whilex.exp, globals, functions, formals);
                  return 0;
    case BEGIN:   /*
                   * The \xbegin form appears in the conclusions of two
                   * rules. A nonempty \xbegin is implemented by iterating
                   * over its subexpressions, leaving the last value in
                   * variable [[lastval]]. If [[lastval]] is initialized
                   * to zero, the same code also implements the empty \
                   * xbegin. [*]
                   * <evaluate [[e->begin]] and return the result>=
                   */
                  {
                      Value lastval = 0;
                      for (Explist es = e->begin; es; es = es->tl)
                          lastval = eval(es->hd, globals, functions, formals);
                      return lastval;
                  }
    case APPLY:   /*
                   * Function application appears in the conclusion of the
                   * \rulenameApplyUser rule, and also in every rule that
                   * describes a primitive function. The rule to be
                   * implemented depends on the form of the function,
                   * which may be [[USERDEF]] or [[PRIMITIVE]]. Given a
                   * function named f ([[e->apply.name]]), the interpreter
                   * discovers its form by looking at phi(f), which it
                   * stores in local variable [[f]].
                   * <evaluate [[e->apply]] and return the result>=
                   */
                  {
                      Func f;
                      /*
                       * If f is not defined as a function, the result is a
                       * run-time error.
                       * <make [[f]] the function denoted by [[e->apply.name]],
                                                          or call [[runerror]]>=
                       */
                      if (!isfunbound(e->apply.name, functions))
                          runerror("call to undefined function %n in %e", e->
                                                                 apply.name, e);
                      f = fetchfun(e->apply.name, functions);
                      switch (f.alt) {
                      case USERDEF:   /*
                                       * <apply [[f.userdef]] and return the
                                                                        result>=
                                       */
                                      {
                                          Namelist  xs = f.userdef.formals;
                                          Valuelist vs = evallist(e->
                                    apply.actuals, globals, functions, formals);
                                          checkargc(e, lengthNL(xs), lengthVL(vs
                                                                             ));
                                          return eval(f.userdef.body, globals,
                                                   functions, mkValenv(xs, vs));
                                      }
                      case PRIMITIVE: /*
                                       * Each primitive function is applied by
                                                                    code that is
                                       * specialized to that primitive. A
                                                                    primitive is
                                       * identified by comparing its name to a
                                                                  name made from
                                       * a known string. More general techniques
                                                                             for
                                       * implementing primitives, which are
                                                                 appropriate for
                                       * larger languages, are shown in the \
                                                               crefscheme.chap (
                                       * \cpagerefscheme.defn-of-Primitive).
                                       * <apply [[f.primitive]] and return the
                                                                        result>=
                                       */
                                      {
                                          Valuelist vs = evallist(e->
                                    apply.actuals, globals, functions, formals);
                                          if (f.primitive == strtoname("print"))
                                              /*
                                               * Only [[print]] is implemented
                                                           here; [[println]] and
                                               * [[printu]] are in \crefapp:
                                                                        impcore.
                                               * <apply \impcore\ primitive
                                                [[print]] to [[vs]] and return>=
                                               */
                                              {
                                                  checkargc(e, 1, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  print("%v", v);
                                                  return v;
                                              }
                                          else if (f.primitive == strtoname(
                                                                     "println"))
                                              /*
                                               * Implementations of the printing
                                                                      primitives
                                               * 
                                               * The implementations of
                                                   Impcore's primitive functions
                                               * [[println]] and [[printu]] are
                                                                   so similar to
                                               * [[print]] that they are not
                                                                  shown in \cref
                                               * impcore.chap. Instead, they are
                                                                           here:
                                               * <apply \impcore\ primitive
                                              [[println]] to [[vs]] and return>=
                                               */
                                              {
                                                  checkargc(e, 1, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  print("%v\n", v);
                                                  return v;
                                              }
                                          else if (f.primitive == strtoname(
                                                                      "printu"))
                                              /*
                                               * <apply \impcore\ primitive
                                               [[printu]] to [[vs]] and return>=
                                               */
                                              {
                                                  checkargc(e, 1, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  print_utf8(v);
                                                  return v;
                                              }
                                          else
                                              /*
                                               * Each arithmetic primitive
                                                             expects exactly two
                                               * arguments, which the code puts
                                                            in C variables [[v]]
                                               *  and [[w]]. The characters of
                                                         the primitive's name go
                                               * in [[s]].
                                               * <apply arithmetic primitive to
                                                             [[vs]] and return>=
                                               */
                                              {
                                                  checkargc(e, 2, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  Value w = nthVL(vs, 1);
                                                  const char *s = nametostr(
                                                                   f.primitive);
                                                  /*
                                                   * But the interpreter cannot
                                                       ignore the possibility of
                                                   * overflow. The rules of
                                                  Impcore are different from the
                                                   * rules of C, and if the
                                                         result of an arithmetic
                                                   * operation does not fit in
                                                        the range -2^31 to 2^31,
                                                   * the operation causes a
                                                         checked run-time error.
                                                   * The error is detected and
                                                            signaled by function
                                                   * [[checkarith]], which is
                                                                defined in \cref
                                                   * app:cinterps.
                                                   * <if operation [[s]] would
                                overflow on [[v]] and [[w]], call [[runerror]]>=
                                                   */
                                                  checkarith(s[0], v, w, 32);
                                                  /*
                                                   * Ignoring the possibility of
                                                          overflow, each Impcore
                                                   * primitive can be
                                                implemented by the corresponding
                                                   * operation in C. The
                                                  primitive's name is synonymous
                                                   * with its first character,
                                                                       [[s[0]]].
                                                   * <return a function of [[v]]
                                                 and [[w]] determined by [[s]]>=
                                                   */
                                                  assert(strlen(s) == 1);
                                                  switch (s[0]) {
                                                  case '<':    return v < w;
                                                  case '>':    return v > w;
                                                  case '=':    return v == w;
                                                  case '+':    return v + w;
                                                  case '-':    return v - w;
                                                  case '*':    return v * w;
                                                  case '/':    if (w == 0)
                                                                   runerror(
                                                   "division by zero in %e", e);
                                                               return v / w;
                                                  default:     assert(0);
                                                  }
                                              }
                                      }
                      default:        assert(0);
                      }
                  }
    }
    assert(0);
}
/*
 * When f is a user-defined function, applying it has
 * something in common with [[begin]]: arguments e_1,
 * ..., e_n have to be evaluated. But where [[begin]]
 * keeps only result v_n (in variable [[v]] in chunk 
 * [->]), function application keeps all the result
 * values, which it binds into a new environment. Values
 * \ldotsnv, are returned by auxiliary function
 * [[evallist]], which is given e_1, ..., e_n along with
 * xi_0, phi, and rho_0. It evaluates e_1, ..., e_n in
 * order, and it mutates the environments so that when
 * it is finished, [[globals]] is xi_n and [[formals]]
 * is rho_n. Finally, [[evallist]] returns the list v_1,
 * ..., v_n. \iimplabelevallist
 * <eval.c>=
 */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions, 
                          Valenv formals) 
{   
    if (es == NULL) {
        return NULL;
    } else {
        Value v = eval(es->hd, globals, functions, formals);
        return mkVL(v, evallist(es->tl, globals, functions, formals));
    }
}
/*
 * The rules of Impcore require that [[es->hd]] be
 * evaluated before [[es->tl]]. To ensure the correct
 * order of evaluation, [[eval(es->hd,]] ...[[)]] and
 * [[evallist(es->tl,]] ...[[)]] are called in separate
 * C statements. Writing both calls as parameters to
 * [[mkVL]] would not guarantee the correct order. [*]
 * 
 * The \rulenameApplyUser rule requires that the
 * application have exactly as many arguments as f is
 * expecting: the list of formal parameters and the list
 * of argument expressions both have length n. But
 * in general, they might be different: the formals \
 * ldotsnx are stored in [[xs]] and the actual values 
 * v_1, ..., v_m are stored in [[vs]]. When both are the
 * same length, as confirmed by [[checkargc]], they are
 * used to create the fresh environment \monoboxmkValenv
 * (xs, vs), which is \nomathbreak{x_1 |->v_1, ..., x_n
 * |->v_n }. This environment is used to evaluate f's
 * body.
 */

/*
 * Evaluating true definitions
 * 
 * [*] As noted on \cpageref
 * impcore.extended-definitions, definitions are divided
 * into two forms: The true definitions can differ in
 * each language; Impcore's true definitions include
 * [[val]] and [[define]]. The extended definitions are
 * shared across languages; they include [[use]] and
 * [[check-expect]]. True definitions have an
 * operational semantics; extended definitions don't.
 * And true definitions are evaluated by code that is
 * explained here; extended definitions are evaluated by
 * code in the Supplement.
 * 
 * The --> relation on the true definitions is
 * implemented by function [[evaldef]]. Calling evaldef
 * (d, xi, phi, echo) finds a xi' and phi' such that \
 * imptopld -->\pxi'phi', and [[evaldef]] mutates the
 * C representation of the environments so the
 * global-variable environment becomes xi' and the
 * function environment becomes phi'. If echo is
 * [[ECHOING]], [[evaldef]] also prints the
 * interpreter's response to the user's input. Printing
 * the response is [[evaldef]]'s job because only
 * [[evaldef]] can tell whether to print a value (for
 * [[EXP]] and [[VAL]]) or a name (for [[DEFINE]]).
 * 
 * Just like [[eval]], [[evaldef]] looks at the
 * syntactic form of [[d]] and implements whatever rules
 * have that form in their conclusions. [*]\iiflabel
 * evaldef\iimplabelevaldef
 * <eval.c>=
 */
void evaldef(Def d, Valenv globals, Funenv functions, Echo echo) {
    switch (d->alt) {
    case VAL:
        /*
         * A \xval form updates xi. The premise shows that
         * value v and environment xi' are obtained by calling
         * [[eval]]. This call uses an empty environment as rho.
         * In the conclusion, the new environment xi' is
         * retained, and the value of the expression, v, is
         * bound to x in it. Value v may also be printed.
         * <evaluate [[d->val]], mutating [[globals]]>=
         */
        {
            Value v = eval(d->val.exp, globals, functions, mkValenv(NULL, NULL))
                                                                               ;
            bindval(d->val.name, v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case EXP:
        /*
         * An \astexp form also updates xi, just as if it were a
         * definition of [[it]].
         * <evaluate [[d->exp]] and possibly print the result>=
         */
        {
            Value v = eval(d->exp, globals, functions, mkValenv(NULL, NULL));
            bindval(strtoname("it"), v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case DEFINE:
        /*
         * A \xdefine form updates phi. The implementation may
         * print the name of the function being defined.
         * <evaluate [[d->define]], mutating [[functions]]>=
         */
        bindfun(d->define.name, mkUserdef(d->define.userfun), functions);
        if (echo == ECHOING)
            print("%n\n", d->define.name);
        /*
         * The evaluator does not check to see that the \ldotsnx
         * are all distinct---the x_i's are checked when the
         * definition is parsed, by function
         * [[check_def_duplicates]] in \chunkref
         * cparse.chunk.check-def-duplicates.
         */

        return;
    }
    assert(0);
}
/*
 * Responsibility for evaluating definitions is divided
 * between two functions. Function [[readevalprint]]
 * takes as input a stream of definitions. The extended
 * definitions are handled directly in
 * [[readevalprint]]:
 * 
 *   • Each unit test is remembered and later run.
 *   • A file mentioned in [[use]] is converted to a
 *  stream of extended definitions, then passed
 *  recursively to [[readevalprint]].
 * 
 * A true definition is passed on to [[evaldef]]. [*]\
 * iiflabelreadevalprint \iimplabelreadevalprint
 * <eval.c>=
 */
void readevalprint(XDefstream xdefs, Valenv globals, Funenv functions, Echo echo
                                                                             ) {
    UnitTestlist pending_unit_tests = NULL;  // run when xdefs is exhausted

    for (XDef d = getxdef(xdefs); d; d = getxdef(xdefs))
        switch (d->alt) {
        case TEST:
            pending_unit_tests = mkUL(d->test, pending_unit_tests);
            break;
        case USE:
            /*
             * On seeing [[use]], [[readevalprint]] opens the file
             * named by [[use]], builds a stream of definitions, and
             * recursively calls itself on that stream. The eventual
             * effect will be to call [[evaldef]] on every
             * definition in the file. When reading definitions via
             * [[use]], the interpreter neither prompts nor echoes.
             * <evaluate [[d->use]], possibly mutating [[globals]] and
                                                                 [[functions]]>=
             */
            {
                const char *filename = nametostr(d->use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    runerror("cannot open file \"%s\"", filename);
                readevalprint(filexdefs(filename, fin, NOT_PROMPTING),
                              globals, functions, echo);
                fclose(fin);
            }
            break;
        case DEF:
            evaldef(d->def, globals, functions, echo);
            break;
        default:
            assert(0);
        }
    reset_overflow_check();     /* OMIT */

    process_tests(pending_unit_tests, globals, functions);
}
/*
 * Function [[process_tests]], which is defined in \
 * crefpage(impcorea.testing, runs the listed
 * [[pending_unit_tests]] in the order in which they
 * appear in the source code.
 */

