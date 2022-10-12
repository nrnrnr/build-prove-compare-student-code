#include "all.h"
/* eval.c 48a */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions,
                          Valenv formals);
/* eval.c 48b */
Value eval(Exp e, Valenv globals, Funenv functions, Valenv formals) {
    checkoverflow(1000000 * sizeof(char *));
                                        // see last section of Appendix A (OMIT)
    switch (e->alt) {
    case LITERAL: /* evaluate [[e->literal]] and return the result 48c */
                  return e->literal;
    case VAR:     /* evaluate [[e->var]] and return the result 49a */
                  if (isvalbound(e->var, formals))
                      return fetchval(e->var, formals);
                  else if (isvalbound(e->var, globals))
                      return fetchval(e->var, globals);
                  else
                      runerror("unbound variable %n", e->var);
    case SET:     /* evaluate [[e->set]] and return the result 49b */
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
    case IFX:     /* evaluate [[e->ifx]] and return the result 49c */
                  if (eval(e->ifx.cond, globals, functions, formals) != 0)
                      return eval(e->ifx.truex, globals, functions, formals);
                  else
                      return eval(e->ifx.falsex, globals, functions, formals);
    case WHILEX:  /* evaluate [[e->whilex]] and return the result 50a */
                  while (eval(e->whilex.cond, globals, functions, formals) != 0)
                      eval(e->whilex.exp, globals, functions, formals);
                  return 0;
    case BEGIN:   /* evaluate [[e->begin]] and return the result 50b */
                  {
                      Value lastval = 0;
                      for (Explist es = e->begin; es; es = es->tl)
                          lastval = eval(es->hd, globals, functions, formals);
                      return lastval;
                  }
    case APPLY:   /* evaluate [[e->apply]] and return the result 50c */
                  {
                      Func f;

/* make [[f]] the function denoted by [[e->apply.name]], or call [[runerror]] 51a */
                      if (!isfunbound(e->apply.name, functions))
                          runerror("call to undefined function %n in %e", e->
                                                                 apply.name, e);
                      f = fetchfun(e->apply.name, functions);
                      switch (f.alt) {
                      case USERDEF:
                             /* apply [[f.userdef]] and return the result 51c */
                                      {
                                          Namelist  xs = f.userdef.formals;
                                          Valuelist vs = evallist(e->
                                    apply.actuals, globals, functions, formals);
                                          checkargc(e, lengthNL(xs), lengthVL(vs
                                                                             ));
                                          return eval(f.userdef.body, globals,
                                                   functions, mkValenv(xs, vs));
                                      }
                      case PRIMITIVE:
                           /* apply [[f.primitive]] and return the result 52a */
                                      {
                                          Valuelist vs = evallist(e->
                                    apply.actuals, globals, functions, formals);
                                          if (f.primitive == strtoname("print"))

              /* apply \impcore\ primitive [[print]] to [[vs]] and return 52b */
                                              {
                                                  checkargc(e, 1, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  print("%v", v);
                                                  return v;
                                              }
                                          else if (f.primitive == strtoname(
                                                                     "println"))

          /* apply \impcore\ primitive [[println]] to [[vs]] and return S299c */
                                              {
                                                  checkargc(e, 1, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  print("%v\n", v);
                                                  return v;
                                              }
                                          else if (f.primitive == strtoname(
                                                                      "printu"))

           /* apply \impcore\ primitive [[printu]] to [[vs]] and return S299d */
                                              {
                                                  checkargc(e, 1, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  print_utf8(v);
                                                  return v;
                                              }
                                          else

                       /* apply arithmetic primitive to [[vs]] and return 52c */
                                              {
                                                  checkargc(e, 2, lengthVL(vs));
                                                  Value v = nthVL(vs, 0);
                                                  Value w = nthVL(vs, 1);
                                                  const char *s = nametostr(
                                                                   f.primitive);

/* if operation [[s]] would overflow on [[v]] and [[w]], call [[runerror]] 53a */
                                                  checkarith(s[0], v, w, 32);

              /* return a function of [[v]] and [[w]] determined by [[s]] 52d */
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
/* eval.c 51b */
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
/* eval.c 53b */
void evaldef(Def d, Valenv globals, Funenv functions, Echo echo) {
    switch (d->alt) {
    case VAL:
        /* evaluate [[d->val]], mutating [[globals]] 53c */
        {
            Value v = eval(d->val.exp, globals, functions, mkValenv(NULL, NULL))
                                                                               ;
            bindval(d->val.name, v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case EXP:
        /* evaluate [[d->exp]] and possibly print the result 54a */
        {
            Value v = eval(d->exp, globals, functions, mkValenv(NULL, NULL));
            bindval(strtoname("it"), v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case DEFINE:
        /* evaluate [[d->define]], mutating [[functions]] 54b */
        bindfun(d->define.name, mkUserdef(d->define.userfun), functions);
        if (echo == ECHOING)
            print("%n\n", d->define.name);
        return;
    }
    assert(0);
}
/* eval.c S296a */
void readevalprint(XDefstream xdefs, Valenv globals, Funenv functions, Echo echo
                                                                             ) {
    UnitTestlist pending_unit_tests = NULL;  // run when xdefs is exhausted

    for (XDef d = getxdef(xdefs); d; d = getxdef(xdefs))
        switch (d->alt) {
        case TEST:
            pending_unit_tests = mkUL(d->test, pending_unit_tests);
            break;
        case USE:

/* evaluate [[d->use]], possibly mutating [[globals]] and [[functions]] S296c */
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
