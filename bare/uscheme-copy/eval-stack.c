#include "all.h"
/* eval-stack.c 227a */
Value eval(Exp e, Env env) {
    Value v;
    Frame *fr;
    /* definition of static [[Exp hole]], which always has a hole S357c */
    static struct Exp holeExp = { HOLE, { { NIL, { 0 } } } };
    static Exp hole = &holeExp;
    static Stack evalstack;

    /* ensure that [[evalstack]] is initialized and empty S344b */
    if (evalstack == NULL)
        evalstack = emptystack();
    else
        clearstack(evalstack);
    /* ensure that [[evalstack]] is initialized and empty S374c */
    assert(topframe(roots.stack) == NULL);
    roots.stack = evalstack;
    /* use the options in [[env]] to initialize the instrumentation S346e */
    high_stack_mark = 0;
    show_high_stack_mark = 
        istrue(getoption(strtoname("&show-high-stack-mark"), env, falsev));
    /* use the options in [[env]] to initialize the instrumentation S347c */
    {   Value *p = find(strtoname("&trace-stack"), env);
        if (p && p->alt == NUM)
            stack_trace_init(&p->num);
        else
            stack_trace_init(NULL);
    }
    /* use the options in [[env]] to initialize the instrumentation S356d */
    optimize_tail_calls = 
        istrue(getoption(strtoname("&optimize-tail-calls"), env, truev));

    exp: 
        stack_trace_current_expression(e, env, evalstack);
        /* take a step from state $\seval e$ 228a */
        switch (e->alt) {
        case LITERAL: /* start [[e->literal]] and step to the next state 229c */
                      v = e->literal;
                      goto value;
        case VAR:     /* start [[e->var]] and step to the next state 229d */
                      if (find(e->var, env) == NULL)
                          runerror("variable %n not found", e->var);
                      v = *find(e->var, env);
                      goto value;
        case SET:     /* start [[e->set]] and step to the next state 230a */
                      if (find(e->set.name, env) == NULL)
                          runerror("set unbound variable %n", e->set.name);
                      pushframe(mkSetStruct(e->set.name, hole), evalstack);
                      e = e->set.exp;
                      goto exp;
        case IFX:     /* start [[e->ifx]] and step to the next state 230c */
                      pushframe(mkIfxStruct(hole, e->ifx.truex, e->ifx.falsex),
                                                                     evalstack);
                      e = e->ifx.cond;
                      goto exp;
        case LETX:
            if (/* [[e->letx]] contains no bindings 234b */
                e->letx.xs == NULL && e->letx.es == NULL) {

         /* continue by evaluating the body of the [[let]] or [[letrec]] 234c */
                 e = e->letx.body;
                 goto exp;
            } else {
                switch (e->letx.let) {
                   case LET:
                  /* start \xlet\ [[e->letx]] and step to the next state 234d */
                                 pushframe(mkLetxStruct(e->letx.let, e->letx.xs,
                                                                                
                                                        copyEL(e->letx.es), e->
                                                                     letx.body),
                                           evalstack);
                                 fr = topframe(evalstack);
                                 e  = head_replaced_with_hole(fr->form.letx.es);
                                 assert(e);
                                 goto exp;
                   case LETSTAR: goto want_lowered;
                   case LETREC:
               /* start \xletrec\ [[e->letx]] and step to the next state 235a */
                                 pushenv_opt(env, NONCALL, evalstack);

/* bind every name in [[e->letx.xs]] to an unspecified value in [[env]] S357d */
                                 {   Namelist xs;
                                     for (xs = e->letx.xs; xs; xs = xs->tl)    
                                         env = bindalloc(xs->hd, unspecified(),
                                                                           env);
                                 }
                                 pushframe(mkLetxStruct(e->letx.let, e->letx.xs,
                                                                                
                                                        copyEL(e->letx.es), e->
                                                                     letx.body),
                                           evalstack);
                                 fr = topframe(evalstack);
                                 e  = head_replaced_with_hole(fr->form.letx.es);
                                 assert(e);
                                 goto exp;
                   default: assert(0);
                }
            }
        case LAMBDAX:
                      /* start [[e->lambdax]] and step to the next state 229e */
                         v = mkClosure(e->lambdax, env);
                         goto value;
        case APPLY:
                        /* start [[e->apply]] and step to the next state 232c */
                         pushframe(mkApplyStruct(mkHole(), copyEL(e->
                                                    apply.actuals)), evalstack);
                         topframe(evalstack)->syntax = e;
                         e = e->apply.fn;
                         goto exp;
        case RETURNX:
                      /* start [[e->returnx]] and step to the next state 237c */
                         pushframe(mkReturnxStruct(hole), evalstack);
                         e = e->returnx;
                         goto exp;
        case LONG_LABEL:
                   /* start [[e->long_label]] and step to the next state 236d */
                         pushenv_opt(env, NONCALL, evalstack);
                         pushframe(mkLongLabelStruct(e->long_label.label, hole),
                                                                     evalstack);
                         e = e->long_label.body;
                         goto exp;
        case LONG_GOTO:
                    /* start [[e->long_goto]] and step to the next state 237a */
                         pushframe(mkLongGotoStruct(e->long_goto.label, hole),
                                                                     evalstack);
                         e = e->long_goto.exp;
                         goto exp;
        case LOWERED:
       /* replace [[e]] with its lowered form and continue in this state 229a */
                         e = e->lowered.after;
                         goto exp;
        case LOOPBACK:
                  /* look inside [[LOOPBACK]] and continue in this state 229b */
                         e = e->loopback;
                         goto exp;
        case WHILEX: case BEGIN: case BREAKX: case CONTINUEX: 
        case THROW: case TRY_CATCH:
        want_lowered:   runerror("internal error: expression %e not lowered", e)
                                                                               ;

    /* expression-evaluation cases for forms that appear only as frames S357a */
        case HOLE:
        case ENV:
            assert(0);
        }
        assert(0);
    value: 
        stack_trace_current_value(v, env, evalstack);
        v = validate(v);

/* if [[evalstack]] is empty, return [[v]]; otherwise step from state $\sevalv {\mathtt{fr} \sconsop S}$ 227b */
        fr = topframe(evalstack);
        if (fr == NULL) {

         /* if [[show_high_stack_mark]] is set, show maximum stack size S346g */
            if (show_high_stack_mark)
                fprintf(stderr, "High stack mark == %d\n", high_stack_mark);
            return v;
        } else {
            /* take a step from state $\sevalv {\mathtt{fr} \sconsop S}$ 228b */
            switch (fr->form.alt) {
            case SET:
             /* fill hole in [[fr->form.set]] and step to the next state 230b */
                        assert(fr->form.set.exp->alt == HOLE);
                        assert(find(fr->form.set.name, env) != NULL);
                        *find(fr->form.set.name, env) = validate(v);
                        popframe(evalstack);
                        goto value;
            case IFX:
             /* fill hole in [[fr->form.ifx]] and step to the next state 231a */
                        assert(fr->form.ifx.cond->alt == HOLE);
                        e = istrue(v) ? fr->form.ifx.truex : fr->form.ifx.falsex
                                                                               ;
                        popframe(evalstack);
                        goto exp;
            case APPLY:
           /* fill hole in [[fr->form.apply]] and step to the next state 233a */
                        if (fr->form.apply.fn->alt == HOLE) {
                                                   // Small-Step-Apply-First-Arg
                            *fr->form.apply.fn = mkLiteralStruct(v);
                            e = head_replaced_with_hole(fr->form.apply.actuals);
                            if (e)
                                goto exp;
                                                   // Small-Step-Apply-First-Arg
                            else
                                goto apply_last_arg;
                                                      // empty list of arguments
                        } else {                                    
                            e = transition_explist(fr->form.apply.actuals, v); 
                            if (e)
                                goto exp;
                                                    // Small-Step-Apply-Next-Arg
                            else goto
                                apply_last_arg;
                                                    // Small-Step-Apply-Last-Arg
                        }
                        apply_last_arg:
                                  // Small-Step-Apply-Last-Arg (or no arguments)

/* apply [[fr->form]]'s [[fn]] to its [[actuals]]; free memory; step to next state 233b */
                            {
                                Value     fn = asLiteral (fr->form.apply.fn);
                                Valuelist vs = asLiterals(fr->form.apply.actuals
                                                                              );
                                free  (fr->form.apply.fn);
                                freeEL(fr->form.apply.actuals);

                                popframe(evalstack);
                                
                                switch (fn.alt) {
                                case PRIMITIVE:

          /* apply [[fn.primitive]] to [[vs]] and step to the next state 233c */
                                    e = fr->syntax;
                                    pushframe(mkEnvStruct(env, NONCALL),
                                                                     evalstack);
                                    env = NULL;
                                    v = fn.primitive.function(e,
                                                          fn.primitive.tag, vs);
                                    freeVL(vs);
                                    goto value;
                                case CLOSURE:

/* save [[env]]; bind [[vs]] to [[fn.closure]]'s formals; step to evaluation of [[fn]]'s body 234a */
                                    {
                                        Namelist xs = fn.closure.lambda.formals;

                                        checkargc(e, lengthNL(xs), lengthVL(vs))
                                                                               ;
                                        pushenv_opt(env, CALL, evalstack);
                                        env = bindalloclist(xs, vs,
                                                                fn.closure.env);
                                        e   = fn.closure.lambda.body;
                                        freeVL(vs);
                                        goto exp;
                                    }
                                default:
                                    runerror(
                                        "%e evaluates to non-function %v in %e",
                                             fr->syntax->apply.fn, fn, fr->
                                                                        syntax);
                                }
                            }
            case LETX:
                switch (fr->form.letx.let) {
                   case LET:
                        /* continue with [[let]] frame [[fr->form.letx]] 235b */
                                 e = transition_explist(fr->form.letx.es, v);
                                 if (e) {         // Small-Step-Next-Let-Exp 
                                     goto exp;
                                 } else {         // Small-Step-Let-Body
                                     Namelist xs  = fr->form.letx.xs;
                                                  // 1. Remember x's and v's    
                                     Explist  es  = fr->form.letx.es;
                                     Valuelist vs = asLiterals(es);
                                     e = fr->form.letx.body;
                                                  // 2. Update e                
                                     popframe(evalstack);
                                                    // 3. Pop the LET frame     
                                     pushenv_opt(env, NONCALL, evalstack);
                                                  // 4. Push env                
                                     env = bindalloclist(xs, vs, env);
                                                // 5. Make new env              
                                     freeEL(es);
                                                  // 6. Recover memory          
                                     freeVL(vs);
                                     goto exp;
                                                        // 7. Step to next state
                                 }
                   case LETSTAR: goto want_lowered;
                   case LETREC:
                     /* continue with [[letrec]] frame [[fr->form.letx]] 236a */
                                 e = transition_explist(fr->form.letx.es, v);
                                 if (e) {  // Small-Step-Next-Letrec-Exp
                                     goto exp;
                                 } else {  // Small-Step-Letrec-Body

/* put values in [[fr->form.letx.es]] in locations bound to [[fr->form.letx.xs]] 236b */
                                     {
                                         Namelist xs = fr->form.letx.xs;
                                         Explist  es = fr->form.letx.es;
                                         while (es || xs) { 
                                             assert(es && xs);
                                             assert(find(xs->hd, env));
                                             *find(xs->hd, env) = asLiteral(es->
                                                                            hd);
                                             es = es->tl;
                                             xs = xs->tl;
                                         }
                                     };
                                     freeEL(fr->form.letx.es);
                                     e = fr->form.letx.body;
                                     popframe(evalstack);
                                     goto exp;
                                 }
                   default:      assert(0);
                }
            case ENV:
       /* restore [[env]] from [[fr->form.env]], pop the stack, and step 236c */
                        env = fr->form.env.contents;
                        popframe(evalstack);
                        goto value;
            case RETURNX:
                 /* return [[v]] from the current function ((prototype)) 237d */
                             runerror(
                         "Implementation of (return e) is left as an exercise");
            case LONG_GOTO:
                  /* unwind [[v]] to the nearest matching [[long-label]] 237b */
                             { Name label = fr->form.long_goto.label;
                               popframe(evalstack);
                                                   // remove the LONG_GOTO frame
                               fr = topframe(evalstack);
                                     // fr now points to the next youngest frame
                               if (fr == NULL) {
                                   runerror(
                                "long-goto %n with no active long-label for %n",
                                            label, label);
                               } else if (fr->form.alt == LONG_LABEL &&
                                          fr->form.long_label.label == label) {
                                   popframe(evalstack);
                                   goto value;
                               } else {
                                   fr->form = mkLongGotoStruct(label, hole);
                                   goto value;
                               }
                             }
            case LONG_LABEL: /* pop the stack and step to the next state 236e */
                             popframe(evalstack);
                             goto value;
            /* cases for forms that never appear as frames S357b */
            case LITERAL:  // syntactic values never appear as frames
            case VAR:
            case LAMBDAX:
            case HOLE:     // and neither do bare holes
            case BREAKX:   // nor does sugar
            case CONTINUEX:
            case WHILEX:
            case BEGIN:
            case TRY_CATCH:
            case THROW:
            case LOWERED: 
            case LOOPBACK: 
                assert(0);
            }
        }

        assert(0);
}
/* eval-stack.c 238 */
void pushenv_opt(Env env, SavedEnvTag tag, Stack s) {
    assert(s);
    Frame *f = optimize_tail_calls ? topframe(s) : NULL;
    if (f && f->form.alt == ENV) {   // don't push a new frame
        if (tag == CALL && f->form.env.tag == NONCALL) 
            f->form.env.tag = CALL;
    } else {
        pushframe(mkEnvStruct(env, tag), s);
    }
}
