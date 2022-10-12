#include "all.h"
/*
 * \qbreak
 * 
 * Structure and invariants of the evaluator
 * 
 * As in \crefimpcore.chap,scheme.chap, the main
 * judgment of the operational semantics—the state
 * transition <e/v,rho,sigma,S> \STEPS<e'/v', rho', 
 * sigma', S'>—is implemented by function [[eval]]. This
 * function starts with an expression e and an empty
 * stack S, and it repeats the transition until the
 * current item is a value v and the stack is once again
 * empty. Then it returns v. Function [[eval]]'s
 * essential invariants are as follows:
 * 
 *   • The environment rho is always in [[env]] and the
 *  stack is always in [[evalstack]]. As part of the
 *  state transition, these variables are mutated in
 *  place to hold rho' and S', respectively.
 *   • When the stack is not empty, the youngest frame
 *  (the ``top'') is pointed to by local variable
 *  [[fr]] (for ``frame'').
 *   • When the current item is an expression e, that
 *  expression is stored in argument [[e]], and the
 *  state transition begins at label [[exp]].
 *   • When the current item is a value v, that value is
 *  stored in local variable [[v]], and the state
 *  transition begins at label [[value]].
 *   • Each state transition ends with \monoboxgoto exp
 *  or \monoboxgoto value. Before the goto, either
 *  [[e]] or [[v]] is set to the current item for the
 *  next state. Variables [[env]] and [[evalstack]]
 *  are also set.
 * 
 * [*]\scmpflabeleval
 * <eval-stack.c>=
 */
Value eval(Exp e, Env env) {
    Value v;
    Frame *fr;
    /*
     * The interpreter uses a lot of holes, and I don't want
     * it to have to allocate each one. Instead, I define a
     * single static value [[hole]], which is stored in
     * initialized data, not on the heap.
     * <definition of static [[Exp hole]], which always has a hole>=
     */
    static struct Exp holeExp = { HOLE, { { NIL, { 0 } } } };
    static Exp hole = &holeExp;
    static Stack evalstack;

    /*
     * On every evaluation, the [[evalstack]] structure is
     * initialized or reset.
     * <ensure that [[evalstack]] is initialized and empty>=
     */
    if (evalstack == NULL)
        evalstack = emptystack();
    else
        clearstack(evalstack);
    /*
     * Instrumentation for the high stack mark
     * 
     * During an evaluation, the maximum size reached by the
     * stack is called the high stack mark. It is tracked
     * here
     * <use the options in [[env]] to initialize the instrumentation>=
     */
    high_stack_mark = 0;
    show_high_stack_mark = 
        istrue(getoption(strtoname("&show-high-stack-mark"), env, falsev));
    /*
     * Function [[stack_trace_init]] is called from
     * [[eval]], which has access to [[env]]. The call does
     * just a little sanity checking. The sanity check does
     * not prevent \uschemeplus code from changing the value
     * of [[ --- trace-stack]] to a non-number. If that
     * happens, chaos may ensue.
     * <use the options in [[env]] to initialize the instrumentation>=
     */
    {   Value *p = find(strtoname("&trace-stack"), env);
        if (p && p->alt == NUM)
            stack_trace_init(&p->num);
        else
            stack_trace_init(NULL);
    }
    /*
     * Stack-tracking options are set above. The remaining
     * option is the one that controls the optimization of
     * tail calls. By default, tail calls are optimized.
     * <use the options in [[env]] to initialize the instrumentation>=
     */
    optimize_tail_calls = 
        istrue(getoption(strtoname("&optimize-tail-calls"), env, truev));

    exp: 
        stack_trace_current_expression(e, env, evalstack);
        /*
         * When the current item is an expression e, 13 of the
         * 22 expression \makenwnotdef(left as exercise) forms
         * in \uschemeplus are legitimate.
         * <take a step from state $\seval e$>=
         */
        switch (e->alt) {
        case LITERAL: /*
                       * To evaluate a literal expression, take v out of the
                       * expression and make it the current item. Don't change
                       * the stack. The machine must step to a state of the
                       * form \sevalvS. Environment rho and store sigma are
                       * unchanged, so the machine needs only to update [[v]]
                       * and to go to [[value]].
                       * <start [[e->literal]] and step to the next state>=
                       */
                      v = e->literal;
                      goto value;
        case VAR:     /*
                       * To evaluate a variable, look up its value and make
                       * that the current item. Don't change the stack. [*]
                       * <start [[e->var]] and step to the next state>=
                       */
                      if (find(e->var, env) == NULL)
                          runerror("variable %n not found", e->var);
                      v = *find(e->var, env);
                      goto value;
        case SET:     /*
                       * <start [[e->set]] and step to the next state>=
                       */
                      if (find(e->set.name, env) == NULL)
                          runerror("set unbound variable %n", e->set.name);
                      pushframe(mkSetStruct(e->set.name, hole), evalstack);
                      e = e->set.exp;
                      goto exp;
                      /*
                       * The \xset is completed by the \rulenameFinish-Assign
                       * rule. When the youngest frame on the stack is a \xset
                       * (x, \hole) frame, the machine completes the \xset by
                       * assigning v to x, then pops the frame.
                       */

        case IFX:     /*
                       * \xif is implemented just like \xset: to evaluate \xif
                       * (e_1, e_2, e_3), push the frame \xif(\hole, e_2, e_3)
                       * , then evaluate the condition e_1.
                       * <start [[e->ifx]] and step to the next state>=
                       */
                      pushframe(mkIfxStruct(hole, e->ifx.truex, e->ifx.falsex),
                                                                     evalstack);
                      e = e->ifx.cond;
                      goto exp;
        case LETX:
            if (/*
                 * Interpreting \xlet and \xletrec
                 * 
                 * To evaluate a \xlet or \xletrec that has no bindings,
                 * evaluate its body in environment rho. \opsEmpty-Let \
                 * eval\xlet(<>, e) ==>\evale
                 * <[[e->letx]] contains no bindings>=
                 */
                e->letx.xs == NULL && e->letx.es == NULL) {
                 /*
                  * <continue by evaluating the body of the [[let]] or
                                                                    [[letrec]]>=
                  */
                 e = e->letx.body;
                 goto exp;
            } else {
                switch (e->letx.let) {
                   case LET:     /*
                                  * To evaluate a nonempty \xlet expression,
                                                                    push a frame
                                  * in which the first right-hand side e_1 is
                                                                     replaced by
                                  * a hole, and start evaluating e_1. The frame
                                                                        is built
                                  * using a copy of [[e->letx.es]], and the hole
                                                                              is
                                  * inserted into the copy by calling
                                  * [[head_replaced_with_hole]].
                                  * <start \xlet\ [[e->letx]] and step to the
                                                                    next state>=
                                  */
                                 pushframe(mkLetxStruct(e->letx.let, e->letx.xs,
                                                                                
                                                        copyEL(e->letx.es), e->
                                                                     letx.body),
                                           evalstack);
                                 fr = topframe(evalstack);
                                 e  = head_replaced_with_hole(fr->form.letx.es);
                                 assert(e);
                                 goto exp;
                   case LETSTAR: goto want_lowered;
                   case LETREC:  /*
                                  * On the concrete side, forms are a little
                                                                            more
                                  * diverse.
                                  * 
                                  *   • Some forms, like [[VAR]] or
                                                                [[LITERAL]], are
                                  *  written syntactically using a single atom.
                                  *   • Most forms, including [[SET]] and
                                                                     [[IF]], are
                                  *  written syntactically as a sequence of
                                                                        [[Par]]s
                                  *  wrapped in parentheses. And with one
                                                                      exception,
                                  *  the first of these [[Par]]s is a keyword,
                                                                            like
                                  *  [[set]] or [[if]]. The exception is the
                                  *  function-application form. (For the
                                                                        extended
                                  *  definitions, the exception is the the top-
                                                                           level
                                  *  expression form—a top-level expression
                                                                       may begin
                                  *  with a keyword, but it's a keyword that the
                                  *  extended-definition parser won't recognize.
                                                                               )
                                  * 
                                  * These properties help determine a plan:
                                  * 
                                  *  1. There will be two parsers: one for
                                                                     expressions
                                  *  and one for extended definitions.
                                  *  2. If a parser sees an atom, it must know
                                                                         what to
                                  *  do.
                                  *  3. If a parser sees a parenthesized
                                                                    [[Parlist]],
                                  *  it will consult a table of rows.
                                  *    □ Each row knows how to parse one
                                                                       syntactic
                                  *      form. What does it mean ``to know how
                                                                              to
                                  *      parse''? The row begins with a keyword
                                                                            that
                                  *      the parser should look for. The row
                                                                            also
                                  *      includes an integer code that
                                                                  identifies the
                                  *      form, and finally, the row lists the
                                  *      components of the form. To see some
                                                                         example
                                  *      rows, look at the parsing table for
                                                                        Impcore,
                                  *      in \tabrefcparse.fig.exptable.
                                  *    □ A row matches an input [[Parlist]] if
                                                                             the
                                  *      row's keyword is equal to the first
                                                                         element
                                  *      of the [[Parlist]]. The parser proceeds
                                  *      through the rows looking for one that
                                                                         matches
                                  *      its input.
                                  * 
                                  *  \promissory
                                  *  ━━━━━━━━━━━━━━�
��━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                        ━━━━━━━━
                                  * 
                                  *                                  |
                                  *       Parsed components          |
                                                                 Unparsed inputs
                                  *                                  |
                                  * 
                                  *  Sketch of a parsing machine (ParserState)
                                  *  ━━━━━━━━━━━━━━�
��━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                        ━━━━━━━━
                                  * 
                                  *  4. Once the parser finds the right row, it
                                                                       gets each
                                  *  component from the input [[Parlist]], then
                                                                          checks
                                  *  to make sure there are no leftover inputs.
                                  *  Finally it passes the components and the
                                                                         integer
                                  *  code to a reduce function. Impcore uses two
                                                                            such
                                  *  functions: [[reduce_to_exp]] and
                                  *  [[reduce_to_xdef]]. Each of these functions
                                                                           takes
                                  *  a sequence of components and reduces it to
                                                                               a
                                  *  single node in an abstract-syntax tree. (
                                                                        The name
                                  *  [[reduce]] comes from shift-reduce parsing,
                                                                           which
                                  *  refers to a family of parsing techniques of
                                                                           which
                                  *  my parsers are members.)
                                  * 
                                  * I've designed the parsers to work this way
                                                                         so that
                                  * you can easily add new syntactic forms. It's
                                                                              as
                                  * simple as adding a row to a table and a case
                                                                            to a
                                  * reduce function. In more detail,
                                  * 
                                  *  1. Decide whether you wish to add an
                                                                 expression form
                                  *  or a definition form. That will tell you
                                                                            what
                                  *  table and reduce function to modify. For
                                                                        example,
                                  *  if you want to add a new expression form,
                                                                          modify
                                  *  functions [[exptable]] and
                                                              [[reduce_to_exp]].
                                  *  2. Choose a keyword and an unused integer
                                                                           code.
                                  *  As shown below, codes for extended
                                                                     definitions
                                  *  have to be chosen with a little care.
                                  *  3. Add a row to your chosen table.
                                  *  4. Add a case to your chosen reduce
                                                                       function.
                                  * 
                                  * I think you'll like being able to extend
                                                                    languages so
                                  * easily, but there's a cost—the table-
                                                                   driven parser
                                  * needs a lot of infrastructure. That
                                                                 infrastructure,
                                  * which lives in file parse.c, is described
                                                                          below.
                                  * <start \xletrec\ [[e->letx]] and step to the
                                                                    next state>=
                                  */
                                 pushenv_opt(env, NONCALL, evalstack);
                                 /*
                                  * The implementation of [[letrec]] begins by
                                                                      allocating
                                  * a new mutable cell for each bound variable.
                                                                        The cell
                                  * is initialized to an unspecified value.
                                  * <bind every name in [[e->letx.xs]] to an
                                                  unspecified value in [[env]]>=
                                  */
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
        case LAMBDAX:    /*
                          * To evaluate a \xlambda, allocate a closure and make
                          * it the current item. Don't change the stack. Formal
                          * parameters \ldotsnx are confirmed to be distinct
                                                                            when
                          * [[e]] is parsed.
                          * <start [[e->lambdax]] and step to the next state>=
                          */
                         v = mkClosure(e->lambdax, env);
                         goto value;
        case APPLY:      /*
                          * <start [[e->apply]] and step to the next state>=
                          */
                         pushframe(mkApplyStruct(mkHole(), copyEL(e->
                                                    apply.actuals)), evalstack);
                         topframe(evalstack)->syntax = e;
                         e = e->apply.fn;
                         goto exp;
        case RETURNX:    /*
                          * <start [[e->returnx]] and step to the next state>=
                          */
                         pushframe(mkReturnxStruct(hole), evalstack);
                         e = e->returnx;
                         goto exp;
                         /*
                          * Once a [[return]]'s expression has been evaluated and
                          * the youngest frame on the stack is \xreturn(\hole),
                          * the machine unwinds the stack until it finds an \
                          * csavedenv frame with a call tag. The implementation
                          * is left for you, as \exrefschemes.ex.return.
                          */

        case LONG_LABEL: /*
                          * Interpreting control operators
                          * 
                          * Only the [[long-label]], [[long-goto]], and
                          * [[return]] forms are interpreted in [[eval]]. The
                          * other control operators are implemented by lowering.
                          * 
                          * To evaluate a label, save the current environment
                                                                             and
                          * the label on the stack, then evaluate the body.
                          * <start [[e->long_label]] and step to the next state
                                                                              >=
                          */
                         pushenv_opt(env, NONCALL, evalstack);
                         pushframe(mkLongLabelStruct(e->long_label.label, hole),
                                                                     evalstack);
                         e = e->long_label.body;
                         goto exp;
        case LONG_GOTO:  /*
                          * <start [[e->long_goto]] and step to the next state>=
                          */
                         pushframe(mkLongGotoStruct(e->long_goto.label, hole),
                                                                     evalstack);
                         e = e->long_goto.exp;
                         goto exp;
        case LOWERED:    /*
                          * The actions in each state are implemented below,
                          * starting with the simplest forms—the ones that
                                                                           don't
                          * look at the stack.
                          * 
                          * Interpreting forms that don't change the stack
                          * 
                          * To evalute an expression that has been lowered,
                          * replace it with its lowered form. Don't change the
                          * stack.
                          * <replace [[e]] with its lowered form and continue in
                                                                    this state>=
                          */
                         e = e->lowered.after;
                         goto exp;
        case LOOPBACK:   /*
                          * To evaluate an expression tagged with [[LOOPBACK]],
                          * replace the tagged form with the untagged form.
                                                                           Don't
                          * change the stack. (The [[LOOPBACK]] tag is used only
                          * by the garbage collector in \crefgc.chap.)
                          * <look inside [[LOOPBACK]] and continue in this state
                                                                              >=
                          */
                         e = e->loopback;
                         goto exp;
        case WHILEX: case BEGIN: case BREAKX: case CONTINUEX: 
        case THROW: case TRY_CATCH:
        want_lowered:   runerror("internal error: expression %e not lowered", e)
                                                                               ;
        /*
         * \qbreak When the interpreter is evaluating a current
         * expression, that expression should never be a hole or
         * an environment. If it is, the interpreter halts with
         * an assertion failure.
         * <expression-evaluation cases for forms that appear only as frames>=
         */
        case HOLE:
        case ENV:
            assert(0);
        }
        assert(0);
    value: 
        stack_trace_current_value(v, env, evalstack);
        v = validate(v);
        /*
         * <if [[evalstack]] is empty, return [[v]]; otherwise step from state
                                            $\sevalv {\mathtt{fr} \sconsop S}$>=
         */
        fr = topframe(evalstack);
        if (fr == NULL) {
            /*
             * At the end of an evaluation, the high stack mark can
             * be recorded.
             * <if [[show_high_stack_mark]] is set, show maximum stack size>=
             */
            if (show_high_stack_mark)
                fprintf(stderr, "High stack mark == %d\n", high_stack_mark);
            return v;
        } else {
            /*
             * When the current item is a value v, 9 of the 22
             * expression forms in \uschemeplus may legitimately
             * appear as the youngest (top) frame on the stack, 
             * [[fr]].
             * <take a step from state $\sevalv {\mathtt{fr} \sconsop S}$>=
             */
            switch (fr->form.alt) {
            case SET:   /*
                         * <fill hole in [[fr->form.set]] and step to the next
                                                                         state>=
                         */
                        assert(fr->form.set.exp->alt == HOLE);
                        assert(find(fr->form.set.name, env) != NULL);
                        *find(fr->form.set.name, env) = validate(v);
                        popframe(evalstack);
                        goto value;
            case IFX:   /*
                         * When the youngest frame on the stack is an \xif(\hole
                         * , e_2, e_3) frame, the machine continues with e_2 or 
                         * e_3, as determined by [[v]].
                         * <fill hole in [[fr->form.ifx]] and step to the next
                                                                         state>=
                         */
                        assert(fr->form.ifx.cond->alt == HOLE);
                        e = istrue(v) ? fr->form.ifx.truex : fr->form.ifx.falsex
                                                                               ;
                        popframe(evalstack);
                        goto exp;
            case APPLY: /*
                         * When the youngest frame on the stack is an \xapply
                         * frame, the next state transition is dictated by one
                         * of these three rules: \punctrule, \punctrule, \
                         * punctrule*. Which rule does the dictating depends how
                         * the hole appears: as the function, as an argument, or
                         * as the last argument. There is also a case not given
                         * in the semantics: the list of arguments might be
                         * empty.
                         * <fill hole in [[fr->form.apply]] and step to the next
                                                                         state>=
                         */
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
                            /*
                             * Once the overwritten [[fr->form.apply.actuals]]
                                                                             and
                             * [[fr->form.apply.fn]] are converted to values,
                                                                           their
                             * memory is freed. The frame is popped, and the
                             * function is applied.
                             * <apply [[fr->form]]'s [[fn]] to its [[actuals]]; free memory; step to next state>=
                             */
                            {
                                Value     fn = asLiteral (fr->form.apply.fn);
                                Valuelist vs = asLiterals(fr->form.apply.actuals
                                                                              );
                                free  (fr->form.apply.fn);
                                freeEL(fr->form.apply.actuals);

                                popframe(evalstack);
                                
                                switch (fn.alt) {
                                case PRIMITIVE:
                                    /*
                                     * A primitive is applied in the standard
                                                                            way.
                                     * <apply [[fn.primitive]] to [[vs]] and
                                                        step to the next state>=
                                     */
                                    v = fn.primitive.function(fr->syntax,
                                                          fn.primitive.tag, vs);
                                    freeVL(vs);
                                    goto value;
                                case CLOSURE:
                                    /*
                                     * A closure is also applied in the standard
                                                                            way,
                                     * according to this rule: \punctrule*.
                                                                 Before the body
                                     * of the closure is evaluated, the
                                                            environment is saved
                                     * on the stack.
                                     * <save [[env]]; bind [[vs]] to
                 [[fn.closure]]'s formals; step to evaluation of [[fn]]'s body>=
                                     */
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
                   case LET:     /*
                                  * A \xlet expression in progress is governed
                                                                        by these
                                  * rules: Both rules are implemented by
                                                                        function
                                  * [[transition_explist]], which puts v in the
                                                                        hole and
                                  * moves the hole. But if the hole is in last
                                                                       position,
                                  * implementing the \rulenameSmall-Step-Let-
                                                                    Body rule is
                                  * tricky. Before the frame \mathbox\xlet(<x_1,
                                                                            v_1,
                                  * ..., x_n, \hole>, e) is popped, names \
                                                                     ldotsnx are
                                  * used to update the environment rho, but
                                                                     after the \
                                  * xlet frame is popped, the original rho needs
                                                                           to be
                                  * saved on the stack. The steps are shown by
                                                                        numbered
                                  * comments in the code.
                                  * <continue with [[let]] frame
                                                             [[fr->form.letx]]>=
                                  */
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
                   case LETREC:  /*
                                  * \qtrim1.1 The expressions in a \xletrec are
                                                                         already
                                  * evaluated in an extended environment, so
                                                                        when the
                                  * last expression is evaluated, the only step
                                                                          needed
                                  * before evaluating the body is to update the
                                                                          store.
                                  * <continue with [[letrec]] frame
                                                             [[fr->form.letx]]>=
                                  */
                                 e = transition_explist(fr->form.letx.es, v);
                                 if (e) {  // Small-Step-Next-Letrec-Exp
                                     goto exp;
                                 } else {  // Small-Step-Letrec-Body
                                     /*
                                      * <put values in [[fr->form.letx.es]] in
                                       locations bound to [[fr->form.letx.xs]]>=
                                      */
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
            case ENV:   /*
                         * The \xlet and \xapply forms both save environments on
                         * the stack. When the youngest frame on the stack is an
                         * \csavedenv frame, the saved environment is restored
                         * by assigning it to [[env]]. In this context, the tag
                         * is ignored.
                         * <restore [[env]] from [[fr->form.env]], pop the stack
                                                                    , and step>=
                         */
                        env = fr->form.env.contents;
                        popframe(evalstack);
                        goto value;
            case RETURNX:    /*
                              * <return [[v]] from the current function ((
                                                                   prototype))>=
                              */
                             runerror(
                         "Implementation of (return e) is left as an exercise");
            case LONG_GOTO:  /*
                              * Once the body of the \xlonggoto has been
                                                                       evaluated
                              * and the youngest frame on the stack is \
                                                                    xlonggoto(L,
                              * \hole), the machine starts looking for a target
                              * label. It uses these two rules: \punctrule, \
                              * punctrule. To implement the rules, the
                                                                     interpreter
                              * pops the stack, setting [[fr]] to the next
                                                                        youngest
                              * frame. If [[fr]] points to the label L, it too
                                                                              is
                              * popped, and the transfer is complete. Otherwise,
                              * [[*fr]] is overwritten with \xlonggoto(L, \hole)
                                                                               ,
                              * effectively unwinding one frame from the stack,
                                                                             and
                              * evaluation continues.
                              * <unwind [[v]] to the nearest matching
                                                                [[long-label]]>=
                              */
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
            case LONG_LABEL: /*
                              * When the youngest frame on the stack is a \
                                                                      xlonglabel
                              * frame, it is simply popped.
                              * <pop the stack and step to the next state>=
                              */
                             popframe(evalstack);
                             goto value;
            /*
             * And when the interpreter is evaluating a current
             * value, the top of the stack should be a well-formed
             * context. If the top of the stack takes any of the
             * forms below, then it does not represent a well-formed
             * context, and the interpreter halts with an assertion
             * failure.
             * <cases for forms that never appear as frames>=
             */
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
/*
 * <eval-stack.c>=
 */
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
