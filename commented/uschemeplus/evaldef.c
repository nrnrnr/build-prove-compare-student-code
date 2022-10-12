#include "all.h"
/*
 * Evaluating true definitions
 * 
 * Each true definition is evaluated by function
 * [[evaldef]], which updates the store and returns a
 * new environment. If [[echo]] is [[ECHOES]],
 * [[evaldef]] also prints.\scmflabelevaldef Function
 * [[evaldef]] doesn't handle [[record]] definitions;
 * the [[record]] form is syntactic sugar, not a true
 * definition.
 * <evaldef.c>=
 */
Env evaldef(Def d, Env env, Echo echo) {
    switch (d->alt) {
    case VAL:    /*
                  * According to the operational semantics, the
                  * right-hand side of a [[val]] binding must be
                  * evaluated in an environment in which the name \qbreak
                  * [[d->val.name]] is bound. If no binding is present,
                  * one is added, with an unspecified value.
                  * <evaluate [[val]] binding and return new environment>=
                  */
                 {
                     if (find(d->val.name, env) == NULL)
                         env = bindalloc(d->val.name, unspecified(), env);
                     Value v = eval(d->val.exp, env);
                     *find(d->val.name, env) = v;
                     /*
                      * \qtrim1
                      * 
                      * Boring evaluator code
                      * 
                      * In \crefscheme.chap, [[evaldef]] uses conditional
                      * code to decide whether and what to print. Because
                      * that code is too boring to appear in \cref
                      * scheme.chap, it appears here.
                      * <if [[echo]] calls for printing, print either [[v]] or
                                                                the bound name>=
                      */
                     if (echo == ECHOING) {
                         if (d->val.exp->alt == LAMBDAX)
                             print("%n\n", d->val.name);
                         else
                             print("%v\n", v);
                     }
                     return env;
                 }
    case EXP:    /*
                  * As in Impcore, evaluating a top-level expression has
                  * the same effect on the environment as evaluating a
                  * definition of [[it]], except that the interpreter
                  * always prints the value, never the name ``it.''
                  * <evaluate expression, assign to [[it]], and return new
                                                                   environment>=
                  */
                 {
                     Value v = eval(d->exp, env);
                     Value *itloc = find(strtoname("it"), env);
                     /*
                      * <if [[echo]] calls for printing, print [[v]]>=
                      */
                     if (echo == ECHOING)
                         print("%v\n", v);
                     if (itloc == NULL) {
                         return bindalloc(strtoname("it"), v, env);
                     } else {
                         *itloc = v;
                         return env;
                     }
                 }
    case DEFINE: /*
                  * A \xdefine is rewritten to \xval.
                  * <evaluate function definition and return new environment>=
                  */
                 return evaldef(mkVal(d->define.name, mkLambdax(d->define.lambda
                                                                             )),
                                env, echo);
    case DEFS:                                                     /*OMIT*/
        for (Deflist ds = d->defs; ds != NULL; ds = ds->tl)      /*OMIT*/
            env = evaldef(ds->hd, env, echo);                      /*OMIT*/
        return env;                                                /*OMIT*/
    }
    assert(0);
}
/*
 * The read-eval-print loop
 * 
 * Function [[readevalprint]] evaluates definitions,
 * updates the environment [[*envp]], and remembers unit
 * tests. After all definitions have been read, it runs
 * the unit tests it has remembered. The last test added
 * to [[unit_tests]] is the one at the front of the
 * list, but the tests must be run in the order in which
 * they appeared in the source code, which is back to
 * front. \scmflabelreadevalprint [*]
 * <evaldef.c>=
 */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo) {
    UnitTestlist pending_unit_tests = NULL;

    for (XDef xd = getxdef(xdefs); xd; xd = getxdef(xdefs)) {
        /*
         * Function [[readevalprint]] is shared with the
         * interpreter for \uschemeplus (\crefschemes.chap). \
         * Crefschemes.chap describes a lowering operation,
         * which is used to implement some of the syntax in \
         * uschemeplus. For plain micro-Scheme, no lowering is
         * needed.
         * <lower extended definition [[xd]] as needed>=
         */
        /* not in uScheme */
        /*
         * Instrumentation for the high stack mark
         * 
         * During an evaluation, the maximum size reached by the
         * stack is called the high stack mark. It is tracked
         * here
         * <lower extended definition [[xd]] as needed>=
         */
        lowerXdef(xd);
        /*
         * A true definition or a [[use]] may modify [[*envp]].
         * A unit test is merely added to the list of pending
         * tests.
         * <evaluate extended definition [[xd]] in environment [[*envp]]>=
         */
        switch (xd->alt) {
        case DEF:
            *envp = evaldef(xd->def, *envp, echo);
            break;
        case USE:
            /*
             * The [[DEF]] case assigns to [[*envp]]. As alluded to
             * in the description of the interface to
             * [[readevalprint]] (\vpagerefscheme.repl.envp), the
             * assignment ensures that after a successful call to
             * [[evaldef]], the new environment is remembered. Even
             * if a later call to [[evaldef]] exits the loop by
             * calling [[runerror]], the assignment to [[*envp]]
             * won't be forgotten.
             * 
             * The [[DEF]] code is trickier than the [[DEF]] code
             * in Impcore: Impcore's [[readevalprint]] simply
             * mutates the global environment. In micro-Scheme,
             * environments are not mutable, so [[readevalprint]]
             * mutates [[*envp]] instead.
             * 
             * A file is read as in Impcore, except that again the
             * environment cannot be mutated, so [[use]] mutates
             * [[*envp]] instead. When [[readevalprint]] calls
             * itself recursively to read a file, it passes the same
             * [[envp]] it was given.
             * <read in a file and update [[*envp]]>=
             */
            {
                const char *filename = nametostr(xd->use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    runerror("cannot open file \"%s\"", filename);
                readevalprint(filexdefs(filename, fin, NOT_PROMPTING), envp,
                                                                          echo);
                fclose(fin);
            }
            break;
        case TEST:
            pending_unit_tests = mkUL(xd->test, pending_unit_tests);
            break;
        default:
            assert(0);
        }
    }

    process_tests(pending_unit_tests, *envp);
}
