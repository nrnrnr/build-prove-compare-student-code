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
                  * <evaluate [[val]] binding and return new environment>=
                  */
                 {
                     pushframe(*d->val.exp, roots.stack);
                     if (find(d->val.name, env) == NULL)
                         env = bindalloc(d->val.name, unspecified(), env);
                     *d->val.exp = topframe(roots.stack)->form;
                     popframe(roots.stack);
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
 * <evaldef.c>=
 */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo) {
    roots.globals.internal.pending_tests =
                              mkULL(NULL, roots.globals.internal.pending_tests);
    roots.registers = NULL;  // clean up after syntax error

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
            roots.globals.internal.pending_tests->hd =
                  mkUL(xd->test, roots.globals.internal.pending_tests->hd);
            break;
        default:
            assert(0);
        }
    }

    process_tests(roots.globals.internal.pending_tests->hd, *envp);
    roots.globals.internal.pending_tests = popULL(
                                          roots.globals.internal.pending_tests);
}
