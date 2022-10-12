#include "all.h"
/*
 * As in standard [[vprintf]], the conversion specifier
 * [[ prints a percent sign, without consuming any
 * arguments.
 * <printfuns.c>=
 */
void printpercent(Printbuf output, va_list_box *box) {
    (void)box;
    bufput(output, '%');
}
/*
 * The printers for strings, pointers, and numbers are
 * textbook examples of how to use [[va_arg]]. (Hey! You
 * are reading a textbook!)
 * <printfuns.c>=
 */
void printstring(Printbuf output, va_list_box *box) {
    const char *s = va_arg(box->ap, char*);
    bufputs(output, s);
}
/*
 * <printfuns.c>=
 */
void printdecimal(Printbuf output, va_list_box *box) {
    char buf[2 + 3 * sizeof(int)];
    snprintf(buf, sizeof(buf), "%d", va_arg(box->ap, int));
    bufputs(output, buf);
}
/*
 * <printfuns.c>=
 */
void printpointer(Printbuf output, va_list_box *box) {
    char buf[12 + 3 * sizeof(void *)];
    snprintf(buf, sizeof(buf), "%p", va_arg(box->ap, void *));
    bufputs(output, buf);
}
/*
 * The printer for names prints a name's string. A 
 * [[Name]] should never be [[NULL]], but if something
 * goes drastically wrong and a [[NULL]] pointer is
 * printed as a name, the code won't crash.
 * <printfuns.c>=
 */
void printname(Printbuf output, va_list_box *box) {
    Name np = va_arg(box->ap, Name);
    bufputs(output, np == NULL ? "<null>" : nametostr(np));
}
/*
 * <printfuns.c>=
 */
void printchar(Printbuf output, va_list_box *box) {
    int c = va_arg(box->ap, int);
    bufput(output, c);
}
/*
 * <printfuns.c>=
 */
void printpar(Printbuf output, va_list_box *box) {
    Par p = va_arg(box->ap, Par);
    if (p == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (p->alt){
    case ATOM:
        bprint(output, "%n", p->atom);
        break;
    case LIST:
        bprint(output, "(%P)", p->list);
        break;
    }
}
/*
 * Printing and values
 * 
 * Most values can be printed using just a few lines of
 * C code, but the code needed to print a closure is
 * long and tedious. When printing a closure, you don't
 * want to see the entire environment that was captured
 * in the closure. You want to see only the parts of the
 * environment that the closure actually depends on—the
 * free variables of the [[lambda]] expression. (See \
 * crefpage,mlscheme.free-bound.)
 * 
 * Finding free variables in an expression
 * 
 * Finding free variables is hard work. I start with a
 * bunch of utility functions on names. Function
 * [[nameinlist]] says whether a particular [[Name]] is
 * on a [[Namelist]].
 * <printfuns.c>=
 */
static bool nameinlist(Name n, Namelist xs) {
    for (; xs; xs=xs->tl)
        if (n == xs->hd)
            return true;
    return false;
}
/*
 * Function [[addname]] adds a name to a list, unless
 * it's already there.
 * <printfuns.c>=
 */
static Namelist addname(Name n, Namelist xs) {
    if (nameinlist(n, xs))
        return xs;
    else
        return mkNL(n, xs);
}
/*
 * Function [[freevars]] is passed an expression, a list
 * of variables known to be bound, and a list of
 * variables known to be free. If the expression
 * contains free variables not on either list,
 * [[freevars]] adds them to the free list and returns
 * the new free list. Function [[freevars]] works by
 * traversing an abstract-syntax tree; when it finds a
 * name, it calls [[addfree]] to calculate the new list
 * of free variables
 * <printfuns.c>=
 */
static Namelist addfree(Name n, Namelist bound, Namelist free) {
    if (nameinlist(n, bound))
        return free;
    else
        return addname(n, free);
}
/*
 * The [[freevars]] function has to know all the rules
 * for environments, which makes computing the free
 * variables of an expression about as much work as
 * evaluating the expression.
 * <printfuns.c>=
 */
Namelist freevars(Exp e, Namelist bound, Namelist free) {
    switch (e->alt) {
    case LITERAL:
        break;
    case VAR:
        free = addfree(e->var, bound, free);
        break;
    case IFX:
        free = freevars(e->ifx.cond, bound, free);
        free = freevars(e->ifx.truex, bound, free);
        free = freevars(e->ifx.falsex, bound, free);
        break;
    case WHILEX:
        free = freevars(e->whilex.cond, bound, free);
        free = freevars(e->whilex.body, bound, free);
        break;
    case BEGIN:
        for (Explist es = e->begin; es; es = es->tl)
            free = freevars(es->hd, bound, free);
        break;
    case SET:
        free = addfree(e->set.name, bound, free);
        free = freevars(e->set.exp, bound, free);
        break;
    case APPLY:
        free = freevars(e->apply.fn, bound, free);
        for (Explist es = e->apply.actuals; es; es = es->tl)
            free = freevars(es->hd, bound, free);
        break;
    case LAMBDAX:
        /*
         * The interesting case is the one for lambda
         * expressions. Any variables that are bound by the
         * [[lambda]] are added to the ``known bound'' list for
         * the recursive examination of the [[lambda]]'s body.
         * <let [[free]] be the free variables for [[e->lambdax]]>=
         */
        for (Namelist xs = e->lambdax.formals; xs; xs = xs->tl)
            bound = addname(xs->hd, bound);
        free = freevars(e->lambdax.body, bound, free);
        break;
    case LETX:
        /*
         * \qbreak The let expressions are a bit tricky; the
         * code must follow the rules exactly.
         * <let [[free]] be the free variables for [[e->letx]]>=
         */
        switch (e->letx.let) {
            Namelist xs;   // used to visit every bound name
            Explist  es;   // used to visit every expression that is bound
        case LET:
            for (es = e->letx.es; es; es = es->tl)
                free = freevars(es->hd, bound, free);
            for (xs = e->letx.xs; xs; xs = xs->tl)
                bound = addname(xs->hd, bound);
            free = freevars(e->letx.body, bound, free);
            break;
        case LETSTAR:
            for (xs = e->letx.xs, es = e->letx.es
               ; xs && es
               ; xs = xs->tl, es = es->tl
               ) 
            {
                free  = freevars(es->hd, bound, free);
                bound = addname(xs->hd, bound);
            }
            free = freevars(e->letx.body, bound, free);
            break;
        case LETREC:
            for (xs = e->letx.xs; xs; xs = xs->tl)
                bound = addname(xs->hd, bound);
            for (es = e->letx.es; es; es = es->tl)
                free = freevars(es->hd, bound, free);
            free = freevars(e->letx.body, bound, free);
            break;
        }
        break;
    /*
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    /*
     * \qtrim1
     * 
     * Finding free variables
     * 
     * In case the interpreter needs to print the free
     * variables of closures, each new syntactic form needs
     * a case in the [[freevars]] function from \cref
     * schemea.chap.
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    case BREAKX:
        break;
    case CONTINUEX:
        break;
    case RETURNX:
        free = freevars(e->returnx, bound, free);
        break;
    case THROW:
        free = freevars(e->throw.exp, bound, free);
        break;
    case TRY_CATCH:
        free = freevars(e->try_catch.body, bound, free);
        free = freevars(e->try_catch.handler, bound, free);
        break;
    /*
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    case LONG_LABEL:
        free = freevars(e->long_label.body, bound, free);
        break;
    case LONG_GOTO:
        free = freevars(e->long_goto.exp, bound, free);
        break;
    case LOWERED:
        free = freevars(e->lowered.before, bound, free);
                 // dare not look at after, because it might loop
        break;
    case LOOPBACK:
        break;
    /*
     * The remaining new forms appear only in contexts.
     * A context should never appear in a closure, so if the
     * interpreter finds a hole or an environment, it halts
     * with an assertion failure.
     * <extra cases for finding free variables in {\uscheme} expressions>=
     */
    case HOLE:
    case ENV:
        assert(0);
        break;
    }
    return free;
}
/*
 * Printing closures and other values
 * 
 * Free variables are used to print closures. A closure
 * is printed by printing its [[lambda]] expression,
 * plus the values of the free variables that are not
 * global. (Every closure contains a zillion bindings to
 * global variables like [[cons]], [[car]], [[+]], and
 * so on. If these bindings were printed, they would
 * overwhelm the ones we actually care about.) The hard
 * work is done by function [[printnonglobals]].
 * 
 * \qtrim1
 * 
 * A recursive micro-Scheme function is represented by a
 * closure whose environment includes a pointer back to
 * the recursive function itself. If such a closure were
 * printed by printing the values of all its free
 * variables, the printer could loop forever. Instead,
 * the looping is cut off by a [[depth]] parameter: when
 * [[depth]] reaches 0, a closure is printed simply as
 * [[<function>]].
 * <printfuns.c>=
 */
static void printnonglobals(Printbuf output, Namelist xs, Env env, int depth);

static void printclosureat(Printbuf output, Lambda lambda, Env env, int depth) {
    if (depth > 0) {
        Namelist vars = freevars(lambda.body, lambda.formals, NULL);
        bprint(output, "<%\\, {", lambda);
        printnonglobals(output, vars, env, depth - 1);
        bprint(output, "}>");
    } else {
        bprint(output, "<function>");
    }
}
/*
 * That same [[depth]] parameter is also used in the
 * value-printing functions.
 * <printfuns.c>=
 */
static void printvalueat(Printbuf output, Value v, int depth);
/*
 * \qvfilbreak[2000]1.2in
 * 
 * The way a [[PAIR]] is printed depends on whether the
 * pair is a proper list, in which the [[cdr]] is a list
 * of values, or an arbitrary cons cell, in which the
 * [[cdr]] can be any value. The difference is handled
 * by function [[printtail]]. If a cons cell doesn't
 * have another cons cell or [[NIL]] in its [[cdr]]
 * field, the [[car]] and [[cdr]] are separated by a
 * dot. This notation has been used since McCarthy's
 * original \lisp. [*]
 * <helper functions for [[printvalue]]>=
 */
static void printtail(Printbuf output, Value v, int depth) {
    switch (v.alt) {
    case NIL:
        bprint(output, ")");
        break;
    case PAIR:
        bprint(output, " ");
        printvalueat(output, *v.pair.car, depth);
        printtail(output, *v.pair.cdr, depth);
        break;
    default:
        bprint(output, " . ");
        printvalueat(output, v, depth);
        bprint(output, ")");
        break;
    }
}
static void printvalueat(Printbuf output, Value v, int depth) {
    switch (v.alt){
    case NIL:
        bprint(output, "()");
        return;
    case BOOLV:
        bprint(output, v.boolv ? "#t" : "#f");
        return;
    case NUM:
        bprint(output, "%d", v.num);
        return;
    case SYM:
        bprint(output, "%n", v.sym);
        return;
    case PRIMITIVE:
        bprint(output, "<function>");
        return;
    case PAIR:
        bprint(output, "(");
        if (v.pair.car == NULL) bprint(output, "<NULL>"); else  // OMIT
        printvalueat(output, *v.pair.car, depth);
        if (v.pair.cdr == NULL) bprint(output, " <NULL>)"); else // OMIT
        printtail(output, *v.pair.cdr, depth);
        return;
    case CLOSURE:
        printclosureat(output, v.closure.lambda, v.closure.env, depth);
        return;
    default:
        bprint(output, "<unknown v.alt=%d>", v.alt);
        return;
    }
}
/*
 * When a value is printed using the [[ the default
 * depth is 0. That is, by default the interpreter
 * doesn't print closures. If you need to debug,
 * increase the default depth.
 * <printfuns.c>=
 */
void printvalue(Printbuf output, va_list_box *box) {
    printvalueat(output, va_arg(box->ap, Value), 0);
}
/*
 * Finally, the implementation of [[printnonglobals]].
 * <printfuns.c>=
 */
Env *globalenv;
static void printnonglobals(Printbuf output, Namelist xs, Env env, int depth) {
    char *prefix = "";
    for (; xs; xs = xs->tl) {
        Value *loc = find(xs->hd, env);
        if (loc && (globalenv == NULL || find(xs->hd, *globalenv) != loc)) {
            bprint(output, "%s%n -> ", prefix, xs->hd);
            prefix = ", ";
            printvalueat(output, *loc, depth);
        }
    }
}
/*
 * Print functions \chaptocsplitfor expressions
 * 
 * micro-Scheme defines special conversion specifiers
 * for printing abstract-syntax trees. The (boring) code
 * the does the printing is written here.
 * <printfuns.c>=
 */
void printdef(Printbuf output, va_list_box *box) {
    Def d = va_arg(box->ap, Def);
    if (d == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (d->alt) {
    case VAL:
        bprint(output, "(val %n %e)", d->val.name, d->val.exp);
        return;
    case EXP:
        bprint(output, "%e", d->exp);
        return;
    case DEFINE:
        bprint(output, "(define %n %\\)", d->define.name, d->define.lambda);
        return;
    case DEFS:
                                                                        /*OMIT*/
        for (Deflist ds = d->defs; ds; ds = ds->tl)                    /*OMIT*/
            bprint(output, "%t%s", ds->hd, ds->tl != NULL ? "\n" : "");
                                                                        /*OMIT*/
        return;
                                                                        /*OMIT*/
    }
    assert(0);
}
/*
 * \qbreak
 * <printfuns.c>=
 */
void printxdef(Printbuf output, va_list_box *box) {
    XDef d = va_arg(box->ap, XDef);
    if (d == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (d->alt) {
    case USE:
        bprint(output, "(use %n)", d->use);
        return;
    case TEST:
        bprint(output, "CANNOT PRINT UNIT TEST XXX\n");
        return;
    case DEF:
        bprint(output, "%t", d->def);
        return;
    }
    assert(0);
}
/*
 * <printfuns.c>=
 */
static void printlet(Printbuf output, Exp let) {
    switch (let->letx.let) {
    case LET:
        bprint(output, "(let (");
        break;
    case LETSTAR:
        bprint(output, "(let* (");
        break;
    case LETREC:
        bprint(output, "(letrec (");
        break;
    default:
        assert(0);
    }
    Namelist xs;  // visits every let-bound name
    Explist es;   // visits every bound expression
    for (xs = let->letx.xs, es = let->letx.es; 
         xs && es;
         xs = xs->tl, es = es->tl)
        bprint(output, "(%n %e)%s", xs->hd, es->hd, xs->tl?" ":"");
    bprint(output, ") %e)", let->letx.body);
}   
/*
 * \qvfilbreak[2000]1in
 * <printfuns.c>=
 */
void printexp(Printbuf output, va_list_box *box) {
    Exp e = va_arg(box->ap, Exp);
    if (e == NULL) {
        bprint(output, "<null>");
        return;
    }

    switch (e->alt) {
    case LITERAL:
        if (e->literal.alt == NUM || e->literal.alt == BOOLV)
            bprint(output, "%v", e->literal);
        else
            bprint(output, "'%v", e->literal);
        break;
    case VAR:
        bprint(output, "%n", e->var);
        break;
    case IFX:
        bprint(output, "(if %e %e %e)", e->ifx.cond, e->ifx.truex, e->ifx.falsex
                                                                              );
        break;
    case WHILEX:
        bprint(output, "(while %e %e)", e->whilex.cond, e->whilex.body);
        break;
    case BEGIN:
        bprint(output, "(begin%s%E)", e->begin ? " " : "", e->begin);
        break;
    case SET:
        bprint(output, "(set %n %e)", e->set.name, e->set.exp);
        break;
    case LETX:
        printlet(output, e);
        break;
    case LAMBDAX:
        bprint(output, "%\\", e->lambdax);
        break;
    case APPLY:
        bprint(output, "(%e%s%E)", e->apply.fn,
              e->apply.actuals ? " " : "", e->apply.actuals);
        break;
    /*
     * \qbreak
     * 
     * Support for \secrlap\titleuschemeplus
     * 
     * In three locations, the micro-Scheme interpreter
     * contains empty placeholders which, in \cref
     * schemes.chap, are filled with code that implements
     * parts of \uschemeplus. (\uschemeplus is an extension
     * that adds control operators to micro-Scheme.)
     * The placeholders are defined as follows:
     * <extra cases for printing {\uscheme} ASTs>=
     */
    /*
     * <extra cases for printing {\uscheme} ASTs>=
     */
    case BREAKX:
        bprint(output, "(break)");
        break;
    case CONTINUEX:
        bprint(output, "(continue)");
        break;
    case RETURNX:
        bprint(output, "(return %e)", e->returnx);
        break;
    case THROW:
        bprint(output, "(throw %n %e)", e->throw.label, e->throw.exp);
        break;
    case TRY_CATCH:
        bprint(output, "(try-catch %e %n %e)", e->try_catch.body,
                       e->try_catch.label, e->try_catch.handler);
        break;
    case LONG_LABEL:
        bprint(output, "(long-label %n %e)", e->long_label.label, e->
                                                               long_label.body);
        break;
    case LONG_GOTO:
        bprint(output, "(long-goto %n %e)", e->long_goto.label, e->long_goto.exp
                                                                              );
        break;
    case HOLE:
        bprint(output, "<*>");
        break;
    case ENV:
        bprint(output, "Saved %senvironment %*",
               e->env.tag == CALL ? "caller's " : "", (void*)e->env.contents);
        break;
    /*
     * <extra cases for printing {\uscheme} ASTs>=
     */
    case LOWERED:
        bprint(output, "%e", e->lowered.before);
        break;
    case LOOPBACK:
        bprint(output, "...loopback...");
        break;
    default:
        assert(0);
    }
}
/*
 * <printfuns.c>=
 */
void printlambda(Printbuf output, va_list_box *box) {
    Lambda l = va_arg(box->ap, Lambda);
    bprint(output, "(lambda (%N) %e)", l.formals, l.body);
}
