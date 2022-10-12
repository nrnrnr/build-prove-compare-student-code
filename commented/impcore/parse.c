#include "all.h"
/*
 * As an example, here's the reduce function for Impcore
 * expressions:
 * <parse.c>=
 */
Exp reduce_to_exp(int code, struct Component *components) {
    switch(code) {
    case SET:    return mkSet   (components[0].name, components[1].exp);
    case IFX:    return mkIfx   (components[0].exp, components[1].exp,
                                 components[2].exp);
    case WHILEX: return mkWhilex(components[0].exp, components[1].exp);
    case BEGIN:  return mkBegin (components[0].exps);
    case APPLY:  return mkApply (components[0].name, components[1].exps);
    /*
     * <cases for \impcore's [[reduce_to_exp]] added in exercises>=
     */
    /* add your syntactic extensions here */
    /*
     * For [[ --- --- ]] and [[||]], as for any other new
     * expression, I have to add five things:
     * 
     *  1. Integer codes for the new expressions
     *  2. New cases for the [[reduce_to_exp]] function
     *  3. New arrays of shift functions (unless an existing
     *  array can be reused)
     *  4. New rows for [[exptable]]
     *  5. New rows for [[usage_table]]
     * 
     * The most interesting of these is the reduce function,
     * which expands the new form into existing syntax. The
     * new codes are named [[CAND]] and [[COR]], which stand
     * for ``conditional and'' and ``conditional or''; these
     * names were used in the programming language \proglang
     * Algol W and in Dijkstra's \citeyearpar
     * dijkstra:discipline unnamed language of ``guarded
     * commands.''
     * <cases for \impcore's [[reduce_to_exp]] added in exercises>=
     */
    case SUGAR(CAND):
        return mkIfx(components[0].exp, components[1].exp, mkLiteral(0));
    case SUGAR(COR):
        return mkIfx(components[0].exp, mkLiteral(1), components[1].exp);
    default:     assert(0);  // incorrectly configured parser
    }
}
/*
 * To extend this function, just add more cases in the
 * spot marked [[<<cases for Impcore's [[reduce_to_exp]]
 * added in exercises>>]].
 */

/*
 * With the codes in place, I can write the reduce
 * function for extended definitions. [*] [*]
 * <parse.c>=
 */
XDef reduce_to_xdef(int alt, struct Component *comps) {
    switch(alt) {
    case ADEF(VAL):    return mkDef(mkVal(comps[0].name, comps[1].exp));
    case ADEF(DEFINE): return mkDef(mkDefine(comps[0].name,
                                             mkUserfun(comps[1].names, comps[2].
                                                                         exp)));
    case ANXDEF(USE):  return mkUse(comps[0].name);
    case ATEST(CHECK_EXPECT): 
                       return mkTest(mkCheckExpect(comps[0].exp, comps[1].exp));
    case ATEST(CHECK_ASSERT): 
                       return mkTest(mkCheckAssert(comps[0].exp));
    case ATEST(CHECK_ERROR): 
                       return mkTest(mkCheckError(comps[0].exp));
    case ADEF(EXP):    return mkDef(mkExp(comps[0].exp));
    default:           assert(0);  // incorrectly configured parser
                       return NULL;
    }
}
/*
 * Here, as promised from \crefpage(cparse.fig.exptable,
 * is [[exptable]]: the parsing table for Impcore
 * expressions. Each row of [[exptable]] refers to an
 * array of shift functions, which must be defined
 * separately and given its own name.
 * <parse.c>=
 */
static ShiftFun setshifts[]   = { sName, sExp,           stop };
static ShiftFun ifshifts[]    = { sExp,  sExp, sExp,     stop };
static ShiftFun whileshifts[] = { sExp,  sExp,           stop };
static ShiftFun beginshifts[] = { sExps,                 stop };
static ShiftFun applyshifts[] = { sName, sExps,          stop };

/*
 * More syntax can be added in exercises.
 * <arrays of shift functions added to \impcore\ in exercises>=
 */
/* for each new row added to exptable, add an array of shift functions here */
/*
 * The components of a short-circuit conditional are the
 * two subexpressions e_1 and e_2, so the extension
 * needs an array of shift functions that shifts two
 * expressions and then stops.
 * <arrays of shift functions added to \impcore\ in exercises>=
 */
static ShiftFun conditionalshifts[] = { sExp, sExp, stop };
/*
 * The [[exptable]] rows use the given shift functions,
 * and the [[usage_table]] entries show the expected
 * syntax.
 */


struct ParserRow exptable[] = {
  { "set",   SET,    setshifts },
  { "if",    IFX,    ifshifts },
  { "while", WHILEX, whileshifts },
  { "begin", BEGIN,  beginshifts },
  /*
   * <rows added to \impcore's [[exptable]] in exercises>=
   */
  /* add a row here for each new syntactic form of Exp */
  /*
   * <rows added to \impcore's [[exptable]] in exercises>=
   */
  { "&&", SUGAR(CAND), conditionalshifts },
  { "||", SUGAR(COR),  conditionalshifts },
  { NULL,    APPLY,  applyshifts }  // must come last
};
/*
 * \qbreak In Impcore, [[exp_of_atom]] classifies each
 * atom as either an integer literal or a variable.
 * <parse.c>=
 */
Exp exp_of_atom(Sourceloc loc, Name atom) {
    const char *s = nametostr(atom);
    char *t;   // to point to the first non-digit in s
    long l = strtol(s, &t, 10);
    if (*t != '\0') // the number is just a prefix
        return mkVar(atom);
    else if (((l == LONG_MAX || l == LONG_MIN) && errno == ERANGE) ||
             l > (long)INT32_MAX || l < (long)INT32_MIN)
    {
        synerror(loc, "arithmetic overflow in integer literal %s", s);
        return NULL; // unreachable
    } else {  // the number is the whole atom, and not too big
        return mkLiteral(l);
    }
}
/*
 * <parse.c>=
 */
struct Usage usage_table[] = {
    { ADEF(VAL),           "(val x e)" },
    { ADEF(DEFINE),        "(define fun (formals) body)" },
    { ANXDEF(USE),         "(use filename)" },
    { ATEST(CHECK_EXPECT), "(check-expect exp-to-run exp-expected)" },
    { ATEST(CHECK_ASSERT), "(check-assert exp)" },
    { ATEST(CHECK_ERROR),  "(check-error exp)" },
    { SET,                 "(set x e)" },
    { IFX,                 "(if cond true false)" },
    { WHILEX,              "(while cond body)" },
    { BEGIN,               "(begin exp ... exp)" },
    /*
     * <\impcore\ [[usage_table]] entries added in exercises>=
     */
    { SUGAR(CAND), "(&& exp exp)" },
    { SUGAR(COR),  "(|| exp exp)" },
    { -1, NULL }  // marks end of table
};
/*
 * Finally, I define a special shift function that
 * doesn't do any shifting. Instead, it sets the context
 * for parsing a function definition. In the list of
 * parsing functions for a function definition, the
 * [[sName]] that parses the function's name is followed
 * immediately by [[setcontextname]].
 * <parse.c>=
 */
void check_def_duplicates(Sourceloc source, Def d) {
    if (d->alt == DEFINE && duplicatename(d->define.userfun.formals) != NULL)
        synerror(source,
                 "Formal parameter %n appears twice "
                 "in definition of function %n",
                 duplicatename(d->define.userfun.formals), d->define.name);
}
/*
 * Impcore has no expressions that bind names, so the
 * expression check does nothing.
 * <parse.c>=
 */
void check_exp_duplicates(Sourceloc source, Exp e) {
    (void)source; (void)e;
}
