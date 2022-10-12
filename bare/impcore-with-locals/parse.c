#include "all.h"
/* parse.c S195b */
Exp reduce_to_exp(int code, struct Component *components) {
    switch(code) {
    case SET:    return mkSet   (components[0].name, components[1].exp);
    case IFX:    return mkIfx   (components[0].exp, components[1].exp,
                                 components[2].exp);
    case WHILEX: return mkWhilex(components[0].exp, components[1].exp);
    case BEGIN:  return mkBegin (components[0].exps);
    case APPLY:  return mkApply (components[0].name, components[1].exps);
    /* cases for \impcore's [[reduce_to_exp]] added in exercises S195c */
    /* add your syntactic extensions here */
    /* cases for \impcore's [[reduce_to_exp]] added in exercises S209b */
    case SUGAR(CAND):
        return mkIfx(components[0].exp, components[1].exp, mkLiteral(0));
    case SUGAR(COR):
        return mkIfx(components[0].exp, mkLiteral(1), components[1].exp);
    default:     assert(0);  // incorrectly configured parser
    }
}
/* parse.c S196a */
XDef reduce_to_xdef(int alt, struct Component *comps) {
    switch(alt) {
    case ADEF(VAL):    return mkDef(mkVal(comps[0].name, comps[1].exp));
    case ADEF(DEFINE): return mkDef(mkDefine(comps[0].name,
                                             mkUserfun(comps[1].names, comps[2].names,
               comps[3].
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
/* parse.c S202f */
static ShiftFun setshifts[]   = { sName, sExp,           stop };
static ShiftFun ifshifts[]    = { sExp,  sExp, sExp,     stop };
static ShiftFun whileshifts[] = { sExp,  sExp,           stop };
static ShiftFun beginshifts[] = { sExps,                 stop };
static ShiftFun applyshifts[] = { sName, sExps,          stop };

/* arrays of shift functions added to \impcore\ in exercises S203c */
/* for each new row added to exptable, add an array of shift functions here */
/* arrays of shift functions added to \impcore\ in exercises S210a */
static ShiftFun conditionalshifts[] = { sExp, sExp, stop };

struct ParserRow exptable[] = {
  { "set",   SET,    setshifts },
  { "if",    IFX,    ifshifts },
  { "while", WHILEX, whileshifts },
  { "begin", BEGIN,  beginshifts },
  /* rows added to \impcore's [[exptable]] in exercises S203d */
  /* add a row here for each new syntactic form of Exp */
  /* rows added to \impcore's [[exptable]] in exercises S210b */
  { "&&", SUGAR(CAND), conditionalshifts },
  { "||", SUGAR(COR),  conditionalshifts },
  { NULL,    APPLY,  applyshifts }  // must come last
};
/* parse.c S203b */
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
/* parse.c S206c */
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
    /* \impcore\ [[usage_table]] entries added in exercises S210c */
    { SUGAR(CAND), "(&& exp exp)" },
    { SUGAR(COR),  "(|| exp exp)" },
    { -1, NULL }  // marks end of table
};
/* parse.c S208e */
void check_def_duplicates(Sourceloc source, Def d) {
    if (d->alt == DEFINE && duplicatename(d->define.userfun.formals) != NULL)
        synerror(source,
                 "Formal parameter %n appears twice "
                 "in definition of function %n",
                 duplicatename(d->define.userfun.formals), d->define.name);
}
/* parse.c S208f */
void check_exp_duplicates(Sourceloc source, Exp e) {
    (void)source; (void)e;
}
