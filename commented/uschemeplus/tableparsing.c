#include "all.h"
/*
 * On the concrete side, forms are a little more
 * diverse.
 * 
 *   • Some forms, like [[VAR]] or [[LITERAL]], are
 *  written syntactically using a single atom.
 *   • Most forms, including [[SET]] and [[IF]], are
 *  written syntactically as a sequence of [[Par]]s
 *  wrapped in parentheses. And with one exception,
 *  the first of these [[Par]]s is a keyword, like
 *  [[set]] or [[if]]. The exception is the
 *  function-application form. (For the extended
 *  definitions, the exception is the the top-level
 *  expression form—a top-level expression may begin
 *  with a keyword, but it's a keyword that the
 *  extended-definition parser won't recognize.)
 * 
 * These properties help determine a plan:
 * 
 *  1. There will be two parsers: one for expressions
 *  and one for extended definitions.
 *  2. If a parser sees an atom, it must know what to
 *  do.
 *  3. If a parser sees a parenthesized [[Parlist]],
 *  it will consult a table of rows.
 *    □ Each row knows how to parse one syntactic
 *      form. What does it mean ``to know how to
 *      parse''? The row begins with a keyword that
 *      the parser should look for. The row also
 *      includes an integer code that identifies the
 *      form, and finally, the row lists the
 *      components of the form. To see some example
 *      rows, look at the parsing table for Impcore,
 *      in \tabrefcparse.fig.exptable.
 *    □ A row matches an input [[Parlist]] if the
 *      row's keyword is equal to the first element
 *      of the [[Parlist]]. The parser proceeds
 *      through the rows looking for one that matches
 *      its input.
 * 
 *  \promissory
 *  ━━━━━━━━━━━━━━━━━━━━━━━━━�
         ��━━━━━━━━━━━━━━━━━━━━━━━
 * 
 *                                  |
 *       Parsed components          |      Unparsed inputs
 *                                  |
 * 
 *  Sketch of a parsing machine (ParserState)
 *  ━━━━━━━━━━━━━━━━━━━━━━━━━�
         ��━━━━━━━━━━━━━━━━━━━━━━━
 * 
 *  4. Once the parser finds the right row, it gets each
 *  component from the input [[Parlist]], then checks
 *  to make sure there are no leftover inputs.
 *  Finally it passes the components and the integer
 *  code to a reduce function. Impcore uses two such
 *  functions: [[reduce_to_exp]] and
 *  [[reduce_to_xdef]]. Each of these functions takes
 *  a sequence of components and reduces it to a
 *  single node in an abstract-syntax tree. (The name
 *  [[reduce]] comes from shift-reduce parsing, which
 *  refers to a family of parsing techniques of which
 *  my parsers are members.)
 * 
 * I've designed the parsers to work this way so that
 * you can easily add new syntactic forms. It's as
 * simple as adding a row to a table and a case to a
 * reduce function. In more detail,
 * 
 *  1. Decide whether you wish to add an expression form
 *  or a definition form. That will tell you what
 *  table and reduce function to modify. For example,
 *  if you want to add a new expression form, modify
 *  functions [[exptable]] and [[reduce_to_exp]].
 *  2. Choose a keyword and an unused integer code.
 *  As shown below, codes for extended definitions
 *  have to be chosen with a little care.
 *  3. Add a row to your chosen table.
 *  4. Add a case to your chosen reduce function.
 * 
 * I think you'll like being able to extend languages so
 * easily, but there's a cost—the table-driven parser
 * needs a lot of infrastructure. That infrastructure,
 * which lives in file parse.c, is described below.
 * <tableparsing.c>=
 */
/*
 * <private function prototypes for parsing>=
 */
static Namelist parsenamelist(Parlist ps, ParsingContext context);
/*
 * <private function prototypes for parsing>=
 */
static bool rowmatches(struct ParserRow *row, Name first);
/*
 * <private function prototypes for parsing>=
 */
void *name_error(Par bad, struct ParsingContext *context); 
                     // expected a name, but got something else
/*
 * A new parser state is created from a [[Par]] to be
 * parsed, as well as the source of the [[Par]]. Those
 * parameters provide the states' input and part of its
 * context. The state's output is empty.
 * <tableparsing.c>=
 */
struct ParserState mkParserState(Par p, Sourceloc source) {
    assert(p->alt == LIST);
    assert(source != NULL && source->sourcename != NULL);
    struct ParserState s;
    s.input          = p->list;
    s.context.par    = p;
    s.context.source = source;
    s.context.name   = NULL;
    s.nparsed        = 0;
    return s;
}
/*
 * These functions have short names because below I put
 * them in arrays: I represent a syntactic form's
 * components as an array of shift functions. This dirty
 * trick is inspired by the functional-programming
 * techniques described in \chaprefscheme. But before I
 * put the shift functions in arrays, let's see their
 * implementations.
 * 
 * A shift operation is divided into two halves. The
 * first half removes an input and ensures that there is
 * room for a component. The second half writes the
 * component and updates [[nparsed]]. The first half is
 * the same for every shift function, and it looks like
 * this:
 * <tableparsing.c>=
 */
void halfshift(ParserState s) {
    assert(s->input);
    s->input = s->input->tl;  // move to the next input
    assert(s->nparsed < MAXCOMPS);
}
/*
 * A full shift calls [[halfshift]] and then places a
 * component. The full shift for an expression is
 * defined as function [[sExp]]. It calls [[parseexp]],
 * with which it is mutually recursive.
 * <tableparsing.c>=
 */
ParserResult sExp(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        halfshift(s);
        s->components[s->nparsed++].exp = parseexp(p, s->context.source);
        return PARSED;
    }
}
/*
 * Function [[sExps]] converts the entire input into an
 * [[Explist]]. The [[halfshift]] isn't useful here. And
 * a [[NULL]] input is OK; it just parses into an empty
 * [[Explist]].
 * <tableparsing.c>=
 */
ParserResult sExps(ParserState s) {
    Explist es = parseexplist(s->input, s->context.source);
    assert(s->nparsed < MAXCOMPS);
    s->input = NULL;
    s->components[s->nparsed++].exps = es;
    return PARSED;
}
/*
 * Function [[parseexplist]] is defined below with the
 * other parsing functions.
 */

/*
 * \qbreak Function [[sName]] is structured just like
 * [[sExp]]; the only difference is that where [[sExp]]
 * calls [[parseexp]], [[sName]] calls [[parsename]].
 * <tableparsing.c>=
 */
ParserResult sName(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        halfshift(s);
        s->components[s->nparsed++].name = parsename(p, &s->context);
        return PARSED;
    }
}
/*
 * Notice that [[parsename]], which is defined below,
 * takes the current context as an extra parameter. That
 * context enables [[parsename]] to give a good error
 * message if it encounters an input that is not a valid
 * name.
 */

/*
 * A [[Namelist]] appears in parentheses and is used
 * only in the [[define]] form.
 * <tableparsing.c>=
 */
ParserResult sNamelist(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        switch (p->alt) {
        case ATOM:
            synerror(s->context.source,
                     "%p: usage: (define fun (formals) body)",
                     s->context.par);
        case LIST:
            halfshift(s);
            s->components[s->nparsed++].names =
                parsenamelist(p->list, &s->context);
            return PARSED;
        }
        assert(0);
    }
}
/*
 * These shift functions aren't used just to move
 * information from input to components. A sequence of
 * shift functions represents the components that are
 * expected to be part of a syntactic form. (This
 * technique of using functions as data is developed at
 * length in \chaprefscheme.) The syntactic form is
 * parsed by calling its functions in sequence. The end
 * of the sequence is marked by function [[stop]]. This
 * function checks to be sure all input is consumed and
 * signals that it is time to stop parsing. Unlike the
 * other shift functions, it does not change the
 * [[state]].
 * <tableparsing.c>=
 */
ParserResult stop(ParserState state) {
    if (state->input == NULL)
        return STOP_PARSING;
    else
        return INPUT_LEFTOVER;
}    
/*
 * Finally, I define a special shift function that
 * doesn't do any shifting. Instead, it sets the context
 * for parsing a function definition. In the list of
 * parsing functions for a function definition, the
 * [[sName]] that parses the function's name is followed
 * immediately by [[setcontextname]].
 * <tableparsing.c>=
 */
ParserResult setcontextname(ParserState s) {
    assert(s->nparsed > 0);
    s->context.name = s->components[s->nparsed-1].name;
    return PARSED;
}
/*
 * I define one more shift function, which is meant to
 * help you with \exrefimpcore.ex.localvars in \cref
 * impcore.chap. This exercise asks you to add local
 * variables to Impcore. A declaration of local
 * variables, if present, is parsed with shift function
 * [[sLocals]]. This function looks for the keyword
 * [[locals]]. If found, the keyword marks a list of the
 * names of local variables, and this list of names is
 * shifted into the [[s->components]] array. If the
 * keyword [[locals]] is not found, there are no local
 * variables, and a [[NULL]] pointer is shifted into the
 * [[s->components]] array.
 * <tableparsing.c>=
 */
ParserResult sLocals(ParserState s) {
    Par p = s->input ? s->input->hd : NULL;  // useful abbreviation
    if (/*
         * The keyword test is just complicated enough that it
         * warrants being put in a named code chunk.
         * <[[Par p]] represents a list beginning with keyword [[locals]]>=
         */
        p != NULL && p->alt == LIST && p->list != NULL &&
        p->list->hd->alt == ATOM && p->list->hd->atom == strtoname("locals")) {
        struct ParsingContext context;
        context.name = strtoname("locals");
        context.par = p;
        halfshift(s);
        s->components[s->nparsed++].names = parsenamelist(p->list->tl, &context)
                                                                               ;
        return PARSED;
    } else {        
        s->components[s->nparsed++].names = NULL;
        return PARSED;
    }
}
/*
 * To parse an input using a row, function [[rowparse]]
 * calls shift functions until a shift function says to
 * stop---or detects an error.
 * <tableparsing.c>=
 */
void rowparse(struct ParserRow *row, ParserState s) {
    ShiftFun *f = &row->shifts[0];

    for (;;) {
        ParserResult r = (*f)(s);
        switch (r) {
        case PARSED:          f++; break;
        case STOP_PARSING:    return;
        case INPUT_EXHAUSTED: 
        case INPUT_LEFTOVER:  
        case BAD_INPUT:       usage_error(row->code, r, &s->context);
        }
    }
}
/*
 * <tableparsing.c>=
 */
struct ParserRow *tableparse(ParserState s, ParserTable t) {
    if (s->input == NULL)
        synerror(s->context.source, "%p: unquoted empty parentheses",
                                    s->context.par);

    Name first = s->input->hd->alt == ATOM ? s->input->hd->atom : NULL;
                     // first Par in s->input, if it is present and an atom

    unsigned i;  // to become the index of the matching row in ParserTable t
    for (i = 0; !rowmatches(&t[i], first); i++) 
        ;
    /*
     * Once a row has matched, the parser state might have
     * to be adjusted. If row [[t[i]]] has a keyword, then
     * the first [[Par]] in the input is that keyword, and
     * that [[Par]] needs to be consumed—so function
     * [[tableparse]] adjusts [[s->input]] and sets the
     * context.
     * <adjust the state [[s]] so it's ready to start parsing using row [[t[i]]]
                                                                              >=
     */
    if (t[i].keyword) {
        assert(first != NULL);
        s->input = s->input->tl;
        s->context.name = first;
    }
    rowparse(&t[i], s);
    return &t[i];
}
/*
 * A row matches if the row's keyword is [[NULL]] or if
 * the keyword stands for the same name as [[first]].
 * <tableparsing.c>=
 */
static bool rowmatches(struct ParserRow *row, Name first) {
    return row->keyword == NULL || strtoname(row->keyword) == first;
}
/*
 * \qbreak The table [[exptable]] is used by parsing
 * function [[parseexp]]. Function [[parseexp]]
 * delegates the heavy lifting to other functions: given
 * an atom, it calls [[exp_of_atom]], and given a list,
 * it calls [[tableparse]] and [[reduce_to_exp]].
 * <tableparsing.c>=
 */
Exp parseexp(Par p, Sourceloc source) {
    switch (p->alt) {
    case ATOM:
        /*
         * Error detection and handling
         * 
         * My code handles four classes of errors: misuse of a
         * reserved word like [[if]] or [[while]], wrong number
         * of components, failure to deliver a name when a name
         * is expected, and a duplicate name where distinct
         * names are expected.
         * 
         * Misuse of reserved words is detected by the following
         * check, which prevents such oddities as a user-defined
         * function named [[if]]. A word is reserved if it
         * appears in [[exptable]] or [[xdeftable]].
         * <if [[p->atom]] is a reserved word, call [[synerror]] with [[source]]
                                                                              >=
         */
        for (struct ParserRow *entry = exptable; entry->keyword != NULL; entry++
                                                                               )
            if (p->atom == strtoname(entry->keyword))
                synerror(source, "%n is a reserved word and may not be used "
                         "to name a variable or function", p->atom);
        for (struct ParserRow *entry = xdeftable; entry->keyword != NULL; entry
                                                                             ++)
            if (p->atom == strtoname(entry->keyword))
                synerror(source, "%n is a reserved word and may not be used "
                         "to name a variable or function", p->atom);
        return exp_of_atom(source, p->atom);
    case LIST: 
        {   struct ParserState s = mkParserState(p, source);
            struct ParserRow *row = tableparse(&s, exptable);
            if (row->code == EXERCISE) {
                synerror(source, "implementation of %n is left as an exercise",
                         s.context.name);
            } else {
                Exp e = reduce_to_exp(row->code, s.components);
                check_exp_duplicates(source, e);
                return e;
            }
        }
    }
    assert(0);
}
/*
 * In later chapters, function [[parseexp]] is reused
 * with different versions of functions [[exp_of_atom]],
 * [[exptable]], and [[reduce_to_exp]].
 */

/*
 * \qbreak Impcore's other parsing table and function
 * handle extended definitions. The extended-definition
 * table is shared among several languages. Because it
 * is shared, I put it in tableparsing.c, not in
 * parse.c.
 * <tableparsing.c>=
 */
static ShiftFun valshifts[]      = { sName, sExp,            stop };
static ShiftFun defineshifts[]   = { sName, setcontextname, 
                                     sNamelist, sExp,        stop };
static ShiftFun useshifts[]      = { sName,                  stop };
static ShiftFun checkexpshifts[] = { sExp, sExp,             stop };
static ShiftFun checkassshifts[] = { sExp,                   stop };
static ShiftFun checkerrshifts[] = { sExp,                   stop };
static ShiftFun expshifts[]      = { use_exp_parser };

void extendDefine(void) { defineshifts[3] = sExps; }

struct ParserRow xdeftable[] = { 
    { "val",          ADEF(VAL),           valshifts },
    { "define",       ADEF(DEFINE),        defineshifts },
    { "use",          ANXDEF(USE),         useshifts },
    { "check-expect", ATEST(CHECK_EXPECT), checkexpshifts },
    { "check-assert", ATEST(CHECK_ASSERT), checkassshifts },
    { "check-error",  ATEST(CHECK_ERROR),  checkerrshifts },
    /*
     * The conditional sugar doesn't require any new
     * definition forms.
     * <rows added to [[xdeftable]] in exercises>=
     */
    /* add new forms for extended definitions here */
    /*
     * Extensions to micro-Scheme
     * 
     * [*] [*]
     * 
     * Syntactic sugar
     * 
     * The \xlet expression
     * 
     */

    { NULL,           ADEF(EXP),           expshifts }  // must come last
};
/*
 * Function [[parsexdef]] is quite similar to
 * [[parseexp]].
 * <tableparsing.c>=
 */
XDef parsexdef(Par p, Sourceloc source) {
    switch (p->alt) {
    case ATOM:
        return mkDef(mkExp(parseexp(p, source)));
    case LIST:;
        struct ParserState s  = mkParserState(p, source);
        struct ParserRow *row = tableparse(&s, xdeftable);
        XDef d = reduce_to_xdef(row->code, s.components);
        if (d->alt == DEF)
            check_def_duplicates(source, d->def);
        return d;
    }
    assert(0);
}
/*
 * The case for a top-level [[ADEF(EXP)]] node has just
 * one component, an [[Exp]], which is parsed using the
 * sequences of parsers [[expshifts]]. But by the time
 * [[tableparse]] gets to [[expshifts]], the input isn't
 * a [[Par]] any more—instead, it's the list of [[Par]]s
 * that appeared inside [[LIST]]. If [[expshifts]] tried
 * to use [[sExp]], it would fail to parse an expression
 * like \monobox(+ 1 2): function [[tableparse]] would
 * unpack the list of atoms inside the brackets,
 * [[sExp]] would try to parse [[+]] as an expression,
 * and the [[1]] and [[2]] would be left over.
 * 
 * \qbreak What's needed is to go back to the original
 * [[Par]] that was input, and to pass it to
 * [[parseexp]]. That's done by shift function
 * [[use_exp_parser]], which is the only ``component''
 * in [[expshifts]].
 * <tableparsing.c>=
 */
ParserResult use_exp_parser(ParserState s) {
    Exp e = parseexp(s->context.par, s->context.source);
    halfshift(s);
    s->components[s->nparsed++].exp = e;
    return STOP_PARSING;
}
/*
 * The next parsing function is [[parsename]]. This
 * function could accept only names, and it would work
 * on every syntactically correct program. But
 * programmers make mistakes, and when a program is not
 * syntactically correct, I'd like my interpreter to
 * issue a helpful error message. In this case I can do
 * it by allowing [[parsename]] to handle any expression
 * , and if the expression isn't a name, [[parsename]]
 * can issue an error message that is aware of the
 * context. The message is actually issued by function
 * [[name_error]], which is defined below.
 * <tableparsing.c>=
 */
Name parsename(Par p, ParsingContext context) {
    Exp e = parseexp(p, context->source);
    if (e->alt != VAR)
        return name_error(p, context);
    else
        return e->var;
}
/*
 * In addition to a parser for expressions and a parser
 * for names, Impcore needs a parser for a list of
 * expressions and a parser for a list of names. A list
 * of expressions is parsed recursively.
 * <tableparsing.c>=
 */
Explist parseexplist(Parlist input, Sourceloc source) {
    if (input == NULL) {
        return NULL;
    } else {
        Exp     e  = parseexp    (input->hd, source);
        Explist es = parseexplist(input->tl, source);
        return mkEL(e, es);
    }
}
/*
 * A list of names is also parsed recursively.
 * <tableparsing.c>=
 */
static Namelist parsenamelist(Parlist ps, ParsingContext context) {
    if (ps == NULL) {
        return NULL;
    } else {
        Exp e = parseexp(ps->hd, context->source);
        if (e->alt != VAR)
            synerror(context->source,
                     "in %p, formal parameters of %n must be names, "
                     "but %p is not a name",
                     context->par, context->name, ps->hd);
        return mkNL(e->var, parsenamelist(ps->tl, context));
    }
}
/*
 * Strictly speaking, if you add new syntax to a
 * language, you should extend not only the parsing
 * table and the reduce function, but also the
 * [[usage_table]]. If there is no usage string for a
 * given code, function [[usage_error]] can't say what
 * the expected usage is.
 * 
 * \qbreak Function [[usage_error]] searches
 * [[usage_table]], then spits out an error message.
 * <tableparsing.c>=
 */
void usage_error(int code, ParserResult why_bad, ParsingContext context) {
    for (struct Usage *u = usage_table; u->expected != NULL; u++)
        if (code == u->code) {
            const char *message;
            switch (why_bad) {
            case INPUT_EXHAUSTED:
                message = "too few components in %p; expected %s";
                break;
            case INPUT_LEFTOVER:
                message = "too many components in %p; expected %s";
                break;
            default:
                message = "badly formed input %p; expected %s";
                break;
            }
            synerror(context->source, message, context->par, u->expected);
        }
    synerror(context->source,
             "something went wrong parsing %p", context->par);
}
/*
 * Finally, if [[parsename]] sees something other than a
 * name, it calls [[name_error]]. The error message says
 * what went wrong and what the context is. The context
 * is identified by [[c->name]], and to make extending
 * [[name_error]] as easy as possible, I first convert
 * [[c->name]] to an integer code. The matching code can
 * be scrutinized using a [[switch]] statement.
 * <tableparsing.c>=
 */
void *name_error(Par bad, struct ParsingContext *c) {
    switch (code_of_name(c->name)) {
    case ADEF(VAL):
        synerror(c->source, "in %p, expected (val x e), but %p is not a name",
                 c->par, bad);
    case ADEF(DEFINE):
        synerror(c->source, "in %p, expected (define f (x ...) e), "
                            "but %p is not a name",
                 c->par, bad);
    case ANXDEF(USE):
        synerror(c->source, "in %p, expected (use filename), "
                            "but %p is not a filename",
                 c->par, bad);
    case SET:
        synerror(c->source, "in %p, expected (set x e), but %p is not a name",
                                                                                
                 c->par, bad);
    case APPLY:
        synerror(c->source, "in %p, expected (function-name ...), "
                            "but %p is not a name", 
                 c->par, bad);
    default:
        synerror(c->source, "in %p, expected a name, but %p is not a name", 
                 c->par, bad);
    }
}
/*
 * \qbreak The code is produced by function
 * [[code_of_name]], which does a reverse lookup in
 * [[exptable]] and [[xdeftable]].
 * <tableparsing.c>=
 */
int code_of_name(Name n) {
    struct ParserRow *entry;
    for (entry = exptable; entry->keyword != NULL; entry++)
        if (n == strtoname(entry->keyword))
            return entry->code;
    if (n == NULL)
        return entry->code;
    for (entry = xdeftable; entry->keyword != NULL; entry++)
        if (n == strtoname(entry->keyword))
            return entry->code;
    assert(0);
}
