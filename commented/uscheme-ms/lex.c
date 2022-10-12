#include "all.h"
/*
 * Implementation of Parstream
 * 
 * The representation of a [[Parstream]] has three
 * parts:
 * 
 *   • The [[lines]] field is a source of input lines.
 *   • The [[input]] field contains characters from an
 *  input line; if a [[Par]] has already been read
 *  from that line, [[input]] contains only the
 *  characters left over.
 *   • The [[prompts]] structure contains strings that
 *  are printed every time a line is taken from
 *  [[lines]]. When the [[Parstream]] is reading a
 *  fresh [[Par]], it issues [[prompts.ps1]] for the
 *  first line of that [[Par]]. When it has to read a
 *  [[Par]] that spans more than one line, like a
 *  long function definition, it issues
 *  [[prompts.ps2]] for all the rest of the lines.
 *  The names [[ps1]] and [[ps2]] stand for ``prompt
 *  string'' 1 and 2; they come from the Unix shell.
 * 
 * \implabelParstream
 * <lex.c>=
 */
struct Parstream {
    Linestream lines;     /* source of more lines */
    const char *input;
                       /* what's not yet read from the most recent input line */
    /* invariant: unread is NULL only if lines is empty */

    struct {
       const char *ps1, *ps2;
    } prompts;
};
/*
 * <lex.c>=
 */
Parstream parstream(Linestream lines, Prompts prompts) {
    Parstream pars = malloc(sizeof(*pars));
    assert(pars);
    pars->lines = lines;
    pars->input = "";
    pars->prompts.ps1 = prompts == PROMPTING ? "-> " : "";
    pars->prompts.ps2 = prompts == PROMPTING ? "   " : "";
    return pars;
}
/*
 * Function [[parsource]] grabs the current source
 * location out of the [[Linestream]].
 * <lex.c>=
 */
Sourceloc parsource(Parstream pars) {
    return &pars->lines->source;
}
/*
 * Function [[getpar]] presents a minor problem: the
 * [[Par]] type is defined recursively, so [[getpar]]
 * itself must be recursive. But the first call to
 * [[getpar]] is distinct from the others in two ways:
 * 
 *   • If the first call prompts, it should use
 *  [[prompts.ps1]]. Other calls should use
 *  [[prompts.ps2]]
 *   • If the first call encounters a right parenthesis,
 *  then the right parenthesis is unbalanced, and
 *  [[getpar]] should report it as a syntax error.
 *  If another call encounters a right parenthesis,
 *  then the right parenthesis marks the end of a
 *  [[LIST]], and [[getpar]] should scan past it and
 *  return.
 * 
 * This distinction is managed by function
 * [[getpar_in_context]], which knows whether it is the
 * first call or another call. This function attempts to
 * read a [[Par]]. If it runs out of input, it returns
 * [[NULL]]. If it sees a right parenthesis, it returns
 * [[NULL]] if and only if [[is_first]] is false;
 * otherwise, it calls [[synerror]]. \implabelgetpar [*]
 * <lex.c>=
 */
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static Name readatom(const char **ps);
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static Parlist reverse_parlist(Parlist p);
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static int  isdelim(char c);
static Name strntoname(const char *s, int n);
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static bool brackets_match(char left, char right);
static Par getpar_in_context(Parstream pars, bool is_first, char left) {
    if (pars->input == NULL)
        return NULL;
    else {
        char right;      // will hold right bracket, if any
        /*
         * \qbreak To scan past whitespace,
         * [[getpar_in_context]] uses the standard C library
         * function [[isspace]]. That function requires an
         * unsigned character.
         * <advance [[pars->input]] past whitespace characters>=
         */
        while (isspace((unsigned char)*pars->input))
            pars->input++;
        switch (*pars->input) {
        case '\0':  /* on end of line, get another line and continue */
        case ';':
            pars->input = getline_(pars->lines,
                                   is_first ? pars->prompts.ps1 : pars->
                                                                   prompts.ps2);
            return getpar_in_context(pars, is_first, left);
        case '(': case '[': 
            /*
             * Inorder and postorder traversals are left for you to
             * implement (\crefscheme.ex.traversals).
             * 
             * Combining theory and practice: Algebraic laws
             * 
             * [*] Throughout this chapter, functions are described
             * by algebraic laws. The laws help us understand what
             * the code does (or what it is supposed to do).
             * Algebraic laws also help with design: they can show
             * what cases need to be handled and what needs to be
             * done in each case. Translating such laws into code is
             * much easier than writing correct code from scratch.
             * 
             * Algebraic laws have many other uses. Any algebraic
             * law can be turned into a test case by substituting
             * example data for metavariables; for example,
             * QuickCheck \citephughes:quick-check:2000
             * automatically substitutes a random input for each
             * metavariable. Algebraic laws are also used to specify
             * the behavior of abstract types, to simplify code,
             * to improve performance, and even to prove properties
             * of code.
             * 
             * Algebraic laws work by specifying equalities: in a
             * valid law, whenever values are substituted for
             * metavariables, the two sides are equal. (The values
             * substituted must respect the conditions surrounding
             * the law. For example, if \ns stands for a list of
             * numbers, we may not substitute a Boolean for it.)
             * The substitution principle extends beyond values;
             * a valid law also holds when program variables are
             * substituted for metavariables, and even when pure
             * expressions are substituted for metavariables. A pure
             * expression is one whose evaluation has no side
             * effects: it does not change the values of any
             * variables and does not do any input or output. And
             * for our purposes, a pure expression runs to
             * successful completion; if an expression's evaluation
             * doesn't terminate or triggers a run-time error,
             * the expression is considered impure.
             * 
             * The equality specified by an algebraic law is a form
             * of observational equivalence: if \nomathbreake_1=e_2,
             * and a program contains e_1, we can replace e_1 with 
             * e_2, and the program won't be able to tell the
             * difference. That is, running the altered program will
             * have the same observable effect as the original.
             * Replacing e_1 with e_2 may, however, change
             * properties that can't be observed by the program
             * itself, such as the time required for completion or
             * the number of locations allocated. When an algebraic
             * law is used to improve performance, changing such
             * properties is the whole point.
             * 
             * In the sections above, algebraic laws are used only
             * to show how to implement functions. In this section,
             * they are also used to specify properties of functions
             * and of combinations of functions, and to prove such
             * properties.
             * 
             * Laws of list primitives
             * 
             * [*] [*]
             * 
             * Algebraic laws can be used to specify the behaviors
             * of primitive functions. After all, programmers never
             * need to see implementations of [[cons]], [[car]],
             * [[cdr]], and [[null?]]; we just need to know how they
             * behave. Their behavior can be specified by
             * operational semantics, but operational semantics
             * often gives more detail than we care to know.
             * For example, if we just want to be able to use
             * [[car]] and [[cdr]] effectively, everything we need
             * to know is captured by these two laws: {laws} \eqlaw\
             * monobox(car (cons \metax \metay))\metax \eqlaw\
             * monobox(cdr (cons \metax \metay))\metay {laws} These
             * laws also tell us something about [[cons]]:
             * implicitly, the laws confirm that [[cons]] may be
             * applied to any two arguments x and y, even if y is
             * not a list (rule [->], \cpagerefscheme.fig.values).
             * Use of cons cells and [['()]] to represent lists is
             * merely a programming convention.
             * 
             * To capture [[cons]] completely also requires laws
             * that tell us how a cons cell is viewed by a type
             * predicate, as in these examples: {laws} \eqlaw\
             * monobox(pair? (cons \metax \metay))[[#t]] \eqlaw\
             * monobox(null? (cons \metax \metay))[[#f]] {laws}
             * 
             * The laws above suffice to enable us to use [[cons]],
             * [[car]], and [[cdr]] effectively. Nothing more is
             * required, and any implementation that satisfies the
             * laws is as correct as any other. To develop an
             * unusual implementation, try \cref
             * scheme.ex.lambda-lists.
             * 
             * Developing laws\protect\cullchap{ by classifying
             * operations}
             * 
             * [*] How many laws are enough? To know if we have
             * enough laws to describe a data type T, we analyze the
             * functions that involve values of type T.
             * 
             *   • A function that makes a new value of type T is a
             *  creator or a producer. A creator is either a
             *  value of type T all by itself, or it is a
             *  function that returns a value of type T without
             *  needing any arguments of type T. As an example,
             *  [['()]] is a creator for lists. A producer is a
             *  function that takes at least one argument of
             *  type T, and possibly additional arguments, and
             *  returns a value of type T. As an example,
             *  [[cons]] is a producer for lists.
             * 
             *  Creators and producers are sometimes grouped into
             *  a single category called constructors, but
             *  ``constructor'' is a slippery word. The grouping
             *  usage comes from algebraic specification, but
             *  ``constructor'' is also used in functional
             *  programming and in object-oriented
             *  programming—and in each community, it means
             *  something different.
             * 
             *   • A function that takes an argument of type T and
             *  gets information out of it is an observer.
             *  An observer ``looks inside'' its argument and
             *  produces some fact about the argument, or perhaps
             *  some constituent value, which may or may not also
             *  be of type T. Observers are sometimes also called
             *  selectors or accessors. As examples, primitives
             *  [[car]], [[cdr]], [[pair?]], and [[null?]] are
             *  observers for lists.
             * 
             *   • Creators, producers, and observers have no side
             *  effects. A function that has side effects on an
             *  existing value of type T is a mutator. Mutators,
             *  too, can fit into the discipline of algebraic
             *  specification. An explanation in depth is beyond
             *  the scope of this book, but a couple of simple
             *  examples appear in \crefpage
             *  (mcl.mutable-ops-laws. For more, see the
             *  excellent book by [cite liskov:abstraction].
             * 
             * This classification of functions tells us how many
             * laws are enough: there are enough laws for type T if
             * the laws specify the result of every permissible
             * combination of observers applied to creators and
             * producers. By this criterion, our list laws aren't
             * yet complete; they don't specify what happens when
             * observers are applied to the empty list. Such
             * observations are specified by these laws:[*] {laws} \
             * eqlaw\monobox(pair? '())[[#f]] \eqlaw\monobox(null? '
             * ())[[#t]] {laws} Not all observations of the empty
             * list are permissible. An observation would cause an
             * error, like \monobox(car '()), isn't specified by any
             * law, and so it is understood that the observation is
             * impermissible. This convention resembles the
             * convention of the operational semantics, where if an
             * evaluation causes an error, no rule applies.
             * 
             * Laws for rich data structures can be extensive.
             * Because S-expressions include both lists and atoms,
             * they have lots of creators, producers, and especially
             * observers. Laws for all combinations would be
             * overwhelming; only a few Boolean laws are sketched
             * below.
             * <read and return a parenthesized [[LIST]]>=
             */
            {
                char left = *pars->input++;
                                         /* remember the opening left bracket */

                Parlist elems_reversed = NULL;
                Par q;
                   /* next par read in, to be accumulated into elems_reversed */
                while ((q = getpar_in_context(pars, false, left)))
                    elems_reversed = mkPL(q, elems_reversed);

                if (pars->input == NULL)
                    synerror(parsource(pars),

              "premature end of file reading list (missing right parenthesis)");
                else
                    return mkList(reverse_parlist(elems_reversed));
            }
        case ')': case ']': case '}':
            right = *pars->input++;
                                 /* pass the bracket so we don't see it again */
            if (is_first) {
                synerror(parsource(pars), "unexpected right bracket %c", right);
            } else if (left == '\'') {
                synerror(parsource(pars), "quote ' followed by right bracket %c"
                                                                               ,
                         right);
            } else if (!brackets_match(left, right)) {
                synerror(parsource(pars), "%c does not match %c", right, left);
            } else {
                return NULL;
            }
        case '{':
            pars->input++;
            synerror(parsource(pars), "curly brackets are not supported");
        default:
            if (read_tick_as_quote && *pars->input == '\'') {
                /*
                 * <read a [[Par]] and return that [[Par]] wrapped in [[quote]]
                                                                              >=
                 */
                {
                    pars->input++;
                    Par p = getpar_in_context(pars, false, '\'');
                    if (p == NULL)
                        synerror(parsource(pars),
                                      "premature end of file after quote mark");
                    assert(p);
                    return mkList(mkPL(mkAtom(strtoname("quote")), mkPL(p, NULL)
                                                                             ));
                }
            } else {
                /*
                 * Atoms are delegated to function [[readatom]], defined
                 * below.
                 * <read and return an [[ATOM]]>=
                 */
                return mkAtom(readatom(&pars->input));
            }
        }   
    }
}
/*
 * With this code in hand, [[getpar]] is a first call.
 */

/*
 * <lex.c>=
 */
Par getpar(Parstream pars) {
    assert(pars);
    return getpar_in_context(pars, true, '\0');
}
/*
 * <lex.c>=
 */
static Parlist reverse_parlist(Parlist p) {
    Parlist reversed = NULL;
    Parlist remaining = p;
    /* Invariant: reversed followed by reverse(remaining) equals reverse(p) */
    while (remaining) {
        Parlist next = remaining->tl;
        remaining->tl = reversed;
        reversed = remaining;
        remaining = next;
    }
    return reversed;
}                      
/*
 * Reading and returning an atom
 * 
 * A lexical analyzer consumes input one character at a
 * time. My code works with a pointer to the input
 * characters. A typical function uses such a pointer to
 * look at the input, converts some of the input to a
 * result, and updates the pointer to point to the
 * remaining, unconsumed input. To make the update
 * possible, I must pass a pointer to the pointer, which
 * has type [[char **]]. [In C++, I would instead pass
 * the pointer by reference.] Here, for example,
 * [[readatom]] consumes the characters that form a
 * single atom.
 * <lex.c>=
 */
static Name readatom(const char **ps) {
    const char *p, *q;

    p = *ps;                          /* remember starting position */
    for (q = p; !isdelim(*q); q++)    /* scan to next delimiter */
        ;
    *ps = q;
                                    /* unconsumed input starts with delimiter */
    return strntoname(p, q - p);      /* the name is the difference */
}
/*
 * A delimiter is a character that marks the end of a
 * name or a token. In bridge languages, delimiters
 * include parentheses, semicolon, whitespace, and end
 * of string.
 * <lex.c>=
 */
static int isdelim(char c) {
    return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
                                                                              ||
           c == ';' || isspace((unsigned char)c) || 
           c == '\0';
}
/*
 * Function [[strntoname]] returns a name built from the
 * first [[n]] characters of a string.
 * <lex.c>=
 */
static Name strntoname(const char *s, int n) {
    char *t = malloc(n + 1);
    assert(t != NULL);
    strncpy(t, s, n);
    t[n] = '\0';
    return strtoname(t);
}
/*
 * <lex.c>=
 */
static bool brackets_match(char left, char right) {
    switch (left) {
        case '(': return right == ')';
        case '[': return right == ']';
        case '{': return right == '}';
        default: assert(0);
    }
}
