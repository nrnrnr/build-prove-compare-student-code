#include "all.h"
/*
 * In Impcore, a tick mark is not read as [[(quote
 * ...)]], so [[read_tick_as_quote]] is false.
 * <impcore.c>=
 */
bool read_tick_as_quote = false;
/*
 * As noted in Exercise [->] (\crefimpcore.chap), this
 * code can leak open file descriptors.
 * 
 * Implementation of [[main]]
 * 
 * [*] The [[main]] function coordinates all the code
 * and forms a working interpreter. Before entering its
 * main loop, the interpreter initializes itself in
 * three phases:
 * 
 *   • It initializes [[print]] and [[fprint]] (\cref
 *  impcorea.printfuns).
 *   • It creates empty environments for functions and
 *  global variables, then populates the
 *  [[functions]] environment with functions from the
 *  initial basis.
 *   • Looking at command-line options, it sets
 *  [[prompts]] and [[echoes]], which determine
 *  whether the interpreter prints (respectively)
 *  a prompt and a result for each definition.
 * 
 * At this point the interpreter is ready to process
 * whatever input or inputs are designated in the
 * remainder of the command line, which are pointed to
 * by [[firstpath]]. [*] \iimplabelmain
 * <impcore.c>=
 */
int main(int argc, char *argv[]) {
    /*
     * \qbreak
     * 
     * Initialization of the extensible printer
     * 
     * [*] \crefpage(impcore.conversion-specifiers lists all
     * the types of values that [[print]], [[fprint]],
     * [[runerror]], and [[synerror]] know how to print.
     * Each of the conversion specifiers mentioned in that
     * table has to be installed. They are installed here:
     * <install conversion specifications for [[print]] and [[fprint]]>=
     */
    installprinter('c', printchar);
    installprinter('d', printdecimal);
    installprinter('e', printexp);
    installprinter('E', printexplist);
    installprinter('f', printfun);
    installprinter('n', printname);
    installprinter('N', printnamelist);
    installprinter('p', printpar);
    installprinter('P', printparlist);
    installprinter('s', printstring);
    installprinter('t', printdef);
    installprinter('v', printvalue);
    installprinter('V', printvaluelist);
    installprinter('%', printpercent);

    Valenv globals   = mkValenv(NULL, NULL);
    Funenv functions = mkFunenv(NULL, NULL);
    /*
     * The initial basis includes both primitives and
     * user-defined functions. The primitives are installed
     * first.
     * <install the initial basis in [[functions]]>=
     */
    {
        static const char *prims[] = 
           { "+", "-", "*", "/", "<", ">", "=", "println", "print", "printu", 0
                                                                              };
        for (const char **p = prims; *p; p++) {
            Name x = strtoname(*p);
            bindfun(x, mkPrimitive(x), functions);
        }
    }
    /*
     * The string is interpreted by [[readevalprint]]. [*]
     * <install the initial basis in [[functions]]>=
     */
    {
        const char *fundefs = 
           
             ";;\n"
             ";;   Primitive, predefined, and basis; the initial basis\n"
             ";;   \n"
             ";;   [*] Programmers like big languages with lots of data\n"
             ";;   types and syntactic forms, but implementors want to\n"
             ";;   keep primitive functionality small and simple. (So do\n"
             ";;   semanticists!) To reconcile these competing desires,\n"
             ";;   language designers have found two strategies:\n"
             ";;   translation into a core language and definition of an\n"
             ";;   initial basis.\n"
             ";;   \n"
             ";;   Using a core language, you stratify your language\n"
             ";;   into two layers. The inner layer defines or\n"
             ";;   implements its constructs directly; it constitutes\n"
             ";;   the core language. The outer layer defines additional\n"
             ";;   constructs by translating them into the core\n"
             ";;   language; these constructs constitute syntactic sugar\n"
             ";;   . In this chapter, the core language is Impcore, and\n"
             ";;   as an example of syntactic sugar, a [[for]]\n"
             ";;   expression can be defined by a translation into\n"
             ";;   [[begin]] and [[while]] (\\crefimpcore.sugar).\n"
             ";;   \n"
             ";;   To be useful to programmers, a language needs to be\n"
             ";;   accompanied by a standard library. In the theory\n"
             ";;   world, a library contributes to a basis; basis is the\n"
             ";;   collective term for all the things that can be named\n"
             ";;   in definitions. In Impcore, these things are\n"
             ";;   functions and global variables. A language's initial\n"
             ";;   basis contains the named things that are available in\n"
             ";;   a fresh interpreter or installation—the things you\n"
             ";;   have access to even before evaluating your own code.\n"
             ";;   \n"
             ";;   Like the Impcore language, Impcore's initial basis is\n"
             ";;   stratified into two layers, one of which is defined\n"
             ";;   in terms of the other. The inner layer includes all\n"
             ";;   the functions that are defined directly by C code in\n"
             ";;   the interpreter; these are called primitive.\n"
             ";;   The outer layer includes functions that are also\n"
             ";;   built into the interpreter, but are defined in terms\n"
             ";;   of the primitives using Impcore source code; they are\n"
             ";;   user-defined functions and are called predefined.\n"
             ";;   \n"
             ";;   Stratifying the initial basis makes life easy for\n"
             ";;   everyone. Implementors make their own lives easy by\n"
             ";;   defining just a few primitives, and they can make\n"
             ";;   programmers' lives easy by defining lots of\n"
             ";;   predefined functions. Predefined functions are just\n"
             ";;   ordinary code, and writing them is lots easier than\n"
             ";;   defining new primitives. Impcore's predefined\n"
             ";;   functions are defined by the code in Figure [->].\n"

";;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
             ";;   \\advance\\linewidthby -2 \\advance\\nwdefspaceby 2\n"

";;   ┌───────────────────────────────────────────────────┐\n"
             ";;   │Boolean connectives are defined using [[if]]       │\n"
             ";;   │expressions. [*]                                   │\n"

";;   └───────────────────────────────────────────────────┘\n"
             ";;   <predefined Impcore functions>=\n"
             ";;\n"
             "(define and (b c) (if b c b))\n"
             "(define or  (b c) (if b b c))\n"
             "(define not (b)   (if b 0 1))\n"
             ";;\n"
             ";;   Unlike the similar constructs built into the syntax\n"
             ";;   of many languages, these versions of [[and]] and\n"
             ";;   [[or]] always evaluate both of their arguments.\n"
             ";;   Section [->] shows how you can use syntactic sugar to\n"
             ";;   define short-circuit variations that evaluate a\n"
             ";;   second expression only when necessary.\n"
             ";;\n"
             "\n"
             ";;\n"
             ";;   Only comparisons [[<]], [[=]], and [[>]] are\n"
             ";;   primitive; the others are predefined.\n"
             ";;   <predefined Impcore functions>=\n"
             ";;\n"
             "(define <= (x y) (not (> x y)))\n"
             "(define >= (x y) (not (< x y)))\n"
             "(define != (x y) (not (= x y)))\n"
             ";;\n"
             ";;   Primitive arithmetic includes [[+]], [[-]], [[*]],\n"
             ";;   and [[/]], to which the predefined functions add\n"
             ";;   modulus and negation.\n"
             ";;   <predefined Impcore functions>=\n"
             ";;\n"
             "(define mod (m n) (- m (* n (/ m n))))\n"
             "(define negated (n) (- 0 n))\n";
        if (setjmp(errorjmp))
            assert(0); // if error in predefined function, die horribly
        readevalprint(stringxdefs("predefined functions", fundefs),
                      globals, functions, NOT_ECHOING);
    }

    Prompts prompts  = PROMPTING;     // default behaviors
    Echo    echoes   = ECHOING;

    char **firstpath; // pointer to first first pathname in argv (or to NULL)
    /*
     * Code to process options has been a thorn in better
     * sides than mine. The only part of this logic worth
     * discussing is what happens after the last option has
     * been seen:
     * 
     *   • A nonempty argument list means to treat every
     *  argument as a file name, and to evaluate the
     *  extended definitions found in the named file.
     *   • An empty argument list means to evaluate standard
     *  input.
     * 
     * The empty argument list is implemented by pointing to
     * a statically allocated list containing only the name
     * [["-"]], which designates standard input.
     * 
     * \qbreak First the options are scanned in a loop, and
     * then [[firstpath]] is set.
     * <process options, leaving [[firstpath]] pointing to the name of the first
                                                                    input file>=
     */
    (void) argc; // not used
    extern void dump_fenv_names(Funenv);
    {   char **nextarg = argv+1;
        bool dumped = false;
        while (*nextarg && **nextarg == '-' && (*nextarg)[1] != '\0') {
            /*
             * The options themselves are handled by a big
             * conditional.
             * <handle option [[*nextarg]]>=
             */
            if (!strcmp(*nextarg, "-q")) {
                prompts = NOT_PROMPTING;
            } else if (!strcmp(*nextarg, "-qq")) { 
                prompts = NOT_PROMPTING;
                echoes  = NOT_ECHOING;
            } else if (!strcmp(*nextarg, "-names")) { 
                dump_fenv_names(functions);
                dumped = true;
                if (nextarg[1]) {
                    fprintf(stderr, "Dump options must not take any files\n");
                    exit(1);
                }
            } else {
                fprintf(stderr, "Usage: impcore [-q|-qq] [pathname ...]\n");
                fprintf(stderr, "       impcore -names\n");
                exit(strcmp(*nextarg, "-help") ? 1 : 0);
            }
            nextarg++;
        }
        static char *default_paths[] = { "-", NULL };
        static char *no_paths[]      = { NULL };
        firstpath = *nextarg ? nextarg : dumped ? no_paths : default_paths;
    }  
    set_toplevel_error_format(prompts == PROMPTING
                                ? WITHOUT_LOCATIONS
                                : WITH_LOCATIONS);
    if (getenv("NOERRORLOC")) set_toplevel_error_format(WITHOUT_LOCATIONS);
                                                            /*testing*/ /*OMIT*/

    for ( ; *firstpath; firstpath++) {
        /*
         * A file is evaluated by setting [[fin]] to the file
         * designated by [[*firstpath]], and by setting
         * [[filename]] to its name. The interpreter then calls
         * [[filexdefs]] to turn [[fin]] into a stream of
         * extended definitions. As a useful pun, when the path
         * name is [["-"]], it designates standard input, not
         * the file named - in the current directory. (Such a
         * file can be designated by the path ./-.)
         * <evaluate definitions in file designated by [[*firstpath]]>=
         */
        FILE *fin = !strcmp(*firstpath, "-") ? stdin : fopen(*firstpath, "r");
        /*
         * Error handling is necessary but uninteresting.
         * <if [[fopen]] failed, roll over and die>=
         */
        if (fin == NULL) {
            fprintf(stderr, "%s: cannot open file \"%s\"", argv[0], *firstpath);
            exit(1);
        }
        const char *filename = fin == stdin ? "standard input" : *firstpath;
        XDefstream xdefs = filexdefs(filename, fin, prompts);
        /*
         * The main loop is in the [[readevalprint]] function,
         * the call to which is preceded by a C idiom: [*]
         * <evaluate all the extended definitions in [[xdefs]]>=
         */
        while (setjmp(errorjmp))
            /* error recovery, if needed, would appear here */;
        readevalprint(xdefs, globals, functions, echoes);
        if (fin != stdin)
            fclose(fin);
    }
    return 0;
}
