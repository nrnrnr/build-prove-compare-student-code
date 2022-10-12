#include "all.h"
/* impcore.c S169b */
bool read_tick_as_quote = false;
/* impcore.c S297a */
int main(int argc, char *argv[]) {
    /* install conversion specifications for [[print]] and [[fprint]] S304c */
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
    /* install the initial basis in [[functions]] S297b */
    {
        static const char *prims[] = 
           { "+", "-", "*", "/", "<", ">", "=", "println", "print", "printu", 0
                                                                              };
        for (const char **p = prims; *p; p++) {
            Name x = strtoname(*p);
            bindfun(x, mkPrimitive(x), functions);
        }
    }
    /* install the initial basis in [[functions]] S297d */
    {
        const char *fundefs = 
           
             ";  predefined Impcore functions 27a \n"
             "(define and (b c) (if b c b))\n"
             "(define or  (b c) (if b b c))\n"
             "(define not (b)   (if b 0 1))\n"
             ";  predefined Impcore functions 27b \n"
             "(define <= (x y) (not (> x y)))\n"
             "(define >= (x y) (not (< x y)))\n"
             "(define != (x y) (not (= x y)))\n"
             ";  predefined Impcore functions 27c \n"
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

/* process options, leaving [[firstpath]] pointing to the name of the first input file S299a */
    (void) argc; // not used
    extern void dump_fenv_names(Funenv);
    {   char **nextarg = argv+1;
        bool dumped = false;
        while (*nextarg && **nextarg == '-' && (*nextarg)[1] != '\0') {
            /* handle option [[*nextarg]] S299b */
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
        /* evaluate definitions in file designated by [[*firstpath]] S298a */
        FILE *fin = !strcmp(*firstpath, "-") ? stdin : fopen(*firstpath, "r");
        /* if [[fopen]] failed, roll over and die S298b */
        if (fin == NULL) {
            fprintf(stderr, "%s: cannot open file \"%s\"", argv[0], *firstpath);
            exit(1);
        }
        const char *filename = fin == stdin ? "standard input" : *firstpath;
        XDefstream xdefs = filexdefs(filename, fin, prompts);
        /* evaluate all the extended definitions in [[xdefs]] S298c */
        while (setjmp(errorjmp))
            /* error recovery, if needed, would appear here */;
        readevalprint(xdefs, globals, functions, echoes);
        if (fin != stdin)
            fclose(fin);
    }
    return 0;
}
