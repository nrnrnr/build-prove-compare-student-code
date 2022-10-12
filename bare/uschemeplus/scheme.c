#include "all.h"
/* scheme.c S315b */
int main(int argc, char *argv[]) {
    /* install conversion specifications for [[print]] and [[fprint]] S315c */
    installprinter('c', printchar);
    installprinter('d', printdecimal);
    installprinter('e', printexp);
    installprinter('E', printexplist);
    installprinter('\\', printlambda);
    installprinter('n', printname);
    installprinter('N', printnamelist);
    installprinter('p', printpar);
    installprinter('P', printparlist);
    installprinter('r', printenv);
    /* install conversion specifications for [[print]] and [[fprint]] S316a */
    installprinter('s', printstring);
    installprinter('t', printdef);
    installprinter('v', printvalue);
    installprinter('V', printvaluelist);
    installprinter('%', printpercent);
    installprinter('*', printpointer);

    initvalue();    // initalizes truev and falsev
    extendSyntax(); // adds new syntax for extended interpreters

    Env env = NULL;
    initallocate(&env);
    /* install primitive functions into [[env]] S312d */
    #define xx(NAME, TAG, FUNCTION) \
        env = bindalloc(strtoname(NAME), mkPrimitive(TAG, FUNCTION), env);
    #include "prim.h"
    #undef xx
    Env primenv = env; // capture for later dump
    /* install predefined functions into [[env]] S316b */
    const char *fundefs = 
        
          ";  predefined uScheme functions 96a \n"
          "(define caar (xs) (car (car xs)))\n"
          "(define cadr (xs) (car (cdr xs)))\n"
          "(define cdar (xs) (cdr (car xs)))\n"

";  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) \n"
          ";  more predefined combinations of [[car]] and [[cdr]] S319e \n"
          "(define cddr  (sx) (cdr (cdr  sx)))\n"
          "(define caaar (sx) (car (caar sx)))\n"
          "(define caadr (sx) (car (cadr sx)))\n"
          "(define cadar (sx) (car (cdar sx)))\n"
          "(define caddr (sx) (car (cddr sx)))\n"
          "(define cdaar (sx) (cdr (caar sx)))\n"
          "(define cdadr (sx) (cdr (cadr sx)))\n"
          "(define cddar (sx) (cdr (cdar sx)))\n"
          "(define cdddr (sx) (cdr (cddr sx)))\n"
          ";  more predefined combinations of [[car]] and [[cdr]] S319f \n"
          "(define caaaar (sx) (car (caaar sx)))\n"
          "(define caaadr (sx) (car (caadr sx)))\n"
          "(define caadar (sx) (car (cadar sx)))\n"
          "(define caaddr (sx) (car (caddr sx)))\n"
          "(define cadaar (sx) (car (cdaar sx)))\n"
          "(define cadadr (sx) (car (cdadr sx)))\n"
          "(define caddar (sx) (car (cddar sx)))\n"
          "(define cadddr (sx) (car (cdddr sx)))\n"
          ";  more predefined combinations of [[car]] and [[cdr]] S320a \n"
          "(define cdaaar (sx) (cdr (caaar sx)))\n"
          "(define cdaadr (sx) (cdr (caadr sx)))\n"
          "(define cdadar (sx) (cdr (cadar sx)))\n"
          "(define cdaddr (sx) (cdr (caddr sx)))\n"
          "(define cddaar (sx) (cdr (cdaar sx)))\n"
          "(define cddadr (sx) (cdr (cdadr sx)))\n"
          "(define cdddar (sx) (cdr (cddar sx)))\n"
          "(define cddddr (sx) (cdr (cdddr sx)))\n"
          ";  predefined uScheme functions 96b \n"
          "(define list1 (x)     (cons x '()))\n"
          "(define list2 (x y)   (cons x (list1 y)))\n"
          "(define list3 (x y z) (cons x (list2 y z)))\n"
          ";  predefined uScheme functions 99b \n"
          "(define append (xs ys)\n"
          "  (if (null? xs)\n"
          "     ys\n"
          "     (cons (car xs) (append (cdr xs) ys))))\n"
          ";  predefined uScheme functions 100b \n"
          "(define revapp (xs ys) ; (reverse xs) followed by ys\n"
          "  (if (null? xs)\n"
          "     ys\n"
          "     (revapp (cdr xs) (cons (car xs) ys))))\n"
          ";  predefined uScheme functions 100c \n"
          "(define reverse (xs) (revapp xs '()))\n"

";  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) \n"

";  definitions of predefined uScheme functions [[and]], [[or]], and [[not]] 96c \n"
          "(define and (b c) (if b  c  b))\n"
          "(define or  (b c) (if b  b  c))\n"
          "(define not (b)   (if b #f #t))\n"
          ";  predefined uScheme functions 103a \n"
          "(define atom? (x)\n"
          "  (or (symbol? x) (or (number? x) (or (boolean? x) (null? x)))))\n"
          ";  predefined uScheme functions 104a \n"
          "(define equal? (sx1 sx2)\n"
          "  (if (atom? sx1)\n"
          "    (= sx1 sx2)\n"
          "    (if (atom? sx2)\n"
          "        #f\n"
          "        (and (equal? (car sx1) (car sx2))\n"
          "             (equal? (cdr sx1) (cdr sx2))))))\n"
          ";  predefined uScheme functions 106a \n"
          "(define make-alist-pair      (k a)   (list2 k a))\n"
          "(define alist-pair-key       (pair)  (car  pair))\n"
          "(define alist-pair-attribute (pair)  (cadr pair))\n"
          ";  predefined uScheme functions 106b \n"

   "(define alist-first-key       (alist) (alist-pair-key       (car alist)))\n"

   "(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))\n"
          ";  predefined uScheme functions 106c \n"
          "(define bind (k a alist)\n"
          "  (if (null? alist)\n"
          "    (list1 (make-alist-pair k a))\n"
          "    (if (equal? k (alist-first-key alist))\n"
          "      (cons (make-alist-pair k a) (cdr alist))\n"
          "      (cons (car alist) (bind k a (cdr alist))))))\n"
          "(define find (k alist)\n"
          "  (if (null? alist)\n"
          "    '()\n"
          "    (if (equal? k (alist-first-key alist))\n"
          "      (alist-first-attribute alist)\n"
          "      (find k (cdr alist)))))\n"
          ";  predefined uScheme functions 125a \n"

  "(define o (f g) (lambda (x) (f (g x))))          ; ((o f g) x) = (f (g x))\n"
          ";  predefined uScheme functions 126c \n"
          "(define curry   (f) (lambda (x) (lambda (y) (f x y))))\n"
          "(define uncurry (f) (lambda (x y) ((f x) y)))\n"
          ";  predefined uScheme functions 129 \n"
          "(define filter (p? xs)\n"
          "  (if (null? xs)\n"
          "    '()\n"
          "    (if (p? (car xs))\n"
          "      (cons (car xs) (filter p? (cdr xs)))\n"
          "      (filter p? (cdr xs)))))\n"
          ";  predefined uScheme functions 130a \n"
          "(define map (f xs)\n"
          "  (if (null? xs)\n"
          "    '()\n"
          "    (cons (f (car xs)) (map f (cdr xs)))))\n"
          ";  predefined uScheme functions 130b \n"
          "(define app (f xs)\n"
          "  (if (null? xs)\n"
          "    #f\n"
          "    (begin (f (car xs)) (app f (cdr xs)))))\n"
          ";  predefined uScheme functions 130c \n"
          "(define exists? (p? xs)\n"
          "  (if (null? xs)\n"
          "    #f\n"
          "    (if (p? (car xs)) \n"
          "      #t\n"
          "      (exists? p? (cdr xs)))))\n"
          "(define all? (p? xs)\n"
          "  (if (null? xs)\n"
          "    #t\n"
          "    (if (p? (car xs))\n"
          "      (all? p? (cdr xs))\n"
          "      #f)))\n"
          ";  predefined uScheme functions 131b \n"
          "(define foldr (combine zero xs)\n"
          "  (if (null? xs)\n"
          "    zero\n"
          "    (combine (car xs) (foldr combine zero (cdr xs)))))\n"
          "(define foldl (combine zero xs)\n"
          "  (if (null? xs)\n"
          "    zero\n"
          "    (foldl combine (combine (car xs) zero) (cdr xs))))\n"
          ";  predefined uScheme functions S319a \n"
          "(val newline      10)   (val left-round    40)\n"
          "(val space        32)   (val right-round   41)\n"
          "(val semicolon    59)   (val left-curly   123)\n"
          "(val quotemark    39)   (val right-curly  125)\n"
          "                        (val left-square   91)\n"
          "                        (val right-square  93)\n"
          ";  predefined uScheme functions S319b \n"
          "(define <= (x y) (not (> x y)))\n"
          "(define >= (x y) (not (< x y)))\n"
          "(define != (x y) (not (= x y)))\n"
          ";  predefined uScheme functions S319c \n"
          "(define max (x y) (if (> x y) x y))\n"
          "(define min (x y) (if (< x y) x y))\n"
          ";  predefined uScheme functions S319d \n"
          "(define negated (n) (- 0 n))\n"
          "(define mod (m n) (- m (* n (/ m n))))\n"
          "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))\n"
          "(define lcm (m n) (if (= m 0) 0 (* m (/ n (gcd m n)))))\n"
          ";  predefined uScheme functions S320b \n"
          "(define list4 (x y z a)         (cons x (list3 y z a)))\n"
          "(define list5 (x y z a b)       (cons x (list4 y z a b)))\n"
          "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))\n"
          "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))\n"
          "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))\n";
    if (setjmp(errorjmp))
        assert(0);  // fail if error occurs in predefined functions
    readevalprint(stringxdefs("predefined functions", fundefs), &env,
                                                                   NOT_ECHOING);

    Prompts prompts  = PROMPTING;     // default behaviors
    Echo    echoes   = ECHOING;
    char **firstpath; // pointer to first first pathname in argv (or to NULL)

/* process options, leaving [[firstpath]] pointing to the name of the first input file S316e */
    (void) argc; // not used
    {   char **nextarg = argv+1;
        bool dumped = false;
        while (*nextarg && **nextarg == '-' && (*nextarg)[1] != '\0') {
            /* handle option [[*nextarg]] S317a */
            extern void dump_env_names(Env);
            if (!strcmp(*nextarg, "-q")) {
                prompts = NOT_PROMPTING;
            } else if (!strcmp(*nextarg, "-qq")) { 
                prompts = NOT_PROMPTING;
                echoes  = NOT_ECHOING;
            } else if (!strcmp(*nextarg, "-names")) { 
                dump_env_names(env);
                dumped = true;
                if (nextarg[1]) {
                    fprintf(stderr, "Dump options must not take any files\n");
                    exit(1);
                }
            } else if (!strcmp(*nextarg, "-primitives")) { 
                dump_env_names(primenv);
                dumped = true;
                if (nextarg[1]) {
                    fprintf(stderr, "Dump options must not take any files\n");
                    exit(1);
                }
            } else {
                fprintf(stderr, "Usage: uscheme [-q|-qq] [pathname ...]\n");
                fprintf(stderr, "       uscheme -primitives\n");
                fprintf(stderr, "       uscheme -names\n");
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
        /* evaluate definitions in file designated by [[*firstpath]] S316c */
        FILE *fin = !strcmp(*firstpath, "-") ? stdin : fopen(*firstpath, "r");
        /* if [[fopen]] failed, roll over and die S316d */
        if (fin == NULL) {
            fprintf(stderr, "%s: cannot open file \"%s\"", argv[0], *firstpath);
            exit(1);
        }
        const char *filename = fin == stdin ? "standard input" : *firstpath;
        XDefstream xdefs = filexdefs(filename, fin, prompts);
        while (setjmp(errorjmp))
            /* error recovery, if needed, would appear here */;
        readevalprint(xdefs, &env, echoes);
        if (fin != stdin)
            fclose(fin);
    }
    return 0;
}
