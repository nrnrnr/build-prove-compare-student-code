#include "all.h"
/*
 * Implementation of the interpreter's [[main]]
 * procedure
 * 
 * As in the Impcore interpreter, function [[main]]
 * processes arguments, initializes the interpreter, and
 * runs the read-eval-print loop. \scmflabelmain
 * <scheme.c>=
 */
int main(int argc, char *argv[]) {
    /*
     * micro-Scheme's many printers have to be installed.
     * <install conversion specifications for [[print]] and [[fprint]]>=
     */
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
    /*
     * <install conversion specifications for [[print]] and [[fprint]]>=
     */
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
    /*
     * In [[addprimitives]], the [[xx]] macro extends the
     * initial environment.
     * <install primitive functions into [[env]]>=
     */
    #define xx(NAME, TAG, FUNCTION) \
        env = bindalloc(strtoname(NAME), mkPrimitive(TAG, FUNCTION), env);
    #include "prim.h"
    #undef xx
    Env primenv = env; // capture for later dump
    /*
     * As in the Impcore interpreter, the C representation
     * of the initial basis is generated automatically from
     * code in [[]]. \
     * makenowebnotdef(from \LApredefined micro-Scheme
     * functions \upshape[->]\RA)
     * <install predefined functions into [[env]]>=
     */
    const char *fundefs = 
        
          ";;\n"
          ";;   In more complex examples, the primitives' definitions\n"
          ";;   have to be used carefully; a literal S-expression\n"
          ";;   might look like a long list even when it's not.\n"
          ";;   For example, list \\monobox(a (b (c d))) looks long,\n"
          ";;   but it has only two elements: the symbol a and the\n"
          ";;   list \\monobox(b (c d)). Its cdr is therefore the\n"
          ";;   single-element list \\monobox((b (c d))).\n"
          ";;   \n"
          ";;   Primitives [[cons]], [[car]], and [[cdr]] are often\n"
          ";;   explained with diagrams. Any nonempty list can be\n"
          ";;   drawn as a box that contains two pointers, one of\n"
          ";;   which points to the [[car]], and the other to the \n"
          ";;   [[cdr]]. This box helps explain not only the behavior\n"
          ";;   but also the cost of running Scheme programs, so it\n"
          ";;   has a name—it is a cons cell. If the [[cdr]] of a\n"
          ";;   cons cell is the empty list, there's nothing to\n"
          ";;   point to; instead, it is drawn as a slash. Using\n"
          ";;   these conventions, the list (a b c) is drawn like\n"
          ";;   this:\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define caar (xs) (car (car xs)))\n"
          "(define cadr (xs) (car (cdr xs)))\n"
          "(define cdar (xs) (cdr (car xs)))\n"
          ";;\n"
          ";;   <predefined uScheme functions ((elided))>=\n"
          ";;\n"
          ";;\n"
          ";;   List operations\n"
          ";;   \n"
          ";;   Nobody should use these operations. I'm not sure why\n"
          ";;   I have kept them. Tradition? \\basislabelcaddr,cddr\n"
          ";;   <more predefined combinations of [[car]] and [[cdr]]>=\n"
          ";;\n"
          "(define cddr  (sx) (cdr (cdr  sx)))\n"
          "(define caaar (sx) (car (caar sx)))\n"
          "(define caadr (sx) (car (cadr sx)))\n"
          "(define cadar (sx) (car (cdar sx)))\n"
          "(define caddr (sx) (car (cddr sx)))\n"
          "(define cdaar (sx) (cdr (caar sx)))\n"
          "(define cdadr (sx) (cdr (cadr sx)))\n"
          "(define cddar (sx) (cdr (cdar sx)))\n"
          "(define cdddr (sx) (cdr (cddr sx)))\n"
          ";;\n"
          ";;   <more predefined combinations of [[car]] and [[cdr]]>=\n"
          ";;\n"
          "(define caaaar (sx) (car (caaar sx)))\n"
          "(define caaadr (sx) (car (caadr sx)))\n"
          "(define caadar (sx) (car (cadar sx)))\n"
          "(define caaddr (sx) (car (caddr sx)))\n"
          "(define cadaar (sx) (car (cdaar sx)))\n"
          "(define cadadr (sx) (car (cdadr sx)))\n"
          "(define caddar (sx) (car (cddar sx)))\n"
          "(define cadddr (sx) (car (cdddr sx)))\n"
          ";;\n"
          ";;   <more predefined combinations of [[car]] and [[cdr]]>=\n"
          ";;\n"
          "(define cdaaar (sx) (cdr (caaar sx)))\n"
          "(define cdaadr (sx) (cdr (caadr sx)))\n"
          "(define cdadar (sx) (cdr (cadar sx)))\n"
          "(define cdaddr (sx) (cdr (caddr sx)))\n"
          "(define cddaar (sx) (cdr (cdaar sx)))\n"
          "(define cddadr (sx) (cdr (cdadr sx)))\n"
          "(define cdddar (sx) (cdr (cddar sx)))\n"
          "(define cddddr (sx) (cdr (cdddr sx)))\n"
          ";;\n"
          ";;   These definitions appear in chunk [[<<predefined\n"
          ";;   micro-Scheme functions>>]], from which they are built\n"
          ";;   into the micro-Scheme interpreter itself and are\n"
          ";;   evaluated when the interpreter starts. Definitions\n"
          ";;   are built in for all combinations of [[car]] and\n"
          ";;   [[cdr]] up to depth five, ending with [[cdddddr]],\n"
          ";;   but the others are relegated to the Supplement.\n"
          ";;   \n"
          ";;   If applying [[car]] or [[cdr]] several times in\n"
          ";;   succession is tiresome, so is applying [[cons]]\n"
          ";;   several times in succession. Common cases are\n"
          ";;   supported by more predefined functions: \\basislabel\n"
          ";;   list1,list2,list3\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define list1 (x)     (cons x '()))\n"
          "(define list2 (x y)   (cons x (list1 y)))\n"
          "(define list3 (x y z) (cons x (list2 y z)))\n"
          ";;\n"
          ";;   Interestingly, [[append]] never looks at \\ys;\n"
          ";;   it inspects only \\xs. And like any list, \\xs is\n"
          ";;   formed using either [['()]] or [[cons]]. If \\xs is\n"
          ";;   empty, [[append]] returns \\ys. If \\xs is \\monobox\n"
          ";;   (cons \\metaz \\zs), [[append]] returns \\metaz followed\n"
          ";;   by \\zs followed by \\ys. The behavior of [[append]]\n"
          ";;   can be specified precisely using two algebraic laws: \n"
          ";;   [*] {llaws} \\monolaw(append '() \\ys)\\ys \\monolaw\n"
          ";;   (append (cons \\metaz \\zs) \\ys)(cons \\metaz (append \\\n"
          ";;   zs \\ys)) {llaws} In the code, argument [[xs]] holds \\\n"
          ";;   xs, argument [[ys]] holds \\ys, \\metaz is \\monobox(car\n"
          ";;   xs), and \\zs is \\monobox(cdr xs): \\basislabelappend\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define append (xs ys)\n"
          "  (if (null? xs)\n"
          "     ys\n"
          "     (cons (car xs) (append (cdr xs) ys))))\n"
          ";;\n"
          ";;   This [[simple-reverse]] function is expensive:\n"
          ";;   [[append]] takes O(n) time and space,\\notation O(...)\n"
          ";;   asymptotic complexity and so [[simple-reverse]] takes\n"
          ";;   O(n^2) time and space, where n is the length of the\n"
          ";;   list. But list reversal can be implemented in linear\n"
          ";;   time. In Scheme, reversal is made efficient by using\n"
          ";;   a trick: take two lists, \\xs and \\ys, and return the\n"
          ";;   reverse of \\xs, followed by (unreversed) \\ys. List \\\n"
          ";;   xs is either empty or is z followed by \\zs, and the\n"
          ";;   computation obeys these laws: {llaws} \\mathlawR(\n"
          ";;   epsilon)\\followedby\\ys\\ys \\mathlawR(z\\followedby\\zs)\\\n"
          ";;   followedby\\ys(R(\\zs)\\followedby z)\\followedby\\ys= R(\\\n"
          ";;   zs)\\followedby(z \\followedby\\ys) {llaws} Translated\n"
          ";;   back to Scheme, the laws for ``reverse-append'' are\n"
          ";;   {llaws} \\monolaw(revapp '() \\ys)\\ys \\monolaw(revapp\n"
          ";;   (cons \\metaz \\zs) \\ys)(revapp \\zs (cons \\metaz \\ys))\n"
          ";;   {llaws} The code looks like this: \\basislabelrevapp\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define revapp (xs ys) ; (reverse xs) followed by ys\n"
          "  (if (null? xs)\n"
          "     ys\n"
          "     (revapp (cdr xs) (cons (car xs) ys))))\n"
          ";;\n"
          ";;   Function [[revapp]] takes time and space linear in\n"
          ";;   the size of [[xs]]. Using it with an empty list makes\n"
          ";;   predefined function [[reverse]] equally efficient. \\\n"
          ";;   basislabelreverse\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define reverse (xs) (revapp xs '()))\n"
          ";;\n"
          ";;   Coding with S-expressions\\nochap: Lists of lists\n"
          ";;   \n"
          ";;   [*]\n"
          ";;   \n"
          ";;   Even more recursion happens when a list element is\n"
          ";;   itself a list, which can contain other lists, and\n"
          ";;   so on. Such lists, together with the atoms (rule [->]\n"
          ";;   , \\cpagerefscheme.atoms), constitute the ordinary\n"
          ";;   S-expressions.\n"
          ";;   \n"
          ";;   An ordinary S-expression is either an atom or a list\n"
          ";;   of ordinary S-expressions. [The empty list [['()]] is\n"
          ";;   \\emph{both} an atom \\emph{and} a list of ordinary\n"
          ";;   S-expressions.] An atom is identified by predefined\n"
          ";;   function [[atom?]]:[*] \\basislabelatom?\n"
          ";;   <predefined uScheme functions ((elided))>=\n"
          ";;\n"
          ";;\n"
          ";;   More cases, for [[list4]] to [[list8]], are defined\n"
          ";;   in the Supplement. In full Scheme, all possible cases\n"
          ";;   are handled by a single, variadic function, list,\n"
          ";;   which takes any number of arguments and returns a\n"
          ";;   list containing those arguments (\\cref\n"
          ";;   scheme.ex.list).\n"
          ";;   \n"

";;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
          ";;   \\advanceby 1.6pt \\advanceby -0.35pt\n"
          ";;   \n"
          ";;   |>p0.25\\mysize >p0.75 Equality and inequality on\n"
          ";;   \\mysize| [[=]], [[!   atoms\n"
          ";;   =]]\n"
          ";;                      Recursive equality on fully\n"
          ";;   [[equal?]]            general S-expressions\n"
          ";;                      (isomorphism, not object\n"
          ";;                      identity)\n"
          ";;   [[/]], [[*]], [[-]],  Integer arithmetic\n"
          ";;   [[+]], [[mod]]\n"
          ";;   [[>]], [[<]], [[>=]], Integer comparison\n"
          ";;   [[<=]]\n"
          ";;   [[lcm]], [[gcd]],     Binary operations on integers\n"
          ";;   [[min]], [[max]]\n"
          ";;   [[lcm*]], [[gcd*]],   The same operations, but taking\n"
          ";;   [[max*]], [[min*]]    one nonempty list of integers\n"
          ";;                      as argument\n"
          ";;                      Basic operations on Booleans,\n"
          ";;   [[not]], [[and]],     which, unlike their\n"
          ";;   [[or]]                counterparts in full Scheme,\n"
          ";;                      evaluate all their arguments\n"
          ";;   [[symbol?]],\n"
          ";;   [[number?]],\n"
          ";;   [[boolean?]],         Type predicates\n"
          ";;   [[null?]], [[pair?]],\n"
          ";;   [[function?]]\n"
          ";;                      Type predicate saying whether a\n"
          ";;   [[atom?]]             value is an atom \\break(not a\n"
          ";;                      function and not a pair)\n"
          ";;   [[cons]], [[car]],    The basic list operations\n"
          ";;   [[cdr]]\n"
          ";;                      Abbreviations for combinations\n"
          ";;   [[caar]], [[cdar]],   of list operations, including\n"
          ";;   [[cadr]], [[cddr]], \\ also [[caaar]], [[cdaar]],\n"
          ";;   ensuremath...         [[caadr]], [[cdadr]], and so\n"
          ";;                      on, all the way to [[cddddr]].\n"
          ";;   [[list1]], [[list2]], Convenience functions for\n"
          ";;   [[list3]], [[list4]], creating lists, including also\n"
          ";;   \\ensuremath...        [[list5]] to [[list8]]\n"
          ";;                      The elements of one list\n"
          ";;   [[append]]            followed by the elements of\n"
          ";;                      another\n"
          ";;   [[revapp]]            The elements of one list,\n"
          ";;                      reversed, followed by another\n"
          ";;   [[reverse]]           A list reversed\n"
          ";;   [[bind]], [[find]]    Insertion and lookup for\n"
          ";;                      association lists\n"
          ";;   [[filter]]            Those elements of a list\n"
          ";;                      satisfying a predicate\n"
          ";;   [[exists?]]           Does any element of a list\n"
          ";;                      satisfy a predicate?\n"
          ";;   [[all?]]              Do all elements of a list\n"
          ";;                      satisfy a predicate?\n"
          ";;                      List of results of applying a\n"
          ";;   [[map]]               function to each element of\n"
          ";;                      a list\n"
          ";;   [[takewhile]]         The longest prefix of a list\n"
          ";;                      satisfying a predicate\n"
          ";;   [[dropwhile]]         What's not taken by\n"
          ";;                      [[takewhile]]\n"
          ";;                      Elements of a list combined by\n"
          ";;   [[foldl]], [[foldr]]  an operator, which associates\n"
          ";;                      to left or right, respectively\n"
          ";;   [[o]]                 Function composition\n"
          ";;   [[curry]]             The curried function equivalent\n"
          ";;                      to some binary function\n"
          ";;   [[uncurry]]           The binary function equivalent\n"
          ";;                      to some curried function\n"
          ";;   [[println]],          Primitives that print one value\n"
          ";;   [[print]]\n"
          ";;   [[printu]]            Primitive that prints a Unicode\n"
          ";;                      character\n"
          ";;                      Primitive that aborts the\n"
          ";;   [[error]]             computation with an error\n"
          ";;                      message\n"
          ";;   \n"
          ";;   The initial basis of micro-Scheme [*]\n"
          ";;   \n"

";;   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
          ";;   \n"
          ";;   [*] Three predefined functions are similar but not\n"
          ";;   identical to functions found in Impcore: the Boolean\n"
          ";;   functions [[and]], [[or]], and [[not]]. Instead of\n"
          ";;   Impcore's 1 and 0, they return Boolean values.\n"

";;   <definitions of predefined uScheme functions [[and]], [[or]], and [[not]]>=\n"
          ";;\n"
          "(define and (b c) (if b  c  b))\n"
          "(define or  (b c) (if b  b  c))\n"
          "(define not (b)   (if b #f #t))\n"
          ";;\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define atom? (x)\n"
          "  (or (symbol? x) (or (number? x) (or (boolean? x) (null? x)))))\n"
          ";;\n"
          ";;   Inspecting multiple inputs: Equality on S-expressions\n"
          ";;   \n"
          ";;   Functions like [[length]], [[append]], [[insert]],\n"
          ";;   and [[has?]] inspect only one list or one\n"
          ";;   S-expression. A function that inspects two\n"
          ";;   S-expressions must prepare for all forms of both\n"
          ";;   inputs, for a total of four cases. As an example,\n"
          ";;   function [[equal?]] compares two S-expressions for\n"
          ";;   equality—they are equal if they are formed from the\n"
          ";;   same atoms in the same way. Breaking the inputs down\n"
          ";;   by cases, two atoms are equal if they are the same,\n"
          ";;   as tested with primitive [[=]]. Two lists are equal\n"
          ";;   if they contain (recursively) equal elements in equal\n"
          ";;   positions. An atom and a nonempty list are never\n"
          ";;   equal. {llaws} \\monolaw[,] (equal? \\meta\\sx_1 \\meta\\\n"
          ";;   sx_2)(= \\meta\\sx_1 \\meta\\sx_2) if \\meta\\sx_1 is an\n"
          ";;   atom and \\meta\\sx_2 is an atom\n"
          ";;   \\monolaw[, if \\meta\\sx_1 is an atom] (equal? \\meta\\sx\n"
          ";;   _1 (cons \\metaw \\metaz)))[[#f]] \\monolaw[, if \\meta\\\n"
          ";;   sx_2 is an atom] (equal? (cons \\metax \\metay) \\meta\\\n"
          ";;   sx_2)[[#f]] \\monolaw(equal? (cons \\metax \\metay)\n"
          ";;   (cons \\metaw \\metaz)) (and (equal? \\metax \\metaw)\n"
          ";;   (equal? \\metay \\metaz)) {llaws} These laws call for\n"
          ";;   four cases, but in an implementation, the first two\n"
          ";;   laws can be combined: the second law calls for\n"
          ";;   [[equal?]] to return [[#f]], but when \\meta\\sx_1 is\n"
          ";;   an atom and \\meta\\sx_2 is \\monobox(cons \\metaw \\meta\n"
          ";;   z), \\monobox(= \\meta\\sx_1 \\meta\\sx_2) always returns\n"
          ";;   false, so both cases where \\meta\\sx_1 is an atom may\n"
          ";;   use [[=]]: \\basislabelequal? [*]\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define equal? (sx1 sx2)\n"
          "  (if (atom? sx1)\n"
          "    (= sx1 sx2)\n"
          "    (if (atom? sx2)\n"
          "        #f\n"
          "        (and (equal? (car sx1) (car sx2))\n"
          "             (equal? (cdr sx1) (cdr sx2))))))\n"
          ";;\n"
          ";;   If [[member?]] used \\monobox= instead of [[equal?]],\n"
          ";;   this last example wouldn't work; I encourage you to\n"
          ";;   explain why (\\schemexset-with-=).\n"
          ";;   \n"
          ";;   Association lists\n"
          ";;   \n"
          ";;   [*]\n"
          ";;   \n"
          ";;   A list of ordered pairs can represent a classic data\n"
          ";;   structure of symbolic computing: the finite map (also\n"
          ";;   called associative array, dictionary, and table).\n"
          ";;   Finite maps are ubiquitous; for example, in this book\n"
          ";;   they are used to represent the environments found in\n"
          ";;   operational semantics and in interpreters. (In an\n"
          ";;   interpreter or compiler, an environment is often\n"
          ";;   called a symbol table.)\n"
          ";;   \n"
          ";;   A small map is often represented as an association\n"
          ";;   list. An association list has the form \\monobox((k_1\n"
          ";;   a_1) ... (k_m a_m)),\\notation k a key in an\n"
          ";;   association list\\notation a an attribute in an\n"
          ";;   association list where each k_i is a symbol, called\n"
          ";;   a key, and each a_i is an arbitrary value, called an \n"
          ";;   attribute. A pair \\monobox(k_i a_i) is made with\n"
          ";;   function [[make-alist-pair]] and inspected with\n"
          ";;   functions [[alist-pair-key]] and\n"
          ";;   [[alist-pair-attribute]]: {llaws} \\monolaw\n"
          ";;   (alist-pair-key (make-alist-pair \\metak \\metaa))\\meta\n"
          ";;   k \\monolaw(alist-pair-attribute (make-alist-pair \\\n"
          ";;   metak \\metaa))\\metaa {llaws} The pair is represented\n"
          ";;   by a two-element list, so the three \\basislabel\n"
          ";;   make-alist-pair,alist-pair-key,alist-pair-attribute\n"
          ";;   [[alist-pair]] functions are implemented as follows:\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define make-alist-pair      (k a)   (list2 k a))\n"
          "(define alist-pair-key       (pair)  (car  pair))\n"
          "(define alist-pair-attribute (pair)  (cadr pair))\n"
          ";;\n"
          ";;   A list of these pairs forms an association list, and\n"
          ";;   when an association list is nonempty, the key and\n"
          ";;   attribute of the \\basislabel\n"
          ";;   alist-first-key,alist-first-attribute first pair are\n"
          ";;   retrieved by these auxiliary functions:\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"

   "(define alist-first-key       (alist) (alist-pair-key       (car alist)))\n"

   "(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))\n"
          ";;\n"
          ";;   An association list is operated on primarily by\n"
          ";;   functions [[bind]] and [[find]], which add bindings\n"
          ";;   and retrieve attributes. Their behavior is described\n"
          ";;   by these laws: [*] {llaws*} \\monolaw(bind \\metak \\\n"
          ";;   metaa '())(cons (make-alist-pair \\metak \\metaa) '())\n"
          ";;   \\monolaw(bind \\metak \\metaa (cons (make-alist-pair \\\n"
          ";;   metak \\metaa') \\ps)) (cons (make-alist-pair \\metak \\\n"
          ";;   metaa) \\ps) \\monolaw(bind \\metak \\metaa (cons\n"
          ";;   (make-alist-pair \\metak' \\metaa') \\ps)) \\monobox(cons\n"
          ";;   (make-alist-pair \\metak' \\metaa') (bind \\metak \\metaa\n"
          ";;   \\ps)),\n"
          ";;   --- --- \\qquadwhen \\metak and \\metak' are different\n"
          ";;   \\monolaw(find \\metak '())'() \\monolaw(find \\metak\n"
          ";;   (cons (make-alist-pair \\metak \\metaa) \\ps))\\metaa \\\n"
          ";;   monolaw(find \\metak (cons (make-alist-pair \\metak' \\\n"
          ";;   metaa) \\ps))(find \\metak \\ps) --- --- \\qquadwhen \\\n"
          ";;   metak and \\metak' are different\n"
          ";;   {llaws*} A missing attribute is retrieved as [['()]].\n"
          ";;   \\basislabelbind,find\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
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
          ";;\n"
          ";;   Function [[irand]] has its own private copy of\n"
          ";;   [[seed]], which only it can access, and which it\n"
          ";;   updates at each call. And function\n"
          ";;   [[repeatable-irand]], which might be used to replay\n"
          ";;   an execution for debugging, has its own private seed.\n"
          ";;   So it repeats the same sequence [1, 14, 131, 160,\n"
          ";;   421, ...] no matter what happens with [[irand]].\n"
          ";;   \n"
          ";;   Useful higher-order functions\n"
          ";;   \n"
          ";;   [*] The [[lambda]] expression does more than just\n"
          ";;   encapsulate mutable state; [[lambda]] helps express\n"
          ";;   and support not just algorithms but also patterns of\n"
          ";;   computation. What a ``pattern of computation'' might\n"
          ";;   be is best shown by example.\n"
          ";;   \n"
          ";;   One minor example is the function [[mk-rand]]: it can\n"
          ";;   be viewed as a pattern that says ``if you tell me how\n"
          ";;   to get from one number to the next, I can deliver an\n"
          ";;   entire sequence of numbers starting with 1.''\n"
          ";;   This pattern of computation, while handy, is not used\n"
          ";;   often. More useful patterns can make new functions\n"
          ";;   from old functions or can express common ways of\n"
          ";;   programming with lists, like ``do something with\n"
          ";;   every element.'' Such patterns are presented in the\n"
          ";;   next few sections.\n"
          ";;   \n"
          ";;   Composition\n"
          ";;   \n"
          ";;   One of the simplest ways to make a new function is by\n"
          ";;   composing two old ones. Function [[o]] (pronounced\n"
          ";;   ``circle'' or ``compose'') returns the composition of\n"
          ";;   two one-argument functions, often written f og.\\\n"
          ";;   stdbreak \\notation [composed with]ofunction\n"
          ";;   composition Composition is described by the algebraic\n"
          ";;   law (f og)(x) = f(g(x)), and like any function that\n"
          ";;   makes new functions, it returns a [[lambda]]: \\\n"
          ";;   basislabelo\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"

  "(define o (f g) (lambda (x) (f (g x))))          ; ((o f g) x) = (f (g x))\n"
          ";;\n"
          ";;   Function composition can negate a predicate by\n"
          ";;   composing [[not]] with it:\n"
          ";;\n"
          "\n"
          ";;\n"
          ";;   Functions needn't always be curried by hand. Any\n"
          ";;   binary function can be converted between its\n"
          ";;   uncurried and curried forms using the predefined\n"
          ";;   functions [[curry]] and [[uncurry]]: \\basislabel\n"
          ";;   curry,uncurry\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define curry   (f) (lambda (x) (lambda (y) (f x y))))\n"
          "(define uncurry (f) (lambda (x y) ((f x) y)))\n"
          ";;\n"
          ";;   More applications of [[foldr]] and [[foldl]] are\n"
          ";;   suggested in Exercises [->], [->], and [->].\n"
          ";;   \n"
          ";;   Visualizations\\cullchap of the standard list\n"
          ";;   functions\n"
          ";;   \n"
          ";;   --- \\bigsize#2 --- \\bigsize#2 --- \\bigsize#2 --- \\\n"
          ";;   bigsize#2 --- #1 --- \\bigsize#2\n"
          ";;   --- \\bigsize#3 --- \\bigsize#2 --- \\bigsize#2 --- \\\n"
          ";;   bigsize#3 --- #1 --- \\bigsize#2\n"
          ";;   \n"
          ";;   Which list functions should be used when? Functions\n"
          ";;   [[exists?]] and [[all?]] are not hard to figure out,\n"
          ";;   but [[map]], [[filter]], and [[foldr]] can be more\n"
          ";;   mysterious. They can be demystified a bit using\n"
          ";;   pictures, as inspired by [cite harvey:simply].\n"
          ";;   \n"
          ";;   A generic list [[xs]] can be depicted as a list of\n"
          ";;   circles:\n"
          ";;   \n"
          ";;    {rowtable} xs --- \\roweq --- \\bigrow\\mycircle \n"
          ";;    {rowtable}\n"
          ";;   \n"
          ";;   If [[f]] is a function that turns one circle into one\n"
          ";;   triangle, as in \\nomathbreak\\monobox(f \\mycircle) = \\\n"
          ";;   mytriangle, then \\monobox(map f xs) turns a list of\n"
          ";;   circles into a list of triangles.\n"
          ";;   \n"
          ";;    {rowtable} xs --- \\roweq --- \\bigrow\\mycircle\n"
          ";;    --- --- \\bigrow[]\\Bigg\\downarrow\n"
          ";;    \\monobox(map f xs) --- \\roweq --- \\bigrow\\\n"
          ";;    mytriangle {rowtable}\n"
          ";;   \n"
          ";;   If [[p?]] is a function that takes a circle and\n"
          ";;   returns a Boolean, as in \\nomathbreak\\monobox(p? \\\n"
          ";;   mycircle) = b, then \\monobox(filter p? xs) selects\n"
          ";;   just some of the circles:\n"
          ";;   \n"
          ";;    {rowtable} xs --- \\roweq --- \\bigrow\\mycircle\n"
          ";;    --- --- \\mixedrow[]\\Bigg\\downarrow*\n"
          ";;    \\monobox(filter p? xs) --- \\roweq --- \\mixedrow\\\n"
          ";;    mycircle {rowtable}\n"
          ";;   \n"
          ";;   Finally, if [[f]] is a function that takes a circle\n"
          ";;   and a box and produces another box, as in \\monobox(f \n"
          ";;   \\mycircle \\mybox [[)]] = \\mybox, then \\monobox(fold f\n"
          ";;   \\mybox xs) folds all of the circles into a single\n"
          ";;   box:\n"
          ";;   \n"
          ";;    {rowtable} xs --- \\roweq --- \\bigrow\\mycircle\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define filter (p? xs)\n"
          "  (if (null? xs)\n"
          "    '()\n"
          "    (if (p? (car xs))\n"
          "      (cons (car xs) (filter p? (cdr xs)))\n"
          "      (filter p? (cdr xs)))))\n"
          ";;\n"
          ";;   Function [[map]] is even simpler. There is no\n"
          ";;   conditional test; the induction step just applies \n"
          ";;   [[f]] to the [[car]], then conses. \\basislabelmap\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define map (f xs)\n"
          "  (if (null? xs)\n"
          "    '()\n"
          "    (cons (f (car xs)) (map f (cdr xs)))))\n"
          ";;\n"
          ";;   Function [[app]] is like [[map]], except its argument\n"
          ";;   is applied only for side effect. Function [[app]] is\n"
          ";;   typically used with [[printu]]. Because [[app]] is\n"
          ";;   executed for side effects, its behavior cannot be\n"
          ";;   expressed using simple algebraic laws. \\basislabelapp\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define app (f xs)\n"
          "  (if (null? xs)\n"
          "    #f\n"
          "    (begin (f (car xs)) (app f (cdr xs)))))\n"
          ";;\n"
          ";;   Each of the preceding functions processes every\n"
          ";;   element of its list argument. Functions [[exists?]]\n"
          ";;   and [[all?]] don't necessarily do so. Function\n"
          ";;   [[exists?]] stops the moment it finds a satisfying\n"
          ";;   element; [[all?]] stops the moment it finds a non\n"
          ";;   -satisfying element. \\basislabelexists?,all?\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
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
          ";;\n"
          ";;   Finally, [[foldr]] and [[foldl]], although simple,\n"
          ";;   are not necessarily easy to understand. Study their\n"
          ";;   algebraic laws, and remember that \\monobox(car xs) is\n"
          ";;   always a first argument to [[combine]], and [[zero]]\n"
          ";;   is always a second argument. \\basislabelfoldl,foldr\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define foldr (combine zero xs)\n"
          "  (if (null? xs)\n"
          "    zero\n"
          "    (combine (car xs) (foldr combine zero (cdr xs)))))\n"
          "(define foldl (combine zero xs)\n"
          "  (if (null? xs)\n"
          "    zero\n"
          "    (foldl combine (combine (car xs) zero) (cdr xs))))\n"
          ";;\n"
          ";;   \\qbreak\n"
          ";;   \n"
          ";;   Unicode code points\n"
          ";;   \n"
          ";;   micro-Scheme has no string literals; it has only\n"
          ";;   quoted symbols. To print a character that can't\n"
          ";;   appear in a quoted symbol, use one of these code\n"
          ";;   points:\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(val newline      10)   (val left-round    40)\n"
          "(val space        32)   (val right-round   41)\n"
          "(val semicolon    59)   (val left-curly   123)\n"
          "(val quotemark    39)   (val right-curly  125)\n"
          "                        (val left-square   91)\n"
          "                        (val right-square  93)\n"
          ";;\n"
          ";;   Integer functions\n"
          ";;   \n"
          ";;   The non-primitive integer operations are defined\n"
          ";;   exactly as they would be in Impcore. First, the\n"
          ";;   comparisons.\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define <= (x y) (not (> x y)))\n"
          "(define >= (x y) (not (< x y)))\n"
          "(define != (x y) (not (= x y)))\n"
          ";;\n"
          ";;   Next, [[min]] and [[max]].\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define max (x y) (if (> x y) x y))\n"
          "(define min (x y) (if (< x y) x y))\n"
          ";;\n"
          ";;   Finally, negation, modulus, greatest common divisor,\n"
          ";;   and least common multiple.\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
          "(define negated (n) (- 0 n))\n"
          "(define mod (m n) (- m (* n (/ m n))))\n"
          "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))\n"
          "(define lcm (m n) (if (= m 0) 0 (* m (/ n (gcd m n)))))\n"
          ";;\n"
          ";;   The functions below, by contrast, are silver.\n"
          ";;   (``Gold'' would be a variadic [[list]] function such\n"
          ";;   as would be enabled by completing \\cref\n"
          ";;   mlscheme.ex.varargs in \\crefmlscheme.chap. Or a\n"
          ";;   variadic [[list]] primitive.) \\basislabel\n"
          ";;   list4,list5,list6,list7,list8\n"
          ";;   <predefined uScheme functions>=\n"
          ";;\n"
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
    /*
     * Options are processed as in Impcore.
     * <process options, leaving [[firstpath]] pointing to the name of the first
                                                                    input file>=
     */
    (void) argc; // not used
    {   char **nextarg = argv+1;
        bool dumped = false;
        while (*nextarg && **nextarg == '-' && (*nextarg)[1] != '\0') {
            /*
             * \qbreak In addition to the options that the Impcore
             * interpreter understands, a micro-Scheme interpreter
             * can also take a [[-primitives]] option, which dumps
             * just the names of the primitive functions.
             * <handle option [[*nextarg]]>=
             */
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
        /*
         * Also as in Impcore, a file is evaluated by setting
         * [[fin]] to the file designated by [[*firstpath]],
         * setting [[filename]] to its name, calling
         * [[filexdefs]], and finally calling [[readevalprint]]
         * under the protection of [[setjmp]]. [*]
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
        while (setjmp(errorjmp))
            /* error recovery, if needed, would appear here */;
        readevalprint(xdefs, &env, echoes);
        if (fin != stdin)
            fclose(fin);
    }
    return 0;
}
