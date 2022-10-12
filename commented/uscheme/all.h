/*
 * \qvfilbreak[2000]1.5in
 * 
 * The interpreter's \chaptocsplitheader file
 * 
 * As in Impcore, the whole interpreter is served by a
 * single C header file.
 * <{\Tt all.h} for \uscheme>=
 */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __GNUC__
#define __noreturn __attribute__((noreturn))
#else
#define __noreturn
#endif

/*
 * <early type definitions for \uscheme>=
 */
typedef struct Valuelist *Valuelist;     // list of Value
/*
 * <type definitions for \uscheme>=
 */
typedef struct Lambda Lambda; 
typedef struct Value Value;
typedef enum { SYM, NUM, BOOLV, NIL, PAIR, CLOSURE, PRIMITIVE } Valuealt;

/*
 * <type definitions for \uscheme>=
 */
typedef struct Def *Def;
typedef enum { VAL, EXP, DEFINE, DEFS } Defalt; 
typedef struct Exp *Exp;
typedef enum {
    LITERAL, VAR, SET, IFX, WHILEX, BEGIN, APPLY, LETX, LAMBDAX
} Expalt;

typedef struct XDef *XDef;
typedef enum { DEF, USE, TEST } XDefalt; 
typedef struct UnitTest *UnitTest;
typedef enum { CHECK_EXPECT, CHECK_ASSERT, CHECK_ERROR } UnitTestalt;

/*
 * <type definitions for \uscheme>=
 */
typedef enum Letkeyword { LET, LETSTAR, LETREC } Letkeyword;
/*
 * <type definitions for \uscheme>=
 */
typedef Value (Primitive)(Exp e, int tag, Valuelist vs);
/*
 * Why does a [[Primitive]] take so many arguments?
 * Shouldn't a primitive function just take a
 * [[Valuelist]] and return a [[Value]]? No. If a
 * primitive fails, it needs to show where the failure
 * occurred; that's the \monoboxExp e. And by using an
 * integer [[tag]], a single [[Primitive]] function can
 * implement multiple micro-Scheme primitives. The C
 * function that implements micro-Scheme's arithmetic
 * primitives, for example, makes it easy for those
 * primitives to share the code that ensures both
 * arguments are numbers. Implementations of the
 * primitives appear in Section [->] and in \cref
 * schemea.chap.
 * 
 * Interfaces
 * 
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * \advanceby 1pt \newskip\myskip \myskip=6pt
 * 
 *    Semantics   Concept     Interpreter
 *        d       Definition  Def (\cpageref
 *                            scheme.type.Def)
 *        e       Expression  \scmtypeExp
 *      \aloc     Location    \monoValue *
 *        x       Name        \iitypeName
 *        v       Value       \scmtypeValue
 *       rho      Environment \scmtypeEnv
 *      sigma     Store       Machine memory (the
 *                            C heap)
 *                Expression      \monoboxeval(e, rho)
 *   [\myskip] \  evaluation      = v, \break with
 *   evale ==>\                   sigma updated to 
 *   evalr['] v                   sigma' \scmfunpage
 *                                eval
 * 
 *                Definition      \monoboxevaldef(d, 
 *  <d,rho,sigma> evaluation      rho, echo) = rho', \
 *   --><rho',                    break with sigma
 *     sigma'>                    updated to sigma' \
 *                                scmfunpageevaldef
 * 
 *   [\myskip] x  Definedness \monofind(x, rho) !=
 *   in dom rho               NULL (\cpageref
 *                            scheme.find.int)
 *     rho(x)     Location    \monofind(x, rho) (\
 *                lookup      cpagerefscheme.find.int)
 *  sigma(rho(x)) Value       \mono*find(x, rho) (\
 *                lookup      cpagerefscheme.find.int)
 *   rho{x |->\   Binding     \scmfunbindalloc
 *      aloc}
 *   \aloc\notin  Allocation  \scmfunbindalloc
 *    dom sigma
 *  sigma{\aloc|  Store       \mono*\aloc = v
 *      ->v}      update
 * 
 * Correspondence between micro-Scheme semantics and
 * code [*]
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 * The Environment and the Store
 * 
 * [*] In the operational semantics, the store sigma
 * models the memory of the abstract machine. In the
 * implementation, the store is represented by the
 * memory of the real machine; a location is represented
 * by a C pointer of type \monoboxValue *. An
 * environment [[Env]] maps names to pointers; find(x, 
 * rho) returns rho(x) whenever x in dom rho; when x \
 * notindom rho, it returns [[NULL]].\scmlabelEnv\
 * scmflabelfind[*]
 * <type definitions for \uscheme>=
 */
typedef struct Env *Env;
/*
 * Both syntax and values may be put in lists.
 * <type definitions for \uscheme>=
 */
typedef struct UnitTestlist  *UnitTestlist;  // list of UnitTest 
typedef struct Explist  *Explist;            // list of Exp 
typedef struct Deflist  *Deflist;            // list of Def    /*OMIT*/
/*
 * To define the primitives and to associate each one
 * with its tag and function, I resort to an old C
 * programmer's technique called ``X macros.'' Each
 * primitive appears in file prim.h as a macro [[xx(]]
 * name\monobox, tag\monobox, function[[)]]. I use the
 * same macros with two different definitions of [[xx]]:
 * one to create an enumeration with distinct tags, and
 * one to install the primitives in an empty
 * environment. There are other initialization
 * techniques that don't require macros, but using
 * X macros ensures there is a single point of truth
 * about the primitives, which helps guarantee that the
 * enumeration type is consistent with the
 * initialization code. (That point of truth is the file
 * prim.h.)
 * <type definitions for \uscheme>=
 */
enum {
  #define xx(NAME, TAG, FUNCTION) TAG,
  #include "prim.h"
  #undef xx
  UNUSED_TAG
};
/*
 * The descriptions above [The alternatives for [[if]]
 * and [[while]] are named [[IFX]] and [[WHILEX]], not
 * [[IF]] and [[WHILE]]. Why? Because corresponding to
 * each alternative, there is a field of a union that
 * uses the same name in \emph{lower} case. For~example,
 * if [[e]] is a [[LITERAL]] expression, the literal
 * [[Value]] is found in field [[e->literal]]. But a
 * structure field can't be named [[if]] or [[while]],
 * because the names [[if]] and [[while]] are \emph
 * {reserved words}---they may be used only to mark \
 * cc~syntax. So~I~call these alternatives [[IFX]] and
 * [[WHILEX]], which I~encourage you to think of as
 * ``[[if]]-expression'' and ``[[while]]-expression.''
 * For similar reasons, the two branches of the [[IFX]]
 * are called [[truex]] and [[falsex]], not [[true]] and
 * [[false]]. And in \cref{scheme.chap}, you'll see
 * [[LETX]] and [[LAMBDAX]] instead of [[LET]] and
 * [[LAMBDA]], so that I~can write an interpreter for \
 * uscheme\ in \uscheme.] are slightly elaborated
 * versions of [[<<simplified example of abstract syntax
 * for Impcore>>]]. Similar descriptions are used for
 * much of the C code in this book.
 * 
 * True definitions and expressions are the essential
 * elements of abstract syntax; once you understand how
 * they work, you will be ready to connect the
 * operational semantics and the code. Impcore's
 * extended definitions, including unit tests, are
 * described in the Supplement.
 * 
 * Interface to names: An abstract type
 * 
 * [*]
 * 
 * Programs are full of names. To make it easy to
 * compare names and look them up in tables, I define an
 * abstract type to represent them. Although each name
 * is built from a string, the abstract type hides the
 * string and its characters. Unlike C strings, names
 * are immutable, and two names are equal if and only if
 * they are the same pointer. \iilabelName\intlabelName
 * <shared type definitions>=
 */
typedef struct Name *Name;
typedef struct Namelist *Namelist;   // list of Name
/*
 * <shared type definitions>=
 */
typedef struct Par *Par;
typedef enum { ATOM, LIST } Paralt; 
/*
 * The important invariant of this data structure is
 * that components[i] is meaningful if and only if 0 <=i
 * < nparsed.
 * 
 * \qbreak I define type abbreviations for
 * [[ParserState]] and [[ParsingContext]].
 * <shared type definitions>=
 */
typedef struct ParserState *ParserState;
typedef struct ParsingContext *ParsingContext;
/*
 * Each form of component is parsed by its own shift
 * function. Why ``shift''? Think of the [[ParserState]]
 * as the state of a machine that puts components on the
 * left and the input on the right. A shift function
 * removes initial inputs and appends to components;
 * this action ``shifts'' information from right to
 * left. Shifting plays a role in several varieties of
 * parsing technology.
 * 
 * A shift function normally updates the inputs and
 * components in the parser state. A shift function also
 * returns one of these results:
 * <shared type definitions>=
 */
typedef enum ParserResult {
  PARSED,            // some input was parsed without any errors
  INPUT_EXHAUSTED,   // there aren't enough inputs
  INPUT_LEFTOVER,    // there are too many inputs
  BAD_INPUT,         // an input wasn't what it should have been
  STOP_PARSING       // all the inputs have been parsed; it's time to stop
} ParserResult;
/*
 * When a shift function runs out of input or sees input
 * left over, it returns either [[INPUT_EXHAUSTED]] or
 * [[INPUT_LEFTOVER]]. Returning one of these error
 * results is better than simply calling [[synerror]],
 * because the calling function knows what row it's
 * trying to parse and so can issue a better error
 * message. But for other error conditions, shift
 * functions can call [[synerror]] directly.
 */

/*
 * The C type of a shift function is [[ShiftFun]].
 * <shared type definitions>=
 */
typedef ParserResult (*ShiftFun)(ParserState);
/*
 * The [[usage_error]] function is discussed below.
 * Meanwhile, [[rowparse]] is called by [[tableparse]],
 * which looks for a keyword in the input, and if it
 * finds one, uses the matching row to parse. Otherwise,
 * it uses the final row, which it identifies by the
 * [[NULL]] keyword.
 * <shared type definitions>=
 */
typedef struct ParserRow *ParserTable;
/*
 * \qbreak
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * \advance\nwdefspaceby 0.05
 * 
 *  ┌────────────────────────�
         ��──────────────────────┐
 *  │|height 12pt width 0pt Here are integer codes  │
 *  │for all the syntactic forms that are suggested │
 *  │to be implemented as syntactic sugar.          │
 *  └────────────────────────�
         ��──────────────────────┘
 * <shared type definitions>=
 */
enum Sugar {
  CAND, COR,    // short-circuit Boolean operators

  WHILESTAR, DO_WHILE, FOR,     // bonus loop forms

  WHEN, UNLESS,       // single-sided conditionals

  RECORD,             // record-type definition

  COND                // McCarthy's conditional from Lisp

};
/*
 * Streams
 * 
 * An evaluator works by repeatedly calling [[getxdef]]
 * on a stream of [[XDef]]s. Behind the scenes, there's
 * a lot going on:
 * 
 *   • Each [[XDef]] is produced from a parenthesized
 *  phrase, for example something like \monobox(val n
 *  0) or \monobox(define id (x) x). A parenthesized
 *  phrase, which in the code is called [[Par]], is
 *  simply a fragment of the input in which
 *  parentheses are balanced; converting a
 *  parenthesized phrase to an expression or an
 *  extended definition is the job of the parser
 *  presented in \crefcparse.chap. Producing
 *  parenthesized phrases, however, is done here;
 *  function [[parstream]] produces a stream of
 *  [[Par]]s, called [[Parstream]], and [[getpar]]
 *  takes a [[Parstream]] and produces a [[Par]].
 *   • A [[Par]] is found on one or more input lines.
 *  (And an input line may contain more than one
 *  [[Par]].) A [[Parstream]] is produced from a
 *  [[Linestream]], and a [[Linestream]] may be
 *  produced either from a string or from an input
 *  file.
 * 
 * Each stream follows the same pattern: there are one
 * or more functions to create streams, and there's a
 * function to get a thing from a stream. Their
 * implementations are also similar. All the streams and
 * their implementations are presented in this section.
 * I present streams of lines first, then parenthesized
 * phrases, and finally extended definitions. That way,
 * as you read each implementation, you'll be familiar
 * with what it depends on.
 * 
 * Streams of lines
 * 
 * A [[Linestream]] encapsulates a seqeuence of input
 * lines.
 * 
 * Interface to \upshapeLinestream
 * 
 * To use a [[Linestream]], call [[getline_]]. [The
 * function is called [[getline_]] with a trailing
 * underscore so as not to conflict with [[getline]], a
 * POSIX standard function. I~was using [[getline]] for
 * 20~years before the POSIX function was standardized,
 * and I'm too stubborn to change.] The [[getline_]]
 * function prints a prompt, reads the next line of
 * input from the source, and returns a pointer to the
 * line. Client code needn't worry about how long the
 * line is; [[getline_]] allocates enough memory to hold
 * it. Because [[getline_]] reuses the same memory to
 * hold successive lines, it is an unchecked run-time
 * error to retain a pointer returned by [[getline_]]
 * after a subsequent call to [[getline_]]. A client
 * that needs to save input characters must copy the
 * result of [[getline_]] before calling [[getline_]]
 * again.\intlabelLinestream\intlabelgetline_
 * <shared type definitions>=
 */
typedef struct Linestream *Linestream;
/*
 * <shared type definitions>=
 */
typedef struct Parlist *Parlist; /* list of Par */
/*
 * This simple structure reflects the concrete syntax of
 * Impcore, micro-Scheme, and the other bridge
 * languages. It's simple because I've stolen the simple
 * concrete syntax that John McCarthy developed for \
 * lisp. Simple syntax is represented by a simple data
 * structure.
 */

/*
 * Interface to Parstream
 * 
 * A [[Parstream]] is an abstract type. \intlabel
 * Parstream
 * <shared type definitions>=
 */
typedef struct Parstream *Parstream;
/*
 * Buffering characters
 * 
 * Alas, when [[runerror]] detects a run-time error,
 * it's not OK for it to write a message to standard
 * error: if [[runerror]] is called during a
 * [[check-error]] test, then the test passes as
 * expected, and no output should be written. To control
 * the output from runerror, the error message is
 * written into a resizeable buffer, by function
 * [[bprint]] or [[vbprint]] (chunks [->] and [->]).
 * The contents are printed only when the dynamic
 * context warrants it. The buffer abstraction has type
 * [[Printbuf]], and it is declared and implemented
 * here.
 * <shared type definitions>=
 */
typedef struct Printbuf *Printbuf;
/*
 * <shared type definitions>=
 */
/*
 * The type [[va_list_box]] is almost, but not quite, a
 * standard C type for holding a variable number of
 * arguments. A function that can accept a variable
 * number of arguments is called variadic, and according
 * to the C standard, the arguments of a variadic
 * function are stored in an object of type [[va_list]],
 * which is defined in the standard library in header
 * file stdarg.h. (If you are not accustomed to variadic
 * functions and [[stdarg.h]], you may wish to consult
 * Sections 7.2 and 7.3 of \citeNNkernighan:c:2.)
 * So what is [[va_list_box]]? It's a workaround for a
 * bug that afflicts some versions of the GNU C compiler
 * on 64-bit hardware. These compilers fail when values
 * of type [[va_list]] are passed as arguments. [Library
 * functions such as [[vfprintf]] itself are OK; only
 * users cannot write functions that take [[va_list]]
 * arguments. Feh.] \codeindexva-list-box@va_list_box
 * A workaround for this problem is to place the
 * [[va_list]] in a structure and pass a pointer to the
 * structure. That structure is called [[va_list_box]],
 * and it is defined here:
 * <definition of [[va_list_box]]>=
 */
typedef struct va_list_box {
  va_list ap;
} va_list_box;
typedef void Printer(Printbuf output, va_list_box *args);
/*
 * Interface to infrastructure: Streams of definitions
 * 
 * The details of reading characters and converting them
 * to abstract syntax are interesting, but they are more
 * relevant to study of compiler construction than to
 * study of programming languages. From the
 * programming-language point of view, all we need to
 * know is that the evaluator needs a source of extended
 * definitions, which the parser can provide. The
 * details appear in \crefcinterps.chap.
 * 
 * \qbreak A source of extended definitions is called an
 * [[XDefstream]].
 * <shared type definitions>=
 */
typedef struct XDefstream *XDefstream;
/*
 * <shared type definitions>=
 */
typedef enum Prompts { NOT_PROMPTING, PROMPTING } Prompts;
/*
 * <shared type definitions>=
 */
typedef enum Echo { NOT_ECHOING, ECHOING } Echo;
/*
 * The conversion specifications shown in \cref
 * impcore.chap are installed when the interpreter
 * launches (chunk [[<<install conversion specifications
 * for [[print]] and [[fprint]]>>]]). The details,
 * including the definition of [[Printer]], are in
 * Sections [->] and [->].
 * 
 * Source locations and error signaling
 * 
 * An error message is often associated with a
 * particular location in the source code, of type
 * [[Sourceloc]]. Values of type [[Sourceloc]] are
 * created by the parsing infrastructure described in \
 * crefcparse.chap, which is the place from which
 * [[synerror]] is called. \intlabelSourceloc
 * <shared type definitions>=
 */
typedef struct Sourceloc *Sourceloc;
/*
 * The possibility of printing source-code locations
 * complicates the interface to the error module. When
 * an interpreter is reading code interactively,
 * printing source-code locations is silly—if there's a
 * syntax error, it's in what you just typed. But if the
 * interpreter is reading code from a file, you'd like
 * to know the file's name and the number of the line
 * containing the bad syntax. The error module doesn't
 * know where the interpreter is reading code from—only
 * the [[main]] function in \chunkrefimpcore.chunk.main
 * knows that. So the error module has to be told how
 * syntax errors should be formatted: with locations or
 * without. \intlabelErrorFormat
 * <shared type definitions>=
 */
typedef enum ErrorFormat { WITH_LOCATIONS, WITHOUT_LOCATIONS } ErrorFormat;
/*
 * <shared type definitions>=
 */
typedef enum TestResult { TEST_PASSED, TEST_FAILED } TestResult;

/*
 * <structure definitions for \uscheme>=
 */
struct Lambda { Namelist formals; Exp body; }; 
struct Value {
    Valuealt alt;
    union {
        Name sym;
        int32_t num;
        bool boolv;
        struct { Value *car; Value *cdr; } pair;
        struct { Lambda lambda; Env env; } closure;
        struct { int tag; Primitive *function; } primitive;
    } ;
};

/*
 * <structure definitions for \uscheme>=
 */
struct Def {
    Defalt alt;
    union {
        struct { Name name; Exp exp; } val;
        Exp exp;
        struct { Name name; Lambda lambda; } define;
        Deflist defs;
    } ;
};

struct Exp {
    Expalt alt;
    union {
        Value literal;
        Name var;
        struct { Name name; Exp exp; } set;
        struct { Exp cond; Exp truex; Exp falsex; } ifx;
        struct { Exp cond; Exp body; } whilex;
        Explist begin;
        struct { Exp fn; Explist actuals; } apply;
        struct { Letkeyword let; Namelist xs; Explist es; Exp body; } letx;
        Lambda lambdax;
    } ;
};

struct XDef { XDefalt alt; union { Def def; Name use; UnitTest test; } ; };

struct UnitTest {
    UnitTestalt alt;
    union {
        struct { Exp check; Exp expect; } check_expect;
        Exp check_assert;
        Exp check_error;
    } ;
};

/*
 * <structure definitions for \uscheme>=
 */
struct Parlist {
   Par hd;
   struct Parlist *tl;
};

struct Namelist {
   Name hd;
   struct Namelist *tl;
};

struct UnitTestlist {
   UnitTest hd;
   struct UnitTestlist *tl;
};

struct Explist {
   Exp hd;
   struct Explist *tl;
};

struct Deflist {
   Def    /*OMIT*/ hd;
   struct Deflist *tl;
};

struct Valuelist {
   Value hd;
   struct Valuelist *tl;
};

/*
 * \qbreak
 * 
 * A parser for micro-Scheme
 * 
 * micro-Scheme is parsed using the shift-reduce parsing
 * technology that is also used for Impcore (\cref
 * cparse.chap).
 * 
 * Parsing tables and reduce functions
 * 
 * A shift-reduce parser must be able to parse any
 * syntactic component that can appear in a micro-Scheme
 * program. micro-Scheme includes all the components
 * used to parse Impcore, plus a [[Value]] component
 * that can appear in a quoted S-expression.
 * <structure definitions for \uscheme>=
 */
struct Component {
    Exp exp;
    Explist exps;
    Name name;
    Namelist names;
    Value value;
    /*
     * The micro-Scheme parser is set up to be extended by
     * adding definitions to the following Noweb chunks. [*]
     * <fields of \uscheme\ [[Component]] added in exercises>=
     */
    /* if implementing COND, add a question-answer field here */
    /*
     * \lisp's original conditional
     * 
     * <fields of \uscheme\ [[Component]] added in exercises>=
     */
    // for COND:
    struct qa_pairs { Explist questions; Explist answers; } qa_pairs;
};
/*
 * <shared structure definitions>=
 */
struct Par { Paralt alt; union { Name atom; Parlist list; } ; }; 
/*
 * Parser state and shift functions
 * 
 * [*] A table-driven parser converts an input
 * [[Parlist]] into components. There are at most
 * [[MAXCOMPS]] components. (The value of [[MAXCOMPS]]
 * must be at least the number of children that can
 * appear in any node of any abstract-syntax tree.
 * To support \exrefpageimpcore.ex.localvars, which has
 * four components in the [[define]] form, I set
 * [[MAXCOMPS]] to 4.) Inputs and components both go
 * into a data structure. And if no programmer ever made
 * a mistake, inputs and components would be enough. But
 * because programmers do make mistakes, the data
 * structure includes additional context, which can be
 * added to an error message. The context I use includes
 * the concrete syntax being parsed, the location where
 * it came from, and the keyword or function name
 * involved, if any. \implabelSourceloc\intlabel
 * ParserState\intlabelParsingContext
 * <shared structure definitions>=
 */
#define MAXCOMPS 4 // max # of components in any syntactic form
struct ParserState {
    int nparsed;       // number of components parsed so far
    struct Component components[MAXCOMPS];  // those components
    Parlist input;     // the part of the input not yet parsed

    struct ParsingContext {   // context of this parse
        Par par;       // the original thing being parsed
        struct Sourceloc {
            int line;                // current line number
            const char *sourcename;  // where the line came from
        } *source;
        Name name;     // a keyword, or name of a function being defined
    } context;
};
/*
 * \qbreak
 * 
 * Representing and parsing tables and rows
 * 
 * The shift functions defined above go into rows. As
 * shown in \cref
 * cparse.fig.exptable,cparse.fig.xdeftable on \cpageref
 * cparse.fig.exptable,cparse.fig.xdeftable, a row needs
 * a keyword, a code, and a sequence of components. The
 * sequence of components is represented as an array of
 * shift functions ending in [[stop]].
 * <shared structure definitions>=
 */
struct ParserRow {
    const char *keyword;
    int code;
    ShiftFun *shifts;  // points to array of shift functions
};
/*
 * <shared structure definitions>=
 */
struct Linestream {
    char *buf;               // holds the last line read
    int bufsize;             // size of buf

    struct Sourceloc source; // where the last line came from
    FILE *fin;               // non-NULL if filelines
    const char *s;           // non-NULL if stringlines
};
/*
 * The rest of the [[Linestream]] structure stores
 * mutable state characterizing the source from which
 * lines come:
 * 
 *   • The [[source]] field tracks the location of the
 *  line currently in [[buf]].
 *   • The [[fin]] field, if the stream is built from a
 *  file, contains the pointer to that file's handle.
 *  Otherwise [[fin]] is [[NULL]].
 *   • The [[s]] field, if the stream is built from a
 *  string, points to the characters of that string
 *  that have not yet been converted to lines.
 *  Otherwise [[s]] is [[NULL]].
 * 
 */


/*
 * <function prototypes for \uscheme>=
 */
Lambda mkLambda(Namelist formals, Exp body);
Value mkSym(Name sym);
Value mkNum(int32_t num);
Value mkBoolv(bool boolv);
Value mkNil(void);
Value mkPair(Value *car, Value *cdr);
Value mkClosure(Lambda lambda, Env env);
Value mkPrimitive(int tag, Primitive *function);
/*
 * <function prototypes for \uscheme>=
 */
Def mkVal(Name name, Exp exp);
Def mkExp(Exp exp);
Def mkDefine(Name name, Lambda lambda);
Def mkDefs(Deflist defs);
struct Def mkValStruct(Name name, Exp exp);
struct Def mkExpStruct(Exp exp);
struct Def mkDefineStruct(Name name, Lambda lambda);
struct Def mkDefsStruct(Deflist defs);
Exp mkLiteral(Value literal);
Exp mkVar(Name var);
Exp mkSet(Name name, Exp exp);
Exp mkIfx(Exp cond, Exp truex, Exp falsex);
Exp mkWhilex(Exp cond, Exp body);
Exp mkBegin(Explist begin);
Exp mkApply(Exp fn, Explist actuals);
Exp mkLetx(Letkeyword let, Namelist xs, Explist es, Exp body);
Exp mkLambdax(Lambda lambdax);
struct Exp mkLiteralStruct(Value literal);
struct Exp mkVarStruct(Name var);
struct Exp mkSetStruct(Name name, Exp exp);
struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex);
struct Exp mkWhilexStruct(Exp cond, Exp body);
struct Exp mkBeginStruct(Explist begin);
struct Exp mkApplyStruct(Exp fn, Explist actuals);
struct Exp mkLetxStruct(Letkeyword let, Namelist xs, Explist es, Exp body);
struct Exp mkLambdaxStruct(Lambda lambdax);
XDef mkDef(Def def);
XDef mkUse(Name use);
XDef mkTest(UnitTest test);
struct XDef mkDefStruct(Def def);
struct XDef mkUseStruct(Name use);
struct XDef mkTestStruct(UnitTest test);
UnitTest mkCheckExpect(Exp check, Exp expect);
UnitTest mkCheckAssert(Exp check_assert);
UnitTest mkCheckError(Exp check_error);
struct UnitTest mkCheckExpectStruct(Exp check, Exp expect);
struct UnitTest mkCheckAssertStruct(Exp check_assert);
struct UnitTest mkCheckErrorStruct(Exp check_error);
/*
 * <function prototypes for \uscheme>=
 */
int     lengthPL(Parlist ps);
Par     nthPL   (Parlist ps, unsigned n);
Parlist mkPL    (Par p, Parlist ps);
Parlist popPL   (Parlist ps);
Printer printparlist;

int      lengthNL(Namelist ns);
Name     nthNL   (Namelist ns, unsigned n);
Namelist mkNL    (Name n, Namelist ns);
Namelist popNL   (Namelist ns);
Printer  printnamelist;

int          lengthUL(UnitTestlist us);
UnitTest     nthUL   (UnitTestlist us, unsigned n);
UnitTestlist mkUL    (UnitTest u, UnitTestlist us);
UnitTestlist popUL   (UnitTestlist us);
Printer      printunittestlist;

int     lengthEL(Explist es);
Exp     nthEL   (Explist es, unsigned n);
Explist mkEL    (Exp e, Explist es);
Explist popEL   (Explist es);
Printer printexplist;

int     lengthDL(Deflist ds);
Def    /*OMIT*/ nthDL   (Deflist ds, unsigned n);
Deflist mkDL    (Def    /*OMIT*/ d, Deflist ds);
Deflist popDL   (Deflist ds);
Printer printdeflist;

int       lengthVL(Valuelist vs);
Value     nthVL   (Valuelist vs, unsigned n);
Valuelist mkVL    (Value v, Valuelist vs);
Valuelist popVL   (Valuelist vs);
Printer   printvaluelist;

/*
 * Parsing parenthesized phrases (including Impcore)
 * in C
 * 
 * [*][*] \invisiblelocaltableofcontents[*]
 * 
 * A key step in the implementation of any programming
 * language is to translate the concrete syntax that
 * appears in the input to the abstract syntax that is
 * used internally. This translation is typically
 * implemented in two steps: lexical analysis groups
 * related characters into tokens, and parsing
 * translates a sequence of tokens into one or more
 * abstract-syntax trees. In the second part of this
 * book, starting with \crefmlscheme.chap, interpreters
 * are written in Standard ML, and they follow exactly
 * this model. But in the first part, where interpreters
 * are written in C, follows a different model:
 * sequences of lines are turned into parenthesized
 * phrases (\crefcinterps.parstream), and these phrases
 * are what is parsed into abstract syntax. The details
 * are the subject of this appendix.
 * 
 * Considering what I hope you'll get out of this book,
 * the implementation of a parser is a side issue.
 * Parsing is an art and a science all its own, and
 * it is the subject of its own learned textbooks. Using
 * parenthesized phrases enables me to avoid the usual
 * challenges and complexities. In their place, however,
 * I have one challenge that is central to what I hope
 * you get out of this book—to get the most out of the
 * Exercises, you have to be able to add new syntactic
 * forms. Using the parser I describe below, adding new
 * syntactic forms is relatively easy: you add new
 * entries to a couple of tables and a new case to a
 * [[switch]] statement in a syntax-building function.
 * But there is a cost: there's a lot of infrastructure
 * to understand. Infrastructure is easier to understand
 * if you can see how it's used, so along with the
 * general parsing infrastructure, I present the code
 * used to parse Impcore. But if you want to avoid
 * studying infrastructure and just get on with adding
 * new syntax, jump to the example and checklist in \
 * crefpage(cparse.conditional-sugar.
 * 
 * The parser in this appendix is easy for you to
 * extend, and it happens to be reasonably efficient,
 * but regrettably, it is not simple. However, it is
 * based on classic ideas developed by \citet
 * knuth:translation-left-right, so if you study it, you
 * will have a leg up on the ``LR parsers'' which so
 * dominated the second half of the twentieth century. [
 * Given the severe memory constraints imposed by
 * machines of the 1970s, LR-parser generators like Yacc
 * and Bison were brilliant innovations. In~the 21st
 * century, we~have memory to burn, and you are better
 * off choosing a parsing technology that will enable
 * you to spend more time getting work done and less
 * time engineering your grammar. But I digress.]
 */

/*
 * <function prototypes for \uscheme>=
 */
Value *find(Name name, Env env);
/*
 * \qtrim1
 * 
 * A location is allocated, initialized, and bound to a
 * name in one step, by function [[bindalloc]].
 * Formally, when called with store sigma, bindalloc(x,
 * v, rho) chooses an \aloc\notindom sigma, updates the
 * store to be sigma{\aloc|->v}, and returns the
 * extended environment \nomathbreakrho{x |->\aloc}. \
 * scmflabelbindalloc,bindalloclist
 * <function prototypes for \uscheme>=
 */
Env bindalloc    (Name name,   Value v,      Env env);
Env bindalloclist(Namelist xs, Valuelist vs, Env env);
/*
 * Calling bindalloclist(<\ldotsnx>, <\ldotsnv>, rho)
 * does the same job for a list of values, returning rho
 * {x_1 |->\aloc_1, ..., x_n |->\aloc_n}, where \ldotsn\
 * aloc are fresh locations, which [[bindalloclist]]
 * initializes to values \ldotsnv.
 * 
 * Although environments use a different interface from
 * Impcore, their representation is unchanged. In both
 * interpreters, an environment is represented using
 * mutable state; that is, the environment associates
 * each name with a pointer to a location that can be
 * assigned to. Impcore's interface hides the use of
 * locations, but micro-Scheme's interface exposes them.
 * As a result, [[find]] can do the work of both
 * [[isvalbound]] and [[fetchval]].
 * 
 *  \qquadImpcore      \qquadmicro-Scheme
 *  isvalbound(x, rho) find(x, rho) !=NULL
 *  fetchval(x, rho)   *find(x, rho)
 * 
 * Using this interface, a micro-Scheme variable can be
 * mutated without using a function like Impcore's
 * [[bindval]]; code just assigns to the location
 * returned by [[find]]. As long as x in dom rho,
 * bindval(x, v, rho) is replaced by *find(x, rho) [[=]]
 * v. Leveraging C pointers and mutable locations makes
 * micro-Scheme's interface simpler than Impcore's.
 * 
 * Allocation
 * 
 * The fresh locations created by [[bindalloc]] and
 * [[bindalloclist]] come from [[allocate]]. Calling
 * [[allocate(v)]] finds a location \aloc\notindom sigma
 * , stores [[v]] in \aloc (thereby updating sigma), and
 * returns \aloc.[*]
 */

/*
 * <function prototypes for \uscheme>=
 */
Value *allocate(Value v);
/*
 * Allocation is described in great detail in Chapter 
 * [->].
 */

/*
 * Values
 * 
 * [*] Values are represented as specified by the data
 * definition in chunk [->]. For convenience, values [[#
 * t]] and [[#f]] are always available in C variables
 * [[truev]] and [[falsev]].
 * <function prototypes for \uscheme>=
 */
extern Value truev, falsev;
/*
 * Values can be tested for truth or falsehood by
 * function [[istrue]]; any value different from [[#f]]
 * is regarded as true.
 * <function prototypes for \uscheme>=
 */
bool istrue(Value v);
/*
 * When an unspecified value is called for by the
 * semantics, one can be obtained by calling function
 * [[unspecified]].
 * <function prototypes for \uscheme>=
 */
Value unspecified(void);
/*
 * If you get the micro-Scheme interpreter to crash,
 * your micro-Scheme code is probably looking at a value
 * returned by [[unspecified]]. That's an unchecked
 * run-time error.
 * 
 * Evaluation
 * 
 * As in Impcore, the --> relation in the operational
 * semantics is implemented by [[evaldef]], and the ==>
 * relation is implemented by [[eval]]. The store sigma
 * is not passed or returned explicitly; it is
 * represented by the C store, i.e., by the contents of
 * memory. Because sigma is single-threaded—every store
 * is used exactly once and then discarded—the semantics
 * can be implemented by updating memory in place.
 * For example, \monoboxeval(e, rho), when evaluated
 * with store sigma, finds a v and a sigma' such that \
 * nomathbreak\evale ==>\evalr[']v, updates the store to
 * be sigma', and returns v.
 */

/*
 * <function prototypes for \uscheme>=
 */
Value eval   (Exp e, Env rho);
Env   evaldef(Def d, Env rho, Echo echo);
/*
 * Similarly, \monoboxevaldef(e, rho, echo), when
 * evaluated with store sigma, finds a rho' and a sigma'
 * such that \evaldefe -->\evaldefr', updates the store
 * to be sigma', and returns rho'. If [[echo]] is
 * [[ECHOING]], [[evaldef]] also prints the name or
 * value of whatever expression is evaluated or added
 * to rho.
 */

/*
 * <function prototypes for \uscheme ((elided))>=
 */
Exp desugarLetStar(Namelist xs, Explist es, Exp body);
Exp desugarLet    (Namelist xs, Explist es, Exp body);
/*
 * The desugared code works just as well as
 * micro-Scheme's core code—and you can prove it (\cref
 * scheme.ex.let-sugar-ok,scheme.ex.letstar-sugar-ok).
 * 
 * Finally, a [[letrec]] can be desugared into a [[let]]
 * expression that introduces all the variables, which
 * is followed by a sequence of assignments: --- \mono
 * (let ([x_1 \unspec] ... [x_n \unspec])
 * --- \mono(begin (set x_1 e_1) ... (set x_n e_n)
 * --- \monoe)).
 * {indented} \letrecsugardisplay This translation works
 * only when each e_i is a [[lambda]] expression, as
 * required by the operational semantics.
 * 
 * Syntactic sugar for \texttt{\upshape cond}\protect\
 * nochap{ (\lisp's original conditional form)}
 * 
 * [*]
 * 
 * micro-Scheme's conditional expression, written using
 * [[if]], allows for only two alternatives. But real
 * programs often choose among three or more
 * alternatives. For some such choices, C and the \algol
 * -like languages offer a [[switch]] statement, but it
 * can choose only among integer values that are known
 * at compile time—typically enumeration literals.
 * A more flexible multi-way choice is offered by a
 * syntactic form from McCarthy's original \lisp: the
 * [[cond]] expression. A [[cond]] expression contains
 * an arbitrarily long sequence of question-answer pairs
 * : one for each choice. I like [[cond]] more than 
 * [[if]] because [[cond]] makes it obvious how many
 * alternatives there are and what each one is doing.
 */

/*
 * Additional interfaces
 * 
 * Allocation
 * 
 * Before the first call to [[allocate]], a client must
 * call [[initallocate]]. For reasons that are discussed
 * in \chaprefgc, [[initallocate]] is given a pointer to
 * the environment containing the global variables.
 * <function prototypes for \uscheme>=
 */
void initallocate(Env *globals);
/*
 * Values
 * 
 * Before executing any code that refers to [[truev]] or
 * [[falsev]], client code must call [[initvalue]].
 * <function prototypes for \uscheme>=
 */
void initvalue(void);
/*
 * Read-eval-print loop
 * 
 * [*] As in the Impcore interpreter, a sequence of
 * extended definitions is handled by [[readevalprint]].
 * In principle, [[readevalprint]] ought to look a lot
 * like [[evaldef]]. In particular, [[readevalprint]]
 * ought to take an environment and return an
 * environment. But when an error occurs,
 * [[readevalprint]] doesn't actually return; it calls
 * [[synerror]] or [[runerror]]. And if an error occurs,
 * the interpreter shouldn't forget the definitions that
 * preceded it. So instead of returning a new
 * environment, [[readevalprint]] writes the new
 * environment through an environment pointer [[envp]],
 * which is passed as a parameter.
 * <function prototypes for \uscheme>=
 */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo);
/*
 * Primitives
 * 
 * micro-Scheme's primitives are installed by function
 * [[addprimitives]], which mutates an existing
 * environment pointer by adding a binding to each
 * primitive operation.
 * <function prototypes for \uscheme>=
 */
void addprimitives(Env *envp);
/*
 * Printing
 * 
 * Functions that print micro-Scheme's syntax and values
 * are declared as follows:
 * <function prototypes for \uscheme>=
 */
void printenv    (Printbuf, va_list_box*);
void printvalue  (Printbuf, va_list_box*);
void printexp    (Printbuf, va_list_box*);
void printdef    (Printbuf, va_list_box*);
void printlambda (Printbuf, va_list_box*);
/*
 * Unit tests are run by code in \crefschemea.testing.
 * <function prototypes for \uscheme>=
 */
void process_tests(UnitTestlist tests, Env rho);
/*
 * These primitives are implemented by function
 * [[binary]], which delegates to functions [[cons]] and
 * [[equalatoms]]. [*]
 * <function prototypes for \uscheme>=
 */
Value cons(Value v, Value w);
Value equalatoms(Value v, Value w);
/*
 * New parsing functions: S-expressions and bindings
 * 
 * Each new shift function is supported by a new parsing
 * function.
 * <function prototypes for \uscheme>=
 */
Value parsesx(Par p, Sourceloc source);
struct Component parseletbindings(ParsingContext context, Parlist input);
/*
 * Function [[number_of_good_tests]] runs each test,
 * last test first, and counts the number that pass.
 * So it can catch errors during testing, it expects the
 * error mode to be [[TESTING]]; calling
 * [[number_of_good_tests]] when the error mode is
 * [[NORMAL]] is an unchecked run-time error. Again,
 * except for the environment, it's just like the
 * Impcore version.
 * <function prototypes for \uscheme>=
 */
int number_of_good_tests(UnitTestlist tests, Env rho);
/*
 * And except for the environment, [[test_result]] is
 * just like the Impcore version.
 * <function prototypes for \uscheme>=
 */
TestResult test_result(UnitTest t, Env rho);
/*
 * \qtrim2.5 The equality check in [[check-expect]] is
 * implemented by function [[equalpairs]]. It resembles
 * function [[equalatoms]] (\chunkref
 * scheme.chunk.equalatoms), which implements the
 * primitive [[=]], with two differences:
 * 
 *  \tightlist
 *   • Its semantics are those of [[equal?]], not [[=]].
 *   • Instead of returning a micro-Scheme Boolean
 *  represented as a C [[Value]], it returns a
 *  Boolean represented as a C [[bool]].
 * 
 * [*]
 * <function prototypes for \uscheme>=
 */
bool equalpairs(Value v, Value w);
/*
 * In micro-Scheme, expressions under test don't have to
 * be lowered: [[testexp]] returns its argument
 * unchanged.
 * <function prototypes for \uscheme>=
 */
Exp testexp(Exp);
/*
 * \qbreak
 * 
 * Support for an exercise: \
 * chaptocbacksplitConcatenating names
 * 
 * If you implement micro-Scheme's syntactic sugar for
 * records (\schemexpagerecord-sugar), you will find a
 * use for function [[namecat]], which concatenates
 * names. (The names of record-accessor and
 * record-constructor functions are formed by
 * concatenation.)
 * <function prototypes for \uscheme>=
 */
Name namecat(Name n1, Name n2);
/*
 * <function prototypes for \uscheme>=
 */
Exp desugarAnd(Explist args);
/*
 * <function prototypes for \uscheme>=
 */
Namelist freevars(Exp e, Namelist bound, Namelist free);
/*
 * <function prototypes for \uscheme>=
 */
Exp desugarOr(Explist args);
/*
 * <function prototypes for \uscheme>=
 */
Exp desugarCond(Explist questions, Explist answers);
/*
 * <function prototypes for \uscheme>=
 */
Deflist desugarRecord(Name recname, Namelist fieldnames);
/*
 * A name may be built from a string or converted to a
 * string.
 * <shared function prototypes>=
 */
Name strtoname(const char *s);
const char *nametostr(Name x);
/*
 * These functions satisfy the following algebraic laws:
 * 
 *  \monoboxstrcmp(s, nametostr(strtoname(s))) == 0
 *  \monoboxstrcmp(s, t) == 0 if and only if \monobox
 *  strtoname(s) == strtoname(t)
 * 
 * The first law says if you build a name from a string,
 * [[nametostr]] returns a copy of your original string.
 * The second law says you can compare names using
 * pointer equality.
 * 
 * Because [[nametostr]] returns a string of type \
 * monoboxconst char*, a client of [[nametostr]] cannot
 * modify that string without subverting the type
 * system. Modification of the string is an unchecked
 * run-time error. New values of type [[Name*]] should
 * be created only by calling [[strtoname]]; to do so by
 * casting other pointers is a subversion of the type
 * system and an unchecked run-time error.
 * 
 * Interface to values
 * 
 * The value interface defines the type of value that an
 * expression may evaluate to. In Impcore, that is
 * always a 32-bit integer. A [[Valuelist]] is a list of
 * [[Value]]s.\iilabelValue
 */

/*
 * Definitions and function prototypes for all the list
 * types can be found in the interpreter's all.h file.
 * Because of the repetition, this code is tedious to
 * read, but generating the code automatically makes the
 * tedium bearable. And ML's polymorphism enables a
 * simpler solution (Chapter [->]).
 * 
 * Interface to infrastructure: Printing
 * 
 * [*] After evaluating a definition, the interpreter
 * prints a name or a value. And when an error occurs,
 * the interpreter may need to print a faulty expression
 * or definition. Strings and numbers can easily be
 * printed using [[printf]], but expressions and
 * definitions can't. So instead, the interpreter uses
 * functions [[print]] and [[fprint]], which replace
 * [[printf]] and [[fprintf]]. These functions, which
 * are defined in the Supplement, support direct
 * printing of [[Exp]]s, [[Def]]s, [[Name]]s, and so on.
 * \intlabelprint\intlabelfprint
 * <shared function prototypes>=
 */
void print (const char *fmt, ...);  // print to standard output
void fprint(FILE *output, const char *fmt, ...);  // print to given file
/*
 * By design, [[print]] and [[fprint]] resemble
 * [[printf]] and [[fprintf]]: the [[fmt]] parameter is
 * a ``format string'' that contains ``conversion
 * specifications.'' Our conversion specifications are
 * like those used by [[printf]], but much simpler.
 * A conversion specification is two characters:
 * a percent sign followed by a character like [[d]] or 
 * [[s]], which is called a conversion specifier. Unlike
 * standard conversion specifications, ours don't
 * contain minus signs, numbers, or dots. The ones used
 * in the Impcore interpreter are shown here in \cref
 * impcore.conversion-specifiers. By convention,
 * lowercase specifiers print individual values;
 * uppercase specifiers print lists. Most specifiers are
 * named for the initial letter of what they print, but
 * the specifier for a [[Def]] must not be [[ firmly
 * established as a specifier for printing decimal
 * integers. Instead, [[Def]] is specified by [[
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 * [[ [[ [[ [[ [[ [[ [[ [[ [[ [[ [[ where definitions
 * appear)
 * [[ [[
 * 
 * Conversion specifiers for impcore [*]
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 * Functions [[print]] and [[fprint]] are unsafe; if you
 * pass an argument that is not consistent with the
 * corresponding conversion specifier, it is an
 * unchecked run-time error.
 * 
 * Interface to infrastructure: Error handling
 * 
 * [*] When it encounters a fault, the Impcore
 * interpreter complains and recovers by calling a
 * function in an error-handling interface. In general,
 * a fault occurs \qbreak whenever a program is ill
 * formed, ill typed, or ill behaved, but Impcore has no
 * static type system, so faults are triggered only by
 * ill-formed and ill-behaved programs:
 * 
 *   • When it detects an ill-formed program during
 *  parsing, the interpreter signals a syntax error
 *  by calling [[synerror]].
 *   • When it detects an ill-behaved program at run
 *  time, the interpreter signals a run-time error by
 *  calling [[runerror]].
 * 
 * Before initiating error recovery, each
 * error-signaling function prints a message. For that
 * reason, an error-signaling function's interface
 * resembles [[print]]. But because different
 * information is available at parse time and at run
 * time, [[synerror]] and [[runerror]] have different
 * interfaces.
 * 
 * The simpler of the two is [[runerror]]. During normal
 * operation, [[runerror]] prints to standard error and
 * then [[longjmp]]s to [[errorjmp]]. [*]\intlabel
 * runerror
 * <shared function prototypes>=
 */
__noreturn // OMIT
void runerror (const char *fmt, ...);
extern jmp_buf errorjmp;        // longjmp here on error
/*
 * During unit testing, [[runerror]] operates in testing
 * mode, and it behaves a little differently (\crefpage
 * ,cinterps.runerror).
 * 
 * Function [[synerror]] is like [[runerror]], except
 * that before its format string, it takes an argument
 * of type [[Sourceloc]], which tracks the source-code
 * location being read at the time of the error.
 * The location can be printed as part of the error
 * message. [*]
 * <shared function prototypes>=
 */
__noreturn // OMIT
void synerror (Sourceloc src, const char *fmt, ...);
/*
 * Error handling, as opposed to error signaling,
 * is implemented by calling [[setjmp]] on [[errorjmp]].
 * Function [[setjmp]] must be called before any
 * error-signaling function. It is an unchecked run-time
 * error to call [[runerror]] or [[synerror]] except
 * when a [[setjmp]] involving [[errorjmp]] is active on
 * the C call stack.
 * 
 * [*] One common run-time error is that an Impcore
 * function is called with the wrong number of
 * arguments. That error is detected by function
 * [[checkargc]]. Its parameter [[e]] holds the call in
 * which the error might occur. \intlabelcheckargc
 * <shared function prototypes>=
 */
void checkargc(Exp e, int expected, int actual);
/*
 * <shared function prototypes>=
 */
Par mkAtom(Name atom);
Par mkList(Parlist list);
struct Par mkAtomStruct(Name atom);
struct Par mkListStruct(Parlist list);
/*
 * Planning an extensible parser
 * 
 * A parser is a function that is given a [[Par]] and
 * builds an abstract-syntax tree, which it then
 * returns. Each of the first three bridge languages
 * (Impcore, micro-Scheme, and \uschemeplus) has two
 * major syntactic categories, which means two types of
 * abstract-syntax trees, which means two parsers.
 * <shared function prototypes>=
 */
Exp  parseexp (Par p, Sourceloc source);
XDef parsexdef(Par p, Sourceloc source);
/*
 * Each parser also takes a pointer to a source-code
 * location, which it uses if it has to report an error.
 * 
 * A parser gets a parenthesized phrase of type [[Par]]
 * and builds an abstract-syntax tree. In this appendix,
 * I call the [[Par]] an input and the abstract-syntax
 * tree a component. Components include all the elements
 * that go into an abstract-syntax trees; in Impcore,
 * a component can be a name, a list of names,
 * an expression, or a list of expressions.
 * 
 * Parsing begins with a look at the input, which is
 * either an [[ATOM]] or a [[LIST]] of [[Par]]s. And the
 * interpretation of the input depends on whether the
 * parser is looking for an [[Exp]] or an [[XDef]].
 * 
 *   • If the input is an [[ATOM]], the parser must be
 *  looking for an expression (in Impcore, a [[VAR]]
 *  or [[LITERAL]] expression), and the job of making
 *  it into an [[Exp]] is given to function
 *  [[exp_of_atom]], which is language-dependent.
 * <shared function prototypes>=
 */
Exp exp_of_atom(Sourceloc loc, Name atom);
/*
 * If you're a seasoned C programmer, you might think
 * that the ``right'' representation of the component
 * abstraction is a [[union]], not a [[struct]]. But
 * unions are unsafe. By using a struct, I give myself a
 * fighting chance to debug the code. If I make a
 * mistake and pick the wrong component,
 * a memory-checking tool like Valgrind (\crefpage
 * ,valgrind) will detect the error.
 * 
 * The standard reduce functions are [[reduce_to_exp]]
 * and [[reduce_to_xdef]]. They take similar arguments:
 * \qbreak the  first argument codes for what kind of
 * node the components should be reduced to, and
 * the second argument points to an array that holds the
 * components.
 * <shared function prototypes>=
 */
Exp  reduce_to_exp (int alt, struct Component *components);
XDef reduce_to_xdef(int alt, struct Component *components);
/*
 * <shared function prototypes>=
 */
struct ParserState mkParserState(Par p, Sourceloc source);
/*
 * The four basic shift functions shift expressions and
 * names:
 * <shared function prototypes>=
 */
ParserResult sExp     (ParserState state);  // shift 1 input into Exp
ParserResult sExps    (ParserState state);  // shift all inputs into Explist
ParserResult sName    (ParserState state);  // shift 1 input into Name
ParserResult sNamelist(ParserState state);  // shift 1 input into Namelist
/*
 * <shared function prototypes>=
 */
void halfshift(ParserState state); // advance input, check for room in output
/*
 * <shared function prototypes>=
 */
Explist parseexplist(Parlist p, Sourceloc source);
/*
 * <shared function prototypes>=
 */
Name parsename(Par p, ParsingContext context);
/*
 * <shared function prototypes>=
 */
ParserResult stop(ParserState state);
/*
 * <shared function prototypes>=
 */
ParserResult setcontextname(ParserState state);
/*
 * <shared function prototypes>=
 */
ParserResult sLocals(ParserState state);
                 // shift locals if [locals x y z ...]
/*
 * <shared function prototypes>=
 */
void rowparse(struct ParserRow *table, ParserState s);
__noreturn /* OMIT */
void usage_error(int alt, ParserResult r, ParsingContext context);
/*
 * <shared function prototypes>=
 */
struct ParserRow *tableparse(ParserState state, ParserTable t);
/*
 * <shared function prototypes>=
 */
ParserResult use_exp_parser(ParserState state);
/*
 * <shared function prototypes>=
 */
int code_of_name(Name n);
/*
 * In addition to syntax errors, parsers can also detect
 * duplicate names. In general, duplicate names can
 * occur in an expression or in an extended definition.
 * <shared function prototypes>=
 */
void check_exp_duplicates(Sourceloc source, Exp e);
void check_def_duplicates(Sourceloc source, Def d);
/*
 * <shared function prototypes>=
 */
char *getline_(Linestream r, const char *prompt);
/*
 * A [[Linestream]] can be created from a string or a
 * file. And when a [[Linestream]] is created, it is
 * given a name for the string or file; that name is
 * used in error messages. [*] [*]
 * <shared function prototypes>=
 */
Linestream stringlines(const char *stringname, const char *s);
Linestream filelines  (const char *filename,   FILE *fin);
/*
 * If an [[s]] passed to [[stringlines]] is nonempty, it
 * is a checked run-time error for it to end in any
 * character except newline. After a call to
 * [[stringlines]], client code must ensure that
 * pointers into [[s]] remain valid until the last call
 * to [[getline_]]. If [[getline_]] is called after the
 * memory pointed to by [[s]] is no longer valid, it is
 * an unchecked run-time error.
 * 
 * \qbreak
 * 
 * Implementation of \upshapeLinestream
 * 
 * A [[Linestream]] owns the memory used to store each
 * line. That memory is pointed to by [[buf]], and its
 * size is stored in [[bufsize]]. \implabelLinestream
 * If no line has been read, [[buf]] is [[NULL]] and
 * [[bufsize]] is zero.
 */

/*
 * To create a [[Parstream]], client code specifies not
 * only the lines from which [[Par]]s will be read but
 * also the prompts to be used (\cpagerefPrompts.int).
 * To get a [[Par]] from a stream, client code calls
 * [[getpar]]. And for error messages, client code can
 * ask a [[Parstream]] for its current source location.
 * \intlabelparstream\intlabelgetpar
 * <shared function prototypes>=
 */
Parstream parstream(Linestream lines, Prompts prompts);
Par       getpar   (Parstream r);
Sourceloc parsource(Parstream pars);
/*
 * The [[Parstream]] interface is completed by global
 * variable [[read_tick_as_quote]]. When 
 * [[read_tick_as_quote]] is true, [[getpar]] turns an
 * input like [['(1 2 3)]] into the parenthesized phrase
 * [[(quote (1 2 3))]]. When set, this variable makes
 * the tick mark behave the way micro-Scheme wants it to
 * behave.
 * <shared function prototypes>=
 */
extern bool read_tick_as_quote;
/*
 * A buffer is created with [[printbuf]] and destroyed
 * with [[freebuf]].
 * <shared function prototypes>=
 */
Printbuf printbuf(void);
void freebuf(Printbuf *);
/*
 * A buffer can be appended to by [[bufput]] or
 * [[bufputs]]; it is emptied by [[bufreset]].
 * <shared function prototypes>=
 */
void bufput(Printbuf, char);
void bufputs(Printbuf, const char*);
void bufreset(Printbuf);
/*
 * The contents of a buffer can be copied to a freshly
 * allocated block of memory, or they can be written to
 * an open file handle.
 * <shared function prototypes>=
 */
char *bufcopy(Printbuf);
void fwritebuf(Printbuf buf, FILE *output);
/*
 * The extensible buffer printer
 * 
 * To recapitulate \crefsec:print-interface, the
 * standard C functions [[printf]] and [[fprintf]] are
 * great, but they don't know how to print things like
 * values and expressions. And when you can't put a
 * value or an expression in a format string, the code
 * needed to print an error message becomes awkward and
 * unreadable. To enable simple, readable printing code,
 * I define new, custom print functions that know how to
 * print values and expressions: [*]
 * <shared function prototypes>=
 */
void print (const char *fmt, ...);                  // print to standard output
void fprint(FILE *output, const char *fmt, ...);    // print to given file
void bprint(Printbuf output, const char *fmt, ...); // print to given buffer
/*
 * I use [[bprint]] to write error messages---if an
 * error message is written during the evaluation of a
 * [[check-expect]] or [[check-error]], the message can
 * be captured and can either be used to explain what
 * went wrong (if an error occurs unexpectedly during a
 * [[check-expect]]) or can be silently discarded (if an
 * error occurs as expected during a [[check-error]]).
 */

/*
 * Function [[irand]] has its own private copy of
 * [[seed]], which only it can access, and which it
 * updates at each call. And function
 * [[repeatable-irand]], which might be used to replay
 * an execution for debugging, has its own private seed.
 * So it repeats the same sequence [1, 14, 131, 160,
 * 421, ...] no matter what happens with [[irand]].
 * 
 * Useful higher-order functions
 * 
 * [*] The [[lambda]] expression does more than just
 * encapsulate mutable state; [[lambda]] helps express
 * and support not just algorithms but also patterns of
 * computation. What a ``pattern of computation'' might
 * be is best shown by example.
 * 
 * One minor example is the function [[mk-rand]]: it can
 * be viewed as a pattern that says ``if you tell me how
 * to get from one number to the next, I can deliver an
 * entire sequence of numbers starting with 1.''
 * This pattern of computation, while handy, is not used
 * often. More useful patterns can make new functions
 * from old functions or can express common ways of
 * programming with lists, like ``do something with
 * every element.'' Such patterns are presented in the
 * next few sections.
 * 
 * Composition
 * 
 * One of the simplest ways to make a new function is by
 * composing two old ones. Function [[o]] (pronounced
 * ``circle'' or ``compose'') returns the composition of
 * two one-argument functions, often written f og.\
 * stdbreak \notation [composed with]ofunction
 * composition Composition is described by the algebraic
 * law (f og)(x) = f(g(x)), and like any function that
 * makes new functions, it returns a [[lambda]]: \
 * basislabelo
 * <shared function prototypes>=
 */
void installprinter(unsigned char specifier, Printer *take_and_print);
/*
 * The function provided has type [[Printer]]. When
 * called, it takes one value out of the list [[args]],
 * then prints the value to the given buffer. [*]\
 * intlabelPrinter
 */

/*
 * The next brick is my function [[vbprint]] and its
 * associated table [[printertab]]. Function [[vbprint]]
 * stands in the same relation to [[bprint]] as standard
 * function [[vfprintf]] stands to [[fprintf]]: [*]
 * <shared function prototypes>=
 */
void vbprint(Printbuf output, const char *fmt, va_list_box *box);
/*
 * Printing functions
 * 
 * [*] The most interesting printing functions are
 * language-dependent; they are found in \cref
 * impcorea.chap,schemea.chap. But functions that print
 * percent signs, strings, decimal integers, characters,
 * and names are shared among all languages, and they
 * are found here.
 * <shared function prototypes>=
 */
Printer printpercent, printstring, printdecimal, printchar, printname, 
        printpointer;
/*
 * The print function for parenthesized phrases is
 * surprisingly simple: it just calls [[bprint]]
 * recursively:
 * <shared function prototypes>=
 */
Printer printpar;
/*
 * In testing mode, [[runerror]] buffers an error
 * message and [[longjmp]]s to [[testjmp]]. \intlabel
 * ErrorMode\intlabelset_error_mode
 * <shared function prototypes>=
 */
typedef enum ErrorMode { NORMAL, TESTING } ErrorMode;
void set_error_mode(ErrorMode mode);
extern jmp_buf testjmp;    // if error occurs during a test, longjmp here
extern Printbuf errorbuf;  // if error occurs during a test, message is here
/*
 * \qvfilbreak3in
 * 
 * Function [[othererror]] generalizes [[runerror]].
 * It's used only in \crefschemes.chap, to signal if an
 * error occurs while \usp is being lowered to Core \usp
 * .
 * <shared function prototypes>=
 */
__noreturn // OMIT
void othererror (const char *fmt, ...);
/*
 * If a list of names contains duplicates,
 * [[duplicatename]] returns a duplicate. It is used to
 * detect duplicate names in lists of formal parameters.
 * Its cost is quadratic in the number of parameters,
 * which for any reasonable function, should be very
 * fast. \intlabelduplicatename \implabelduplicatename
 * <shared function prototypes>=
 */
Name duplicatename(Namelist names);
/*
 * Stack-overflow detection
 * 
 * If somebody writes a recursive Impcore or
 * micro-Scheme function that calls itself forever, what
 * should the interpreter do? An ordinary recursive
 * [[eval]] would call itself forever, and eventually
 * the C code would run out of resources and would be
 * terminated. There's a better way. My implementation
 * of [[eval]] contains a hidden call to a function
 * called [[checkoverflow]], which detects very deep
 * recursion and calls [[runerror]].
 * 
 * The implementation uses C trickery with [[volatile]]
 * variables: the address of a [[volatile]] local
 * variable [[c]] is used as a proxy for the stack
 * pointer. (Because I spent years writing compilers,
 * I understand a little of how these things work.) The
 * first call to [[checkoverflow]] captures the stack
 * pointer and stores as a ``low-water mark.'' Each
 * later call checks the current stack pointer against
 * that low-water mark. If the distance exceeds
 * [[limit]], [[checkoverflow]] calls [[runerror]].
 * Otherwise it returns the distance.
 * <shared function prototypes>=
 */
extern int  checkoverflow(int limit);
extern void reset_overflow_check(void);
/*
 * Arithmetic-overflow detection
 * 
 * Unlike standard C arithmetic, the arithmetic in this
 * book detects arithmetic overflow: an operation on
 * 32-bit signed integers whose result cannot also be
 * represented as a 32-bit signed integer. Such
 * arithmetic is defined by the C standard as
 * ``undefined behavior,'' so my code needs to detect it
 * before it might happen. Function [[checkarith]] does
 * arithmetic using 64-bit integers, and if the result
 * does not fit in the specified number of bits, it
 * triggers a checked run-time error.
 * <shared function prototypes>=
 */
extern void checkarith(char operation, int32_t n, int32_t m, int precision);
/*
 * \qbreak
 * 
 * Unicode support
 * 
 * Unicode is a standard that attempts to describe all
 * the world's character sets. In Unicode, each
 * character is described by a ``code point,'' which is
 * an unsigned integer. Example code points include
 * ``capital A'' (code point 65) and ``capital Å with a
 * circle over it'' (code point 197). Most character
 * sets fit in the Basic Multilingual Plane, whose code
 * points can be expressed as 16-bit unsigned integers.
 * 
 * UTF-8 stands for ``Unicode Transfer Format
 * (8 bits).'' UTF-8 is a variable-length binary code in
 * which each 16-bit code point is coded as a one-byte,
 * two-byte, or three-byte UTF-8 sequence. The coding of
 * code points with values up to 65535 is as follows:
 * 
 *   hex         binary                   UTF-8 binary
 *   0000-007F   00000000 0abcdefg   =>   0abcdefg
 *   0080-07FF   00000abc defghijk   =>   110abcde 10fghijk
 *   0800-FFFF   abcdefgh ijklmnop   =>   1110abcd 10efghij 10klmnop
 * 
 *   010000-001FFFFF:      11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 * 
 * Code points from Western languages have short UTF-8
 * sequences: often one byte, almost always two.
 * 
 * Unicode characters are printed as UTF-8 sequences by
 * these two functions:
 * <shared function prototypes>=
 */
void fprint_utf8(FILE *output, unsigned code_point);
void print_utf8 (unsigned u);
/*
 * To obtain the next definition from such a source,
 * function [[readevalprint]] calls [[getxdef]] (\
 * chunkreffollowing-eval). Function [[getxdef]] returns
 * either a pointer to the next definition or, if the
 * source is exhausted, [[getxdef]] returns the [[NULL]]
 * pointer. And if there is some problem converting
 * input to abstract syntax, [[getxdef]] may call
 * [[synerror]] (\chunkrefcinterps.chunk.synerror). \
 * iilabelXDefstream\intlabelXDefstream\intlabelgetxdef
 * <shared function prototypes>=
 */
XDef getxdef(XDefstream xdefs);
/*
 * A stream of definitions is created from a source of
 * lines. That source can be a string compiled into the
 * program, created by function [[stringxdefs]], or it
 * can be an external file, created by [[filexdefs]].
 * Either way, error messages will need to refer to the
 * source by name, so each stream-creation function
 * expects a name. And [[filexdefs]] expects a parameter
 * of type [[Prompts]], which tells it whether to prompt
 * for input. \intlabelfilexdefs\intlabelstringxdefs
 * <shared function prototypes>=
 */
XDefstream stringxdefs(const char *stringname, const char *input);
XDefstream filexdefs  (const char *filename, FILE *input, Prompts prompts);
/*
 * Prompts are either absent or standard. \intlabel
 * Prompts
 */

/*
 * Interface to the extensible printer
 * 
 * The implementations of [[print]] and [[fprint]] are
 * extensible; adding a new conversion specification is
 * as simple as calling [[installprinter]]:\intlabel
 * installprinter
 * <shared function prototypes>=
 */
void installprinter(unsigned char c, Printer *take_and_print);
/*
 * <shared function prototypes>=
 */
void set_toplevel_error_format(ErrorFormat format);
/*
 * The auxiliary function [[report_test_results]] prints
 * a report of the results. The reporting code is shared
 * among all interpreters written in C; its
 * implementation appears in \crefpage
 * (cinterps.report_test_results.
 * <shared function prototypes>=
 */
void report_test_results(int npassed, int ntests);
/*
 * Functions [[printdecimal]], [[printname]],
 * [[printstring]], and [[printpercent]] are defined in
 * \crefpage(cinterps.printfuns. Functions that print
 * lists are generated automatically. The remaining
 * functions, which print Impcore's abstract syntax and
 * values, are defined below.
 * 
 * Printing functions for Impcore syntax and values
 * 
 * The following printing functions are specialized for
 * Impcore:
 * <shared function prototypes>=
 */
Printer printexp, printdef, printvalue, printfun;
/*
 * Primitives
 * 
 * Compared with Impcore, micro-Scheme has a ton of
 * primitives. They are grouped into these three
 * functions:
 * <shared function prototypes>=
 */
Primitive arith, binary, unary;
/*
 * Shift functions are as in Impcore, but with two
 * additions: to parse a quoted S-expression, shift
 * function [[sSexp]] has been added, and to parse
 * bindings in [[LETX]] forms, [[sBindings]] has been
 * added.
 * <shared function prototypes>=
 */
ParserResult sSexp    (ParserState state);
ParserResult sBindings(ParserState state);
/*
 * Extending syntax likely also requires some
 * initialization, which is done by function
 * [[extendSyntax]].
 * <shared function prototypes>=
 */
void extendSyntax(void);

/*
 * The trickiest part of writing a reduce function is
 * figuring out the integer codes. Codes for expressions
 * are easy: all expressions are represented by abstract
 * syntax of the same C type, so I already have the
 * perfect codes—the C enumeration literals used in the
 * [[alt]] field of an [[Exp]]. Codes for extended
 * definitions are more complicated: sometimes an
 * extended definition is an [[XDef]] directly, but more
 * often it is a [[Def]] or a [[UnitTest]].
 * And unfortunately, the [[alt]] fields for all three
 * forms overlap. For example, code 1 means [[EXP]] as a
 * [[Def]], [[CHECK_ERROR]] as a [[UnitTest]], and
 * [[USE]] as an [[XDef]]. All three of these forms are
 * ultimately extended definitions, so to distinguish
 * among them, I need a more elaborate coding scheme.
 * Here it is:
 * 
 *   Code   In C         Meaning
 *   Range
 *   0–99   \codemeANEXP Expressions
 *  100–199 \codemeADEF  Definitions
 *  200–299 \codemeATEST Unit tests
 *  300–399 \codeme      Other extended definitions
 *          ANXDEF
 *  400–499 \codemeALET  [[LET]] expressions used in
 *                       \chaprefscheme
 *  500–599 \codemeSUGAR Syntactic sugar
 *   1000   LATER        Syntax used in a later
 *                       chapter
 *   1001   EXERCISE     Syntax to be added for an
 *                       Exercise
 * 
 * In the table, alt stands for an enumeration literal
 * of the sort to go in an [[alt]] field.
 * 
 * So they can appear after [[case]] in [[switch]],
 * codes are defined using C macros:
 * <macro definitions used in parsing>=
 */
#define ANEXP(ALT)  (  0+(ALT))
#define ADEF(ALT)   (100+(ALT))
#define ATEST(ALT)  (200+(ALT))
#define ANXDEF(ALT) (300+(ALT))
#define ALET(ALT)   (400+(ALT))
#define SUGAR(CODE) (500+(CODE))
#define LATER       1000
#define EXERCISE    1001
/*
 * Parsing tables and functions
 * 
 * Every language has two parsing tables: one for
 * expressions and one for extended definitions.
 * <declarations of globals used in lexical analysis and parsing>=
 */
extern struct ParserRow exptable[];
extern struct ParserRow xdeftable[];
/*
 * \qtrim2
 * 
 * When a parser sees an input that has the wrong number
 * of components, as in \monobox(if p (set x 5)) or \
 * monobox(set x y z), it calls [[usage_error]] with a
 * code, a [[ParserResult]], and a context. The code is
 * looked up in [[usage_table]], which contains a sample
 * string showing what sort of syntax was expected.
 * <declarations of globals used in lexical analysis and parsing>=
 */
extern struct Usage {
    int code;             // codes for form in reduce_to_exp or reduce_to_xdef
    const char *expected; // shows the expected usage of the identified form
} usage_table[];
