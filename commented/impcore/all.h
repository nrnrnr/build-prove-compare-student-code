/*
 * Organizing interfaces into a header file
 * 
 * C provides poor support for separating interfaces
 * from implementations. The best a programmer can do is
 * put each interface in a .h file and use the
 * C preprocessor to [[#include]] those .h files where
 * they are needed. Ensuring that the right files are
 * [[#include]]'d, that they are [[#include]]'d in the
 * right order, and that no file is [[#include]]'d more
 * than once are all up to the programmer; the
 * C language and preprocessor don't help. These
 * problems are common, and C programmers have developed
 * conventions to deal with them, but these conventions
 * are better suited to large software projects than to
 * small interpreters. I have therefore chosen simply to
 * put all the interfaces into one header file, all.h.
 * When Noweb extracts code from the book, it
 * automatically puts [[#include "all.h"]] at the
 * beginning of each C file.
 * 
 * File all.h, which includes all interfaces used in the
 * interpreter, is split into these parts:
 * 
 *  \tightlist
 *   • Imports of header files from the standard
 *  C library
 *   • Type definitions
 *   • Structure definitions
 *   • Function prototypes
 *   • Arcana used in lexical analysis and parsing
 * 
 * Putting types, structures, and functions in that
 * order makes it easy for functions or structures
 * declared in one interface to use types defined in
 * another. And because declarations and definitions of
 * types always precede the function prototypes that use
 * those types, I need not worry about getting things in
 * the right order.
 * 
 * \qbreak To make it possible to reuse the
 * general-purpose interfaces in multiple interpreters,
 * the definitions and prototypes are split into two
 * groups: shared and unshared. A definition is
 * ``shared'' if it is used in another interpreter.
 * <{\Tt all.h} for \impcore>=
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

/*
 * <type definitions for \impcore>=
 */
typedef int32_t Value;
typedef struct Valuelist *Valuelist;     // list of Value
/*
 * Interface to functions, both user-defined and
 * primitive
 * 
 * In the Impcore interpreter, the type ``function'' is
 * another sum type. This type specifies two
 * alternatives: user-defined functions and primitive
 * functions. Following the operational semantics, which
 * represents a user-defined function as \user(<x_1,
 * ..., x_n>, e), a user-defined function is represented
 * as a pair containing formals and body. A primitive is
 * represented by its name. \iilabelFunc
 * <type definitions for \impcore>=
 */
typedef struct Funclist *Funclist; // list of Func
/*
 * Interface to environments: More abstract types
 * 
 * In the operational semantics, the environments rho
 *  and xi hold values, and the environment phi holds
 * functions. Each kind of environment has its own
 * representation. [By defining one \cc~type for
 * environments that hold a [[Value]] and another for
 * environments that hold a [[Func]], I~ensure type
 * safety---but at the cost of having to write two
 * essentially identical versions of each function. In~\
 * cc, the only alternative is to define a single \
 * cc~type for environments, which would hold a
 * [[void*]] pointer, which would then be cast to a
 * [[Value*]] or [[Func*]] as needed. This choice
 * duplicates no code, but it is unsafe; if we
 * accidentally put a [[Value*]] in an environment
 * intended to hold a [[Func*]], it is an error that
 * neither the \cc~compiler nor the run-time system can
 * detect. In the interests of safety, I~duplicate code.
 * Chapter~\ref{mlscheme.chap} shows how in another
 * implementation language, ML, we~can use \emph
 * {polymorphism} to achieve type safety without
 * duplicating code. Similar results can be obtained
 * using \cxx, \java, and other languages.] \iilabel
 * Funenv\iilabelValenv
 * <type definitions for \impcore>=
 */
typedef struct Valenv *Valenv;
typedef struct Funenv *Funenv;
/*
 * Every unit test in a file is stored in a list of type
 * [[UnitTestlist]].
 * <type definitions for \impcore>=
 */
typedef struct UnitTestlist *UnitTestlist; // list of UnitTest
/*
 * A [[UnitTestlist]] is list of pointers of type
 * [[UnitTest]]. This naming convention is used in all
 * my C code. List types are manifest, and their
 * definitions are in the lists interface in chunk [->].
 * 
 * Lists of expressions
 * 
 * A type for lists of [[Exp]]s is also declared here.
 */

/*
 * <type definitions for \impcore>=
 */
typedef struct Explist *Explist; // list of Exp
/*
 * <type definitions for \impcore>=
 */
typedef struct Exp *Exp;
typedef enum { LITERAL, VAR, SET, IFX, WHILEX, BEGIN, APPLY } Expalt;

/*
 * <type definitions for \impcore>=
 */
typedef struct Userfun Userfun; 
typedef struct Def *Def;
typedef enum { VAL, EXP, DEFINE } Defalt; 
/*
 * <type definitions for \impcore>=
 */
typedef struct XDef *XDef;
typedef enum { DEF, USE, TEST } XDefalt; 
typedef struct UnitTest *UnitTest;
typedef enum { CHECK_EXPECT, CHECK_ASSERT, CHECK_ERROR } UnitTestalt;

/*
 * <type definitions for \impcore>=
 */
typedef struct Func Func;
typedef enum { USERDEF, PRIMITIVE } Funcalt; 
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
 * <shared type definitions>=
 */
typedef struct Par *Par;
typedef enum { ATOM, LIST } Paralt; 

/*
 * Components, reduce functions, and form codes
 * 
 * A parser consumes inputs and puts components into an
 * array. (Inputs are [[Par]]s and components are
 * abstract syntax.) A reduce function takes the
 * components in the array and reduces the them to a
 * single node: an even bigger abstract-syntax tree,
 * which may then be stored as a component in another
 * array. ``Reduction'' is done by applying the build
 * function for the node to the components that are
 * reduced. In Impcore, a component is an expression, a
 * list of expressions, a name, or a list of names. [*]
 * <structure definitions for \impcore>=
 */
struct Component {
    Exp exp;
    Explist exps;
    Name name;
    Namelist names;
};
/*
 * <structure definitions for \impcore>=
 */
struct Exp {
    Expalt alt;
    union {
        Value literal;
        Name var;
        struct { Name name; Exp exp; } set;
        struct { Exp cond; Exp truex; Exp falsex; } ifx;
        struct { Exp cond; Exp exp; } whilex;
        Explist begin;
        struct { Name name; Explist actuals; } apply;
    } ;
};

/*
 * <structure definitions for \impcore>=
 */
struct Userfun { Namelist formals; Exp body; }; 
struct Def {
    Defalt alt;
    union {
        struct { Name name; Exp exp; } val;
        Exp exp;
        struct { Name name; Userfun userfun; } define;
    } ;
};

/*
 * <structure definitions for \impcore>=
 */
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
 * <structure definitions for \impcore>=
 */
struct Func { Funcalt alt; union { Userfun userdef; Name primitive; } ; }; 
/*
 * <structure definitions for \impcore>=
 */
struct Parlist {
   Par hd;
   struct Parlist *tl;
};

struct Namelist {
   Name hd;
   struct Namelist *tl;
};

struct Valuelist {
   Value hd;
   struct Valuelist *tl;
};

struct Funclist {
   Func hd;
   struct Funclist *tl;
};

struct UnitTestlist {
   UnitTest hd;
   struct UnitTestlist *tl;
};

struct Explist {
   Exp hd;
   struct Explist *tl;
};

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
 * <shared structure definitions>=
 */
struct Par { Paralt alt; union { Name atom; Parlist list; } ; }; 

/*
 * A function's prototype can include an annotation that
 * tells gcc or clang that the function doesn't return.
 * To hold the annotation, I define a macro
 * [[__noreturn]]. If the code is compiled with gcc or
 * clang, the macro expands to the GNU C extension
 * [[__attribute__((noreturn))]]. If the code is
 * compiled with another C compiler, the macro expands
 * to the empty string—other compilers have to live
 * without the information that the function doesn't
 * return.
 * <definition of [[__noreturn]]>=
 */
#ifdef __GNUC__
#define __noreturn __attribute__((noreturn))
#else
#define __noreturn
#endif
/*
 * A new environment may be created by passing a list of
 * names and a list of associated values or function
 * definitions to [[mkValenv]] or [[mkFunenv]].
 * For example, calling mkValenv(<x_1, ..., x_n>, <v_1,
 * ..., v_n>) returns {x_1 |->v_1, ..., x_n |->v_n}.
 * Passing lists of different lengths is a checked
 * run-time error.
 * <function prototypes for \impcore>=
 */
Valenv mkValenv(Namelist vars, Valuelist vals);
Funenv mkFunenv(Namelist vars, Funclist  defs);
/*
 * A value or a function definition is retrieved using
 * [[fetchval]] or [[fetchfun]]. In the operational
 * semantics, the lookup \monoboxfetchval(x, rho) is
 * simply rho(x). \iiflabelfetchval\iiflabelfetchfun
 * <function prototypes for \impcore>=
 */
Value fetchval(Name name, Valenv env);
Func  fetchfun(Name name, Funenv env);
/*
 * If the given name is not bound in the environment,
 * calling [[fetchval]] or [[fetchfun]] is a checked
 * run-time error. To ensure that fetching is safe,
 * first call [[isvalbound]] or [[isfunbound]]; these
 * functions return [[1]] if the given name is in the
 * environment, and [[0]] otherwise. Formally, \monobox
 * isvalbound(x, rho) is written \nomathbreakx in dom 
 * rho. \iiflabelisvalbound\iiflabelisfunbound [*]
 */

/*
 * <function prototypes for \impcore>=
 */
bool isvalbound(Name name, Valenv env);
bool isfunbound(Name name, Funenv env);
/*
 * To add new bindings to an environment, use
 * [[bindval]] and [[bindfun]]. Unlike the previous six
 * functions, [[bindval]] and [[bindfun]] are not pure:
 * instead of returning new environments, [[bindval]]
 * and [[bindfun]] mutate their argument environments,
 * replacing the old bindings with new ones. Calling \
 * monoboxbindval(x, v, rho) is equivalent to performing
 * the assignment rho := rho{x |->v}. Because rho is a
 * mutable abstraction, modifications to the environment
 * are visible to whatever code calls [[bindval]]. \
 * iiflabelbindval\iiflabelbindfun
 */

/*
 * <function prototypes for \impcore>=
 */
void bindval(Name name, Value val, Valenv env);
void bindfun(Name name, Func  fun, Funenv env);
/*
 * These functions can be used to replace existing
 * bindings or to add new ones.
 */

/*
 * Interface to the evaluator
 * 
 * The evaluator works with abstract syntax and values,
 * whose representations are exposed, and with names and
 * environments, whose representations are not exposed.
 * Its interface exports functions [[eval]] and
 * [[evaldef]], which evaluate expressions and true
 * definitions, respectively. (Extended definitions are
 * evaluated by function [[readevalprint]], which is
 * described in the Supplement.) Function [[eval]]
 * implements the ==> relation in our operational
 * semantics. For example, eval(e, xi, phi, rho) finds a
 * v, xi', and rho' such that \evale ==>\eval[']v,
 * assigns rho := rho' and xi := xi', and returns v.
 * Function [[evaldef]] similarly implements the -->
 * relation. \iintlabeleval\iintlabelevaldef
 * <function prototypes for \impcore>=
 */
Value eval   (Exp e, Valenv globals, Funenv functions, Valenv formals);
void  evaldef(Def d, Valenv globals, Funenv functions, Echo echo_level);
/*
 * Function [[readevalprint]] consumes a stream of
 * extended definitions. It evaluates each true
 * definition, remembers each unit test, and calls
 * itself recursively on each [[use]]. When the stream
 * of extended definitions is exhausted,
 * [[readevalprint]] runs the unit tests it has
 * remembered.\iintlabelreadevalprint
 * <function prototypes for \impcore>=
 */
void readevalprint(XDefstream s, Valenv globals, Funenv functions, Echo
                                                                    echo_level);
/*
 * The [[echo_level]] parameter controls whether
 * [[readevalprint]] prints the values and names of
 * top-level expressions and functions. In \cref
 * impcore.chap, a similar parameter is given to
 * [[evaldef]]. \intlabelEcho
 */

/*
 * <function prototypes for \impcore>=
 */
void process_tests(UnitTestlist tests, Valenv globals, Funenv functions);
/*
 * <function prototypes for \impcore>=
 */
int number_of_good_tests(UnitTestlist tests, Valenv globals, Funenv functions);
/*
 * <function prototypes for \impcore>=
 */
TestResult test_result(UnitTest t, Valenv globals, Funenv functions);
/*
 * <function prototypes for \impcore>=
 */
Exp mkLiteral(Value literal);
Exp mkVar(Name var);
Exp mkSet(Name name, Exp exp);
Exp mkIfx(Exp cond, Exp truex, Exp falsex);
Exp mkWhilex(Exp cond, Exp exp);
Exp mkBegin(Explist begin);
Exp mkApply(Name name, Explist actuals);
struct Exp mkLiteralStruct(Value literal);
struct Exp mkVarStruct(Name var);
struct Exp mkSetStruct(Name name, Exp exp);
struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex);
struct Exp mkWhilexStruct(Exp cond, Exp exp);
struct Exp mkBeginStruct(Explist begin);
struct Exp mkApplyStruct(Name name, Explist actuals);
/*
 * <function prototypes for \impcore>=
 */
Userfun mkUserfun(Namelist formals, Exp body);
Def mkVal(Name name, Exp exp);
Def mkExp(Exp exp);
Def mkDefine(Name name, Userfun userfun);
struct Def mkValStruct(Name name, Exp exp);
struct Def mkExpStruct(Exp exp);
struct Def mkDefineStruct(Name name, Userfun userfun);
/*
 * <function prototypes for \impcore>=
 */
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
 * <function prototypes for \impcore>=
 */
Func mkUserdef(Userfun userdef);
Func mkPrimitive(Name primitive);
/*
 * <function prototypes for \impcore>=
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

int       lengthVL(Valuelist vs);
Value     nthVL   (Valuelist vs, unsigned n);
Valuelist mkVL    (Value v, Valuelist vs);
Valuelist popVL   (Valuelist vs);
Printer   printvaluelist;

int      lengthFL(Funclist fs);
Func     nthFL   (Funclist fs, unsigned n);
Funclist mkFL    (Func f, Funclist fs);
Funclist popFL   (Funclist fs);
Printer  printfunclist;

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
 * <shared function prototypes>=
 */
Par mkAtom(Name atom);
Par mkList(Parlist list);
struct Par mkAtomStruct(Name atom);
struct Par mkListStruct(Parlist list);

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
