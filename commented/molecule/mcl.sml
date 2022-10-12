(* <mcl.sml>=                                   *)
exception Unimp of string (* raised if a feature is not implemented *)
fun unimp s = raise Unimp s
fun concatMap f = List.concat o map f  (* List.concatMap is not in Moscow ML *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE CHECKING             *)
(*                                                               *)
(*****************************************************************)

(* All interpreters that include type checkers also *)
(* incorporate these exceptions:                *)
(* <exceptions used in languages with type checking>= *)
exception TypeError of string
exception BugInTypeChecking of string


(*****************************************************************)
(*                                                               *)
(*   \FOOTNOTESIZE SHARED: NAMES, ENVIRONMENTS, STRINGS, ERRORS, PRINTING, INTERACTION, STREAMS, \&\ INITIALIZATION *)
(*                                                               *)
(*****************************************************************)

(* <\footnotesize shared: names, environments, strings, errors, printing, interaction, streams, \&\ initialization>= *)
(* <for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]]>= *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* Functions that serve as [[f]]'s are created in a *)
(* variety of ways. Many such functions are Curried. *)
(* Some of them are defined here.               *)
(* <boxed values 331>=                          *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* [[bindList]] tried to                        *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)

(* <support for names and environments>=        *)
type name = string
(* ML's [[type]] syntax is like C's [[typedef]]; *)
(* it defines a type by type abbreviation.      *)

(* <support for names and environments>=        *)
type 'a env = (name * 'a) list
(* <support for names and environments>=        *)
val emptyEnv = []
(* <support for names and environments>=        *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (x, v)::tail) = if name = x then v else find (name, tail)
(* The [[fun]] definition form is ML's analog to *)
(* [[define]], but unlike micro-Scheme's [[define]], *)
(* it uses multiple clauses with pattern matching. Each *)
(* clause is like an algebraic law. The first clause *)
(* says that calling [[find]] with an empty environment *)
(* raises an exception; the second clause handles a *)
(* nonempty environment. The infix [[::]] is ML's way of *)
(* writing [[cons]], and it is pronounced ``cons.'' *)
(*                                              *)
(* To check x in dom rho, the ML code uses function *)
(* [[isbound]].                                 *)
(* <support for names and environments>=        *)
fun isbound (name, []) = false
  | isbound (name, (x, v)::tail) = name = x orelse isbound (name, tail)
(* <support for names and environments>=        *)
fun bind (name, v, rho) =
  (name, v) :: rho
(* <support for names and environments>=        *)
exception BindListLength
fun bindList (x::vars, v::vals, rho) = bindList (vars, vals, bind (x, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength

fun mkEnv (xs, vs) = bindList (xs, vs, emptyEnv)
(* <support for names and environments>=        *)
(* composition *)
infix 6 <+>
fun pairs <+> pairs' = pairs' @ pairs
(* <boxed values 29>=                           *)
val _ = op emptyEnv : 'a env
(* (The phrase in the box is like a declaration that *)
(* could appear in an interface to an ML module; through *)
(* some Noweb hackery, it is checked by the     *)
(* ML compiler.)                                *)
(*                                              *)
(* A name is looked up by function [[find]], which is *)
(* closely related to the [[find]] from \cref   *)
(* scheme.chap: it returns whatever is in the   *)
(* environment, which has type [['a]]. If the name is *)
(* unbound, [[find]] raises an exception. Raising an *)
(* exception is a lot like the [[throw]] operator in \ *)
(* crefschemes.chap; it is roughly analogous to *)
(* [[longjmp]]. The exceptions I use are listed in \vref *)
(* mlscheme.tab.exns.                           *)
(* <boxed values 29>=                           *)
val _ = op find : name * 'a env -> 'a
(* \mlsflabelfind                               *)

(* Again using [[::]], function [[bind]] adds a new *)
(* binding to an existing environment. Unlike \cref *)
(* scheme.chap's [[bind]], it does not allocate a *)
(* mutable reference cell.                      *)
(* <boxed values 29>=                           *)
val _ = op bind : name * 'a * 'a env -> 'a env
(* \mlsflabelbind                               *)

(* Even though an \monobox'a env is a list of pairs, *)
(* functions that operate on two lists, like those in \ *)
(* crefimpcore.chap,scheme.chap, are still useful. *)
(* Function [[bindList]] adds a sequence of bindings to *)
(* an environment; it is used to implement      *)
(* micro-Scheme's [[let]] and [[lambda]]. If the lists *)
(* aren't the same length, it raises another exception. *)
(* Function [[bindList]] resembles \crefscheme.chap's *)
(* [[bindalloclist]], but it does not allocate. Related *)
(* function [[mkEnv]] manufactures a new environment *)
(* given just a list of names and [['a]]'s. \mlsflabel *)
(* bindList,mkEnv                               *)
(* <boxed values 29>=                           *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* <boxed values 29>=                           *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* <support for names and environments>=        *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* <boxed values 124>=                          *)
val _ = op duplicatename : name list -> name option
(* <support for names and environments>=        *)
exception DisjointUnionFailed of name
fun disjointUnion envs =
  let val env = List.concat envs
  in  case duplicatename (map fst env)
        of NONE => env
         | SOME x => raise DisjointUnionFailed x
  end
(* Disjoint union of environments (for pattern matching) *)
(*                                              *)
(* When a pattern match is typechecked, each variable in *)
(* the pattern induces an environment. Function *)
(* [[disjointUnion]] combines the environments and *)
(* checks for duplicate names, making sure all the *)
(* variables are distinct. If [[disjointUnion]] finds a *)
(* duplicate name, it raises [[DisjointUnionFailed]]. *)
(* This exception can be raised only during type *)
(* inference, not during evaluation.            *)
(* <boxed values 193>=                          *)
val _ = op disjointUnion : 'a env list -> 'a env
(* All interpreters incorporate these two exceptions: *)
(* <exceptions used in every interpreter>=      *)
exception RuntimeError of string (* error message *)
exception LeftAsExercise of string (* string identifying code *)
(* Some errors might be caused not by a fault in a *)
(* user's code but in my interpreter code. Such faults *)
(* are signaled by the [[InternalError]] exception. *)
(* <support for detecting and signaling errors detected at run time>= *)
exception InternalError of string (* bug in the interpreter *)
(* <list functions not provided by \sml's initial basis>= *)
fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end

fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths
(* \qbreak                                      *)
(*                                              *)
(* List utilities                               *)
(*                                              *)
(* Most of the list utilities anyone would need are part *)
(* of the initial basis of Standard ML. But the type *)
(* checker for pattern matching in \crefadt.chap *)
(* sometimes needs to unzip a list of triples into a *)
(* triple of lists. I define [[unzip3]] and also the *)
(* corresponding [[zip3]].                      *)
(* <boxed values 122>=                          *)
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
(* Standard ML's list-reversal function is called *)
(* [[rev]], but in this book I use [[reverse]]. *)
(* <list functions not provided by \sml's initial basis>= *)
val reverse = rev
(* <list functions not provided by \sml's initial basis>= *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
(* Function [[optionList]] inspects a list of optional *)
(* values, and if every value is actually present (made *)
(* with [[SOME]]), then it returns the values. Otherwise *)
(* it returns [[NONE]].                         *)
(* <boxed values 123>=                          *)
val _ = op optionList : 'a option list -> 'a list option
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <utility functions for string manipulation and printing>= *)
val lower = String.map Char.toLower
val upper = String.map Char.toUpper
(* <utility functions for string manipulation and printing>= *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* Reusable utility \chaptocsplitfunctions      *)
(*                                              *)
(* The interpreters share some small utility functions *)
(* for printing, for manipulating automatically *)
(* generated names, and for manipulating sets.  *)
(*                                              *)
(* Utility functions for creating and hashing strings *)
(*                                              *)
(* Standard ML's built-in support for converting *)
(* integers to strings uses the [[ ]] character as a *)
(* minus sign. But the bridge languages represent a *)
(* minus sign as a hyphen.                      *)
(* <boxed values 114>=                          *)
val _ = op intString : int -> string
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <utility functions for string manipulation and printing>= *)
fun plural what [x] = what
  | plural what _   = what ^ "s"

fun countString xs what =
  intString (length xs) ^ " " ^ plural what xs
(* <utility functions for string manipulation and printing>= *)
val spaceSep = String.concatWith " "   (* list separated by spaces *)
val commaSep = String.concatWith ", "  (* list separated by commas *)
(* <boxed values 115>=                          *)
val _ = op spaceSep : string list -> string
val _ = op commaSep : string list -> string
(* [[bindList]] tried to                        *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)

(* <utility functions for string manipulation and printing>= *)
fun nullOrCommaSep empty [] = empty
  | nullOrCommaSep _     ss = commaSep ss                   
(* <boxed values 116>=                          *)
val _ = op nullOrCommaSep : string -> string list -> string
(* <utility functions for string manipulation and printing>= *)
fun fnvHash s =
  let val offset_basis = 0wx011C9DC5 : Word.word  (* trim the high bit *)
      val fnv_prime    = 0w16777619  : Word.word
      fun update (c, hash) = Word.xorb (hash, Word.fromInt (ord c)) * fnv_prime
      fun int w =
        Word.toIntX w handle Overflow => Word.toInt (Word.andb (w, 0wxffffff))
  in  int (foldl update offset_basis (explode s))
  end
(* The [[hash]] primitive in the \usm interpreter uses *)
(* an algorithm by Glenn Fowler, Phong Vo, and Landon *)
(* Curt Noll, which I implement in function [[fnvHash]]. *)
(* [*] I have adjusted the algorithm's ``offset basis'' *)
(* by removing the high bit, so the computation works *)
(* using 31-bit integers. The algorithm is described by *)
(* an IETF draft at \urlhttp://tools.ietf.org/html/ *)
(* draft-eastlake-fnv-03, and it's also described by the *)
(* web page at \urlhttp://www.isthe.com/chongo/tech/comp *)
(* /fnv/.                                       *)
(* <boxed values 117>=                          *)
val _ = op fnvHash : string -> int
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Lexical analysis, parsing, and reading input using ML *)
(*                                              *)
(* [*][*] \invisiblelocaltableofcontents[*]     *)
(*                                              *)
(* How is a program represented? If you have worked *)
(* through this book, you will believe (I hope) that the *)
(* most fundamental and most useful representation of a *)
(* program is its abstract-syntax tree. But syntax trees *)
(* aren't easy to create or specify directly, so syntax *)
(* usually has to be written using a sequence of *)
(* characters. To help myself write parsers by hand, *)
(* I have created [I~say ``created,'' but it would be *)
(* more accurate to say ``stolen.'' ] a set of  *)
(* higher-order functions designed especially to *)
(* manipulate parsers. Such functions are known as *)
(* parsing combinators. My parsing combinators appear in *)
(* this appendix.                               *)
(*                                              *)
(* Most parsing techniques have been invented for use in *)
(* compilers. and a typical compiler swallows programs *)
(* in large gulps, one file at a time. Unlike these *)
(* typical compilers, the interpreters in this book are *)
(* interactive, and they swallow just one line at a *)
(* time. Interactivity imposes additional requirements: *)
(*                                              *)
(*   • Before reading a line of input, an interactive *)
(*  interpreter should issue a suitable prompt. *)
(*  The prompt should tell the user whether the *)
(*  parser is waiting for a new definition or is in *)
(*  the middle of parsing a current definition—which *)
(*  means that the line-reading functions must be in *)
(*  cahoots with the parser.                    *)
(*   • If a parser encounters an error, it can't just *)
(*  give up. It needs to get itself back into a state *)
(*  where the user can continue to interact.    *)
(*                                              *)
(* These requirements make my parsing combinators a bit *)
(* different from standard ones. In particular, in order *)
(* to be sure that the actions of printing a prompt and *)
(* reading a line of input occur in the proper sequence, *)
(* I manage these actions using the lazy streams defined *)
(* in \crefmlinterps.streams. Unlike the lazy streams *)
(* built into Haskell, these lazy streams can do input *)
(* and output and can perform other actions.    *)
(*                                              *)
(* Parsing is about turning a stream of lines (from a *)
(* file or from a list of strings) into a stream of *)
(* extended definitions. It happens in stages:  *)
(*                                              *)
(*   • In a stream of lines, each line is split into *)
(*  characters.                                 *)
(*   • A lexical analyzer turns a stream of characters *)
(*  into a stream of tokens. Using              *)
(*  [[streamConcatMap]] with the lexical analyzer *)
(*  then turns a stream of lines into a stream of *)
(*  tokens.                                     *)
(*   • A parser turns a stream of tokens into a stream *)
(*  of syntax. I define parsers for expressions, true *)
(*  definitions, unit tests, and extended       *)
(*  definitions.                                *)
(*                                              *)
(* The fundamental parser is [[one]], which takes one *)
(* token from a stream and produces that token. Other *)
(* parsers are built on top of [[one]], usually using *)
(* higher-order functions. Functions [[<>]] and [[<*>]] *)
(* act like [[map]] for parsers, applying a function the *)
(* result a parser returns. Function [[sat]] acts like *)
(* [[filter]], allowing a parser to fail if it doesn't *)
(* recognize its input. Functions [[<*>]], [[<*]], and  *)
(* [[*>]] combine parsers in sequence, and function *)
(* [[<|>]] defines a parser as a choice between two *)
(* other parsers. Functions [[many]] and [[many1]] turn *)
(* a parser for a thing into a parser for a list of *)
(* things; function [[optional]] does the same thing for *)
(* ML's [[option]] type. These functions are known *)
(* collectively as parsing combinators, and together *)
(* they form a powerful language for defining lexical *)
(* analyzers and parsers.                       *)
(*                                              *)
(* I divide parsers and parsing combinators into three *)
(* groups:                                      *)
(*                                              *)
(*   • A stream transformer doesn't care what comes in *)
(*  or goes out; it is polymorphic in both the input *)
(*  and output type. Stream transformers are used to *)
(*  build both lexical analyzers and parsers.   *)
(*   • A lexer is a stream transformer that is *)
(*  specialized to take a stream of characters as *)
(*  input. Lexers may be defined with any output *)
(*  type, but a value of that output type should *)
(*  represent a token.                          *)
(*   • A parser is a stream transformer that is *)
(*  specialized to take a stream of tokens as input. *)
(*  A parser's input stream also includes source-code *)
(*  locations and end-of-line markers. Parsers may be *)
(*  defined with any output type, but the rest of the *)
(*  interpreter is most interested in the parser that *)
(*  produces a stream of definitions (abstract-syntax *)
(*  trees).                                     *)
(*                                              *)
(* The polymorphic functions are described in \crefpage *)
(* (lazyparse.fig.xformer; the specialized functions are *)
(* described in \crefpage(lazyparse.fig.lexers-parsers. *)
(*                                              *)
(* The code is divided among these chunks:      *)
(* <utility functions for string manipulation and printing>= *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* <utility functions for string manipulation and printing>= *)
fun predefinedFunctionError s =
  eprintln ("while reading predefined functions, " ^ s)
(* Not all printing is directed to standard output or *)
(* standard error. To implement the [[check-print]] unit *)
(* test (uSmalltalk, \crefsmall.chap), printing must be *)
(* directed to a buffer. Functions [[xprint]] and *)
(* [[xprintln]] direct output either to standard output *)
(* or to a buffer, depending on what printing function *)
(* is currently stored in the mutable reference cell *)
(* [[xprinter]].                                *)
(* <utility functions for string manipulation and printing>= *)
val xprinter = ref print
fun xprint   s = !xprinter s
fun xprintln s = (xprint s; xprint "\n")
(* <utility functions for string manipulation and printing>= *)
fun tryFinally f x post =
  (f x handle e => (post (); raise e)) before post ()

fun withXprinter xp f x =
  let val oxp = !xprinter
      val ()  = xprinter := xp
  in  tryFinally f x (fn () => xprinter := oxp)
  end
(* \qbreak The printing function that is stored in *)
(* [[xprinter]] can be changed temporarily by calling *)
(* function [[withXprinter]]. This function changes *)
(* [[xprinter]] just for the duration of a call to \ *)
(* monoboxf x. To restore [[xprinter]], function *)
(* [[withXprinter]] uses [[tryFinally]], which ensures *)
(* that its [[post]] handler is always run, even if an *)
(* exception is raised.                         *)
(* <boxed values 118>=                          *)
val _ = op withXprinter : (string -> unit) -> ('a -> 'b) -> ('a -> 'b)
val _ = op tryFinally   : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b
(* The stream-transformer abstraction supports many, *)
(* many operations. These operations, known as parsing *)
(* combinators, have been refined by functional *)
(* programmers for over two decades, and they can be *)
(* expressed in a variety of guises. The guise I have *)
(* chosen uses notation from applicative functors and *)
(* from the ParSec parsing library.             *)
(*                                              *)
(* I begin very abstractly, by presenting combinators *)
(* that don't actually consume any inputs. The next two *)
(* sections present only ``constant'' transformers and *)
(* ``glue'' functions that build transformers from other *)
(* transformers. With those functions in place, *)
(* I proceed to real, working parsing combinators. These *)
(* combinators are split into two groups: ``universal'' *)
(* combinators that work with any stream, and   *)
(* ``parsing'' combinators that expect a stream of *)
(* tokens with source-code locations.           *)
(*                                              *)
(* My design includes a lot of combinators. Too many, *)
(* really. I would love to simplify the design, but *)
(* simplifying software can be hard, and I don't want to *)
(* delay the book by another year.              *)
(*                                              *)
(* --- #2                                       *)
(* \newskip\myskip \myskip=4pt                  *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \catcode`=\other \catcode`_=\other \catcode`$=\other *)
(*                                              *)
(*  Stream transformers; applying functions to  *)
(*  transformers                                *)
(*  \type('a, 'b) xformer \                     *)
(*  tableboxpure : 'b -> ('a, 'b)               *)
(*  xformer \splitbox<*>('a, 'b ->              *)
(*  'c) xformer * ('a, 'b)                      *)
(*  xformer-> ('a, 'c) xformer \                *)
(*  tablebox<> : ('b -> 'c) * ('a,              *)
(*  'b) xformer -> ('a, 'c)                     *)
(*  xformer \tablebox<>? : ('b ->               *)
(*  'c option) * ('a, 'b) xformer               *)
(*  -> ('a, 'c) xformer \splitbox               *)
(*  <*>!('a, 'b -> 'c error)                    *)
(*  xformer * ('a, 'b) xformer->                *)
(*  ('a, 'c) xformer \tablebox<>!               *)
(*  : ('b -> 'c error) * ('a, 'b)               *)
(*  xformer -> ('a, 'c) xformer                 *)
(*  [8pt] Functions useful with                 *)
(*  [[<>]] and [[<*>]]                          *)
(*  \tableboxfst : ('a * 'b) -> 'a              *)
(*  \tableboxsnd : ('a * 'b) -> 'b              *)
(*  \tableboxpair : 'a -> 'b -> 'a              *)
(*  * 'b \tableboxcurry : ('a * 'b              *)
(*  -> 'c) -> ('a -> 'b -> 'c) \                *)
(*  tableboxcurry3 : ('a * 'b * 'c              *)
(*  -> 'd) -> ('a -> 'b -> 'c ->                *)
(*  'd) [8pt] Combining                         *)
(*  transformers in sequence,                   *)
(*  alternation, or conjunction                 *)
(*  \tablebox<* : ('a, 'b) xformer >]] : ('a, 'b) *)
(*  * ('a, 'c) xformer -> ('a, 'b) xformer * ('a, 'c) *)
(*  xformer \tablebox *> : ('a,    xformer -> ('a, *)
(*  'b) xformer * ('a, 'c) xformer 'c) xformer [8pt] *)
(*  -> ('a, 'c) xformer \tablebox< Transformers *)
(*  : 'b * ('a, 'c) xformer ->     useful for both *)
(*  ('a, 'b) xformer \tablebox<|>  lexical analysis *)
(*  : ('a, 'b) xformer * ('a, 'b)  and parsing  *)
(*  xformer -> ('a, 'b) xformer \               *)
(*  tableboxpzero : ('a, 'b)                    *)
(*  xformer \tableboxanyParser :                *)
(*  ('a, 'b) xformer list -> ('a,               *)
(*  'b) xformer \tablebox[[<                    *)
(*  \tableboxone : ('a, 'a)                     *)
(*  xformer \tableboxeos : ('a,                 *)
(*  unit) xformer \tableboxsat :                *)
(*  ('b -> bool) -> ('a, 'b)                    *)
(*  xformer -> ('a, 'b) xformer \               *)
(*  tableboxeqx : ''b -> ('a, ''b)              *)
(*  xformer -> ('a, ''b) xformer                *)
(*  notFollowedBy                               *)
(*                                 ('a, 'b) xformer *)
(*                                 -> ('a, unit) *)
(*                                 xformer      *)
(*  \tableboxmany : ('a, 'b)                    *)
(*  xformer -> ('a, 'b list)                    *)
(*  xformer \tableboxmany1 : ('a,               *)
(*  'b) xformer -> ('a, 'b list)                *)
(*  xformer \tableboxoptional :                 *)
(*  ('a, 'b) xformer -> ('a, 'b                 *)
(*  option) xformer \tableboxpeek               *)
(*  : ('a, 'b) xformer -> 'a                    *)
(*  stream -> 'b option \tablebox               *)
(*  rewind : ('a, 'b) xformer ->                *)
(*  ('a, 'b) xformer                            *)
(*                                              *)
(* Stream transformers and their combinators [*] *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Error-free transformers and their composition *)
(*                                              *)
(* The [[pure]] combinator takes a value [[y]] of type B *)
(* as argument. It returns an \atob transformer that *)
(* consumes no A's as input and produces [[y]]. *)
(* <utility functions for string manipulation and printing>= *)
fun bprinter () =
  let val buffer = ref []
      fun bprint s = buffer := s :: !buffer
      fun contents () = concat (rev (!buffer))
  in  (bprint, contents)
  end
(* \qtrim2                                      *)
(*                                              *)
(* Function [[xprint]] is used by function      *)
(* [[printUTF8]], which prints a Unicode character using *)
(* the Unicode Transfer Format (UTF-8).         *)
(* <utility functions for string manipulation and printing>= *)
fun printUTF8 code =
  let val w = Word.fromInt code
      val (&, >>) = (Word.andb, Word.>>)
      infix 6 & >>
      val _ = if (w & 0wx1fffff) <> w then
                raise RuntimeError (intString code ^
                                    " does not represent a Unicode code point")
              else
                 ()
      val printbyte = xprint o str o chr o Word.toInt
      fun prefix byte byte' = Word.orb (byte, byte')
  in  if w > 0wxffff then
        app printbyte [ prefix 0wxf0  (w >> 0w18)
                      , prefix 0wx80 ((w >> 0w12) & 0wx3f)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w      ) & 0wx3f)
                      ]
      else if w > 0wx7ff then
        app printbyte [ prefix 0wxe0  (w >> 0w12)
                      , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else if w > 0wx7f then
        app printbyte [ prefix 0wxc0  (w >>  0w6)
                      , prefix 0wx80 ((w        ) & 0wx3f)
                      ]
      else
        printbyte w
  end
(* Utility functions for renaming variables     *)
(*                                              *)
(* In the theory of programming languages, it's common *)
(* to talk about fresh names, where ``fresh'' means *)
(* ``different from any name in the program or its *)
(* environment'' (\creftypesys.chap,ml.chap,adt.chap). *)
(* And if you implement a type checker for a polymorphic *)
(* language like Typed uScheme, or if you implement type *)
(* inference, or if you ever implement the lambda *)
(* calculus, you will need code that generates fresh *)
(* names. You can always try names like [[t1]], [[t2]], *)
(* and so on. But if you want to debug, it's usually *)
(* helpful to relate the fresh name to a name already in *)
(* the program. I like to do this by tacking on a *)
(* numeric suffix; for example, to get a fresh name *)
(* that's like [[x]], I might try [[x-1]], [[x-2]], and *)
(* so on. But if the process iterates, I don't want to *)
(* generate a name like [[x-1-1-1]]; I'd much rather *)
(* generate [[x-3]]. This utility function helps by *)
(* stripping off any numeric suffix to recover the *)
(* original [[x]].                              *)
(* <utility functions for string manipulation and printing>= *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* \stdbreak                                    *)
(*                                              *)
(* Utility functions for sets, collections, and lists *)
(*                                              *)
(* Sets                                         *)
(*                                              *)
(* Quite a few analyses of programs, including a type *)
(* checker in \creftypesys.chap and the type inference *)
(* in \crefml.chap, need to manipulate sets of  *)
(* variables. In small programs, such sets are usually *)
(* small, so I provide a simple implementation that *)
(* represents a set using a list with no duplicate *)
(* elements. It's essentially the same implementation *)
(* that you see in micro-Scheme in \crefscheme.chap. [ *)
(* The~\ml~types of the set operations include type *)
(* variables with double primes, like~[[''a]]. The type *)
(* variable~[[''a]] can be instantiated only with an *)
(* ``equality type.'' Equality types include base types *)
(* like strings and integers, as well as user-defined *)
(* types that do not contain functions. Functions \emph *)
(* {cannot} be compared for equality.]          *)

(* <support for representing errors as \ml\ values>= *)
datatype 'a error = OK of 'a | ERROR of string
(* <support for representing errors as \ml\ values>= *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* What if we have a function [[f]] that could return *)
(* an [['a]] or an error, and another function [[g]] *)
(* that expects an [['a]]? Because the expression \ *)
(* monoboxg (f x) isn't well typed, standard function *)
(* composition doesn't exactly make sense, but the idea *)
(* of composition is good. Composition just needs to *)
(* take a new form, and luckily, there's already a *)
(* standard. The standard composition relies on a *)
(* sequencing operator written [[>>=]], which uses a *)
(* special form of continuation-passing style. (The *)
(* [[>>=]] operator is traditionally called ``bind,'' *)
(* but you might wish to pronounce it ``and then.'') *)
(* The idea is to apply [[f]] to [[x]], and if the *)
(* result is [[OK y]], to continue by applying [[g]] to  *)
(* [[y]]. But if the result of applying [[(f x)]] is an *)
(* error, that error is the result of the whole *)
(* computation. The [[>>=]] operator sequences the *)
(* possibly erroneous result [[(f x)]] with the *)
(* continuation [[g]], so where we might wish to write \ *)
(* monoboxg (f x), we instead write             *)
(*                                              *)
(*  [[f x >>= g]].                              *)
(*                                              *)
(* In the definition of [[>>=]], I write the second *)
(* function as [[k]], not [[g]], because [[k]] is a *)
(* traditional metavariable for a continuation. *)
(* <boxed values 126>=                          *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* A very common special case occurs when the   *)
(* continuation always succeeds; that is, the   *)
(* continuation [[k']] has type \monobox'a -> 'b instead *)
(* of \monobox'a -> b error. In this case, the execution *)
(* plan is that when [[(f x)]] succeeds, continue by *)
(* applying [[k']] to the result; otherwise propagate *)
(* the error. I know of no standard way to write this *)
(* operator, [Haskell uses [[flip fmap]].] , so I use  *)
(* [[>>=+]], which you might also choose to pronounce *)
(* ``and then.''                                *)

(* <support for representing errors as \ml\ values>= *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* <boxed values 127>=                          *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* <support for representing errors as \ml\ values>= *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* <boxed values 128>=                          *)
val _ = op errorList : 'a error list -> 'a list error
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Finally, I sometimes want to label an error message *)
(* with a string [[s]], which can identify where the *)
(* error originates:                            *)
(* <support for representing errors as \ml\ values>= *)
fun errorLabel s (OK x) = OK x
  | errorLabel s (ERROR msg) = ERROR (s ^ msg)
(* A reusable read-eval-print loop              *)
(*                                              *)
(* [*] In each bridge-language interpreter, functions *)
(* [[eval]] and [[evaldef]] process expressions and true *)
(* definitions. But each interpreter also has to process *)
(* the extended definitions [[USE]] and [[TEST]], which *)
(* need more tooling:                           *)
(*                                              *)
(*   • To process a [[USE]], the interpreter must be *)
(*  able to parse definitions from a file and enter a *)
(*  read-eval-print loop recursively.           *)
(*   • To process a [[TEST]] (like [[check_expect]] or *)
(*  [[check_error]]), the interpreter must be able to *)
(*  run tests, and to run a test, it must call  *)
(*  [[eval]].                                   *)
(*                                              *)
(* Much the tooling can be shared among more than one *)
(* bridge language. To make sharing easy, I introduce *)
(* some abstraction.                            *)
(*                                              *)
(*   • Type [[basis]], which is different for each *)
(*  bridge language, stands for the collection of *)
(*  environment or environments that are used at top *)
(*  level to evaluate a definition. The name basis *)
(*  comes from The Definition of Standard ML \citep *)
(*  milner:definition-revised.                  *)
(*                                              *)
(*  For micro-Scheme, a [[basis]] is a single   *)
(*  environment that maps each name to a mutable *)
(*  location holding a value. For Impcore, a    *)
(*  [[basis]] would include both global-variable and *)
(*  function environments. And for later languages *)
(*  that have static types, a [[basis]] includes *)
(*  environments that store information about types. *)
(*   • Function [[processDef]], which is different for *)
(*  each bridge language, takes a [[def]] and a *)
(*  [[basis]] and returns an updated [[basis]]. *)
(*  For micro-Scheme, [[processDef]] just evaluates *)
(*  the definition, using [[evaldef]]. For languages *)
(*  that have static types (Typed Impcore, Typed *)
(*  uScheme, and \nml in \creftuscheme.chap,ml.chap, *)
(*  among others), [[processDef]] includes two  *)
(*  phases: type checking followed by evaluation. *)
(*                                              *)
(*  Function [[processDef]] also needs to be told *)
(*  about interaction, which has two dimensions: *)
(*  input and output. On input, an interpreter may or *)
(*  may not prompt:                             *)
(* <type [[interactivity]] plus related functions and value>= *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* On output, an interpreter may or may not show a *)
(* response to each definition.                 *)

(* <type [[interactivity]] plus related functions and value>= *)
datatype output_interactivity = ECHOING | NOT_ECHOING
(* <type [[interactivity]] plus related functions and value>= *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_ECHOING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun echoes (_, ECHOING)     = true
  | echoes (_, NOT_ECHOING) = false
(* <boxed values 159>=                          *)
type interactivity = interactivity
val _ = op noninteractive : interactivity
val _ = op prompts : interactivity -> bool
val _ = op echoes  : interactivity -> bool
(* <simple implementations of set operations>=  *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs
(* <boxed values 119>=                          *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* [[bindList]] tried to                        *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)

(* <ability to interrogate environment variable [[BPCOPTIONS]]>= *)
local
  val split = String.tokens (fn c => c = #",")
  val optionTokens =
    getOpt (Option.map split (OS.Process.getEnv "BPCOPTIONS"), [])
in
  fun hasOption s = member s optionTokens
(* Raising [[InternalError]] is the equivalent of an *)
(* assertion failure in a language like C.      *)
(*                                              *)
(* I must not confuse [[InternalError]] with    *)
(* [[RuntimeError]]. When the interpreter raises *)
(* [[RuntimeError]], it means that a user's program got *)
(* stuck: evaluation led to a state in which the *)
(* operational semantics couldn't make progress. *)
(* The fault is the user's. But when the interpreter *)
(* raises [[InternalError]], it means there is a fault *)
(* in my code; the user's program is blameless. *)
(*                                              *)
(* Control by environment variable              *)
(*                                              *)
(* Environment variable [[BPCOPTIONS]], if set, contains *)
(* a comma-separated list of flags like [[throttle]] or *)
(* [[norun]]. Function [[hasOption]] detects if a flag *)
(* is present.                                  *)
(* <boxed values 125>=                          *)
val _ = op hasOption : string -> bool
end
(* <collections with mapping and combining functions>= *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* \qbreak                                      *)
(*                                              *)
(* Collections                                  *)
(*                                              *)
(* [*] In the functions above, a set has the same *)
(* representation as a list, and they can be used *)
(* interchangeably. Sometimes, however, the thing you're *)
(* collecting is itself a set, and you want to  *)
(* distinguish the two forms of set (for an example, see *)
(* \crefpageadt.ex.exhaustiveness). For that purpose, *)
(* I define a type [[collection]] that is distinct from *)
(* the set/list type.                           *)
(* <boxed values 120>=                          *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* <collections with mapping and combining functions>= *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* The [[collection]] type is intended to be used some *)
(* more functions that are defined below. In particular, *)
(* functions [[joinC]] and [[mapC]], together with *)
(* [[singleC]], form a monad. (If you've heard of *)
(* monads, you may know that they are a useful  *)
(* abstraction for containers and collections of all *)
(* kinds; they also have more exotic uses, such as *)
(* expressing input and output as pure functions. The  *)
(* [[collection]] type is the monad for nondeterminism, *)
(* which is to say, all possible combinations or *)
(* outcomes. If you know about monads, you may have *)
(* picked up some programming tricks you can reuse. But *)
(* you don't need to know monads to do any of the *)
(* exercises in this book.)                     *)
(*                                              *)
(* The key functions on collections are as follows: *)
(*                                              *)
(*   • Functions [[mapC]] and [[filterC]] do for *)
(*  collections what [[map]] and [[filter]] do for *)
(*  lists.                                      *)
(*   • Function [[joinC]] takes a collection of *)
(*  collections of tau's and reduces it to a single *)
(*  collection of tau's. When [[mapC]] is used with a *)
(*  function that itself returns a collection,  *)
(*  [[joinC]] usually follows, as exemplified in the *)
(*  implementation of [[mapC2]] below.          *)
(*   • Function [[mapC2]] is the most powerful of *)
(*  all—its type resembles the type of Standard ML's *)
(*  [[ListPair.map]], but it works differently: where *)
(*  [[ListPair.map]] takes elements pairwise,   *)
(*  [[mapC2]] takes all possible combinations.  *)
(*  In particular, if you give [[ListPair.map]] two *)
(*  lists containing N and M elements respectively, *)
(*  the number of elements in the result is min(N,M). *)
(*  If you give collections of size N and M to  *)
(*  [[mapC2]], the number of elements in the result *)
(*  is N\atimesM.                               *)
(*                                              *)
(* \nwnarrowboxes                               *)
(* <boxed values 121>=                          *)
val _ = op joinC   : 'a collection collection -> 'a collection
val _ = op mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
val _ = op filterC : ('a -> bool)    -> ('a collection -> 'a collection)
val _ = op mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
(* <suspensions>=                               *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(* Suspensions: repeatable access to the result of one *)
(* action                                       *)
(*                                              *)
(* Streams are built around a single abstraction: the *)
(* suspension, which is also called a thunk.    *)
(* A suspension of type [['a susp]] represents a value *)
(* of type [['a]] that is produced by an action, like *)
(* reading a line of input. The action is not performed *)
(* until the suspension's value is demanded by function *)
(* [[demand]]. [If~you're familiar with suspensions or *)
(* with lazy computation in general, you know that the *)
(* function [[demand]] is traditionally called  *)
(* [[force]]. But I~use the name [[force]] to refer to a *)
(* similar function in the \uhaskell\ interpreter, which *)
(* implements a full language around the idea of lazy *)
(* computation. It~is possible to have two functions *)
(* called [[force]]---they can coexist peacefully---but *)
(* I~think it's too confusing. So~the less important *)
(* function, which is presented here, is called *)
(* [[demand]]. Even though my \uhaskell\ chapter never *)
(* made it into the book.] The action itself is *)
(* represented by a function of type \monoboxunit -> 'a. *)
(* A suspension is created by passing an action to the *)
(* function [[delay]]; at that point, the action is *)
(* ``pending.'' If [[demand]] is never called, the *)
(* action is never performed and remains pending. The *)
(* first time [[demand]] is called, the action is *)
(* performed, and the suspension saves the result that *)
(* is produced. \stdbreak If [[demand]] is called *)
(* multiple times, the action is still performed just *)
(* once—later calls to [[demand]] don't repeat the *)
(* action; instead they return the value previously *)
(* produced.                                    *)
(*                                              *)
(* To implement suspensions, I use a standard   *)
(* combination of imperative and functional code. *)
(* A suspension is a reference to an [[action]], which *)
(* can be pending or can have produced a result. *)
(* <boxed values 135>=                          *)
type 'a susp = 'a susp
(* <suspensions>=                               *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* <boxed values 136>=                          *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
(* Streams: results of a sequence of actions    *)
(*                                              *)
(* [*] A stream behaves much like a list, except that *)
(* the first time an element is inspected, an action *)
(* might be taken. And unlike a list, a stream can be *)
(* infinite. My code uses streams of lines, streams of *)
(* characters, streams of definitions, and even streams *)
(* of source-code locations. In this section I define *)
(* streams and many related utility functions. Most of *)
(* the utility functions are inspired by list functions *)
(* like [[map]], [[filter]], [[concat]], [[zip]], and *)
(* [[foldl]].                                   *)
(*                                              *)
(* Stream representation and basic functions    *)
(*                                              *)
(* The representation of a stream takes one of three *)
(* forms: [There are representations that use fewer *)
(* forms, but this one has the merit that I~can define a *)
(* polymorphic empty stream without running afoul of \ *)
(* ml's ``value restriction.'']                 *)
(*                                              *)
(*   • The [[EOS]] constructor represents an empty *)
(*  stream.                                     *)
(*                                              *)
(*   • The [[:::]] constructor (pronounced ``cons''), *)
(*  which should remind you of ML's [[::]]      *)
(*  constructor for lists, represents a stream in *)
(*  which an action has already been taken, and the *)
(*  first element of the stream is available (as are *)
(*  the remaining elements). Like the [[::]]    *)
(*  constructor for lists, the [[:::]] constructor is *)
(*  written as an infix operator.               *)
(*                                              *)
(*   • The [[SUSPENDED]] constructor represents a stream *)
(*  in which the action needed to produce the next *)
(*  element may not yet have been taken. Getting the *)
(*  element requires demanding a value from a   *)
(*  suspension, and if the action in the suspension *)
(*  is pending, it is performed at that time.   *)
(*                                              *)
(* [*]                                          *)
(* <streams>=                                   *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* <streams>=                                   *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* <streams>=                                   *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* <boxed values 137>=                          *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <boxed values 137>=                          *)
val _ = op streamOfList : 'a list -> 'a stream
(* <streams>=                                   *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* <streams>=                                   *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* Function [[listOfStream]] creates a list from a *)
(* stream. It is useful for debugging.          *)
(* <boxed values 138>=                          *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 138>=                          *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* <streams>=                                   *)
fun streamOfEffects action =
  delayedStream (fn () => case action ()
                            of NONE   => EOS
                             | SOME a => a ::: streamOfEffects action)
(* Creating streams using actions and functions *)
(*                                              *)
(* Function [[streamOfEffects]] produces the stream of *)
(* results obtained by repeatedly performing a single *)
(* action (like reading a line of input). \stdbreak The *)
(* action must have type [[unit -> 'a option]]; the *)
(* stream performs the action repeatedly, producing a *)
(* stream of [['a]] values until performing the action *)
(* returns [[NONE]].                            *)
(* <boxed values 139>=                          *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* And [[xdef]] parses all the extended definitions. *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 140>=                          *)
type line = line
val _ = op filelines : TextIO.instream -> line stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <streams>=                                   *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* Where [[streamOfEffects]] produces the results of *)
(* repeating a single action again and again,   *)
(* [[streamRepeat]] repeats a single value again and *)
(* again. This operation might sound useless, but here's *)
(* an example: suppose we read a sequence of lines from *)
(* a file, and for error reporting, we want to tag each *)
(* line with its source location, i.e., file name and *)
(* line number. Well, the file names are all the same, *)
(* and one easy way to associate the same file name with *)
(* every line is to repeat the file name indefinitely, *)
(* then join the two streams using [[streamZip]]. *)
(* Function [[streamRepeat]] creates an infinite stream *)
(* that repeats a value. It works on values of any type. *)
(* <boxed values 141>=                          *)
val _ = op streamRepeat : 'a -> 'a stream
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <streams>=                                   *)
fun streamOfUnfold next state =
  delayedStream
    (fn () => case next state
                of NONE => EOS
                 | SOME (a, state') => a ::: streamOfUnfold next state')
(* A more sophisticated way to produce a stream is to *)
(* use a function that depends on an evolving state of *)
(* some unknown type [['b]]. The function is applied to *)
(* a state (of type [['b]]) and may produce a pair *)
(* containing a value of type [['a]] and a new state. *)
(* Repeatedly applying the function can produce a *)
(* sequence of results of type [['a]]. This operation, *)
(* in which a function is used to expand a value into a *)
(* sequence, is the dual of the fold operation, which is *)
(* used to collapse a sequence into a value. The new *)
(* operation is therefore called unfold.        *)
(* <boxed values 142>=                          *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* Function [[streamOfUnfold]] can turn any ``get'' *)
(* function into a stream. In fact, the unfold and get *)
(* operations should obey the following algebraic law: *)
(*                                              *)
(*  streamOfUnfold streamGet xs ===xs\text.     *)
(*                                              *)
(* Another useful ``get'' function is [[(fn n => SOME *)
(* (n, n+1))]]; passing this function to        *)
(* [[streamOfUnfold]] results in an infinite stream of *)
(* increasing integers. [*]                     *)

(* <streams>=                                   *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* <boxed values 143>=                          *)
val _ = op naturals : int stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* (Streams, like lists, support not only unfolding but *)
(* also folding. The fold function [[streamFold]] is *)
(* defined below in chunk [->].)                *)
(*                                              *)
(* Attaching extra actions to streams           *)
(*                                              *)
(* A stream built with [[streamOfEffects]] or   *)
(* [[filelines]] has an imperative action built in. *)
(* But in an interactive interpreter, the action of *)
(* reading a line should be preceded by another action: *)
(* printing the prompt. And deciding just what prompt to *)
(* print requires orchestrating other actions.  *)
(* One option, which I use below, is to attach an *)
(* imperative action to a ``get'' function used with *)
(* [[streamOfUnfold]]. Another option, which is *)
(* sometimes easier to understand, is to attach an *)
(* action to the stream itself. Such an action could *)
(* reasonably be performed either before or after the *)
(* action of getting an element from the stream. *)
(*                                              *)
(* Given an action [[pre]] and a stream xs, I define a *)
(* stream \monoboxpreStream (pre, xs) that adds \monobox *)
(* pre () to the action performed by the stream. Roughly *)
(* speaking,                                    *)
(*                                              *)
(*  \monostreamGet (preStream (pre, xs)) = \mono(pre *)
(*  (); streamGet xs).                          *)
(*                                              *)
(* (The equivalence is only rough because the pre action *)
(* is performed lazily, only when an action is needed to *)
(* get a value from xs.)                        *)

(* <streams>=                                   *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* It's also useful to be able to perform an action *)
(* immediately after getting an element from a stream. *)
(* In [[postStream]], I perform the action only if *)
(* [[streamGet]] succeeds. By performing the [[post]] *)
(* action only when [[streamGet]] succeeds, I make it *)
(* possible to write a [[post]] action that has access *)
(* to the element just gotten. Post-get actions are *)
(* especially useful for debugging.             *)

(* <streams>=                                   *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* <boxed values 144>=                          *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 144>=                          *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* <boxed values 145>=                          *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <streams>=                                   *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* <boxed values 146>=                          *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 147>=                          *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
(* <streams>=                                   *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* <streams>=                                   *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* Function [[streamZip]] returns a stream that is as *)
(* long as the shorter of the two argument streams. *)
(* In particular, if [[streamZip]] is applied to a *)
(* finite stream and an infinite stream, the result is a *)
(* finite stream.                               *)
(* <boxed values 148>=                          *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* Concatenation turns a stream of streams of tau's into *)
(* a single stream of tau's. I define it using a *)
(* [[streamOfUnfold]] with a two-part state: the first *)
(* element of the state holds an initial [[xs]], \qbreak *)
(* and the second part holds the stream of all remaining *)
(* streams, [[xss]]. To concatenate the stream of *)
(* streams [[xss]], I use an initial state of [[(EOS, *)
(* xss)]].                                      *)
(* <boxed values 148>=                          *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* <boxed values 149>=                          *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* The code used to append two streams is much like the *)
(* code used to concatenate arbitrarily many streams. *)
(* To avoid duplicating the tricky manipulation of *)
(* states, I implement append using concatenation. *)
(* <boxed values 150>=                          *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <streams>=                                   *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(* Whenever I rename bound variables, for example in a *)
(* type \/\ldotsnalpha\alldottau, I have to choose new *)
(* names that don't conflict with existing names in tau *)
(* or in the environment. The easiest way to get good *)
(* names to build an infinite stream of names by using *)
(* [[streamMap]] on [[naturals]], then use      *)
(* [[streamFilter]] to choose only the good ones, and *)
(* finally to take exactly as many good names as I need *)
(* by calling [[streamTake]], which is defined here. *)
(* <boxed values 151>=                          *)
val _ = op streamTake : int * 'a stream -> 'a list
(* <streams>=                                   *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* <boxed values 152>=                          *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* <stream transformers and their combinators>= *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* Stream transformers, which act as parsers    *)
(*                                              *)
(* The purpose of a parser is to turn streams of input *)
(* lines into streams of definitions. Intermediate *)
(* representations may include streams of characters, *)
(* tokens, types, expressions, and more. To handle all *)
(* these different kinds of streams using a single set *)
(* of operators, I define a type representing a stream *)
(* transformer. A stream transformer from A to B takes a *)
(* stream of A's as input and either succeeds, fails, or *)
(* detects an error:                            *)
(*                                              *)
(*   • If it succeeds, it consumes zero or more A's from *)
(*  the input stream and produces exactly one B. *)
(*  It returns a pair containing [[OK]] B plus  *)
(*  whatever A's were not consumed.             *)
(*   • If it fails, it returns [[NONE]].      *)
(*   • If it detects an error, it returns a pair *)
(*  containing [[ERROR]] m, where m is a message, *)
(*  plus whatever A's were not consumed.        *)
(*                                              *)
(* A stream transformer from A to B has type \monobox(A, *)
(* B) transformer.                              *)
(* <boxed values 327>=                          *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun pure y = fn xs => SOME (OK y, xs)
(* The stream-transformer abstraction supports many, *)
(* many operations. These operations, known as parsing *)
(* combinators, have been refined by functional *)
(* programmers for over two decades, and they can be *)
(* expressed in a variety of guises. The guise I have *)
(* chosen uses notation from applicative functors and *)
(* from the ParSec parsing library.             *)
(*                                              *)
(* I begin very abstractly, by presenting combinators *)
(* that don't actually consume any inputs. The next two *)
(* sections present only ``constant'' transformers and *)
(* ``glue'' functions that build transformers from other *)
(* transformers. With those functions in place, *)
(* I proceed to real, working parsing combinators. These *)
(* combinators are split into two groups: ``universal'' *)
(* combinators that work with any stream, and   *)
(* ``parsing'' combinators that expect a stream of *)
(* tokens with source-code locations.           *)
(*                                              *)
(* My design includes a lot of combinators. Too many, *)
(* really. I would love to simplify the design, but *)
(* simplifying software can be hard, and I don't want to *)
(* delay the book by another year.              *)
(*                                              *)
(* --- #2                                       *)
(* \newskip\myskip \myskip=4pt                  *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \catcode`=\other \catcode`_=\other \catcode`$=\other *)
(*                                              *)
(*  Stream transformers; applying functions to  *)
(*  transformers                                *)
(*  \type('a, 'b) xformer \                     *)
(*  tableboxpure : 'b -> ('a, 'b)               *)
(*  xformer \splitbox<*>('a, 'b ->              *)
(*  'c) xformer * ('a, 'b)                      *)
(*  xformer-> ('a, 'c) xformer \                *)
(*  tablebox<> : ('b -> 'c) * ('a,              *)
(*  'b) xformer -> ('a, 'c)                     *)
(*  xformer \tablebox<>? : ('b ->               *)
(*  'c option) * ('a, 'b) xformer               *)
(*  -> ('a, 'c) xformer \splitbox               *)
(*  <*>!('a, 'b -> 'c error)                    *)
(*  xformer * ('a, 'b) xformer->                *)
(*  ('a, 'c) xformer \tablebox<>!               *)
(*  : ('b -> 'c error) * ('a, 'b)               *)
(*  xformer -> ('a, 'c) xformer                 *)
(*  [8pt] Functions useful with                 *)
(*  [[<>]] and [[<*>]]                          *)
(*  \tableboxfst : ('a * 'b) -> 'a              *)
(*  \tableboxsnd : ('a * 'b) -> 'b              *)
(*  \tableboxpair : 'a -> 'b -> 'a              *)
(*  * 'b \tableboxcurry : ('a * 'b              *)
(*  -> 'c) -> ('a -> 'b -> 'c) \                *)
(*  tableboxcurry3 : ('a * 'b * 'c              *)
(*  -> 'd) -> ('a -> 'b -> 'c ->                *)
(*  'd) [8pt] Combining                         *)
(*  transformers in sequence,                   *)
(*  alternation, or conjunction                 *)
(*  \tablebox<* : ('a, 'b) xformer >]] : ('a, 'b) *)
(*  * ('a, 'c) xformer -> ('a, 'b) xformer * ('a, 'c) *)
(*  xformer \tablebox *> : ('a,    xformer -> ('a, *)
(*  'b) xformer * ('a, 'c) xformer 'c) xformer [8pt] *)
(*  -> ('a, 'c) xformer \tablebox< Transformers *)
(*  : 'b * ('a, 'c) xformer ->     useful for both *)
(*  ('a, 'b) xformer \tablebox<|>  lexical analysis *)
(*  : ('a, 'b) xformer * ('a, 'b)  and parsing  *)
(*  xformer -> ('a, 'b) xformer \               *)
(*  tableboxpzero : ('a, 'b)                    *)
(*  xformer \tableboxanyParser :                *)
(*  ('a, 'b) xformer list -> ('a,               *)
(*  'b) xformer \tablebox[[<                    *)
(*  \tableboxone : ('a, 'a)                     *)
(*  xformer \tableboxeos : ('a,                 *)
(*  unit) xformer \tableboxsat :                *)
(*  ('b -> bool) -> ('a, 'b)                    *)
(*  xformer -> ('a, 'b) xformer \               *)
(*  tableboxeqx : ''b -> ('a, ''b)              *)
(*  xformer -> ('a, ''b) xformer                *)
(*  notFollowedBy                               *)
(*                                 ('a, 'b) xformer *)
(*                                 -> ('a, unit) *)
(*                                 xformer      *)
(*  \tableboxmany : ('a, 'b)                    *)
(*  xformer -> ('a, 'b list)                    *)
(*  xformer \tableboxmany1 : ('a,               *)
(*  'b) xformer -> ('a, 'b list)                *)
(*  xformer \tableboxoptional :                 *)
(*  ('a, 'b) xformer -> ('a, 'b                 *)
(*  option) xformer \tableboxpeek               *)
(*  : ('a, 'b) xformer -> 'a                    *)
(*  stream -> 'b option \tablebox               *)
(*  rewind : ('a, 'b) xformer ->                *)
(*  ('a, 'b) xformer                            *)
(*                                              *)
(* Stream transformers and their combinators [*] *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Error-free transformers and their composition *)
(*                                              *)
(* The [[pure]] combinator takes a value [[y]] of type B *)
(* as argument. It returns an \atob transformer that *)
(* consumes no A's as input and produces [[y]]. *)
(* <boxed values 328>=                          *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
infix 3 <*>
fun tx_f <*> tx_b =
  fn xs => case tx_f xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK f, xs) =>
                  case tx_b xs
                    of NONE => NONE
                     | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
                     | SOME (OK y, xs) => SOME (OK (f y), xs)
(* To build a stream transformer that reads inputs in *)
(* sequence, I compose smaller stream transformers that *)
(* read parts of the input. The sequential composition *)
(* operator may look quite strange. To compose [[tx_f]] *)
(* and [[tx_b]] in sequence, I use the infix operator *)
(* [[<*>]], which is pronounced ``applied to.'' The *)
(* composition is written \monobox[[tx_f]] <*> [[tx_b]], *)
(* and it works like this:                      *)
(*                                              *)
(*  1. First [[tx_f]] reads some A's and produces a *)
(*  function [[f]] of type B -->C.              *)
(*  2. Next [[tx_b]] reads some more A's and produces a *)
(*  value [[y]] of type B.                      *)
(*  3. The combination [[tx_f <*> tx_b]] reads no more *)
(*  input but simply applies [[f]] to [[y]] and *)
(*  returns \monoboxf y (of type C) as its result. *)
(*                                              *)
(* This idea may seem crazy. How can reading a sequence *)
(* of A's produce a function? The secret is that almost *)
(* always, the function is produced by [[pure]], without *)
(* actually reading any A's, or it's the result of using *)
(* the [[<*>]] operator to apply a Curried function to *)
(* its first argument. But the                  *)
(* read-and-produce-a-function idiom is a great way to *)
(* do business, because when the parser is written using *)
(* the [[pure]] and [[<*>]] combinators, the code *)
(* resembles a Curried function application.    *)
(*                                              *)
(* For the combination [[tx_f <*> tx_b]] to succeed, *)
(* both [[tx_f]] and [[tx_b]] must succeed. Ensuring *)
(* that two transformers succeed requires a nested case *)
(* analysis.                                    *)
(* <boxed values 329>=                          *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 330>=                          *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <stream transformers and their combinators>= *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* As an example, if a name is parsed by [[name]] and an *)
(* expression is parsed by [[exp]], then a name followed *)
(* by an expression, such as might appear in a [[let]] *)
(* binding, can be turned into (name, expression) pair *)
(* by the parser {nwverbatim} pair <> name <*> exp  *)
(* {nwverbatim} (Parsing the actual micro-Scheme syntax *)
(* would also require a parser to handle the surrounding *)
(* parentheses.) As another example, if a micro-Scheme *)
(* parser has seen a left bracket followed by the *)
(* keyword [[if]], it can call the parser {nwverbatim} *)
(* curry3 IFX <> exp <*> exp <*> exp {nwverbatim} which *)
(* creates the abstract-syntax tree for an [[if]] *)
(* expression.                                  *)
(*                                              *)
(* The combinator [[<*>]] creates parsers that read *)
(* things in sequence; but it can't make a choice. *)
(* If any parser in the sequence fails, the whole *)
(* sequence fails. A choice, as in ``[[val]] or *)
(* expression or [[define]] or [[use]],'' is made by a *)
(* choice operator. The choice operator is written *)
(* [[<|>]] and pronounced ``or.'' If [[t1]] and [[t2]] *)
(* are both \atob transformers, then \monoboxt1 <|> t2 *)
(* is an \atob transformer that first tries [[t1]], then *)
(* tries [[t2]]. Transformer \monoboxt1 <|> t2 succeeeds *)
(* if either [[t1]] or [[t2]] succeeds, detects an error *)
(* if either [[t1]] or [[t2]] detects an error, and *)
(* fails only if both [[t1]] and [[t2]] fail. To assure *)
(* that \monoboxt1 <|> t2 has a predictable type no *)
(* matter which transformer is chosen, both [[t1]] and  *)
(* [[t2]] have to have the same type.           *)
(* <boxed values 332>=                          *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* I sometimes want to combine a list of parsers with *)
(* the choice operator. I can do this by folding over *)
(* the list, provided I have a ``zero'' parser, which *)
(* always fails.                                *)

(* <stream transformers and their combinators>= *)
fun pzero _ = NONE
(* <stream transformers and their combinators>= *)
fun anyParser ts = 
  foldr op <|> pzero ts
(* <boxed values 333>=                          *)
val _ = op pzero : ('a, 'b) xformer
(* This parser obeys the algebraic law          *)
(*                                              *)
(*  \monoboxt <|> pzero = \monoboxpzero <|> t = \ *)
(*  monoboxt\text.                              *)
(*                                              *)
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)
(* <boxed values 333>=                          *)
val _ = op anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <stream transformers and their combinators>= *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* Ignoring results produced by transformers    *)
(*                                              *)
(* If a parser sees the stream of tokens {indented} *)
(* [[(]]                                        *)
(* [[if]]                                       *)
(* [[(]]                                        *)
(* [[<]]                                        *)
(* [[x]]                                        *)
(* [[y]]                                        *)
(* [[)]]                                        *)
(* [[x]]                                        *)
(* [[y]]                                        *)
(* [[)]] , {indented} I want it to build an     *)
(* abstract-syntax tree using [[IFX]] and three *)
(* expressions. The parentheses and keyword [[if]] serve *)
(* to identify the [[if]]-expression and to make sure it *)
(* is well formed, so the parser does have to read them *)
(* from the input, but it doesn't need to do anything *)
(* with the results that are produced. Using a parser *)
(* and then ignoring the result is such a common *)
(* operation that special abbreviations have evolved to *)
(* support it.                                  *)
(*                                              *)
(* The abbreviations are formed by modifying the [[<*>]] *)
(* or [[<>]] operator to remove the angle bracket on the *)
(* side containing the result to be ignored. For *)
(* example,                                     *)
(*                                              *)
(*   • Parser [[p1 <* p2]] reads the input of [[p1]] and *)
(*  then the input of [[p2]], but it returns only the *)
(*  result of [[p1]].                           *)
(*   • Parser [[p1 *> p2]] reads the input of [[p1]] and *)
(*  then the input of [[p2]], but it returns only the *)
(*  result of [[p2]].                           *)
(*   • Parser [[v < p]] parses the input the way [[p]] *)
(*   does, but it then ignores [[p]]'s result and *)
(*  instead produces the value [[v]].           *)
(*                                              *)
(* <boxed values 334>=                          *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* At last, transformers that look at the input stream *)
(*                                              *)
(* None of the transformers above inspects an input *)
(* stream. The fundamental operations are [[pure]], *)
(* [[<*>]], and [[<|>]]; [[pure]] never looks at the *)
(* input, and [[<*>]] and [[<|>]] simply sequence or *)
(* alternate between other parsers which do the actual *)
(* looking. Those parsers are up next.          *)
(*                                              *)
(* The simplest input-inspecting parser is [[one]]. It's *)
(* an \atoa transformer that succeeds if and only if *)
(* there is a value in the input. If there's no value in *)
(* the input, [[one]] fails; it never signals an error. *)
(* <boxed values 335>=                          *)
val _ = op one : ('a, 'a) xformer
(* <stream transformers and their combinators>= *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* The counterpart of [[one]] is a parser that succeeds *)
(* if and only if there is no input—that is, if it is *)
(* parsing the end of a stream. This parser, which is *)
(* called [[eos]] (``end of stream''), can produce no *)
(* useful result, so it produces the empty tuple, which *)
(* has type [[unit]].                           *)
(* <boxed values 336>=                          *)
val _ = op eos : ('a, unit) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Perhaps surprisingly, these are the only two standard *)
(* parsers that inspect input. The only other parsing *)
(* combinator that looks directly at input is the *)
(* function [[stripAndReportErrors]], which removes *)
(* [[ERROR]] and [[OK]] from error streams.     *)

(* <stream transformers and their combinators>= *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* It is sometimes useful to look at input without *)
(* consuming it. For this purpose I define two  *)
(* functions: [[peek]] just looks at a transformed *)
(* stream and maybe produces a value, whereas [[rewind]] *)
(* changes any transformer into a transformer that *)
(* behaves identically, but that doesn't consume any *)
(* input. I use these functions either to debug, or to *)
(* find the source-code location of the next token in a *)
(* token stream.                                *)
(* <boxed values 337>=                          *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <stream transformers and their combinators>= *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* Given a transformer [[tx]], transformer \monobox *)
(* rewind tx computes the same value as [[tx]], but when *)
(* it's done, it rewinds the input stream back to where *)
(* it was before it ran [[tx]]. The actions performed by *)
(* [[tx]] can't be undone, but the inputs can be read *)
(* again.                                       *)
(* <boxed values 338>=                          *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <stream transformers and their combinators>= *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* Parsing combinators                          *)
(*                                              *)
(* Real parsers use [[<>]], [[<*>]], [[<|>]], and *)
(* [[one]] as a foundation, then add ideas like these: *)
(*                                              *)
(*   • Maybe the parser should succeed only if an input *)
(*  satisfies certain conditions. For example, if *)
(*  I want to parse numeric literals, I might want a *)
(*  character parser that succeeds only when the *)
(*  character is a digit.                       *)
(*   • Most utterances in programming languages are made *)
(*  by composing things in sequence. For example, in *)
(*  micro-Scheme, the characters in an identifier are *)
(*  a nonempty sequence of ``ordinary'' characters. *)
(*  And the arguments in a function application are a *)
(*  possibly empty sequence of expressions. Parser *)
(*  combinators for sequences are useful!       *)
(*   • Although I've avoided using ``optional'' syntax *)
(*  in the bridge languages, many, many programming *)
(*  languages do use constructs in which parts are *)
(*  optional. For example, in C, the use of an  *)
(*  [[else]] clause with an [[if]] statement is *)
(*  optional. A parser combinator for this idiom can *)
(*  also be useful.                             *)
(*                                              *)
(* This section presents standard parsing combinators *)
(* that help implement conditional parsers, parsers for *)
(* sequences, and parsers for optional syntax.  *)
(*                                              *)
(* Parsers based on conditions                  *)
(*                                              *)
(* Combinator [[sat]] wraps an \atob transformer with a *)
(* B-predicate such that the wrapped transformer *)
(* succeeds only when the underlying transformer *)
(* succeeds and produces a value that satisfies the *)
(* predicate.                                   *)
(* <boxed values 339>=                          *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 340>=                          *)
val _ = op eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <stream transformers and their combinators>= *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* A predicate of type \monobox('b -> bool) asks, ``Is *)
(* this a thing?'' But sometimes code wants to ask, ``Is *)
(* this a thing, and if so, what thing is it?'' *)
(* For example, a parser for Impcore or micro-Scheme *)
(* will want to know if an atom represents a numeric *)
(* literal, but if so, it would also like to know what *)
(* number is represented. Instead of a predicate, the *)
(* parser would use a function of type \monoboxatom -> *)
(* int option. In general, an \atob transformer can be *)
(* composed with a function of type \monoboxB -> C *)
(* option, and the result is an \atoxC transformer. *)
(* Because there's a close analogy with the application *)
(* operator [[<>]], I notate the composition operator as *)
(* [[<>?]], with a question mark.               *)
(* <boxed values 341>=                          *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* Transformer \monoboxf [[<>?]] tx can be defined as \ *)
(* monoboxvalOf [[<>]] sat isSome (f [[<>]] tx), but *)
(* writing out the cases helps clarify what's going on. *)
(*                                              *)
(* A transformer might be run only if a another *)
(* transformer succeeds on the same input. For example, *)
(* the parser for uSmalltalk tries to parse an array *)
(* literal only when it knows the input begins with a *)
(* left bracket. Transformer \monoboxt1 [[< --- >]] t2 *)
(* succeeds only if both [[t1]] and [[t2]] succeed at *)
(* the same point. An error in [[t1]] is treated as *)
(* failure. The combined transformer looks at enough *)
(* input to decide if [[t1]] succeeds, but it does not *)
(* consume input consumed by [[t1]]—it consumes only the *)
(* input of [[t2]].                             *)
(* <boxed values 342>=                          *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <stream transformers and their combinators>= *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* A transformer can be complemented, turning success *)
(* into failure and vice versa. Transformer \monobox *)
(* notFollowedBy t succeeds if and only if [[t]] fails. *)
(* Transformer \monoboxnotFollowedBy t may look at *)
(* input, but it never consumes any input. This *)
(* transformer is used when trying to read an integer *)
(* literal, to make sure that the digits are not *)
(* followed by a letter or other non-delimiting symbol. *)
(* <boxed values 343>=                          *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* <stream transformers and their combinators>= *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* Adding [[< --- >]] and [[notFollowedBy]] to our *)
(* library gives it the flavor of a little Boolean *)
(* algebra for transformers: functions [[< --- >]], *)
(* [[<|>]], and [[notFollowedBy]] play the roles of *)
(* ``and,'' ``or,'' and ``not,'' and [[pzero]] plays the *)
(* role of ``false.''                           *)
(*                                              *)
(* Transformers for sequences                   *)
(*                                              *)
(* Concrete syntax is full of sequences. A function *)
(* takes a sequence of arguments, a program is a *)
(* sequence of definitions, and a method definition *)
(* contains a sequence of expressions. To create *)
(* transformers that process sequences, I define *)
(* functions [[many]] and [[many1]]. If [[t]] is an \ *)
(* atob transformer, then \monoboxmany t is an \atox *)
(* list-of-B transformer. It runs [[t]] as many times as *)
(* possible. And even if [[t]] fails, \monoboxmany t *)
(* always succeeds: when [[t]] fails, \monoboxmany t *)
(* returns an empty list of B's.                *)
(* <boxed values 344>=                          *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* I'd really like to write that first alternative as *)
(*                                              *)
(*  [[curry (op ::) <> t <*> many t]]           *)
(*                                              *)
(* but that formulation leads to instant death by *)
(* infinite recursion. In your own parsers, it's a *)
(* problem to watch out for.                    *)
(*                                              *)
(* Sometimes an empty list isn't acceptable. In such *)
(* cases, I use \monoboxmany1 t, which succeeds only if *)
(* [[t]] succeeds at least once—in which case it returns *)
(* a nonempty list.                             *)

(* <stream transformers and their combinators>= *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* <boxed values 345>=                          *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Although \monoboxmany t always succeeds, \monobox *)
(* many1 t can fail.                            *)

(* <stream transformers and their combinators>= *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* Both [[many]] and [[many1]] are ``greedy''; that is, *)
(* they repeat [[t]] as many times as possible. Client *)
(* code has to be careful to ensure that calls to *)
(* [[many]] and [[many1]] terminate. In particular, if *)
(* [[t]] can succeed without consuming any input, then \ *)
(* monoboxmany t does not terminate. To pass [[many]] a *)
(* transformer that succeeds without consuming input is *)
(* therefor an unchecked run-time error. The same goes *)
(* for [[many1]].                               *)
(*                                              *)
(* Client code also has to be careful that when [[t]] *)
(* sees something it doesn't recognize, it doesn't *)
(* signal an error. In particular, [[t]] had better not *)
(* be built with the [[<?>]] operator defined in \ *)
(* chunkrefmlinterps.chunk.<?> below.           *)
(*                                              *)
(* Sometimes instead of zero, one, or many B's, concrete *)
(* syntax calls for zero or one; such a B might be *)
(* called ``optional.'' For example, a numeric literal *)
(* begins with an optional minus sign. Function *)
(* [[optional]] turns an \atob transformer into an \atox *)
(* optional-B transformer. Like \monoboxmany t, \monobox *)
(* optional t always succeeds.                  *)
(* <boxed values 346>=                          *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <stream transformers and their combinators>= *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* Transformers made with [[many]] and [[optional]] *)
(* succeed even when there is no input. They also *)
(* succeed when there is input that they don't  *)
(* recognize.                                   *)
(*                                              *)
(* Error-detecting transformers and their composition *)
(*                                              *)
(* Sometimes an error is detected not by a parser but by *)
(* a function that is applied to the results of parsing. *)
(* A classic example is a function definition: if the *)
(* formal parameters are syntactically correct but *)
(* contain a duplicate name, an error should be *)
(* signaled. Formal parameters could be handled by a *)
(* parser whose result type is \monoboxname list *)
(* error—but every transformer type already includes the *)
(* possibility of error! I would prefer that the *)
(* parser's result type be just \monoboxname list, and *)
(* that if duplicate names are detected, that the error *)
(* be managed in the same way as a syntax error. *)
(* To enable such management, I define [[<*>!]] and [[< *)
(* >!]] combinators, which merge function-detected *)
(* errors with parser-detected errors. \nwnarrowboxes *)
(* <boxed values 347>=                          *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)                 * ('a, 'b) xformer -> ('a, 'c
                                                                       ) xformer
(* <support for source-code locations and located streams>= *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
(* Source-code locations are useful when reading code *)
(* from a file. When reading code interactively, *)
(* however, a message that says the error occurred ``in *)
(* standard input, line 12,'' is more annoying than *)
(* helpful. As in the C code in \crefpage       *)
(* (cinterps.error-format, I use an error format to *)
(* control when error messages include source-code *)
(* locations. The format is initially set to include *)
(* them. [*]                                    *)
(* <support for source-code locations and located streams>= *)
datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
val toplevel_error_format = ref WITH_LOCATIONS
(* The format is consulted by function [[synerrormsg]], *)
(* which produces the message that accompanies a syntax *)
(* error. The source location may be omitted only for *)
(* standard input; error messages about files loaded *)
(* with [[use]] are always accompanied by source-code *)
(* locations.                                   *)
(* <support for source-code locations and located streams>= *)
fun synerrormsg (source, line) strings =
  if !toplevel_error_format = WITHOUT_LOCATIONS
  andalso source = "standard input"
  then
    concat ("syntax error: " :: strings)
  else    
    concat ("syntax error in " :: srclocString (source, line) :: ": " :: strings
                                                                               )

(* The same format determines how warnings are  *)
(* delivered.                                   *)
(* <support for source-code locations and located streams>= *)
fun warnAt (source, line) strings =
  ( app eprint
      (if !toplevel_error_format = WITHOUT_LOCATIONS
       andalso source = "standard input"
       then
         "warning: " :: strings
       else
         "warning in " :: srclocString (source, line) :: ": " :: strings)
  ; eprint "\n"
  )
(* Parsing bindings used in LETX forms          *)
(*                                              *)
(* A sequence of let bindings has both names and *)
(* expressions. To capture both, [[parseletbindings]] *)
(* returns a component with both [[names]] and [[exps]] *)
(* fields set.                                  *)
(* <support for source-code locations and located streams>= *)
exception Located of srcloc * exn
(* <support for source-code locations and located streams>= *)
type 'a located = srcloc * 'a
(* Tracking and reporting source-code locations *)
(*                                              *)
(* An error message is more informative if it says where *)
(* the error occurred. ``Where'' means a source-code *)
(* location. Compilers that take themselves seriously *)
(* report source-code locations right down to the *)
(* individual character: file broken.c, line 12, *)
(* column 17. In production compilers, such precision is *)
(* admirable. But in a pedagogical interpreter, *)
(* precision sometimes gives way to simplicity. A good *)
(* compromise is to track only source file and line *)
(* number. That's precise enough to help programmers *)
(* find errors, and it simplifies the implementation by *)
(* eliminating the bookkeeping that would otherwise be *)
(* needed to track column numbers.              *)
(* <boxed values 154>=                          *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 154>=                          *)
type 'a located = 'a located
(* <support for source-code locations and located streams>= *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* \qbreak The [[testIsGood]] function for Typed uScheme *)
           (* mirrors the [[testIsGood]] function for Typed *)
           (* Impcore, but the environments are different. Because *)
           (* the tests are also different, I didn't try to share *)
           (* the [[ty]] or [[outcome]] functions.         *)
           (* <more handlers for [[atLoc]]>=               *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
           (* <more handlers for [[atLoc]] ((type-checking))>= *)
           | e as TypeError _         => raise Located (loc, e)
           | e as BugInTypeChecking _ => raise Located (loc, e)
(* <boxed values 155>=                          *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <support for source-code locations and located streams>= *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* Function [[atLoc]] is often called by the    *)
(* higher-order function [[located]], which converts a *)
(* function that expects [['a]] into a function that *)
(* expects \monobox'a located. Function [[leftLocated]] *)
(* does something similar for a pair in which only the *)
(* left half must include a source-code location. *)
(* <boxed values 156>=                          *)
val _ = op located : ('a -> 'b) -> ('a located -> 'b)
val _ = op leftLocated : ('a * 'b -> 'c) -> ('a located * 'b -> 'c)
(* [[bindList]] tried to                        *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)

(* <support for source-code locations and located streams>= *)
fun fillComplaintTemplate (s, maybeLoc) =
  let val string_to_fill = " <at loc>"
      val (prefix, atloc) =
        Substring.position string_to_fill (Substring.full s)
      val suffix = Substring.triml (size string_to_fill) atloc
      val splice_in =
        Substring.full
          (case maybeLoc
             of NONE => ""
              | SOME (loc as (file, line)) =>
                  if !toplevel_error_format = WITHOUT_LOCATIONS
                  andalso file = "standard input"
                  then
                    ""
                  else
                    " in " ^ srclocString loc)
  in  if Substring.size atloc = 0 then (* <at loc> is not present *)
        s
      else
        Substring.concat [prefix, splice_in, suffix]
  end
fun fillAtLoc (s, loc) = fillComplaintTemplate (s, SOME loc)
fun stripAtLoc s = fillComplaintTemplate (s, NONE)
(* \qbreak A source-code location can appear anywhere in *)
(* an error message. To make it easy to write error *)
(* messages that include source-code locations, I define *)
(* function [[fillComplaintTemplate]]. This function *)
(* replaces the string \monobox"<at loc>" with a *)
(* reference to a source-code location—or if there is no *)
(* source-code location, it strips \monobox"<at loc>" *)
(* entirely. The implementation uses Standard ML's *)
(* [[Substring]] module.                        *)
(* <boxed values 157>=                          *)
val _ = op fillComplaintTemplate : string * srcloc option -> string
(* <support for source-code locations and located streams>= *)
fun synerrorAt msg loc = 
  ERROR (synerrormsg loc [msg])
(* <support for source-code locations and located streams>= *)
fun locatedStream (streamname, inputs) =
  let val locations =
        streamZip (streamRepeat streamname, streamDrop (1, naturals))
  in  streamZip (locations, inputs)
  end
(* To signal a syntax error at a given location, code *)
(* calls [[synerrorAt]]. [*]                    *)
(* <boxed values 158>=                          *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 158>=                          *)
val _ = op locatedStream : string * line stream -> line located stream
(* <streams that track line boundaries>=        *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(* <streams that track line boundaries>=        *)
local 
  fun asEol (EOL n) = SOME n
    | asEol (INLINE _) = NONE
  fun asInline (INLINE x) = SOME x
    | asInline (EOL _)    = NONE
in
  fun eol    xs = (asEol    <$>? one) xs
  fun inline xs = (asInline <$>? many eol *> one) xs
  fun srcloc xs = rewind (fst <$> inline) xs
end
(* Parsers: reading tokens and source-code locations *)
(*                                              *)
(* [*] To read definitions, expressions, and types, *)
(* it helps to work at a higher level of abstraction *)
(* than individual characters. All the parsers in this *)
(* book use two stages: first a lexer groups characters *)
(* into tokens, then a parser transforms tokens into *)
(* syntax. Not all languages use the same tokens, so the *)
(* code in this section assumes that the type [[token]] *)
(* and function [[tokenString]] are defined. Function *)
(* [[tokenString]] returns a string representation of *)
(* any given token; it is used in debugging. As an *)
(* example, the definitions used in micro-Scheme appear *)
(* in \crefmlschemea.chap (\cpagerefmlschemea.tokens). *)
(*                                              *)
(* Transforming a stream of characters to a stream of *)
(* tokens to a stream of definitions should sound *)
(* appealing, but it simplifies the story a little too *)
(* much. \qbreak That's because if something goes wrong, *)
(* a parser can't just throw up its hands. If an error *)
(* occurs,                                      *)
(*                                              *)
(*   • The parser should say where things went wrong—at *)
(*  what source-code location.                  *)
(*   • The parser should get rid of the bad tokens that *)
(*  caused the error.                           *)
(*   • The parser should be able to keep going, without *)
(*  having to kill the interpreter and start over. *)
(*                                              *)
(* To support error reporting and recovery takes a lot *)
(* of machinery. And that means a parser's input has to *)
(* contain more than just tokens.               *)
(*                                              *)
(* Flushing bad tokens                          *)
(*                                              *)
(* A standard parser for a batch compiler needs only to *)
(* see a stream of tokens and to know from what *)
(* source-code location each token came. A batch *)
(* compiler can simply read all its input and report all *)
(* the errors it wants to report. [Batch compilers vary *)
(* widely in the ambitions of their parsers. Some simple *)
(* parsers report just one error and stop. Some *)
(* sophisticated parsers analyze the entire input and *)
(* report the smallest number of changes needed to make *)
(* the input syntactically correct. And some    *)
(* ill-mannered parsers become confused after an error *)
(* and start spraying meaningless error messages. But *)
(* all of them have access to the entire input. *)
(* The~bridge-language interpreters don't. ] But an *)
(* interactive interpreter may not use an error as an *)
(* excuse to read an indefinite amount of input. It must *)
(* instead recover from the error and ready itself to *)
(* read the next line. To do so, it needs to know where *)
(* the line boundaries are! For example, if a parser *)
(* finds an error on line 6, it should read all the *)
(* tokens on line 6, throw them away, and start over *)
(* again on line 7. And it should do this without *)
(* reading line 7—reading line 7 will take an action and *)
(* will likely have the side effect of printing a *)
(* prompt. To mark line boundaries, I define a new type *)
(* constructor [[eol_marked]]. A value of type \monobox *)
(* 'a [[eol_marked]] is either an end-of-line marker, or *)
(* it contains a value of type [['a]] that occurs in a *)
(* line. A stream of such values can be drained up to *)
(* the end of the line.                         *)
(* <boxed values 356>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* \qbreak To support a stream of marked lines—possibly *)
(* marked, located lines—I define transformers [[eol]], *)
(* [[inline]], and [[srcloc]]. The [[eol]] transformer *)
(* returns the number of the line just ended.   *)
(* <boxed values 356>=                          *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* <support for lexical analysis>=              *)
type 'a lexer = (char, 'a) xformer
(* Lexical analyzers: transformers of characters *)
(*                                              *)
(* The interpreters in this book consume one line at a *)
(* time. But characters within a line may be split into *)
(* multiple tokens. For example, the line       *)
(*                                              *)
(*   (define list1 (x) (cons x '()))            *)
(*                                              *)
(* should be split into the tokens {indented}   *)
(* (                                            *)
(* define                                       *)
(* list1                                        *)
(* (                                            *)
(* x                                            *)
(* )                                            *)
(* (                                            *)
(* cons                                         *)
(* x                                            *)
(* '                                            *)
(* (                                            *)
(* )                                            *)
(* )                                            *)
(* ) {indented} This section defines reusable,  *)
(* specialized transformers that transform streams of *)
(* characters into something else, usually tokens. *)
(* <boxed values 348>=                          *)
type 'a lexer = 'a lexer
(* The type [['a lexer]] should be pronounced ``lexer *)
(* returning [['a]].''                          *)

(* <support for lexical analysis>=              *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* In popular languages, a character like a semicolon or *)
(* comma usually does not join with other tokens to form *)
(* a character. In this book, left and right brackets of *)
(* all shapes keep to themselves and don't group with *)
(* other characters. And in just about every    *)
(* non-esoteric language, blank space separates tokens. *)
(* A character whose presence marks the end of one token *)
(* (and possibly the beginning of the next) is called a *)
(* delimiter. In this book, the main delimiter  *)
(* characters are whitespace and brackets. The other *)
(* delimiter is the semicolon, which introduces a *)
(* comment. [*]                                 *)
(* <boxed values 349>=                          *)
val _ = op isDelim : char -> bool
(* [[                                           *)
(* Char.isSpace]] recognizes all whitespace     *)
(* characters. [[Char.contains]] takes a string and a *)
(* character and says if the string contains the *)
(* character. These functions are in the initial basis *)
(* of Standard ML.                              *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \catcode`=\other \catcode`_=\other \catcode`$=\other *)
(*                                              *)
(*  Lexical analyzers; tokens                   *)
(*  \type'a lexer = (char, 'a) xformer \        *)
(*  tableboxisDelim : char -> bool \            *)
(*  tableboxwhitespace : char list lexer \      *)
(*  tableboxintChars : (char -> bool) ->        *)
(*  char list lexer \tableboxintFromChars :     *)
(*  char list -> int error \tablebox            *)
(*  intToken : (char -> bool) -> int lexer      *)
(*  \typetoken \tableboxtokenString : token     *)
(*  -> string \tableboxlexLineWith : token      *)
(*  lexer -> line -> token stream [8pt]         *)
(*  Streams with end-of-line markers            *)
(*  \type'a eol_marked \tableboxdrainLine :     *)
(*  'a eol_marked stream -> 'a eol_marked       *)
(*  stream [8pt] Parsers                        *)
(*  \type'a parser = (token located             *)
(*  eol_marked, 'a) xformer \tableboxeol :      *)
(*  ('a eol_marked, int) xformer \tablebox      *)
(*  inline : ('a eol_marked, 'a) xformer \      *)
(*  tableboxtoken : token parser \tablebox      *)
(*  srcloc : srcloc parser \tablebox            *)
(*  noTokens : unit parser \tablebox@@ : 'a     *)
(*  parser -> 'a located parser \tablebox       *)
(*  <?> : 'a parser * string -> 'a parser \     *)
(*  tablebox<!> : 'a parser * string -> 'b      *)
(*  parser \tableboxliteral : string ->         *)
(*  unit parser \tablebox>– : string * 'a     *)
(*  parser -> 'a parser \tablebox–< : 'a      *)
(*  parser * string -> 'a parser \tablebox      *)
(*  bracket : string * string * 'a parser       *)
(*  -> 'a parser \splitboxnodupsstring *        *)
(*  string -> srcloc * name list-> name         *)
(*  list error \tableboxsafeTokens : token      *)
(*  located eol_marked stream -> token list     *)
(*  \tableboxechoTagStream : line stream ->     *)
(*  line stream stripAndReportErrors            *)
(*                                          'a error *)
(*                                          stream -> *)
(*                                          'a stream *)
(*  [8pt] A complete, interactive source of     *)
(*  abstract syntax                             *)
(*  interactiveParsedStream : token lexer * 'a parser *)
(*                                          -> string *)
(*                                          * line *)
(*                                          stream * *)
(*                                          prompts *)
(*                                          -> 'a *)
(*                                          stream *)
(*                                              *)
(* Transformers specialized for lexical analysis or *)
(* parsing [*]                                  *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* All languages in this book ignore whitespace. Lexer *)
(* [[whitespace]] is typically combined with another *)
(* lexer using the [[*>]] operator.             *)

(* <support for lexical analysis>=              *)
val whitespace = many (sat Char.isSpace one)
(* <boxed values 350>=                          *)
val _ = op whitespace : char list lexer
(* <support for lexical analysis>=              *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*>
  many1 (sat Char.isDigit one) <* 
  notFollowedBy (sat (not o isDelim) one)
(* Most languages in this book are, like Scheme, liberal *)
(* about names. Just about any sequence of characters, *)
(* as long as it is free of delimiters, can form a name. *)
(* But there's one big exception: a sequence of digits *)
(* forms an integer literal, not a name. Because integer *)
(* literals introduce several complications, and because *)
(* they are used in all the languages in this book, *)
(* it makes sense to deal with the complications in one *)
(* place: here.                                 *)
(*                                              *)
(* Integer literals are subject to these rules: *)
(*                                              *)
(*   • An integer literal may begin with a minus sign. *)
(*   • It continues with one or more digits.  *)
(*   • If it is followed by character, that character *)
(*  must be a delimiter. (In other words, it must not *)
(*  be followed by a non-delimiter.)            *)
(*   • When the sequence of digits is converted to an *)
(*  [[int]], the arithmetic used in the conversion *)
(*  must not overflow.                          *)
(*                                              *)
(* Function [[intChars]] does the lexical analysis to *)
(* grab the characters; [[intFromChars]] handles the *)
(* conversion and its potential overflow, and   *)
(* [[intToken]] puts everything together. Because not *)
(* every language uses the same delimiters, both *)
(* [[intChars]] and [[intToken]] receive a predicate *)
(* that identifies delimiters.                  *)
(* <boxed values 351>=                          *)
val _ = op intChars : (char -> bool) -> char list lexer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Function [[Char.isDigit]], like [[Char.isSpace]], is *)
(* part of Standard ML.                         *)

(* <support for lexical analysis>=              *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow =>
        ERROR "this interpreter can't read arbitrarily large integers"
(* Function [[intFromChars]] composes three functions *)
(* from Standard ML's initial basis. Function   *)
(* [[implode]] converts a list of characters to a *)
(* string; [[Int.fromString]] converts a string to an \ *)
(* monoboxint option (raising [[Overflow]] if the *)
(* literal is too big); and [[valOf]] converts an \ *)
(* monoboxint option to an [[int]]. The [[Int. ]] *)
(* function, which is used when I see a minus sign, *)
(* negates an integer. The [[ ]] is meant to resemble a *)
(* ``high minus'' sign, a notational convention that *)
(* goes back at least to \apl.                  *)
(* <boxed values 352>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 353>=                          *)
val _ = op intToken : (char -> bool) -> int lexer
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* All the bridge languages use balanced brackets, which *)
(* may come in three shapes. So that lexers for *)
(* different languages can share code related to *)
(* brackets, bracket shapes and tokens are defined here. *)
(* <support for lexical analysis>=              *)
datatype bracket_shape = ROUND | SQUARE | CURLY
(* <support for lexical analysis>=              *)
datatype 'a plus_brackets
  = LEFT  of bracket_shape
  | RIGHT of bracket_shape
  | PRETOKEN of 'a

fun bracketLexer pretoken
  =  LEFT  ROUND  <$ eqx #"(" one
 <|> LEFT  SQUARE <$ eqx #"[" one
 <|> LEFT  CURLY  <$ eqx #"{" one
 <|> RIGHT ROUND  <$ eqx #")" one
 <|> RIGHT SQUARE <$ eqx #"]" one
 <|> RIGHT CURLY  <$ eqx #"}" one
 <|> PRETOKEN <$> pretoken
(* Bracket tokens are added to a language-specific *)
(* ``pre-token'' type by using the type constructor *)
(* [[plus_brackets]].[*] Function [[bracketLexer]] takes *)
(* as an argument a lexer for pre-tokens, and it returns *)
(* a lexer for tokens:                          *)
(* <boxed values 354>=                          *)
type 'a plus_brackets = 'a plus_brackets
val _ = op bracketLexer : 'a lexer -> 'a plus_brackets lexer
(* [[bindList]] tried to                        *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)

(* <support for lexical analysis>=              *)
fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"

fun plusBracketsString _   (LEFT shape)  = leftString shape
  | plusBracketsString _   (RIGHT shape) = rightString shape
  | plusBracketsString pts (PRETOKEN pt)  = pts pt
(* For debugging and error messages, brackets and tokens *)
(* can be converted to strings.                 *)
(* <boxed values 355>=                          *)
val _ = op plusBracketsString : ('a -> string) -> ('a plus_brackets -> string)
(* <common parsing code>=                       *)
(* Parsing located, in-line tokens              *)
(*                                              *)
(* In each interpreter, a value of type \monobox'a *)
(* parser is a transformer that takes a stream of *)
(* located tokens set between end-of-line markers, and *)
(* it returns a value of type [['a]], plus any leftover *)
(* tokens. But each interpreter has its own token type, *)
(* and the infrastructure needs to work with all of *)
(* them. That is, it needs to be polymorphic. So a value *)
(* of type \monobox('t, 'a) polyparser is a parser that *)
(* takes tokens of some unknown type [['t]].    *)
(* <combinators and utilities for parsing located streams>= *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* <combinators and utilities for parsing located streams>= *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* When defining a parser, I want not to worry about the *)
(* [[EOL]] and [[INLINE]] constructors. These   *)
(* constructors are essential for error recovery, but *)
(* for parsing, they just get in the way. My first order *)
(* of business is therefore to define analogs of [[one]] *)
(* and [[eos]] that ignore [[EOL]]. Parser [[token]] *)
(* takes one token; parser [[srcloc]] looks at the *)
(* source-code location of a token, but leaves the token *)
(* in the input; and parser [[noTokens]] succeeds only *)
(* if there are no tokens left in the input. They are *)
(* built on top of ``utility'' parsers [[eol]] and *)
(* [[inline]]. The two utility parsers have different *)
(* contracts; [[eol]] succeeds only when at [[EOL]], but *)
(* [[inline]] scans past [[EOL]] to look for [[INLINE]]. *)
(* <boxed values 357>=                          *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* <combinators and utilities for parsing located streams>= *)
fun @@ p = pair <$> srcloc <*> p
(* Parser [[noTokens]] is not that same as [[eos]]: *)
(* parser [[eos]] succeeds only when the input stream is *)
(* empty, but [[noTokens]] can succeed when the input *)
(* stream is not empty but contains only [[EOL]] *)
(* markers—as is likely on the last line of an input *)
(* file.                                        *)
(*                                              *)
(* Source-code locations are useful by themselves, but *)
(* they are also useful when paired with a result from a *)
(* parser. For example, when parsing a message send for *)
(* uSmalltalk, the source-code location of the send is *)
(* used when writing a stack trace. To make it easy to *)
(* add a source-code location to any result from any *)
(* parser, I define the [[@@]] function. (Associate the *)
(* word ``at'' with the idea of ``location.'') The code *)
(* uses a dirty trick: it works because [[srcloc]] looks *)
(* at the input but does not consume any tokens. *)
(* <boxed values 358>=                          *)
val _ = op @@ : ('t, 'a) polyparser -> ('t, 'a located) polyparser
(* <combinators and utilities for parsing located streams>= *)
fun asAscii p =
  let fun good c = Char.isPrint c andalso Char.isAscii c
      fun warn (loc, s) =
        case List.find (not o good) (explode s)
          of NONE => OK s
           | SOME c => 
               let val msg =
                     String.concat ["name \"", s, "\" contains the ",
                                    "non-ASCII or non-printing byte \"",
                                    Char.toCString c, "\""]
               in  synerrorAt msg loc
               end
  in  warn <$>! @@ p
  end
(* <combinators and utilities for parsing located streams>= *)
infix 0 <?>
fun p <?> what = p <|> synerrorAt ("expected " ^ what) <$>! srcloc
(* I usually want names to contain only printing ASCII *)
(* characters. Imagine a student who thinks they've *)
(* written a minus sign but has actually written some *)
(* accursed Unicode character that looks just like a *)
(* minus sign. Nobody is more confused or frustrated, *)
(* and justifiably so.                          *)
(* <boxed values 359>=                          *)
val _ = op asAscii : ('t, string) polyparser -> ('t, string) polyparser
(* Parsers that report errors                   *)
(*                                              *)
(* A typical syntactic form (expression, unit test, or *)
(* definition, for example) is parsed by a sequence of *)
(* alternatives separated with [[<|>]]. When no *)
(* alternative succeeds, the collective should usually *)
(* be reported as a syntax error. An error-reporting *)
(* parser can be created using the [[<?>]] function: *)
(* parser \monoboxp <?> what succeeds when [[p]] *)
(*  succeeds, but when [[p]] fails, parser \monoboxp <?> *)
(* what reports an error: it expected [[what]]. *)
(* The error says what the parser was expecting, and it *)
(* gives the source-code location of the unrecognized *)
(* token. If there is no token, there is no error—at end *)
(* of file, rather than signal an error, a parser made *)
(* using [[<?>]] fails. An example appears in the parser *)
(* for extended definitions in micro-Scheme (\chunkref *)
(* mlschemea.chunk.xdef). [*]                   *)
(* <boxed values 359>=                          *)
val _ = op <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* The [[<?>]] operator must not be used to define a *)
(* parser that is passed to [[many]], [[many1]], or *)
(* [[optional]] In that context, if parser [[p]] fails, *)
(* it must not signal an error; it must instead *)
(* propagate the failure to [[many]], [[many1]], or *)
(* [[optional]], so those combinators know there is not *)
(* a [[p]] there.                               *)

(* <combinators and utilities for parsing located streams>= *)
infix 4 <!>
fun p <!> msg =
  fn tokens => (case p tokens
                  of SOME (OK _, unread) =>
                       let val outcome =
                             case peek srcloc tokens
                              of SOME loc => synerrorAt msg loc
                               | NONE => ERROR msg
                                             
                       in  SOME (outcome, unread)
                       end
                   | _ => NONE)
(* Another common error-detecting technique is to use a *)
(* parser [[p]] to detect some input that shouldn't be *)
(* there. For example, a parser is just starting to read *)
(* a definition, the input shouldn't begin with a right *)
(* parenthesis. I can write a parser [[p]] that *)
(* recognizes a right parenthesis, but I can't simply *)
(* combine [[p]] with [[synerrorAt]] and [[srcloc]] in *)
(* the same way that [[<?>]] does, because I want my *)
(* combined parser to do two things: consume the tokens *)
(* recognized by [[p]], and also report the error at the *)
(* location of the first of those tokens. I can't use *)
(* [[synerrorAt]] until after [[p]] succeeds, but I have *)
(* to use [[srcloc]] on the input stream as it is before *)
(* [[p]] is run. I solve this problem by defining a *)
(* special combinator that keeps a copy of the tokens *)
(* inspected by [[p]]. If parser [[p]] succeeds, then *)
(* parser \monoboxp <!> msg consumes the tokens consumed *)
(* by [[p]] and reports error [[msg]] at the location of *)
(* [[p]]'s first token.                         *)
(* <boxed values 360>=                          *)
val _ = op <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser
(* <combinators and utilities for parsing located streams>= *)
fun nodups (what, context) (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) =
            if List.exists (fn y => y = x) xs then
              synerrorAt (what ^ " " ^ x ^ " appears twice in " ^ context) loc
            else
              dup xs
  in  dup names
  end
(* \qbreak                                      *)
(*                                              *)
(* Detection of duplicate names                 *)
(*                                              *)
(* Most of the languages in this book allow you to *)
(* define functions or methods that take formal *)
(* parameters. It is never permissible to use the same *)
(* name for formal parameters in two different  *)
(* positions. There are surprisingly many other places *)
(* where it's not acceptable to have duplicates in a *)
(* list of strings. Function [[nodups]] takes two *)
(* Curried arguments: a pair saying what kind of thing *)
(* might be duplicated and where it appeared, followed *)
(* by a pair containing a list of names and the *)
(* source-code location of the list. If there are no *)
(* duplicates, it returns [[OK]] applied to the list of *)
(* names; otherwise it returns an [[ERROR]]. Function *)
(* [[nodups]] is typically applied to a pair of strings, *)
(* after which the result is applied to a parser using *)
(* the [[<>!]] function.                        *)
(*                                              *)
(* \qbreak                                      *)
(* <boxed values 369>=                          *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* Function [[List.exists]] is like the micro-Scheme *)
(* [[exists?]]. It is in the initial basis for  *)
(* Standard ML.                                 *)

(* <combinators and utilities for parsing located streams>= *)
fun rejectReserved reserved x =
  if member x reserved then
    ERROR ("syntax error: " ^ x ^ " is a reserved word and " ^
           "may not be used to name a variable or function")
  else
    OK x
(* Detection of reserved words                  *)
(*                                              *)
(* To rule out such nonsense as ``\monobox(val if 3),'' *)
(* parsers use function [[rejectReserved]], which issues *)
(* a syntax-error message if a name is on a list of *)
(* reserved words.                              *)
(* <boxed values 370>=                          *)
val _ = op rejectReserved : name list -> name -> name error
(* <transformers for interchangeable brackets>= *)
fun left  tokens = ((fn (loc, LEFT  s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun right tokens = ((fn (loc, RIGHT s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* Parsers that involve brackets                *)
(*                                              *)
(* Almost every language in this book uses a    *)
(* parenthesis-prefix syntax (Scheme syntax) in which *)
(* round and square brackets must match, but are *)
(* otherwise interchangeable. [I~have spent entirely too *)
(* much time working with Englishmen who call   *)
(* parentheses ``brackets.'' I~now find it hard even to *)
(* \emph{say} the word ``parenthesis,'' let alone *)
(* type~it. ] Brackets are treated specially by the *)
(* [[plus_brackets]] type (\cpageref            *)
(* lazyparse.plus-brackets), which identifies every *)
(* token as a left bracket, a right bracket, or a *)
(* ``pre-token.'' Each of these alternatives is *)
(* supported by its own parser. A parser that finds a *)
(* bracket returns the bracket's shape and location; *)
(* a parser the finds a pre-token returns the pre-token. *)
(* <boxed values 362>=                          *)
val _ = op left  : ('t plus_brackets, bracket_shape located) polyparser
val _ = op right : ('t plus_brackets, bracket_shape located) polyparser
val _ = op pretoken : ('t plus_brackets, 't) polyparser
(* <transformers for interchangeable brackets>= *)
fun badRight msg =
  (fn (loc, shape) => synerrorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* Every interpreter needs to be able to complain when *)
(* it encounters an unexpected right bracket.   *)
(* An interpreter can build a suitable parser by passing *)
(* a message to [[badRight]]. Since the parser never *)
(* succeeds, it can have any result type.       *)
(* <boxed values 363>=                          *)
val _ = op badRight : string -> ('t plus_brackets, 'a) polyparser
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <transformers for interchangeable brackets>= *)
fun notCurly (_, CURLY) = false
  | notCurly _          = true
fun leftCurly tokens = sat (not o notCurly) left tokens
(* With the bracket parsers defined, I can use Noweb to *)
(* drop the definition of function [[errorAtEnd]] into *)
(* this spot.                                   *)
(* <transformers for interchangeable brackets>= *)
(* <definition of function [[errorAtEnd]]>=     *)
infix 4 errorAtEnd
fun p errorAtEnd mkMsg =
  fn tokens => 
    (case (p <* rewind right) tokens
       of SOME (OK s, unread) =>
            let val outcome =
                  case peek srcloc tokens
                    of SOME loc => synerrorAt ((concat o mkMsg) s) loc
                     | NONE => ERROR ((concat o mkMsg) s)
            in  SOME (outcome, unread)
            end
        | _ => NONE)
(* Function [[<!>]] is adequate for simple cases, but to *)
(* produce a really good error message, I might wish to *)
(* use the result from [[p]] to build a message. *)
(* My interpreters produce such messages only for text *)
(* appearing in brackets, so [[errorAtEnd]] triggers *)
(* only when [[p]] parses tokens that are followed by a *)
(* right bracket. \nwcrazynarrowboxes           *)
(* <boxed values 361>=                          *)
val _ = op errorAtEnd : ('t plus_brackets, 'a) polyparser * ('a -> string list)
                                            -> ('t plus_brackets, 'b) polyparser
(* <transformers for interchangeable brackets>= *)
datatype right_result
  = FOUND_RIGHT      of bracket_shape located
  | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
  | NO_RIGHT
(* In this book, round and square brackets are  *)
(* interchangeable, but curly brackets are special. *)
(* Predicate [[notCurly]] identifies those non-curly, *)
(* interchangeable bracket shapes. Parser [[leftCurly]] *)
(* is just like [[left]], except it recognizes only *)
(* curly left brackets.                         *)
(* <boxed values 364>=                          *)
val _ = op notCurly  : bracket_shape located -> bool
val _ = op leftCurly : ('t plus_brackets, bracket_shape located) polyparser
(* Brackets by themselves are all very well, but what I *)
(* really want is to parse syntax that is wrapped in *)
(* matching brackets. But what if something goes wrong *)
(* inside the brackets? In that case, I want each of my *)
(* parsers to skip tokens until it gets to the matching *)
(* right bracket, and I'll likely want it to report the *)
(* source-code location of the left bracket. To look *)
(* ahead for a right bracket is the job of parser *)
(* [[matchingRight]]. This parser is called when a left *)
(* bracket has already been consumed, and it searches *)
(* the input stream for a right bracket, skipping every *)
(* left/right pair that it finds in the interim. Because *)
(* it's meant for error handling, it always succeeds. *)
(* And to communicate its findings, it produces one of *)
(* three outcomes:                              *)
(*                                              *)
(*   • Result \monobox[[FOUND_RIGHT]] (loc, s) says, *)
(*  ``I found a right bracket exactly where     *)
(*  I expected to, and its shape and location are s *)
(*  and loc.''                                  *)
(*   • Result \monobox[[SCANNED_TO_RIGHT]] loc says, *)
(*  ``I didn't find a right bracket at loc, but *)
(*  I scanned to a matching right bracket       *)
(*  eventually.''                               *)
(*   • Result \monobox[[NO_RIGHT]] says, ``I scanned the *)
(*  entire input without finding a matching right *)
(*  bracket.''                                  *)
(*                                              *)
(* This result is defined as follows:           *)
(* <boxed values 364>=                          *)
type right_result = right_result
(* <transformers for interchangeable brackets>= *)
type ('t, 'a) pb_parser = ('t plus_brackets, 'a) polyparser
fun matchingRight tokens =
  let fun scanToClose tokens = 
        let val loc = getOpt (peek srcloc tokens, ("end of stream", 9999))
            fun scan nlp tokens =
              (* nlp is the number of unmatched left parentheses *)
              case tokens
                of EOL _                  ::: tokens => scan nlp tokens
                 | INLINE (_, PRETOKEN _) ::: tokens => scan nlp tokens
                 | INLINE (_, LEFT  _)    ::: tokens => scan (nlp+1) tokens
                 | INLINE (_, RIGHT _)    ::: tokens =>
                     if nlp = 0 then
                       pure (SCANNED_TO_RIGHT loc) tokens
                     else
                       scan (nlp-1) tokens
                 | EOS         => pure NO_RIGHT tokens
                 | SUSPENDED s => scan nlp (demand s)
        in  scan 0 tokens
        end
  in  (FOUND_RIGHT <$> right <|> scanToClose) tokens
  end
(* A value of type [[right_result]] is produced by *)
(* parser [[matchingRight]]. A right bracket in the *)
(* expected position is successfully found by the \ *)
(* qbreak [[right]] parser; when tokens have to be *)
(* skipped, they are skipped by parser [[scanToClose]]. *)
(* The ``matching'' is done purely by counting left and *)
(* right brackets; [[scanToClose]] does not look at *)
(* shapes.                                      *)
(* <boxed values 365>=                          *)
val _ = op matchingRight : ('t, right_result) pb_parser
(* <transformers for interchangeable brackets>= *)
fun matchBrackets _ (loc, left) a (FOUND_RIGHT (loc', right)) =
      if left = right then
        OK a
      else
        synerrorAt (rightString right ^ " does not match " ^ leftString left ^
                 (if loc <> loc' then " at " ^ srclocString loc else "")) loc'
  | matchBrackets _ (loc, left) _ NO_RIGHT =
      synerrorAt ("unmatched " ^ leftString left) loc
  | matchBrackets e (loc, left) _ (SCANNED_TO_RIGHT loc') =
      synerrorAt ("expected " ^ e) loc
(* The right-bracket result is used in function *)
(* [[matchBrackets]], which looks for a right bracket *)
(* after a thing is matched. Function [[matchBrackets]] *)
(* does look at shapes: if the right bracket is *)
(* immediately present ([[FOUND_RIGHT]]) and is of the *)
(* proper shape, then the match succeeds. Otherwise, *)
(* it produces a suitable error. \nwverynarrowboxes *)
(* <boxed values 366>=                          *)
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
(* <transformers for interchangeable brackets>= *)

fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
(* \qbreak The bracket matcher is then used to help wrap *)
(* other parsers in brackets. A parser may be wrapped in *)
(* a variety of ways, depending on what may be allowed *)
(* to fail without causing an error.            *)
(*                                              *)
(*   • To wrap parser [[p]] in matching round or square *)
(*  brackets when [[p]] may fail: use           *)
(*  [[liberalBracket]]. If [[p]] succeeds but the *)
(*  brackets don't match, that's an error.      *)
(*   • To wrap parser [[p]] in matching round or square *)
(*  brackets when [[p]] must succeed: use       *)
(*  [[bracket]].                                *)
(*   • To wrap parser [[p]] in matching curly brackets *)
(*  when [[p]] must succeed: use [[curlyBracket]]. *)
(*   • To put parser [[p]] after a keyword, all wrapped *)
(*  in brackets: use [[bracketKeyword]]. Once the *)
(*  keyword is seen, [[p]] must not fail—if it does, *)
(*  that's an error.                            *)
(*                                              *)
(* Each of these functions takes a parameter    *)
(* [[expected]] of type [[string]]; when anything goes *)
(* wrong, this parameter says what the parser was *)
(* expecting. [*] \nwnarrowboxes                *)
(* <boxed values 367>=                          *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* <boxed values 367>=                          *)
val _ = op bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* <transformers for interchangeable brackets>= *)
fun usageParser keyword =
  let val left = eqx #"(" one <|> eqx #"[" one
      val getkeyword = left *> (implode <$> many1 (sat (not o isDelim) one))
  in  fn (usage, p) =>
        case getkeyword (streamOfList (explode usage))
          of SOME (OK k, _) => bracketKeyword (keyword k, usage, p)
           | _ => raise InternalError ("malformed usage string: " ^ usage)
  end
(* The [[bracketKeyword]] function is what's used to *)
(* build parsers for [[if]], [[lambda]], and many other *)
(* syntactic forms. And if one of these parsers fails, *)
(* I want it to show the programmer what's expected, *)
(* like for example \monobox"(if e1 e2 e3)". The *)
(* expectation is represented by a usage string. A usage *)
(* string begins with a left bracket, which is followed *)
(* by its keyword. I want not to write the keyword *)
(* twice, so [[usageParser]] pulls the keyword out of *)
(* the usage string—using a parser. [*]       *)
(* <boxed values 368>=                          *)
val _ = op usageParser : (string -> ('t, string) pb_parser) ->
                               string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* [[bindList]] tried to                        *)
(*              extend an environment, but it passed *)
(*              two lists (names and values) of *)
(*              different lengths.              *)
(* RuntimeError    Something else went wrong during *)
(*              evaluation, i.e., during the    *)
(*              execution of [[eval]].          *)
(* \bottomrule                                  *)
(*                                              *)
(* Exceptions defined especially for this interpreter  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Abstract syntax and values                   *)
(*                                              *)
(* An abstract-syntax tree can contain a literal value. *)
(* A value, if it is a closure, can contain an  *)
(* abstract-syntax tree. These two types are therefore *)
(* mutually recursive, so I define them together, using *)
(* [[and]].                                     *)
(*                                              *)
(* These particular types use as complicated a nest of *)
(* definitions as you'll ever see. The keyword  *)
(* [[datatype]] defines a new algebraic datatype; the *)
(* keyword [[withtype]] introduces a new type   *)
(* abbreviation that is mutually recursive with the *)
(* [[datatype]]. The first group of [[and]] keywords *)
(* define additional algebraic datatypes, and the second *)
(* group of [[and]] keywords define additional type *)
(* abbreviations. Everything in the whole nest is *)
(* mutually recursive. [*] [*]                  *)

(* <code used to debug parsers>=                *)
fun safeTokens stream =
  let fun tokens (seenEol, seenSuspended) =
            let fun get (EOL _         ::: ts) = if seenSuspended then []
                                                 else tokens (true, false) ts
                  | get (INLINE (_, t) ::: ts) = t :: get ts
                  | get  EOS                   = []
                  | get (SUSPENDED (ref (PRODUCED ts))) = get ts
                  | get (SUSPENDED s) = if seenEol then []
                                        else tokens (false, true) (demand s)
            in   get
            end
  in  tokens (false, false) stream
  end
(* Code used to debug parsers                   *)
(*                                              *)
(* When debugging parsers, I often find it helpful to *)
(* dump out the tokens that a parser is looking at. *)
(* I want to dump only the tokens that are available *)
(* without triggering the action of reading another line *)
(* of input. To get those tokens, function      *)
(* [[safeTokens]] reads until it has got to both an *)
(* end-of-line marker and a suspension whose value has *)
(* not yet been demanded.                       *)
(* <boxed values 371>=                          *)
val _ = op safeTokens : 'a located eol_marked stream -> 'a list
(* <code used to debug parsers>=                *)
fun showErrorInput asString p tokens =
  case p tokens
    of result as SOME (ERROR msg, rest) =>
         if String.isSubstring " [input: " msg then
           result
         else
           SOME (ERROR (msg ^ " [input: " ^
                        spaceSep (map asString (safeTokens tokens)) ^ "]"),
               rest)
     | result => result
(* \qbreak Another way to debug is to show whatever *)
(* input tokens might cause an error. They can be shown *)
(* using function [[showErrorInput]], which transforms *)
(* an ordinary parser into a parser that, when it *)
(* errors, shows the input that caused the error. *)
(* It should be applied routinely to every parser you *)
(* build. \nwverynarrowboxes                    *)
(* <boxed values 372>=                          *)
val _ = op showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
(* <code used to debug parsers>=                *)
fun wrapAround tokenString what p tokens =
  let fun t tok = " " ^ tokenString tok
      val _ = app eprint ["Looking for ", what, " at"]
      val _ = app (eprint o t) (safeTokens tokens)
      val _ = eprint "\n"
      val answer = p tokens
      val _ = app eprint [ case answer of NONE => "Didn't find "
                                        | SOME _ => "Found "
                         , what, "\n"
                         ]
  in  answer
  end handle e =>
        ( app eprint ["Search for ", what, " raised ", exnName e, "\n"]
        ; raise e
        )
(* What if a parser doesn't cause an error, but it fails *)
(* when you were expecting it to succeed? Try applying *)
(* [[wrapAround]] to it; using [[wrapAround]] with a *)
(* parser [[p]] shows what [[p]] was looking for, what *)
(* tokens it was looking at, and whether it found *)
(* something. \nwverynarrowboxes                *)
(* <boxed values 373>=                          *)
val _ = op wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t,
                                                                  'a) polyparser
(* <streams that issue two forms of prompts>=   *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* Support for testing                          *)
(*                                              *)
(* I begin with testing support. As in the C code, *)
(* I want each interpreter to print out any line read *)
(* that begins with the special string [[;#]]. This *)
(* string is a formal comment that helps test chunks *)
(* marked \LAtranscript\RA. The strings are printed in a *)
(* modular way: a post-stream action prints any line *)
(* meeting the criterion. Function [[echoTagStream]] *)
(* transforms a stream of lines to a stream of lines, *)
(* adding the behavior I want.                  *)
(* <boxed values 374>=                          *)
val _ = op echoTagStream : line stream -> line stream 
(* <streams that issue two forms of prompts>=   *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* \qbreak                                      *)
(*                                              *)
(* Issuing messages for error values            *)
(*                                              *)
(* Next is error handling. A process that can detect *)
(* errors produces a stream of type \monobox'a error *)
(* stream, for some unspecified type [['a]]. The  *)
(* [[ERROR]] and [[OK]] tags can be removed by reporting *)
(* errors and passing on values tagged [[OK]], resulting *)
(* in a new stream of type \monobox'a stream. Values *)
(* tagged with [[OK]] are passed on to the output stream *)
(* unchanged; messages tagged with [[ERROR]] are printed *)
(* to standard error, using [[eprintln]].       *)
(* <boxed values 375>=                          *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* <streams that issue two forms of prompts>=   *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* Using [[stripAndReportErrors]], I can turn a lexical *)
(* analyzer into a function that takes an input line and *)
(* returns a stream of tokens. Any errors detected *)
(* during lexical analysis are printed without any *)
(* information about source-code locations. That's *)
(* because, to keep things somewhat simple, I've chosen *)
(* to do lexical analysis on one line at a time, and my *)
(* code doesn't keep track of the line's source-code *)
(* location.                                    *)
(* <boxed values 376>=                          *)
val _ = op lexLineWith : 't lexer -> line -> 't stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <streams that issue two forms of prompts>=   *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) =
            SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* When an error occurs during parsing, I want the *)
(* parser to drain the rest of the tokens on the line *)
(* where the error occurred. And errors aren't stripped *)
(* yet; errors are passed on to the interactive stream *)
(* because when an error is detected, the prompt may *)
(* need to be changed.                          *)
(* <boxed values 377>=                          *)
val _ = op parseWithErrors : ('t, 'a) polyparser ->                     't
                                    located eol_marked stream -> 'a error stream
(* <streams that issue two forms of prompts>=   *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* Prompts                                      *)
(*                                              *)
(* Each interpreter in this book issues prompts using *)
(* the model established by the Unix shell. This model *)
(* uses two prompt strings. The first prompt string, *)
(* called [[ps1]], is issued when starting to read a *)
(* definition. The second prompt string, called [[ps2]], *)
(* is issued when in the middle of reading a definition. *)
(* Prompting can be disabled by making both [[ps1]] and *)
(* [[ps2]] empty.                               *)
(* <boxed values 378>=                          *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* <streams that issue two forms of prompts>=   *)
fun ('t, 'a) interactiveParsedStream (lexer, parser) (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref ps1
      fun setPrompt ps = fn _ => thePrompt := ps

      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      fun lexAndDecorate (loc, line) =
        let val tokens = postStream (lexLineWith lexer line, setPrompt ps2)
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val xdefs_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        (name, lines)
(* Building a reader                            *)
(*                                              *)
(* All that is left is to combine lexer and parser. The *)
(* combination manages the flow of information from the *)
(* input through the lexer and parser, and by monitoring *)
(* the flow of tokens in and syntax out, it arranges *)
(* that the right prompts ([[ps1]] and [[ps2]]) are *)
(* printed at the right times. The flow of information *)
(* involves multiple steps:                     *)
(*                                              *)
(*  1. [*] The input is a stream of lines. The stream is *)
(*  transformed with [[preStream]] and          *)
(*  [[echoTagStream]], so that a prompt is printed *)
(*  before every line, and when a line contains the *)
(*  special tag, that line is echoed to the output. *)
(*  2. Each line is converted to a stream of tokens by *)
(*  function \monoboxlexLineWith lexer. Each token is *)
(*  then paired with a source-code location and, *)
(*  tagged with [[INLINE]], and the stream of tokens *)
(*  is followed by an [[EOL]] value. This extra *)
(*  decoration transforms the \monoboxtoken stream *)
(*  provided by the lexer to the \monoboxtoken  *)
(*  located [[eol_marked]] stream needed by the *)
(*  parser. The work is done by function        *)
(*  [[lexAndDecorate]], which needs a located line. *)
(*                                              *)
(*  The moment a token is successfully taken from the *)
(*  stream, a [[postStream]] action sets the prompt *)
(*  to [[ps2]].                                 *)
(*  3. A final stream of definitions is computed by *)
(*  composing [[locatedStream]] to add source-code *)
(*  locations, \monoboxstreamConcatMap lexAndDecorate *)
(*  to add decorations, and \monoboxparseWithErrors *)
(*  parser to parse. The entire composition is  *)
(*  applied to the stream of lines created in step  *)
(*  [<-].                                       *)
(*                                              *)
(* The composition is orchestrated by function  *)
(* [[interactiveParsedStream]].                 *)
(*                                              *)
(* To deliver the right prompt in the right situation, *)
(* [[interactiveParsedStream]] stores the current prompt *)
(* in a mutable cell called [[thePrompt]]. The prompt is *)
(* initially [[ps1]], and it stays [[ps1]] until a token *)
(* is delivered, at which point the [[postStream]] *)
(* action sets it to [[ps2]]. But every time a new *)
(* definition is demanded, a [[preStream]] action on the *)
(* syntax stream [[xdefs_with_errors]] resets the prompt *)
(* to [[ps1]]. \qbreak This combination of pre- and *)
(* post-stream actions, on different streams, ensures *)
(* that the prompt is always appropriate to the state of *)
(* the parser. [*] \nwnarrowboxes               *)
(* <boxed values 379>=                          *)
val _ = op interactiveParsedStream : 't lexer * ('t, 'a) polyparser ->
                                     string * line stream * prompts -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> 't located eol_marked stream
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
(* The functions defined in this appendix are useful for *)
(* reading all kinds of input, not just computer *)
(* programs, and I encourage you to use them in your own *)
(* projects. But here are two words of caution: with so *)
(* many abstractions in the mix, the parsers are tricky *)
(* to debug. And while some parsers built from  *)
(* combinators are very efficient, mine aren't. *)

(* <common parsing code ((elided))>=            *)
fun ('t, 'a) finiteStreamOfLine fail (lexer, parser) line =
  let val lines = streamOfList [line] @@@ streamOfEffects fail
      fun lexAndDecorate (loc, line) =
        let val tokens = lexLineWith lexer line
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val things_with_errors : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        ("command line", lines)
  in  
      stripAndReportErrors things_with_errors
  end 
val _ = finiteStreamOfLine :
          (unit -> string option) -> 't lexer * ('t, 'a) polyparser -> line ->
                                                                       'a stream
(* \qbreak                                      *)
(*                                              *)
(* Interpreter setup and command-line \         *)
(* chaptocsplitprocessing                       *)
(*                                              *)
(* In each interpreter, something has to act like the *)
(* C function [[main]]. This code has to initialize the *)
(* interpreter and start evaluating extended    *)
(* definitions.                                 *)
(*                                              *)
(* Part of initialization is setting the global error *)
(* format. The reusable function [[setup_error_format]] *)
(* uses interactivity to set the error format, which, as *)
(* in the C versions, determines whether syntax-error *)
(* messages include source-code locations (see functions *)
(* [[synerrorAt]] and [[synerrormsg]] on \      *)
(* cpagerefmlinterps.synerrormsg,mlinterps.synerrorAt). *)
(* <shared utility functions for initializing interpreters>= *)
fun override_if_testing () =                           (*OMIT*)
  if isSome (OS.Process.getEnv "NOERRORLOC") then      (*OMIT*)
    toplevel_error_format := WITHOUT_LOCATIONS         (*OMIT*)
  else                                                 (*OMIT*)
    ()                                                 (*OMIT*)
fun setup_error_format interactivity =
  if prompts interactivity then
    toplevel_error_format := WITHOUT_LOCATIONS
    before override_if_testing () (*OMIT*)
  else
    toplevel_error_format := WITH_LOCATIONS
    before override_if_testing () (*OMIT*)
(* \qbreak                                      *)
(*                                              *)
(* Utility functions for limiting computation   *)
(*                                              *)
(* Each interpreter is supplied with two ways of *)
(* stopping a runaway computation:              *)
(*                                              *)
(*   • A recursion limit halts the computation if its *)
(*  call stack gets deeper than 6,000 calls.    *)
(*   • A supply of evaluation fuel halts the computation *)
(*  after a million calls to [[eval]]. That's enough *)
(*  to compute the 25th Catalan number in uSmalltalk, *)
(*  for example.                                *)
(*                                              *)
(* If environment variable [[BPCOPTIONS]] includes the *)
(* string [[nothrottle]], evaluation fuel is ignored. *)
(* <function application with overflow checking>= *)
local
  val defaultRecursionLimit = 6000
  val recursionLimit = ref defaultRecursionLimit
  datatype checkpoint = RECURSION_LIMIT of int

  val evalFuel = ref 1000000
  val throttleCPU = not (hasOption "nothrottle")
in
  (* manipulate recursion limit *)
  fun checkpointLimit () = RECURSION_LIMIT (!recursionLimit)
  fun restoreLimit (RECURSION_LIMIT n) = recursionLimit := n

  (* work with fuel *)
  val defaultEvalFuel = ref (!evalFuel)
  fun fuelRemaining () = !evalFuel
  fun withFuel n f x = 
    let val old = !evalFuel
        val _ = evalFuel := n
    in  (f x before evalFuel := old) handle e => (evalFuel := old; raise e)
    end
(* \qbreak                                      *)
(* <function application with overflow checking>= *)
  (* convert function `f` to respect computation limits *)
  fun applyWithLimits f =
    if !recursionLimit <= 0 then
      ( recursionLimit := defaultRecursionLimit
      ; raise RuntimeError "recursion too deep"
      )
    else if throttleCPU andalso !evalFuel <= 0 then
      ( evalFuel := !defaultEvalFuel
      ; raise RuntimeError "CPU time exhausted"
      )
    else
      let val _ = recursionLimit := !recursionLimit - 1
          val _ = evalFuel       := !evalFuel - 1
      in  fn arg => f arg before (recursionLimit := !recursionLimit + 1)
      end
  fun resetComputationLimits () = ( recursionLimit := defaultRecursionLimit
                                  ; evalFuel := !defaultEvalFuel
                                  )
end


(*****************************************************************)
(*                                                               *)
(*   PRETTYPRINTING COMBINATORS                                  *)
(*                                                               *)
(*****************************************************************)

(* <prettyprinting combinators>=                *)
(* Where [[streamOfEffects]] produces the results of *)
(* repeating a single action again and again,   *)
(* [[streamRepeat]] repeats a single value again and *)
(* again. This operation might sound useless, but here's *)
(* an example: suppose we read a sequence of lines from *)
(* a file, and for error reporting, we want to tag each *)
(* line with its source location, i.e., file name and *)
(* line number. Well, the file names are all the same, *)
(* and one easy way to associate the same file name with *)
(* every line is to repeat the file name indefinitely, *)
(* then join the two streams using [[streamZip]]. *)
(* Function [[streamRepeat]] creates an infinite stream *)
(* that repeats a value. It works on values of any type. *)
(* <definition of [[doc]] and functions>=       *)
datatype doc 
  = ^^     of doc * doc
  | TEXT   of string
  | BREAK  of string
  | INDENT of int * doc
  | GROUP  of break_line or_auto * doc
(* The grouping mechanism is defined two layers. The *)
(* inner layer, [[break_line]], includes the three basic *)
(* ways of deciding whether [[BREAK]] should be turned *)
(* into newline-plus-indentation. The outer layer adds *)
(* [[AUTO]], which is converted to either [[YES]] or *)
(* [[NO]] inside the implementation:            *)

(* <definition of [[doc]] and functions>=       *)
and break_line
  = NO      (* hgrp -- every break is a space *)
  | YES     (* vgrp -- every break is a newline *)
  | MAYBE   (* fgrp -- paragraph fill (break is newline only when needed) *)
and 'a or_auto
  = AUTO    (* agrp -- NO if the whole group fits; otherwise YES *)
  | B of 'a
(* <definition of [[doc]] and functions>=       *)
val doc    = TEXT
val brk    = BREAK " "
val indent = INDENT
val empty  = TEXT ""
infix 2 ^^

fun hgrp d = GROUP (B NO,    d)
fun vgrp d = GROUP (B YES,   d)
fun agrp d = GROUP (  AUTO,  d)
fun fgrp d = GROUP (B MAYBE, d)
(* <definition of [[doc]] and functions>=       *)

fun format w k [] = []
  | format w k (tagged_doc :: z) = 
      let fun reformat item = format w k (item::z)
          fun copyChar 0 c = []
            | copyChar n c = c :: copyChar (n - 1) c
          fun addString s = s :: format w (k + size s) z
          fun breakAndIndent i =
                implode (#"\n" :: copyChar i #" ") :: format w i z
(* The [[layout]] function converts a document into a *)
(* string. But it's easier to define a function that *)
(* solves a more general problem: convert a list of *)
(* documents, each of which is tagged with a current *)
(* indentation and a break mode. [And for efficiency, *)
(* I~make the result a list of strings, which are *)
(* concatenated at the very end. This trick is important *)
(* because repeated concatenation has costs that are *)
(* quadratic in the size of the result; the cost of a *)
(* single concatenation at the end is linear.] Making *)
(* the input a tagged list makes most of the operations *)
(* easy:                                        *)
(*                                              *)
(*   • When a \monoboxd ^^ d' is removed from the head *)
(*  of the list, [[d]] and [[d']] are put back  *)
(*  separately.                                 *)
(*   • When a \monoboxTEXT s is removed from the head of *)
(*  the list, [[s]] is added to the result list. *)
(*   • When an \monoboxINDENT (i, d) is removed from the *)
(*  head of the list, it is replaced with [[d]], *)
(*  appropriately tagged with the additional    *)
(*  indentation.                                *)
(*   • When a [[BREAK]] is removed from the head of the *)
(*  list, a newline with indentation may or may not *)
(*  be added to the result, depending on the break *)
(*  mode and the space available.               *)
(*   • When a \monoboxGROUP(AUTO, d) is removed from the *)
(*  head of the list, [[d]] is tagged with either *)
(*  [[YES]] or [[NO]], depending on space available, *)
(*  and it is put back on the head of the list. *)
(*   • When any other kind of \monoboxGROUP(B mode, d) *)
(*  is removed from the head of the list, [[d]] is *)
(*  tagged with [[mode]] and is put back on the head *)
(*  of the list.                                *)
(*                                              *)
(* Function [[format]] takes a total line width, the *)
(* number of characters consumed on the current line, *)
(* and a list of tagged [[doc]]s. ``Putting an item back *)
(* on the head of the list'' is accomplished with *)
(* internal function [[reformat]].              *)
(* <boxed values 27>=                           *)
val _ = op format : int -> int -> (int * break_line * doc) list -> string list
(* CLOSING IN ON CHECK-PRINT:                   *)

      in  case tagged_doc
            of (i,b, d ^^ d')         => format w k ((i,b,d)::(i,b,d')::z)
             | (i,b,TEXT s)           => addString s
             | (i,b,INDENT(j,d))      => reformat (i+j,b,d)
             | (i,NO, BREAK s)        => addString s
             | (i,YES,BREAK _)        => breakAndIndent i
             | (i,MAYBE, BREAK s)     => if fits (w - k - size s, z)
                                           then addString s
                                           else breakAndIndent i
             | (i,b,GROUP(AUTO, d))   => if fits (w - k, (i,NO,d) :: z)
                                           then reformat (i,NO,d)
                                           else reformat (i,YES,d)
             | (i,b,GROUP(B break,d)) => reformat (i,break,d)
      end
(* Decisions about whether space is available are made *)
(* by the [[fits]] function. It looks ahead at a list of *)
(* documents and says whether everything up to the next *)
(* possible break will fit in [[w]] characters. *)

(* <definition of [[doc]] and functions>=       *)
and fits (w, []) = w >= 0
  | fits (w, tagged_doc::z) = 
      w >= 0 andalso
      case tagged_doc
       of (i, m,     x ^^ y)      => fits (w, (i,m,x)::(i,m,y)::z)
        | (i, m,     TEXT s)      => fits (w - size s, z)
        | (i, m,     INDENT(j,x)) => fits (w, (i+j,m,x)::z)
        | (i, NO,    BREAK s)     => fits (w - size s, z)
        | (i, YES,   BREAK _)     => true
        | (i, MAYBE, BREAK _)     => true 
        | (i, m,     GROUP(_,x))  => fits (w, (i,NO,x)::z)
(* <boxed values 28>=                           *)
val _ = op fits : int * (int * break_line * doc) list -> bool
(* If [[fits]] reaches a mandatory or optional [[BREAK]] *)
(* before running out of space, the input fits. The *)
(* interesting policy decision is for [[GROUP]]: \qbreak *)
(* for purposes of deciding whether to break a line, all *)
(* groups are considered without line breaks (mode  *)
(* [[NO]]). This policy will break a line in an outer *)
(* group in order to try to keep documents in an inner *)
(* group together on a single line.             *)
(*                                              *)
(* The [[layout]] function lays out a single document by *)
(* converting it to an instance of the more general *)
(* problem solved by [[format]]: wrap the document in an *)
(* [[AUTO]] group (so that lines are broken optionally); *)
(* tag it in [[NO]]-break mode with no indentation; put *)
(* it in a singleton list; and format it on a line of *)
(* width [[w]] with no characters consumed.     *)
(* <definition of [[doc]] and functions>=       *)
fun layout w doc = concat (format w 0 [(0, NO, GROUP (AUTO, doc))])
(* Where [[streamOfEffects]] produces the results of *)
(* repeating a single action again and again,   *)
(* [[streamRepeat]] repeats a single value again and *)
(* again. This operation might sound useless, but here's *)
(* an example: suppose we read a sequence of lines from *)
(* a file, and for error reporting, we want to tag each *)
(* line with its source location, i.e., file name and *)
(* line number. Well, the file names are all the same, *)
(* and one easy way to associate the same file name with *)
(* every line is to repeat the file name indefinitely, *)
(* then join the two streams using [[streamZip]]. *)
(* Function [[streamRepeat]] creates an infinite stream *)
(* that repeats a value. It works on values of any type. *)

infix 2 ^/
fun l ^/ r = l ^^ brk ^^ r
fun addBrk d = d ^^ brk
val semi = doc ";"
fun addSemi d = d ^^ semi
(* <boxed values 10>=                           *)
type doc = doc
val _ = op doc    : string -> doc
val _ = op ^^     : doc * doc -> doc
val _ = op empty  : doc
val _ = op indent : int * doc -> doc
val _ = op brk    : doc
(* <boxed values 10>=                           *)
val _ = op layout : int -> doc -> string
(* \qbreak And the last law for [[layout]] is a bit of a *)
(* lie; the truth about [[brk]] is that it is not always *)
(* converted to a newline (plus indentation):   *)
(*                                              *)
(*   • When [[brk]] is in a vertical group, it always *)
(*  converts to a newline followed by the number of *)
(*  spaces specified by its indentation.        *)
(*   • When [[brk]] is in a horizontal group, it never *)
(*  converts to a newline; instead it converts to a *)
(*  space.                                      *)
(*   • When [[brk]] is in an automatic group, it *)
(*  converts to a space only if the entire group will *)
(*  the width available; otherwise the [[brk]], and *)
(*  all [[brk]]s in the group, convert to       *)
(*  newline-indents.                            *)
(*   • When [[brk]] is in a fill group, it might convert *)
(*  to a space. Each [[brk]] is free to convert to *)
(*  newline-indent or to space independently of all *)
(*  the other [[brk]]s; the layout engine uses only *)
(*  as many newlines as are needed to fit the text *)
(*  into the space available.                   *)
(*                                              *)
(* \qbreak Groups are created by grouping functions, and *)
(* for convenience I add a line-breaking concatenate *)
(* ([[^/]]) and some support for adding breaks and *)
(* semicolons:                                  *)
(* <boxed values 10>=                           *)
val _ = op vgrp    : doc -> doc
val _ = op hgrp    : doc -> doc
val _ = op agrp    : doc -> doc
val _ = op fgrp    : doc -> doc
val _ = op ^/      : doc * doc -> doc
val _ = op addBrk  : doc -> doc
val _ = op semi    : doc
val _ = op addSemi : doc -> doc
(* Prettyprinting of types and module types     *)
(*                                              *)
(* ``String conversion'' includes not only conversion to *)
(* strings, but also conversion to [[doc]]s (for *)
(* prettyprinting). A couple of additional combinators *)
(* supplement the ones from \crefasdlml.chap.   *)
(* <prettyprinting combinators>=                *)
infix 2 ^/+
fun l ^/+ r = l ^^ indent (2, agrp (brk ^^ r))

fun separateDoc (zero, sep) =
  (* list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^^ sep ^^ s t
  in  s
  end
val brkSep  = separateDoc (doc "", brk)



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \MCL                         *)
(*                                                               *)
(*****************************************************************)

(* Abstract syntax and values                   *)
(*                                              *)
(* \qtrim2                                      *)
(*                                              *)
(* Abstract syntax and values are defined by chunks *)
(* organized as follows:                        *)
(* <abstract syntax and values for \mcl>=       *)
(* <paths for \mcl>=                            *)
type modcon = { printName : name, serial : int }
datatype modident = MODCON of modcon | MODTYPLACEHOLDER of name

(* A module identifier is genreated as follows  *)
(* <definition of function [[genmodident]]>=    *)
local
  val timesDefined : int env ref = ref emptyEnv
     (* how many times each modident is defined *)
in
  fun genmodident name =
    let val n = find (name, !timesDefined) handle NotFound _ => 0
        val n = 0  (* grotesque hack *)
        val _ = timesDefined := bind (name, n + 1, !timesDefined)
    in  MODCON { printName = name, serial = n }
    end
end
(* <boxed values 221>=                          *)
val _ = op genmodident : name -> modident
(* A module may be identified by its module constructor *)
(* ([[modcon]], similar to [[tycon]] in uML), or it may *)
(* just be a placeholder. (A path in a module-type *)
(* definition starts with [[MODTYPLACEHOLDER]].) A fresh *)
(* module constructor can be generated by function *)
(* [[genmodident]].                             *)

(* <paths for \mcl>=                            *)
datatype 'modname path' = PNAME of 'modname
                        | PDOT of 'modname path' * name
                        | PAPPLY of 'modname path' * 'modname path' list

type pathex = name located path'
type path   = modident path'
(* Types and type equality                      *)
(*                                              *)
(* There are no type constructors or type applications. *)
(* (Instead, there are module constructors and module *)
(* applications, which of course are found in type *)
(* [[path]].) Every type is either a component of a *)
(* module or a function type. As with modules, [[tyex]] *)
(* is the syntax and [[ty]] is the elaborated form. \ *)
(* mcllabelty                                   *)
(* <definition of [[ty]] for \mcl>=             *)
datatype 'modname ty' = TYNAME of 'modname path'
                      | FUNTY  of 'modname ty' list * 'modname ty'
                      | ANYTYPE   (* type of (error ...) *)
type tyex = name located ty'
type ty   = modident ty'
(* Declarations and module types                *)
(*                                              *)
(* A module type is either an export list, a module *)
(* arrow, or [[allof]] a collection of module types (an *)
(* intersection type). An export list may export four *)
(* different forms of component.                *)
(* <definition of [[modty]] for \mcl>=          *)
datatype modty
  = MTEXPORTS of (name * component) list
  | MTARROW   of (modident * modty) list * modty
  | MTALLOF   of modty list
and component
  = COMPVAL    of ty
  | COMPMANTY  of ty
  | COMPABSTY  of path
  | COMPMOD    of modty
(* Many, many operations in \crefmcl.chap operate on *)
(* ``rooted'' entities. A suitable type is defined here, *)
(* along with a function that extracts the root, and a *)
(* functorial map.                              *)
(* <definition of [[modty]] for \mcl>=          *)
type 'a rooted = 'a * path
fun root (_, path) = path
fun rootedMap f (a, path) = (f a, path)
(* \qbreak Names that can be bound in an environment *)
(* include values, types, modules, module types, and *)
(* overloaded names (a collection of values of different *)
(* types).                                      *)
(*                                              *)
(* <definition of [[modty]] for \mcl>=          *)
type print_string = string
datatype binding
  = ENVVAL    of ty
  | ENVMANTY  of ty
  | ENVMOD    of modty rooted
  | ENVOVLN   of ty list  (* overloaded name *)
  | ENVMODTY  of modty
(* An [[ENVMOD]] has a module identifier only if it is a *)
(* top-level module and has been elaborated.    *)
(*                                              *)
(* Bindings are closely related to declarations, but *)
(* they are not quite the same thing.           *)
(* <definition of [[modty]] for \mcl>=          *)
datatype decl
  = DECVAL    of tyex
  | DECABSTY
  | DECMANTY  of tyex
  | DECMOD    of modtyx
  | DECMODTY  of modtyx  (* only at top level *)
and modtyx
  = MTNAMEDX   of name
  | MTEXPORTSX of (name * decl) located list
  | MTALLOFX   of modtyx located list
  | MTARROWX   of (name located * modtyx located) list * modtyx located
type vcon = name path'
datatype pat = WILDCARD
             | PVAR     of name
             | CONPAT   of vcon * pat list
(* <definitions of [[exp]] and [[value]] for \mcl>= *)
type overloading = int ref
type formal = name * tyex
datatype exp 
  = LITERAL    of value
  | VAR        of pathex
  | VCONX      of vcon
  | CASE       of exp * (pat * exp) list   (* XXX pat needs to hold a path *)
  | IFX        of exp * exp * exp (* could be syntactic sugar for CASE *)
  | SET        of name * exp
  | WHILEX     of exp * exp
  | BEGIN      of exp list
  | APPLY      of exp * exp list * overloading
  | LETX       of let_flavor * (name * exp) list * exp
  | LETRECX    of ((name * tyex) * exp) list * exp
  | LAMBDA     of formal list * exp
  | MODEXP     of (name * exp) list    (* from body of a generic module *)
  | ERRORX     of exp list
  | EXP_AT     of srcloc * exp
and let_flavor = LET | LETSTAR
(* <definitions of [[exp]] and [[value]] for \mcl>= *)
and value
  = CONVAL of vcon * value ref list
  | SYM  of name
  | NUM  of int
  | MODVAL of value ref env
  | CLOSURE   of lambda * value ref env
  | PRIMITIVE of primop
  | ARRAY     of value array
 withtype lambda = name list * exp
      and primop = value list -> value
(* \qbreak Expressions.                         *)
(* <boxed values 223>=                          *)
type exp = exp
(* <boxed values 223>=                          *)
type value = value
(* Like uML, \mcl deals in constructed data, functions, *)
(* and a few primitive types. It also has module values. *)

val unitVal = CONVAL (PNAME "unit", [])
(* <definition of [[def]] for \mcl>=            *)
type modtyex = modtyx
datatype baredef
           = VAL    of name * exp
           | VALREC   of name * tyex * exp
           | EXP      of exp
                                                           (* not in a module *)
           | QNAME    of pathex
                                                           (* not in a module *)
           | DEFINE   of name * tyex * (formal list * exp)
           | TYPE     of name * tyex
           | DATA     of data_def
           | OVERLOAD of pathex list
           | MODULE   of name * moddef
           | GMODULE  of name * (name * modtyex) list * moddef
           | MODULETYPE of name * modtyex
                                                           (* not in a module *)
and moddef = MPATH       of pathex
           | MPATHSEALED of modtyex * pathex
           | MSEALED     of modtyex * def list
           | MUNSEALED   of def list
  withtype data_def = name * (name * tyex) list
       and def = baredef located
(* The definition forms of \mcl are the definition forms *)
(* of \nml, plus [[DATA]], [[OVERLOAD]], and three *)
(* module-definition forms. \mcllabeldef        *)
(* <boxed values 224>=                          *)
type def = def
type data_def = data_def
(* Generalization and instantiation             *)
(*                                              *)
(* Calling [[generalize]](tau, \tyvarset) generalizes *)
(* type tau to a type scheme by closing over type *)
(* variables not in \tyvarset. It also puts the type *)
(* scheme into canonical form.                  *)
(* <definition of [[unit_test]] for explicitly typed languages>= *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_TYPE_ERROR  of def
  | CHECK_MTYPE of pathex * modtyx
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
val BugInTypeInference = BugInTypeChecking (* to make \uml utils work *)
(* <string conversion of \mcl\ values>=         *)
fun vconString (PNAME c) = c
  | vconString (PDOT (m, c)) = vconString m ^ "." ^ c
  | vconString (PAPPLY _) = "can't happen! (vcon PAPPLY)"
(* \qbreak                                      *)
(*                                              *)
(* Conversion of values                         *)
(*                                              *)
(* The conversion of constructed values is very close to *)
(* what's in uML (\crefadta.chap), but I have not tried *)
(* to share code.                               *)
(* <boxed values 289>=                          *)
val _ = op vconString : vcon -> string
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <string conversion of \mcl\ values>=         *)
fun valueString (CONVAL (PNAME "cons", [ref v, ref vs])) = consString (v, vs)
  | valueString (CONVAL (PNAME "'()",  []))      = "()"
  | valueString (CONVAL (c, []))  = vconString c
  | valueString (CONVAL (c, vs))  =
      "(" ^ vconString c ^ " " ^ spaceSep (map (valueString o !) vs) ^ ")"
  | valueString (NUM n      )   =
      String.map (fn #"~" => #"-" | c => c) (Int.toString n)
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<function>"
  | valueString (PRIMITIVE _)   = "<function>"
  | valueString (MODVAL _)      = "<module>"
  | valueString (ARRAY a)       =
      "[" ^ spaceSep (map valueString (Array.foldr op :: [] a)) ^ "]"
(* <string conversion of \mcl\ values>=         *)
and consString (v, vs) =
      let fun tail (CONVAL (PNAME "cons", [ref v, ref vs])) =
                " " ^ valueString v ^ tail vs
            | tail (CONVAL (PNAME "'()", [])) =
                ")"
            | tail _ =
                raise BugInTypeChecking
                  "bad list constructor (or cons/'() redefined)"
      in  "(" ^ valueString v ^ tail vs
      end
(* <boxed values 290>=                          *)
val _ = op valueString : value -> string
(* Render a cons cell as a list.                *)
(* <boxed values 290>=                          *)
val _ = op consString : value * value -> string
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Rendering for \mcl patterns is also defined here. *)
(* <definition of [[patString]] for \uml\ and \uhaskell ((Molecule))>= *)
fun patString WILDCARD    = "_"
  | patString (PVAR x)    = x
  | patString (CONPAT (vcon, []))   = vconString vcon
  | patString (CONPAT (vcon, pats)) =
      "(" ^ spaceSep (vconString vcon :: map patString pats) ^ ")"
(* <string conversion of \mcl\ types and module types>= *)
fun modidentString (MODCON { printName = m, serial = 0 }) = m
  | modidentString (MODCON { printName = m, serial = k }) =
      m ^ "@{" ^ intString k ^ "}" 
  | modidentString (MODTYPLACEHOLDER s) = "<signature: " ^ s ^ ">"
(* Conversion of types and module types         *)
(*                                              *)
(* Modules with the same name but different serial *)
(* numbers must print differently.              *)
(* <boxed values 291>=                          *)
val _ = op modidentString : modident -> string
(* <string conversion of \mcl\ types and module types>= *)
fun pathString (PNAME a) = modidentString a
  | pathString (PDOT (PNAME (MODTYPLACEHOLDER _), x)) = x
  | pathString (PDOT (p, x)) = pathString p ^ "." ^ x
  | pathString (PAPPLY (f, args)) =
      spaceSep ("(@m" :: pathString f :: map pathString args) ^ ")"
(* <boxed values 292>=                          *)
val _ = op pathString : path -> string
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <string conversion of \mcl\ types and module types>= *)
val pathexString =
  let fun s (PNAME a) = snd a
        | s (PDOT (p, x)) = s p ^ "." ^ x
        | s (PAPPLY (f, args)) = spaceSep ("(@m" :: s f :: map s args) ^ ")"
  in  s
  end
(* <boxed values 293>=                          *)
val _ = op pathexString : pathex -> string
(* <string conversion of \mcl\ types and module types>= *)
fun typeString' ps (TYNAME p) = ps p
  | typeString' ps (FUNTY (args, res)) = 
      let val ts = typeString' ps
      in  "(" ^ spaceSep (map ts args @ "->" :: [ts res]) ^ ")"
      end
  | typeString' ps ANYTYPE = "<any type>"

val typeString = typeString' pathString
val tyexString = typeString' pathexString
(* Function [[typeString']] takes as argument function *)
(* [[ps]], which converts a path to a string.   *)
(* <boxed values 294>=                          *)
val _ = op tyexString : tyex -> string
val _ = op typeString : ty -> string
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <string conversion of \mcl\ types and module types>= *)
fun mtString (MTEXPORTS []) = "(exports)"
  | mtString (MTEXPORTS comps) = 
      "(exports " ^ spaceSep (map ncompString comps) ^ ")"
  | mtString (MTALLOF  mts) = "(allof " ^ spaceSep (map mtString mts) ^ ")"
  | mtString (MTARROW (args, res)) =
      "(" ^ spaceSep (map modformalString args) ^ " --m-> " ^ mtString res ^ ")"
and modformalString (m, t) = "[" ^ modidentString m ^ " : " ^ mtString t ^ "]"
and ncompString (x, c) = (* "named component" *)
  case c
    of COMPVAL tau => "[" ^ x ^ " : " ^ typeString tau ^ "]"
     | COMPABSTY _   => "[abstype " ^ x ^ "]"
     | COMPMANTY tau => "[type " ^ x ^ " " ^ typeString tau ^ "]"
     | COMPMOD mt => "(module [" ^ x ^ " : " ^ mtString mt ^ "])"
(* <boxed values 295>=                          *)
val _ = op mtString : modty -> string
val _ = op ncompString : name * component -> string
(* <string conversion of \mcl\ types and module types>= *)
fun mtxString (MTNAMEDX m) = m
  | mtxString (MTEXPORTSX []) = "(exports)"
  | mtxString (MTEXPORTSX lcomps) = 
      "(exports " ^ spaceSep (map ncompxString lcomps) ^ ")"
  | mtxString (MTALLOFX  mts) =
      "(allof " ^ spaceSep (map (mtxString o snd) mts) ^ ")"
  | mtxString (MTARROWX (args, res)) =
      "(" ^ spaceSep (map modformalString args) ^ " --m-> " ^
            mtxString (snd res) ^ ")"
(* \qbreak                                      *)
(* <string conversion of \mcl\ types and module types>= *)
and modformalString (m, t) = "[" ^ snd m ^ " : " ^ mtxString (snd t) ^ "]"
and ncompxString (loc, (x, c)) =
  case c
    of DECVAL tau => "[" ^ x ^ " : " ^ tyexString tau ^ "]"
     | DECABSTY   => "(abstype " ^ x ^ ")"
     | DECMANTY tau => "(type " ^ x ^ " " ^ tyexString tau ^ ")"
     | DECMOD mt => "(module [" ^ x ^ " : " ^ mtxString mt ^ "])"
     | DECMODTY mt => "(module-type " ^ x ^ " " ^ mtxString mt ^ ")"
(* <boxed values 300>=                          *)
val _ = op mtxString : modtyex -> string
val _ = op ncompxString : (name * decl) located -> string
(* <prettyprinting of \mcl\ types and module types>= *)
fun arrowdoc [] arrow res =
      doc "( " ^^ doc arrow ^^ doc " " ^^
      indent(3 + size arrow, res) ^^ doc ")"
  | arrowdoc (arg :: args) arrow res =
      let val docs =  (doc "(" ^^ arg)
                   :: args
                   @  doc arrow
                   :: [res ^^ doc ")"]
      in  indent(1, agrp (brkSep docs))
      end
(* For arrow types (functions or generic modules). *)
(* <boxed values 296>=                          *)
val _ = op arrowdoc : doc list -> string -> doc -> doc
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <prettyprinting of \mcl\ types and module types>= *)
fun typeDoc (TYNAME p) = doc (pathString p)
  | typeDoc (FUNTY (args, res)) =
      arrowdoc (map typeDoc args) "->" (typeDoc res)
  | typeDoc ANYTYPE = doc "<any-type>"
(* <boxed values 297>=                          *)
val _ = op typeDoc : ty -> doc
(* <prettyprinting of \mcl\ types and module types>= *)
fun stdindent doc = indent (2, doc)

fun mtDoc (MTEXPORTS []) = doc "(exports)"
  | mtDoc (MTEXPORTS comps) = 
      agrp (doc "(exports" ^^
            stdindent (brk ^^ brkSep (map ncompDoc comps) ^^ doc ")"))
  | mtDoc (MTALLOF  mts) =
      doc "(allof" ^/+ brkSep (map mtDoc mts) ^^ doc ")"
  | mtDoc (MTARROW (args, res)) =
      arrowdoc (map modformalDoc args) "--m->" (mtDoc res)
(* <prettyprinting of \mcl\ types and module types>= *)
and modformalDoc (m, t) =
    agrp (doc "[" ^^ doc (modidentString m) ^^ doc " :" ^^
          indent(2, brk ^^ mtDoc t ^^ doc "]"))
and ncompDoc (x, c) =
  case c
    of COMPVAL tau =>
         agrp (doc "[" ^^ doc x ^^ doc " :" ^^
               stdindent (brk ^^ typeDoc tau ^^ doc "]"))
     | COMPABSTY _   => doc ("[abstype " ^ x ^ "]")
     | COMPMANTY tau => agrp (doc "[type " ^^ doc x ^^ doc " " ^^
                                  indent (4, typeDoc tau) ^^ doc "]")
     | COMPMOD mt =>
         agrp (doc "(module" ^^
                   indent(2, brk ^^ doc "[" ^^ doc x ^^ doc " :" ^^
                   indent(2, brk ^^ agrp (mtDoc mt ^^ doc"])"))))
(* \qbreak                                      *)
(* <boxed values 298>=                          *)
val _ = op mtDoc : modty -> doc
(* <boxed values 298>=                          *)
val _ = op modformalDoc : modident * modty -> doc
val _ = op ncompDoc : name * component -> doc
(* <prettyprinting of \mcl\ types and module types>= *)
fun ndecString (x, c) =
  case c
    of ENVVAL tau => "[" ^ x ^ " : " ^ typeString tau ^ "]"
     | ENVMANTY tau => "(type " ^ x ^ " " ^ typeString tau ^ ")"
     | ENVMOD (mt, _) => "(module [" ^ x ^ " : " ^ mtString mt ^ "])"
     | ENVOVLN _ => "<overloaded name " ^ x ^ " ...>"
     | ENVMODTY mt => "(module-type " ^ x ^ " " ^ mtString mt ^ ")"
(* <boxed values 299>=                          *)
val _ = op ndecString : name * binding -> string
(* <string conversion of \mcl's abstract syntax>= *)
fun stripExpAt (EXP_AT (_, e)) = stripExpAt e
  | stripExpAt e = e

fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      fun sqbracket s = "[" ^ s ^ "]"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = sqbracket (x ^ " " ^ expString e)
      fun formal (x, ty) = sqbracket (x ^ " : " ^ tyexString ty)
      fun tbindings bs = bracket (spaceSep (map tbinding bs))
      and tbinding ((x, tyex), e) = 
        bracket (formal (x, tyex) ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*"
  in  case e
        of LITERAL v => valueString v
         | VAR name => pathexString name
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | WHILEX (c, b) =>
             bracketSpace ["while", expString c, expString b]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es, _) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) =>
             bracketSpace [letkind lk, bindings bs, expString e]
         | LETRECX (bs, e) => 
             bracketSpace ["letrec", tbindings bs, expString e]
         | LAMBDA (xs, body) => 
             bracketSpace ("lambda" :: map formal xs @ [expString body])
         | VCONX vcon => vconString vcon
         | CASE (e, matches) =>
             let fun matchString (pat, e) =
                   sqbracket (spaceSep [patString pat, expString e])
             in  bracketSpace 
                   ("case" :: expString e :: map matchString matches)
             end
         | MODEXP components => 
             bracketSpace ("modexp" :: map binding components)
         | ERRORX es => bracketSpace ("error" :: exps es)
         | EXP_AT (_, e) => expString e
  end
(* \qvfilbreak4in                               *)
(*                                              *)
(* Conversion of abstract syntax                *)
(*                                              *)
(* <boxed values 301>=                          *)
val _ = op expString : exp -> string
(* <string conversion of \mcl's abstract syntax>= *)
fun defString d =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun sq s = "[" ^ s ^ "]"
      val sqSpace = sq o spaceSep
      fun formal (x, t) = "[" ^ x ^ " : " ^ tyexString t ^ "]"
  in  case d
        of EXP e         => expString e
         | VAL    (x, e) => bracketSpace ["val",     x, expString e]
         | VALREC (x, t, e) =>
             bracketSpace ["val-rec", sqSpace [x, ":", tyexString t],
                           expString e]
         | DEFINE (f, ty, (formals, body)) =>
             bracketSpace ["define", tyexString ty, f,
                           bracketSpace (map formal formals), expString body]
         | QNAME p => pathexString p
         | TYPE (t, tau) => bracketSpace ["type", t, tyexString tau]
         | DATA (t, _) => bracketSpace ["data", t, "..."]
         | OVERLOAD paths => bracketSpace ("overload" :: map pathexString paths)
         | MODULE (m, _) => bracketSpace ["module", m, "..."]
         | GMODULE (m, _, _) => bracketSpace ["generic-module", m, "..."]
         | MODULETYPE (t, mt) => bracketSpace ["module-type", t, "..."]
  end
(* \qvfilbreak2in                               *)
(*                                              *)
(* <boxed values 302>=                          *)
val _ = op defString : baredef -> string
(* In uML, as in \ocaml, comparing functions for *)
(* equality causes a run-time error. Standard ML has a *)
(* more elaborate type system which rejects such *)
(* comparisons during type checking.            *)
(*                                              *)
(* \qvfilbreak2in                               *)
(*                                              *)
(* The primitive equality used in \mcl (\crefmcl.chap) *)
(* is almost the same as the version used in uML. *)
(* The only difference is that in \mcl, a constructed *)
(* value contains mutable reference cells, not values. *)
(* Notionally, this version of primitive equality *)
(* belongs in \crefmcla.chap, but because it is so *)
(* similar to uML's primitive equality, I define it *)
(* here. That way if I need to change one, I will know *)
(* also to change the other.                    *)
(* <utility functions on \uml\ values ((Molecule))>= *)
fun primitiveEquality (v, v') =
  let fun noFun () = raise RuntimeError "compared functions for equality"
  in  case (v, v')
        of (NUM  n1,  NUM  n2)  => (n1 = n2)
         | (SYM  v1,  SYM  v2)  => (v1 = v2)
         | (CONVAL (vcon, vs), CONVAL (vcon', vs')) =>
             vcon = vcon' andalso
                    ListPair.allEq primitiveEquality (map ! vs, map ! vs')
         | (CLOSURE   _, _) => noFun ()
         | (PRIMITIVE _, _) => noFun ()
         | (_, CLOSURE   _) => noFun ()
         | (_, PRIMITIVE _) => noFun ()
         | _ => raise BugInTypeInference
                        ("compared incompatible values " ^ valueString v ^
                         " and " ^ valueString v' ^ " for equality")
  end
val testEquals = primitiveEquality
(* The version used in \mcl has the same idea, but both *)
(* value constructors and their arguments are   *)
(* represented differently.                     *)
(* <utility functions on \uml\ values ((Molecule))>= *)
fun embedList []      = CONVAL (PNAME "'()", [])
  | embedList (v::vs) = CONVAL (PNAME "cons", [ref v, ref (embedList vs)])
(* And again the \mcl version is placed here.   *)
(* <utility functions on \uml\ values ((Molecule))>= *)
fun embedBool b = CONVAL (PNAME (if b then "#t" else "#f"), [])
fun projectBool (CONVAL (PNAME "#t", [])) = true
  | projectBool _                         = false


(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR OPERATOR OVERLOADING IN \MCL                    *)
(*                                                               *)
(*****************************************************************)

(* <support for operator overloading in \mcl>=  *)
fun plast (PDOT (_, x)) = x
  | plast (PNAME (_, x)) = x
  | plast (PAPPLY _) = "??last??"
(* Each [[APPLY]] node in the abstract-syntax tree *)
(* stores the index of the overloaded instance to be *)
(* used at that [[APPLY]] node. If the function applied *)
(* is not an overloaded name, that index is -1. *)
(* <support for operator overloading in \mcl>=  *)
val notOverloadedIndex = ~1


(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \MCL, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* Parsing                                      *)
(*                                              *)
(* [*] [*] Parsing code is shared with uML and Typed *)
(* uScheme. And because those other parsers call *)
(* [[booltok]] and [[tyvar]], they have to be defined. *)
(* Here they are defined as [[pzero]], which always *)
(* fails.                                       *)
(* <lexical analysis and parsing for \mcl, providing [[filexdefs]] and [[stringsxdefs]]>= *)
(* Lexical analysis                             *)
(*                                              *)
(* [*] In order to recognize qualified names, \mcl needs *)
(* its own lexer: one in which dots separate tokens. In *)
(* this lexer, every name is recognized as [[DOTTED]] *)
(* —even one with no dots. A name with no dots is *)
(* represented as [[DOTTED]] with an empty list. *)
(* <lexical analysis for {\mcl}>=               *)
datatype pretoken
  = QUOTE
  | INT      of int
  | RESERVED of string
  | DOTTED   of string * string list
  | DOTNAMES of string list (* .x.y and so on *)
type token = pretoken plus_brackets
(* <lexical analysis for {\mcl}>=               *)
fun pretokenString (QUOTE)      = "'"
  | pretokenString (INT  n)     = intString n
  | pretokenString (DOTTED (s, ss))  = String.concatWith "." (s::ss)
  | pretokenString (DOTNAMES ss)= (concat o map (fn s => "." ^ s)) ss
  | pretokenString (RESERVED x) = x
val tokenString = plusBracketsString pretokenString
(* <lexical analysis for {\mcl}>=               *)
local
  (* <functions used in all lexers>=              *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* If the lexer doesn't recognize a bracket, quote mark, *)
  (* integer, or other atom, it's expecting the line to *)
  (* end. The end of the line may present itself as the *)
  (* end of the input stream or as a stream of characters *)
  (* beginning with a semicolon, which marks a comment. *)
  (* If the lexer encounters any other character, *)
  (* something has gone wrong. (The polymorphic type of *)
  (* [[noneIfLineEnds]] provides a subtle but strong hint *)
  (* that no token can be produced; the only possible *)
  (* outcomes are that nothing is produced, or the lexer *)
  (* detects an error.) [*]                       *)
  (* <boxed values 54>=                           *)
  val _ = op noneIfLineEnds : 'a lexer
  (* \qbreak \mcl's reserved words are listed here, and if *)
  (* a single name x is reserved, it is converted from \ *)
  (* monoboxDOTTED (x, []) to \monoboxRESERVED x by *)
  (* function [[reserve]].                        *)
  (* <support for \mcl's reserved words>=         *)
  val reserved = 
    [ (* The arrow is reserved for function types. The colon *)
      (* is reserved to mark formal parameters, and to improve *)
      (* error detection, I lump it in with types.    *)
      (* <words reserved for \mcl\ types>=            *)
      "->", ":"
    , (* For the non-atomic expressions, the following words *)
      (* are reserved:                                *)
      (* <words reserved for \mcl\ expressions>=      *)
      "@m", "if", "&&", "||", "set", "let", "let*", "letrec", "case", "lambda",
      "val", "set", "while", "begin", "error",
      "when", "unless", "assert"
    , (* \qbreak The definition forms reserve these words: *)
      (* <words reserved for \mcl\ definitions>=      *)
      ":", 
      "val", "define", "exports", "allof", "module-type", "module", "--m->",
      "generic-module", "unsealed-module", "type", "abstype", "data",
      "record-module", "exports-record-ops",
      "use", "check-expect", "check-assert",
      "check-error", "check-type", "check-type-error",
      "check-module-type",
      "overload"
    ]
  fun isReserved x = member x reserved
  fun reserve (token as DOTTED (s, [])) =
        if isReserved s then
          RESERVED s
        else
          token
    | reserve token = token
  (* A dotted name is recognized by splitting the input *)
  (* characters into delimiters and nondelimiters. *)
  (* The delimiters include the usual delimiters from *)
  (* other lexers (brackets and so on, as defined by *)
  (* [[isDelim]]), plus the dot.                  *)
  (* <lexing functions for \mcl's dotted names>=  *)
  val isDelim = fn c => isDelim c orelse c = #"."
  (* A dotted name is formed from a sequence of parts. *)
  (* Each part is either a contiguous sequence of *)
  (* nondelimiters or a single dot.               *)
  (* <lexing functions for \mcl's dotted names>=  *)
  datatype part = DOT | NONDELIMS of string
  val nondelims = (NONDELIMS o implode) <$> many1 (sat (not o isDelim) one)
  val dot       = DOT <$ eqx #"." one
  (* <lexing functions for \mcl's dotted names>=  *)
  fun dottedNames things =
    let fun preDot (ss', DOT :: things)    = postDot (ss', things)
          | preDot (ss', nil)              = OK (rev ss')
          | preDot (ss', NONDELIMS _ :: _) =
              raise InternalError "bad dot in lexer"
        and postDot (ss', DOT :: _) =
              ERROR "A qualified name may not contain consecutive dots"
          | postDot (ss', nil)      =
              ERROR "A qualified name may not end with a dot"
          | postDot (ss', NONDELIMS s :: things) =
              if isReserved s then
                ERROR ("reserved word '" ^ s ^ "' used in qualified name")
              else
                preDot (s :: ss', things)
    in  case things
          of NONDELIMS s :: things => preDot  ([], things) >>=+ curry DOTTED s
           | DOT         :: things => postDot ([], things) >>=+ DOTNAMES
           | [] => ERROR "Lexer is broken; report to nr@cs.tufts.edu"
    end
in
  val mclToken =
    whitespace *>
    bracketLexer (  QUOTE   <$  eqx #"'" one
                <|> INT     <$> intToken isDelim
                <|> reserve <$> (dottedNames <$>! many1 (nondelims <|> dot))
                <|> noneIfLineEnds
                 )
  fun badReserved r = 
    ERROR ("reserved word '" ^ r ^ "' where name was expected")
(* The sequence of parts is turned into a token by *)
(* function [[dottedNames]]. Inner functions [[preDot]] *)
(* and [[postDot]] are called before and after each dot, *)
(* respectively. A token with consecutive dots or a *)
(* final dot is ill formed.                     *)
(* <boxed values 303>=                          *)
val _ = op dottedNames : part list -> pretoken error
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* \qbreak A token is either a quote mark; an integer *)
(* literal; or a (dotted) name, possibly reserved. *)
(* Or a bracket, which is taken care of by      *)
(* [[bracketLexer]].                            *)
(* <boxed values 303>=                          *)
val _ = op mclToken : token lexer
val _ = op badReserved : name -> 'a error
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

end
(* Parsers for tokens are defined as follows:   *)
(* <parsers for \mcl\ tokens>=                  *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val namelike  = ((fn (DOTTED (x, []))   => SOME x  | _ => NONE) <$>? pretoken)
val dotted  = (fn (DOTTED (x, xs))   => SOME (x, xs)  | _ => NONE) <$>? pretoken
val dotnames = (fn (DOTNAMES xs)  => SOME xs | _ => NONE) <$>? pretoken
val reserved = (fn RESERVED r => SOME r | _ => NONE) <$>? pretoken
val name = asAscii namelike  (* reserved words handled in lexer *)


val arrow = eqx "->" reserved <|> eqx "--m->" reserved

val showErrorInput = (fn p => showErrorInput tokenString p)
val booltok = pzero
val tyvar = pzero : name parser
(* To identify value constructors and value variables, *)
(* I define two predicates.                     *)
(* <parsers for \uml\ value constructors and value variables>= *)
fun isVcon x =
  let val lastPart = List.last (String.fields (curry op = #".") x)
      val firstAfterdot = String.sub (lastPart, 0) handle Subscript => #" "
  in  x = "cons" orelse x = "'()" orelse
      Char.isUpper firstAfterdot orelse firstAfterdot = #"#" orelse
      String.isPrefix "make-" x
  end
fun isVvar x = x <> "->" andalso not (isVcon x)
(* Each namelike thing gets its own parser. A value *)
(* constructor may be not only a suitable name but also *)
(* a Boolean literal or the empty list.         *)
(* <parsers for \uml\ value constructors and value variables>= *)
val arrow = sat (fn n => n = "->") namelike
val vvar  = sat isVvar namelike
val tyname = vvar
val vcon  = 
  let fun isEmptyList (left, right) =
            notCurly left andalso snd left = snd right
      val boolcon = (fn p => if p then "#t" else "#f") <$> booltok
  in  boolcon <|> sat isVcon namelike <|>
      "'()" <$ quote <* sat isEmptyList (pair <$> left <*> right)
  end
(* <parsers and parser builders for formal parameters and bindings>= *)
fun formalsOf what name context = 
  nodups ("formal parameter", context) <$>! @@ (bracket (what, many name))

fun bindingsOf what name exp =
  let val binding = bracket (what, pair <$> name <*> exp)
  in  bracket ("(... " ^ what ^ " ...) in bindings", many binding)
  end
(* <parsers and parser builders for formal parameters and bindings>= *)
fun distinctBsIn bindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map fst bs) >>=+ (fn _ => bs)
  in  check <$>! @@ bindings
  end
(* <boxed values 56>=                           *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
(* <boxed values 56>=                           *)
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* <parsers and parser builders for formal parameters and bindings ((higher-order))>= *)
fun asLambda inWhat (loc, e as LAMBDA _) = OK e
  | asLambda inWhat (loc, e) = 
      synerrorAt ("in " ^ inWhat ^ ", expression " ^ expString e ^ 
                  " is not a lambda")
                 loc

val asLambda = fn what => fn eparser => asLambda what <$>! @@ eparser
(* <boxed values 57>=                           *)
val _ = op asLambda : string -> exp parser -> exp parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* <boxed values 58>=                           *)
val _ = op recordFieldsOf : name parser -> name list parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and parser builders for formal parameters and bindings>= *)
fun kw keyword = 
  eqx keyword namelike
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* To define a parser for the bracketed expressions, *)
(* I deploy the ``usage parser'' described in \cref *)
(* lazyparse.chap. It enables me to define most of the *)
(* parser as a table containing usage strings and *)
(* functions. Function [[kw]] parses only the keyword *)
(* passed as an argument. Using it, function    *)
(* [[usageParsers]] strings together usage parsers. *)
(* <boxed values 60>=                           *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* <parser builders for typed languages>=       *)
val distinctTyvars = 
  nodups ("quantified type variable", "forall") <$>! @@ (many tyvar)
(* <parser builders for typed languages>=       *)
fun arrowsOf conapp funty =
  let fun arrows []              [] = ERROR "empty type ()"
        | arrows (tycon::tyargs) [] = OK (conapp (tycon, tyargs))
        | arrows args            [rhs] =
            (case rhs
               of [result] => OK (funty (args, result))
                | [] => ERROR "no result type after function arrow"
                | _  => ERROR "multiple result types after function arrow")
        | arrows args (_::_::_) = ERROR "multiple arrows in function type"
  in  fn xs => errorLabel "syntax error: " o arrows xs
  end
(* <boxed values 86>=                           *)
val _ = op tyvar : name parser
(* <boxed values 86>=                           *)
val _ = op distinctTyvars : name list parser
(* Function [[distinctTyvars]] is used in multiple *)
(* interpreters.                                *)

(* Parsing a type in brackets is tricky enough that *)
(* I define a function just for that job. The [[arrows]] *)
(* function takes two lists: the types that appear *)
(* before the first arrow, and a list of lists of types *)
(* that appear after an arrow. To make it usable with *)
(* languages beyond Typed uScheme, I abstract over the *)
(* [[conapp]] and [[funty]] functions that are used to *)
(* build types.                                 *)
(* <boxed values 86>=                           *)
val _ = op arrowsOf : ('ty * 'ty list -> 'ty) -> ('ty list * 'ty -> 'ty) -> 'ty
                                              list -> 'ty list list -> 'ty error
(* <parser builders for typed languages>=       *)
fun distinctTBsIn tbindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map (fst o fst) bs) >>=+ (fn _ =>
                                                                             bs)
  in  check <$>! @@ tbindings
  end
(* Each [[letrec]] form has to have mutually distinct *)
(* names. That requirement is enforced by function *)
(* [[distinctTBsIn]].                           *)
(* <boxed values 90>=                           *)
val _ = op distinctTBsIn : ((name * 't) * 'e) list parser -> string -> ((name *
                                                           't) * 'e) list parser
(* <parser builders for typed languages>=       *)
fun typedFormalOf name colon ty =
      bracket ("[x : ty]", pair <$> name <* colon <*> ty)
fun typedFormalsOf name colon ty context = 
  let val formal = typedFormalOf name colon ty
  in  distinctBsIn (bracket("(... [x : ty] ...)", many formal)) context
  end                            
(* A function definition has a list of typed formal *)
(* parameters. Each formal parameter is bound to a type, *)
(* and the names must be mutually distinct.     *)
(* <boxed values 398>=                          *)
val _ = op typedFormalsOf : string parser -> 'b parser -> 'a parser  -> string
                                                    -> (string * 'a) list parser
(* To use the common [[usageParsers]] function, I need a *)
(* recognizer for keywords. Which is useful for the *)
(* language-specific parsers as well.           *)
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun kw keyword = eqx keyword reserved
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun addDots p xs = 
  foldl (fn (x, p) => PDOT (p, x)) p xs
fun path tokens =
  let fun qname (loc, (x, xs)) = addDots (PNAME (loc, x)) xs
      val appl = curry PAPPLY <$> (PNAME <$> @@ name) <*> many path
  in  qname <$> @@ dotted
        <|>
      addDots <$> bracketKeyword (kw "@m", "(@m name path ...)", appl)
              <*> (dotnames <|> pure [])
  end tokens
(* Parsers for paths, types, and expressions    *)
(*                                              *)
(* A path may include an application to paths, which *)
(* makes [[path]] recursive. And an application may be *)
(* followed by more component selection (\monobox.x.y *)
(* and so on), which is parsed by [[dotnames]]. *)
(* <boxed values 304>=                          *)
val _ = op addDots : pathex -> name list -> pathex
val _ = op path : pathex parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun mkTyex br tokens =
  let val ty = showErrorInput (mkTyex br)
      fun arrows []              [] = ERROR "empty type ()"
        | arrows (tycon::tyargs) [] = ERROR "missing @@ or ->"
        | arrows args            [rhs] =
            (case rhs of [result] => OK (FUNTY (args, result))
                       | [] => ERROR "no result type after function arrow"
                       | _  => ERROR ("multiple result types " ^
                                      "after function arrow"))
        | arrows args (_::_::_) = ERROR "multiple arrows in function type"
  in
      TYNAME <$> path
        <|> 
      br ( "(ty ty ... -> ty)"
         ,  arrows <$> many ty <*>! many (kw "->" *> many ty)
         )
  end tokens
val tyex = mkTyex (showErrorInput o bracket)
(* val liberalTyex = mkTyex bracketOrFail *) (*OMIT*)
(* A [[tyex]] is either a path or is a function type *)
(* formed from types and an arrow. Function [[br]] *)
(* handles the function type and any errors that may *)
(* occur there. It also can provide a usage string. *)
(* <boxed values 305>=                          *)
val _ = op mkTyex : (string * tyex parser -> tyex parser) -> tyex parser
val _ = op tyex : tyex parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
val bare_vcon = vcon
fun dottedVcon (x, xs) = addDots (PNAME x) xs
fun vconLast (PDOT (_, x)) = x
  | vconLast (PNAME x) = x
  | vconLast (PAPPLY _) = raise InternalError "application vcon"

val vcon =  
  let fun notAVcon (loc, (x, xs)) =
        let val name = String.concatWith "." (x::xs)
        in  synerrorAt ("Expected value constructor, but got name " ^ name) loc
        end
  in  sat (isVcon o vconLast) (dottedVcon <$> dotted) 
        <|> PNAME <$> bare_vcon
        <|> notAVcon <$>! @@ dotted
  end
(* \qvfilbreak1.5in                             *)
(*                                              *)
(* A value constructor is like a path, except the last *)
(* component must satisfy [[isVcon]].           *)
(* <boxed values 306>=                          *)
val _ = op vcon : vcon parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun pattern tokens =  (
                WILDCARD    <$  eqx "_" vvar
      <|>       PVAR        <$> vvar
      <|> curry CONPAT      <$> vcon <*> pure []
      <|> bracket ( "(C x1 x2 ...) in pattern"
                  , curry CONPAT <$> vcon <*> many pattern
                  )
       ) tokens
(* Within a pattern, only a [[vcon]] may be applied. *)
(* <boxed values 307>=                          *)
val _ = op pattern : pat parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun quoteName "#f" = CONVAL (PNAME "#f", [])
  | quoteName "#t" = CONVAL (PNAME "#t", [])
  | quoteName s    = SYM s

fun quotelit tokens = (
         quoteName <$> name
    <|>  NUM <$> int
    <|>  (ARRAY o Array.fromList) <$> bracket ("(literal ...)", many quotelit)
    ) tokens
(* Quoted literals need their own parser.       *)
(* <boxed values 308>=                          *)
val _ = op quoteName : string -> value
val _ = op quotelit : value parser
(* \mcl has its share of atomic expressions.    *)
(* <parsers and [[xdef]] streams for \mcl>=     *)
val atomicExp =  VAR <$> path
             <|> badReserved <$>! reserved
             <|> dotnames <!> "a qualified name may not begin with a dot"
             <|> LITERAL <$> NUM <$> int
             <|> VCONX <$> vcon
             <|> quote *> (LITERAL <$> quotelit)
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun bindTo exp = bracket ("[x e]", pair <$> name <*> exp)
val formal = bracket ("[x : ty]", pair <$> name <* kw ":" <*> tyex)
val lformals = bracket ("([x : ty] ...)", many formal)
fun nodupsty what (loc, xts) = 
  nodups what (loc, map fst xts) >>=+ (fn _ => xts)
                              (* error on duplicate names *)
(* \qbreak Parsers for bindings and formal parameters *)
(* are built as follows:                        *)
(* <boxed values 309>=                          *)
val _ = op lformals : (name * tyex) list parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun smartBegin [e] = e
  | smartBegin es = BEGIN es
(* In common with uSmalltalk, \mcl allows a function *)
(* body to contain a sequence of expressions; [[begin]] *)
(* can be implicit. A [[BEGIN]] is added only when *)
(* needed, by a ``smart constructor.''          *)
(* <boxed values 310>=                          *)
val _ = op smartBegin : exp list -> exp
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun cand [e] = e
  | cand (e::es) = IFX (e, cand es, LITERAL (embedBool false))
  | cand [] = raise InternalError "parsing &&"

fun cor [e] = e
  | cor (e::es) = IFX (e, LITERAL (embedBool true), cor es)
  | cor [] = raise InternalError "parsing ||"
(* Short-circuit Boolean connectives desugar to [[IFX]] *)
(* as follows:                                  *)
(* <boxed values 311>=                          *)
val _ = op cand : exp list -> exp
val _ = op cor  : exp list -> exp
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun assert e =
  IFX (e, BEGIN [], ERRORX [LITERAL (SYM "assertion-failure")])
(* And [[assert]] desugars into another [[IFX]]: *)
(* <boxed values 312>=                          *)
val _ = op assert: exp -> exp
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <parsers and [[xdef]] streams for \mcl>=     *)
fun exptable exp =
  let fun surpriseReserved words =
        let fun die w = ERROR ("while trying to parse an expression, I see " ^
                               "reserved word " ^ w ^

                            "... did you misspell a statement keyword earlier?")
        in  die <$>! sat (fn w => member w words) (left *> reserved)
        end
      val bindings = bindingsOf "[x e]" name exp
      val tbindings = bindingsOf "[x : ty]" formal exp
      val dbs       = distinctBsIn bindings
      val dtbs      = distinctTBsIn tbindings

      val choice   = bracket ("[pattern exp]", pair <$> pattern <*> exp)
      val body = smartBegin <$> many1 exp
      val nothing = pure (BEGIN [])

     fun lambda (xs : (name * tyex) list located) exp =
       nodupsty ("formal parameter", "lambda") xs >>=+ (fn xs => LAMBDA (xs, exp
                                                                              ))

  in usageParsers
     [ ("(if e1 e2 e3)",            curry3 IFX          <$> exp <*> exp <*> exp)
     , ("(when e1 e ...)",          curry3 IFX          <$> exp <*> body <*>
                                                                        nothing)
     , ("(unless e1 e ...)",        curry3 IFX          <$> exp <*> nothing <*>
                                                                           body)
     , ("(set x e)",                curry  SET          <$> name <*> exp)
     , ("(while e body)",           curry  WHILEX       <$> exp  <*> body)
     , ("(begin e ...)",                   BEGIN        <$> many exp)
     , ("(error e ...)",                   ERRORX       <$> many exp)
     , ("(let (bindings) body)",    curry3 LETX LET     <$> dbs "let"    <*>
                                                                           body)
     , ("(let* (bindings) body)",   curry3 LETX LETSTAR <$> bindings     <*>
                                                                           body)
     , ("(letrec (typed-bindings) body)", curry LETRECX <$> dtbs "letrec" <*>
                                                                           body)
     , ("(case exp (pattern exp) ...)", curry CASE <$> exp <*> many choice)
     , ("(lambda ([x : ty] ...) body)", lambda <$> @@ lformals <*>! body)
     , ("(&& e ...)",               cand <$> many1 exp)
     , ("(|| e ...)",               cor  <$> many1 exp)
     , ("(assert e)",               assert <$> exp)
     , ("(quote sx)",               LITERAL <$> quotelit)
     ]
    <|> surpriseReserved [
                       (* The arrow is reserved for function types. The colon *)

                     (* is reserved to mark formal parameters, and to improve *)
                          (* error detection, I lump it in with types.    *)
                          (* <words reserved for \mcl\ types>=            *)
                          "->", ":",

                         (* \qbreak The definition forms reserve these words: *)
                          (* <words reserved for \mcl\ definitions>=      *)
                          ":", 
                          "val", "define", "exports", "allof", "module-type",
                                                              "module", "--m->",
                          "generic-module", "unsealed-module", "type", "abstype"
                                                                       , "data",
                          "record-module", "exports-record-ops",
                          "use", "check-expect", "check-assert",
                          "check-error", "check-type", "check-type-error",
                          "check-module-type",
                          "overload"]
  end
(* \qtrim2 \mcl's expression parser has two unusual *)
(* features: For overloading, every [[APPLY]] node *)
(* allocates a ref cell. And every expression is wrapped *)
(* in [[EXP_AT]], which gives its location.     *)
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun exp tokens = 
  let fun applyNode f args = APPLY (f, args, ref notOverloadedIndex)
      val unlocatedExp = showErrorInput 
       (  atomicExp
      <|> exptable exp
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "empty application"
      <|> bracket("function application", applyNode <$> exp <*> many exp)
       ) 
  in  EXP_AT <$> @@ unlocatedExp
  end tokens
(* \qvfilbreak3.5in \qtrim0.9 The expression parser is *)
(* built recursively in the usual way, using    *)
(* [[exptable]] and [[exp]]. Function           *)
(* [[supriseReserved]] tries to spot a common mistake: *)
(* using a type or module keyword where an expression is *)
(* expected.                                    *)
(* <boxed values 313>=                          *)
val _ = op exptable : exp parser -> exp parser
val _ = op exp      : exp parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun formalWith whatTy aTy =
  bracket ("[x : " ^ whatTy ^ "]", pair <$> name <* kw ":" <*> aTy)

val formal = formalWith "ty" tyex
(* Parsers for declarations and definitions     *)
(*                                              *)
(* Both [[lambda]] expressions and generic modules can *)
(* have typed formal parameters, so to parse them, *)
(* I define [[formalWith]].                     *)
(* <boxed values 314>=                          *)
val _ = op formalWith : string -> 'a parser -> (name * 'a) parser
val _ = op formal : (name * tyex) parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun recordOpsType tyname (loc, formals) =
  let val t = TYNAME (PNAME (loc, tyname))
      val unitty  = TYNAME (PDOT (PNAME (loc, "Unit"), "t"))
      val conty = FUNTY (map snd formals, t)
      fun getterty (x, tau) = (loc, (x, DECVAL (FUNTY ([t], tau))))
      fun setname x = "set-" ^ x ^ "!"
      fun setterty (x, tau) =
            (loc, (setname x, DECVAL(FUNTY ([t, tau], unitty))))
      val exports =
            (loc, (tyname, DECABSTY)) :: (loc, ("make", DECVAL conty)) ::
            map getterty formals @ map setterty formals
  in  MTEXPORTSX exports
  end
(* The [[exports-record-ops]] form takes a type name and *)
(* a list of typed fields. Those elements are desugared *)
(* into a module type by function [[recordOpsType]]. *)
(* <boxed values 315>=                          *)
val _ = op recordOpsType : string -> (name * tyex) list located -> modtyex
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun recordModule (loc, name) tyname fields =
  let val t = TYNAME (PNAME (loc, tyname))
      val vcon = "make-" ^ name ^ "." ^ tyname
      val conpat = CONPAT (PNAME vcon, map (PVAR o fst) fields)
      fun var x = VAR (PNAME (loc, x))
      (* \qbreak Each getter gets the ith field, and the *)
      (* setter sets the ith field. It's all done by using *)
      (* [[conpat]] to match the record value.        *)
      (* <definitions of [[getterComponent]] and [[setterComponent]]>= *)
      fun getter i =
        (LAMBDA ([("r", t)],
                 CASE (var "r",
                       [(conpat, var (fst (List.nth (fields, i))))])))
      fun setter i = 
        (LAMBDA ([("the record", t),
                  ("the value", snd (List.nth (fields, i)))],
                 CASE (var "the record",
                       [(conpat, SET (fst (List.nth (fields, i)),
                                      var "the value"))])))

      fun getterComponent ((x, _), i) = VAL (x, getter i)
      fun setterComponent ((x, _), i) = VAL ("set-" ^ x ^ "!", setter i)
      val conval = (* the record constructor *)
        LAMBDA (fields, APPLY (VCONX (PNAME vcon),
                                map (var o fst) fields,
                                ref notOverloadedIndex))

      val indices = List.tabulate (length fields, id)
      val components =
        DATA (tyname, [(vcon, FUNTY (map snd fields, t))]) ::
        VAL ("make", conval) ::
        ListPair.mapEq getterComponent (fields, indices) @
        ListPair.mapEq setterComponent (fields, indices)
      val modty = recordOpsType tyname (loc, fields)
  in  MODULE (name, MSEALED (modty, map (fn d => (loc, d)) components))
  end
(* Similarly, the definition of a record module is *)
(* desugared by function [[recordModule]]. Every record *)
(* module includes a type definition, a constructor *)
(* function, and a getter and a setter component for *)
(* each field.                                  *)
(* <boxed values 316>=                          *)
val _ = op recordModule : name located -> name -> (name * tyex) list -> baredef
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun prightmap f (x, a) = (x, f a)
fun crightmap f x a = (x, f a)
(* Below I use two forms of ``right map'' to change a *)
(* named thing. One form takes a pair and the other is *)
(* curried. In both cases a function is applied to a *)
(* value that is paired with a name.            *)
(* <boxed values 317>=                          *)
val _ = op prightmap : ('a -> 'b) -> name * 'a  -> name * 'b
val _ = op crightmap : ('a -> 'b) -> name -> 'a -> name * 'b
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun flipPair tx c = (c, tx)
(* Another handy function acts like [[pair]] but swaps *)
(* left and right.                              *)
(* <boxed values 318>=                          *)
val _ = op flipPair : 'a -> 'b -> 'b * 'a
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun decl tokens =
  (  usageParsers
       [ ("(abstype t)",          pair <$> name <*> pure DECABSTY)
       , ("(type t ty)",          crightmap DECMANTY  <$> name <*> tyex)
       , ("(module [A : modty])", prightmap DECMOD <$> modformal)
       ]
 <|> prightmap DECVAL <$> formal
  )
  tokens
and locmodformal tokens =
  bracket ("[M : modty]", pair <$> @@ name <* kw ":" <*> @@ modtype) tokens
and modformal tokens =
  ((fn (x, t) => (snd x, snd t)) <$> locmodformal) tokens
and modtype tokens = (
  usageParsers
  [ ("(exports component...)", MTEXPORTSX <$> many (@@ decl))
  , ("(allof module-type...)", MTALLOFX   <$> many (@@ modtype))
  , ("(exports-record-ops t ([x : ty] ...))",
                               recordOpsType <$> name <*> @@ lformals)
  ] 
  <|> MTNAMEDX <$> name
  <|> bracket ("([A : modty] ... --m-> modty)",
               curry MTARROWX <$> many locmodformal <*> kw "--m->" *> @@ modtype
                                                                               )
  ) tokens
(* \qtrim1                                      *)
(*                                              *)
(* The four forms of declaration: abstract type, *)
(* manifest type, module, and value.            *)
(* <boxed values 319>=                          *)
val _ = op decl : (name * decl) parser
val _ = op locmodformal : (name located * modtyex located) parser
val _ = op modformal    : (name * modtyex) parser
val _ = op modtype      : modtyex parser
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun wantedVcon (loc, x) =
  synerrorAt ("expected value constructor, but got name " ^ x) loc
fun wantedVvar (loc, x) =
  synerrorAt ("expected variable name, but got value constructor " ^ x) loc

val vvar = sat isVvar name
val vcon = 
  let fun isEmptyList (left, right) =
            notCurly left andalso snd left = snd right
      val boolcon  = (fn p => if p then "#t" else "#f") <$> booltok
  in  boolcon <|> sat isVcon name <|>
      "'()" <$ quote <* sat isEmptyList (pair <$> left <*> right)
  end

val (vcon, vvar) = ( vcon <|> wantedVcon <$>! @@ vvar
                   , vvar <|> wantedVvar <$>! @@ vcon
                   )
(* A common mistake is to misspell a value variable for *)
(* a constructor or vice versa. Or just to confuse them. *)
(* I update parsers for value variables and value *)
(* constructors to issue length error messages. *)
(* <boxed values 320>=                          *)
val _ = op vcon : name parser
val _ = op vvar : name parser
(* Value variables and value constructors.      *)

(* <parsers and [[xdef]] streams for \mcl>=     *)
fun def tokens = 
  showErrorInput (@@ baredef) tokens
and baredef tokens = 
  let fun define tau f formals body =
        nodupsty ("formal parameter", "definition of function " ^ f) formals
                                                                            >>=+
          (fn xts => DEFINE (f, tau, (xts, body)))
      fun definestar _ = ERROR "define* is left as an exercise"
      val tyname = name
      fun valrec (x, tau) e = VALREC (x, tau, e)
      fun sealedWith f (m : name, mt : modtyex) rhs = (m, f (mt, rhs))
      val conTy = typedFormalOf vcon (kw ":") tyex
      val body : exp parser = smartBegin <$> many1 exp
(* \qvfilbreak4in                               *)
(*                                              *)
(* All your definition forms are belong to us. \ *)
(* nwnarrowboxes                                *)
(* <boxed values 321>=                          *)
val _ = op def     : def     parser
val _ = op baredef : baredef parser
val _ = op define : tyex -> name -> (name * tyex) list located -> exp -> baredef
                                                                           error
val _ = op sealedWith : (modtyex * 'a -> moddef) -> name * modtyex -> 'a -> name
                                                                        * moddef
  in  usageParsers
      [ ("(define type f (args) body)",
                         define <$> tyex <*> name <*> @@ lformals <*>! body)
      , ("(val x e)",    curry VAL <$> vvar <*> exp)
      , ("(val-rec [x : type] e)",  valrec <$> formal <*> exp)

      , ("(data t [vcon : ty] ...)", (curry DATA <$> tyname <*> many conTy))
      , ("(type t ty)",           curry TYPE <$> name <*> tyex)
      , ("(module-type T modty)", curry MODULETYPE <$> name <*> modtype)
      , ("(module M path) or (module [M : T] path/defs)",
            MODULE <$> (  (pair <$> name <*> MPATH <$> path)
                      <|> (sealedWith MPATHSEALED <$> modformal <*> path)
                      <|> (sealedWith MSEALED <$> modformal <*> many def)
                       ))

      , ("(generic-module [M : T] defs)",
            let fun project ((_, m), (_, t)) = (m, t)
                fun gen ((loc, M), (loc', T)) defs =
                  case T
                    of MTARROWX (formals, result) =>
                         OK (GMODULE (M, map project formals,
                                      MSEALED (snd result, defs)))
                     | _ => 
                         ERROR ("at " ^ srclocString loc' ^ ", generic " ^
                                "module " ^ M ^ " does not have an arrow type")
            in   gen <$> locmodformal <*>! many def
            end)
      , ("(unsealed-module M defs)", 
            MODULE <$> (crightmap MUNSEALED <$> name <*> many def))
      , ("(record-module M t ([x : ty] ...))",
            recordModule <$> @@ name <*> name <*> lformals)
      , ("(overload qname ...)", OVERLOAD <$> many path)
      ]
     <|> QNAME <$> path
     <|> EXP <$> exp : baredef parser
  end tokens
(* \qbreak Parsers for unit tests.              *)
(* <parsers and [[xdef]] streams for \mcl>=     *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)",    curry CHECK_EXPECT     <$> exp <*> exp)
  , ("(check-assert e)",              CHECK_ASSERT     <$> exp)
  , ("(check-error e)",               CHECK_ERROR      <$> exp)
  , ("(check-type e tau)",      curry CHECK_TYPE       <$> exp <*> tyex)
  , ("(check-type-error e)",          CHECK_TYPE_ERROR <$> def)
  , ("(check-module-type M T)", curry CHECK_MTYPE      <$> path <*> modtype)
  ]
(* <parsers and [[xdef]] streams for \mcl>=     *)
fun filenameOfDotted (x, xs) =
  String.concatWith "." (x :: xs) 
val xdeftable = usageParsers
  [ ("(use filename)", (USE o filenameOfDotted) <$> dotted)
  ]

val xdef =  TEST <$> testtable
        <|>          xdeftable
        <|> DEF <$>  def
        <|> badRight "unexpected right bracket"
        <?> "definition"
(* And the extended definitions.                *)
(* <boxed values 322>=                          *)
val _ = op xdef : xdef parser

val xdefstream = 
  interactiveParsedStream (mclToken, xdef)
(* <shared definitions of [[filexdefs]] and [[stringsxdefs]]>= *)
fun filexdefs (filename, fd, prompts) =
      xdefstream (filename, filelines fd, prompts)
fun stringsxdefs (name, strings) =
      xdefstream (name, streamOfList strings, noPrompts)
(* \qbreak                                      *)
(*                                              *)
(* Streams of extended definitions              *)
(*                                              *)
(* Every bridge language has its own parser, called *)
(* [[xdefstream]], which converts a stream of lines to a *)
(* stream of [[xdef]]s. But as in \cref         *)
(* cinterps.shared-xdef-streams, the convenience *)
(* functions [[filexdefs]] and [[stringsxdefs]] are *)
(* shared.                                      *)
(* <boxed values 153>=                          *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   ENVIRONMENTS FOR \MCL'S DEFINED NAMES                       *)
(*                                                               *)
(*****************************************************************)

(* \qbreak In type-checking code, it's frequently useful *)
(* to refer to components of a module to things bound in *)
(* the static environment. These functions are used for *)
(* diagnostic messages throughout the type checker. *)
(* <environments for \mcl's defined names>=     *)
fun whatcomp (COMPVAL _) = "a value"
  | whatcomp (COMPABSTY _) = "an abstract type"
  | whatcomp (COMPMANTY _) = "a manifest type"
  | whatcomp (COMPMOD _) = "a module"
(* <environments for \mcl's defined names>=     *)
fun whatdec (ENVVAL _) = "a value"
  | whatdec (ENVMANTY _) = "a manifest type"
  | whatdec (ENVOVLN _) = "an overloaded name"
  | whatdec (ENVMOD _) = "a module"
  | whatdec (ENVMODTY _) = "a module type"
(* <environments for \mcl's defined names>=     *)
fun compString (ENVVAL tau) = "a value of type " ^ typeString tau
  | compString (ENVMANTY tau) = "manifest type " ^ typeString tau
  | compString (ENVOVLN _) = "an overloaded name"
  | compString (ENVMOD (mt, path)) = "module " ^ pathString path ^
                                     " of type " ^ mtString mt
  | compString (ENVMODTY _) = "a module type"
(* \qbreak                                      *)
(*                                              *)
(* String conversion                            *)
(*                                              *)
(* String conversion in \mcl differs from string *)
(* conversion in other languages in two ways:   *)
(*                                              *)
(*   • There are many more types of things to be *)
(*  converted.                                  *)
(*   • Some things, notably types, are converted to a *)
(*  value of type [[doc]], so they can be       *)
(*  prettyprinted.                              *)
(*                                              *)
(* Conversion for messaging about the environment *)
(*                                              *)
(* <boxed values 288>=                          *)
val _ = op compString : binding -> string



(*****************************************************************)
(*                                                               *)
(*   TYPE CHECKING FOR {\MCL}                                    *)
(*                                                               *)
(*****************************************************************)

(* Implementation of \mcl's type system         *)
(*                                              *)
(* Fasten your seat belt. The elements are as follows: *)
(* <type checking for {\mcl}>=                  *)
(* <additional operations for composing [[error]] values>= *)
infix 1 >>
fun (OK ()) >> c = c
  | (ERROR msg) >> _ = ERROR msg

fun firstE []      = OK ()
  | firstE (e::es) = e >> firstE es
(* <[[context]] for a {\mcl} definition>=       *)
datatype context
  = TOPLEVEL
  | INMODULE of path

fun contextDot (TOPLEVEL, name) = PNAME (genmodident name)
  | contextDot (INMODULE path, name) = PDOT (path, name)

fun contextString TOPLEVEL = "at top level"
  | contextString (INMODULE p) = "in module " ^ pathString p
(* Typechecking definitions                     *)
(*                                              *)
(* A definition can appear at top level or inside a *)
(* module body. When a module definition appears at top *)
(* level, its path is formed with a fresh module *)
(* identifier. The identifier is generated by   *)
(* [[genmodident]], which guarantees the uniqueness of *)
(* the new top-level module. But when a module is *)
(* defined inside another module, its path is computed *)
(* by combining its name with the path of the module in *)
(* which it is defined. No new unique identifier is *)
(* necessary.                                   *)
(*                                              *)
(* The place where a module definition (or any other *)
(* form of definition) appears is represented by a *)
(* [[context]].                                 *)
(* <boxed values 259>=                          *)
type context = context
val _ = op contextDot : context * name -> path
(* <type equality for \mcl>=                    *)
fun eqType (TYNAME p, TYNAME p') = p = p'
  | eqType (FUNTY (args, res), FUNTY (args', res')) =
      eqTypes (args, args') andalso eqType (res, res')
  | eqType (ANYTYPE, _) = true
  | eqType (_, ANYTYPE) = true
  | eqType _ = false
and eqTypes (taus, tau's) = ListPair.allEq eqType (taus, tau's)
(* Type [[ANYTYPE]] is equal to any type, and otherwise *)
(* type equality is structural. (It really hinges on *)
(* path equality.)                              *)
(* <boxed values 222>=                          *)
val _ = op eqType  : ty      * ty      -> bool
val _ = op eqTypes : ty list * ty list -> bool
(* When a [[data]] definition is typechecked or *)
(* evaluated, the treatment of each value constructor *)
(* depends on whether its type is a function type. *)
(* <recognition of function types>=             *)
fun isfuntype (FUNTY _) = true
  | isfuntype _         = false
(* <substitutions for \mcl>=                    *)
type rootsubst = (modident * path) list
val idsubst = []
(* Substitutions (boring)                       *)
(*                                              *)
(* Substitutions are used to propagate information about *)
(* manifest types. Everything you need to know about *)
(* substitution appears in \creftypesys.chap.   *)
(* <boxed values 266>=                          *)
type rootsubst = rootsubst
val _ = op idsubst : rootsubst
(* <substitutions for \mcl>=                    *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* <boxed values 267>=                          *)
val _ = op |--> : modident * path -> rootsubst
(* <substitutions for \mcl>=                    *)
type tysubst = 
  (path * ty) list
fun associatedWith (x, []) =
      NONE
  | associatedWith (x, (key, value) :: pairs) =
      if x = key then SOME value else associatedWith (x, pairs)

fun hasKey [] x = false
  | hasKey ((key, value) :: pairs) x = x = key orelse hasKey pairs x
(* <boxed values 268>=                          *)
type tysubst = tysubst
val _ = op associatedWith : path * tysubst -> ty option
val _ = op hasKey : tysubst -> path -> bool
(* <substitutions for \mcl>=                    *)
fun pathsubstRoot theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* <boxed values 269>=                          *)
val _ = op pathsubstRoot : rootsubst -> path -> path
(* <substitutions for \mcl>=                    *)
fun tysubstRoot theta (TYNAME p)          = TYNAME (pathsubstRoot theta p)
  | tysubstRoot theta (FUNTY (args, res)) =
      FUNTY (map (tysubstRoot theta) args, tysubstRoot theta res)
  | tysubstRoot theta ANYTYPE = ANYTYPE
(* <boxed values 270>=                          *)
val _ = op tysubstRoot : rootsubst -> ty -> ty
(* <substitutions for \mcl>=                    *)
fun dom theta = 
  map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubstRoot theta2 o pathsubstRoot theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* Functions [[dom]] and [[compose]] may be familiar *)
(* from \crefml.chap.                           *)
(* <boxed values 271>=                          *)
val _ = op dom     : rootsubst -> modident set
val _ = op compose : rootsubst * rootsubst -> rootsubst
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \mcl>=                    *)
fun bsubstRoot s = 
  map (fn (x, a) => (x, s a))

fun mtsubstRoot theta =
  let fun s (MTEXPORTS comps) = MTEXPORTS (bsubstRoot (compsubstRoot theta)
                                                                          comps)
        | s (MTALLOF mts)     = MTALLOF (map s mts)
        | s (MTARROW (args, res)) = MTARROW (bsubstRoot s args, s res)
  in  s
  end
and compsubstRoot theta =
  let fun s (COMPVAL t) = COMPVAL (tysubstRoot theta t)
        | s (COMPABSTY path) = COMPABSTY (pathsubstRoot theta path)
        | s (COMPMANTY t)  = COMPMANTY (tysubstRoot theta t)
        | s (COMPMOD mt)  = COMPMOD (mtsubstRoot theta mt)
  in  s
  end
(* <boxed values 272>=                          *)
val _ = op mtsubstRoot   : rootsubst -> modty      -> modty
val _ = op compsubstRoot : rootsubst -> component -> component
(* <substitutions for \mcl>=                    *)
fun tysubstManifest mantypes =
  let fun r (TYNAME path) = getOpt (associatedWith (path, mantypes), TYNAME path
                                                                               )
        | r (FUNTY (args, res)) = FUNTY (map r args, r res)
        | r (ANYTYPE) = ANYTYPE
  in  r
  end
(* <boxed values 273>=                          *)
val _ = op tysubstManifest : tysubst -> ty -> ty
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \mcl>=                    *)
fun mtsubstManifest mantypes mt =
  let val newty = tysubstManifest mantypes
      fun newmt (MTEXPORTS cs) =
            MTEXPORTS (map (fn (x, c) => (x, newcomp c)) cs)
        | newmt (MTALLOF mts) =
            MTALLOF (map newmt mts)  (* can't violate unmix invariant *)
        | newmt (MTARROW (args, result)) =
            MTARROW (map (fn (x, mt) => (x, newmt mt)) args, newmt result)
      and newcomp (COMPVAL tau) = COMPVAL (newty tau)
        | newcomp (COMPABSTY p) =
           (case associatedWith (p, mantypes)
              of SOME tau => COMPMANTY tau
               | NONE => COMPABSTY p)   (* used to be this on every path *)
        | newcomp (COMPMANTY tau) = COMPMANTY (newty tau)
        | newcomp (COMPMOD mt) = COMPMOD (newmt mt)
  in  newmt mt
  end
(* <boxed values 274>=                          *)
val _ = op mtsubstManifest : tysubst -> modty -> modty
(* <type components of module types>=           *)
fun abstractTypePaths (MTEXPORTS cs, path : path) =
      let fun ats (t, COMPABSTY _) = [PDOT (path, t)]
            | ats (x, COMPMOD mt) = abstractTypePaths (mt, PDOT (path, x))
            | ats _ = []
(* The abstract-type paths are enumerated here. *)
(* <boxed values 242>=                          *)
val _ = op abstractTypePaths : modty rooted -> path list
      in  concatMap ats cs
      end
  | abstractTypePaths (MTALLOF mts, path) =
      concatMap (fn mt => abstractTypePaths (mt, path)) mts
  | abstractTypePaths (MTARROW _, _) = []
                                          (* could be bogus, cf Leroy rule 21 *)
(* <utilities for module-type realization>=     *)
fun filterdec p (MTARROW f, path) = MTARROW f
  | filterdec p (MTALLOF mts, path) =
      MTALLOF (map (fn mt => filterdec p (mt, path)) mts)
  | filterdec p (MTEXPORTS xcs, path) =
      let fun cons ((x, c), xcs) =
            let val path = PDOT (path, x)
                val c = case c
                          of COMPMOD mt => COMPMOD (filterdec p (mt, path))
                           | _ => c
            in  if p (c, path) then
                  (x, c) :: xcs
                else
                  xcs
            end
      in  MTEXPORTS (foldr cons [] xcs)
      end
(* \qtrim2.5 Function [[filterdec]] selects only those *)
(* components accepted by a predicate.          *)
(* <boxed values 250>=                          *)
val _ = op filterdec : (component * path -> bool) -> modty rooted -> modty
(* <utilities for module-type realization>=     *)
fun emptyExports (MTEXPORTS []) = true
  | emptyExports _ = false
(* The module type (exports) is an identity of \land, so *)
(* it is filtered out of [[MTALLOF]].           *)
(* <boxed values 251>=                          *)
val _ = op emptyExports : modty -> bool
(* Realization of module types                  *)
(*                                              *)
(* The essences of module-type realization is   *)
(* substitution of manifest types for abstract types, as *)
(* implemented by function [[msubsn]]. Substitution is *)
(* also used to implement intersection (module) types, *)
(* since the intersection of a manifest type with an *)
(* abstract type requires substituting for the abstract *)
(* type. All functions related to this substitution are *)
(* therefore grouped here.                      *)
(* <module-type realization>=                   *)
(* <definition of [[msubsn]]>=                  *)
fun msubsn (MTEXPORTS cs, path) =
      let fun mts (x, COMPMANTY tau) = [(PDOT (path, x), tau)]
            | mts (x, COMPMOD mt) = msubsn (mt, PDOT(path, x))
            | mts _ = []
      in  concatMap mts cs
      end
  | msubsn (MTALLOF mts, path) =
      concatMap (fn mt => msubsn (mt, path)) mts
  | msubsn (MTARROW _, path) = []   (* could be bogus, cf Leroy rule 21 *)
(* \qbreak Realization substitutes manifest types for *)
(* abstract types in a module type. Function [[msubsn]] *)
(* is specified in \crefmcl.fig.msubsn in \mclpage *)
(* mcl.fig.msubsn.                              *)
(* <boxed values 248>=                          *)
val _ = op msubsn : modty rooted -> tysubst
(* <definition of [[simpleSyntacticMeet]]>=     *)
val simpleSyntacticMeet =
  let val path = PNAME (MODTYPLACEHOLDER "syntactic meet")
      fun filterManifest (prev', []) = rev prev'
        | filterManifest (prev', mt :: mts) =
            let val manifests = msubsn (MTALLOF prev', path)
                fun redundant (COMPMANTY tau, p) =
                      (case associatedWith (p, manifests)
                         of SOME tau' => eqType (tau, tau')
                          | NONE => false)
                  | redundant _ = false
            in  filterManifest (filterdec (not o redundant) (mt, path) :: prev',
                                                                            mts)
            end
      val filterManifest = fn mts => filterManifest ([], mts)
      fun mtall [mt] = mt
        | mtall mts  = MTALLOF mts
      val meet = mtall o List.filter (not o emptyExports) o filterManifest
  in  fn (MTALLOF mts) => meet mts
       | mt => mt
  end
(* \qvfilbreak2.5in                             *)
(*                                              *)
(* Quite often an [[allof]] type can be rewritten as an *)
(* [[exports]] type. The game is simply to combine the *)
(* export lists of the intersected module types. This *)
(* combination is accomplished by filtering out *)
(* redundant manifest-type declarations, then dropping *)
(* any module type that consists only of redundant *)
(* declarations (or is otherwise empty). This heuristic *)
(* makes a type look nicer without changing its meaning. *)
(* <boxed values 249>=                          *)
val _ = op simpleSyntacticMeet : modty -> modty
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <definition of smart constructor [[allofAt]]>= *)
fun allofAt (mts, path) =
  let val mt = MTALLOF mts
      val mantypes = msubsn (mt, path)
      val abstypes = abstractTypePaths (mt, path)
  in  if List.exists (hasKey mantypes) abstypes then
        simpleSyntacticMeet (mtsubstManifest mantypes mt)
      else
        mt
  end
(* And the invariant is established by this smart *)
(* constructor. It uses functions [[msubsn]] and *)
(* [[simpleSyntacticMeet]] defined below.       *)
(* <boxed values 243>=                          *)
val _ = op allofAt : modty list rooted -> modty
(* <definition of [[unmixTypes]]>=              *)
fun unmixTypes (mt, path) =
  let fun mtype (MTEXPORTS cs) = MTEXPORTS (map comp cs)
        | mtype (MTALLOF mts)  = allofAt (map mtype mts, path)
        | mtype (MTARROW (args, result)) =
            MTARROW (map (fn (x, mt) => (x, mtype mt)) args, mtype result)
      and comp (x, COMPMOD mt) = (x, COMPMOD (unmixTypes (mt, PDOT (path, x))))
        | comp c = c
  in  mtype mt
  end
(* In case an intersection type contains a mix of *)
(* manifest and abstract definitions of the same type, *)
(* the invariant can be restored by judicious use of *)
(* [[allofAt]]. Function [[unmixTypes]] uses [[allofAt]] *)
(* to restore the invariant.                    *)
(* <boxed values 244>=                          *)
val _ = op unmixTypes : modty rooted -> modty
(* <invariants of \mcl>=                        *)
fun mixedManifestations mt =
  let val path = PNAME (MODTYPLACEHOLDER "invariant checking")
      val manifests = msubsn (mt, path)
      val abstracts = abstractTypePaths (mt, path)
  in  List.exists (hasKey manifests) abstracts
  end
(* An invariant on combined module types        *)
(*                                              *)
(* Important invariant of the least upper bound: In any *)
(* semantic [[MTALLOF]], if a type name appears as *)
(* manifest in any alternative, it appears only as *)
(* manifest, never as abstract—and the module type has *)
(* no references to an abstract type of that name. *)
(*                                              *)
(* Violations of this invariant are detected by function *)
(* [[mixedManifestations]], which looks for     *)
(* abstract-type paths that include a manifest type. *)
(* <boxed values 241>=                          *)
val _ = op mixedManifestations : modty -> bool
(* <[[implements]] relation, based on [[subtype]] of two module types>= *)
fun subtype mts =
  let fun st (MTARROW (args, res), MTARROW (args', res')) =
            let fun contra ([], [], res') = st (res, res')
                  | contra ((x, tau) :: args, (x', tau') :: args', res') =
                       (* substitute x for x' *)
                       let val theta = mtsubstRoot (x' |--> PNAME x)
                       in  st (theta tau', tau) >>
                           contra (args, map (prightmap theta) args', theta res'
                                                                               )
                       end
                  | contra _ =
                      ERROR
                           "generic modules have different numbers of arguments"
            in  contra (args, args', res')
            end
        | st (MTARROW (args, _), _) =
            ERROR ("expected an exporting module but got one that takes " ^
                   countString args "parameter")
        | st (_, MTARROW (args, _)) =
            ERROR ("expected a module that takes " ^
                   countString args "parameter" ^
                                                ", but got an exporting module")
        | st (mt, MTALLOF mts') =
            firstE (map (fn mt' => st (mt, mt')) mts')
        | st (mt, MTEXPORTS comps') =
            compsSubtype (components mt, comps')
      and components (MTEXPORTS cs) = cs
        | components (MTALLOF mts) = concatMap components mts
        | components (MTARROW _) = raise InternalError "meet of arrow types"
      and compsSubtype (comps, comps') =
            let fun supplied (x, _) = List.exists (fn (y, _) => x = y) comps
                val (present, absent) = List.partition supplied comps'
                fun check (x, supercomp) =
                  let (* <definition of [[csubtype]]>=                *)
                      fun csubtype (COMPVAL tau, COMPVAL tau') =
                            if eqType (tau, tau') then OK ()
                            else ERROR ("interface calls for value " ^ x ^
                                                              " to have type " ^
                                        typeString tau' ^
                             ",\n            but it has type " ^ typeString tau)
                        | csubtype (COMPABSTY _, COMPABSTY _) = OK ()
                                               (* OK without comparing paths? *)
                        | csubtype (COMPMANTY _, COMPABSTY _) = OK ()
                                                                 (* likewise? *)
                        | csubtype (COMPMANTY tau, COMPMANTY tau') = 
                            if eqType (tau, tau') then OK ()
                            else ERROR ("interface calls for type " ^ x ^
                                                       " to manifestly equal " ^
                                        typeString tau' ^
                                   ",\n            but it is " ^ typeString tau)
                        | csubtype (COMPABSTY path, COMPMANTY tau') =
                            if eqType (TYNAME path, tau') then OK ()
                            else ERROR ("interface calls for type " ^ x ^
                                                       " to manifestly equal " ^
                                        typeString tau' ^
                         ",\n            but it is " ^ typeString (TYNAME path))
                        | csubtype (COMPMOD m, COMPMOD m') =
                            subtype (m, m')
                        | csubtype (c, c') =
                            ERROR ("interface calls for " ^ x ^ " to be " ^
                                                                   whatcomp c' ^
                                   ",\n            but implementation provides "
                                                                   ^ whatcomp c)
                      (* \qbreak                                      *)
                      (* <boxed values 246>=                          *)
                      val _ = op csubtype : component * component -> unit error
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* {mathpar} \textstyle\issubtype\amodtype\amodtype' *)
(*                                              *)
(* \inferrule \issubtype\amodtype \amodtype'_1  *)
(* \issubtype\amodtype \amodtype'_2 \issubtype\amodtype *)
(* \amodtype'_1 \land\amodtype'_2               *)
(*                                              *)
(* \inferrule \issubtype\comps(\amodtype) \decls' \ *)
(* issubtype\amodtype \astexports(\decls')      *)
(*                                              *)
(*                                              *)
(* \textstyle\issubtype\decls\decls'            *)
(*                                              *)
(* \inferrule \issubtype\decls\emptylist \inferrule \ *)
(* decls= \decls_pre, \adecl, \decls_post       *)
(* \issubtype\adecl \adecl''                    *)
(* \issubtype\decls \decls'' \issubtype\decls (\adecl'', *)
(* \decls'')                                    *)
(*                                              *)
(*                                              *)
(* \textstyle\issubtype\adecl \adecl'           *)
(*                                              *)
(* \inferrule \issubtype\absdeclpatht \apath\absdeclpath *)
(* t \apath'                                    *)
(*                                              *)
(* \inferrule \issubtypet = tau\absdeclpatht \apath' *)
(*                                              *)
(* \inferrule \issubtypet = taut = tau          *)
(*                                              *)
(* \inferrule \issubtype\absdeclpatht \apatht = \apath *)
(*                                              *)
(* \inferrule \issubtypex:taux:tau              *)
(*                                              *)
(* \inferrule\issubtype\amodtype\amodtype' \issubtypeM : *)
(* \amodtypeM : \amodtype'                      *)
(*                                              *)
(* {mathpar} Subtyping                          *)
(*                                              *)
(* [*] [*]                                      *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Module subtyping                             *)
(*                                              *)
(* The subtyping rules are reproduced in \cref  *)
(* mcla.fig.subtyping. To understand them, you must *)
(* understand Leroy's substitutions.            *)
(*                                              *)
(* The key goals of the implementation are as follows: *)
(*                                              *)
(*   • If subtyping fails, a witness should be keyed by *)
(*  path.                                       *)
(*   • Error messages should tell the whole story, e.g., *)
(*  ``context requires that [[t]] be both [[int]] and *)
(*  [[bool]].''                                 *)
(*   • If an intersection is uninhabited, ... ``Try a *)
(*  cheap and cheerful solution to uninhabited  *)
(*  intersections, e.g., incompatible manifest types? *)
(*  ''                                          *)
(*                                              *)
(* The result of a subtype test is represented not by a *)
(* Boolean but by a value of type \monoboxunit error. *)
(* When more than error is detected, I use only the *)
(* first one, by applying one of the following two *)
(* functions:                                   *)
(* <boxed values 245>=                          *)
val _ = op >> : unit error * unit error -> unit error
val _ = op firstE : unit error list -> unit error
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* \qvfilbreak4in                               *)
(*                                              *)
(* Function [[subtype]] implements relation \issubtype\ *)
(* amodtype\amodtype', and [[csubtype]] implements *)
(* relation \issubtype\adecl\adecl'.            *)
(* <boxed values 245>=                          *)
val _ = op csubtype : component * component -> unit error
val _ = op subtype  : modty * modty -> unit error
                  in  csubtype (find (x, comps), supercomp)
                  end
                    handle NotFound y =>
                      raise InternalError "missed present component"
                fun missingComponent (x, c) = x ^ " (" ^ whatcomp c ^ ")"
                val missedMsg =
                  if null absent then OK ()
                  else
                    ERROR ("an interface expected some components that are " ^
                           "missing: " ^
                           commaSep (map missingComponent absent))
            in  firstE (map check present) >> missedMsg
            end
  in  st mts
  end
(* <[[implements]] relation, based on [[subtype]] of two module types ((elided))>= *)
val mtsubstManifestDebug = fn theta => fn (super, p) =>
  let val mt' = mtsubstManifest theta super
      val () = app eprint [countString theta "substitution", "\n"]
      val () = app (fn (pi, tau) =>
                      app eprint ["   ", pathString pi, " |--> ", typeString tau
                                                                        , "\n"])
                   theta
      val () = app eprint ["realized: ", mtString mt', "\n"]
      
  in  mt'
  end
(* <[[implements]] relation, based on [[subtype]] of two module types>= *)
(*OMIT*) fun substString pairs =
(*OMIT*)   let fun pairString (p, tau) = pathString p ^ " |--> " ^ typeString
                                                                             tau
(*OMIT*)   in  "{ " ^ commaSep (map pairString pairs) ^ " } "
(*OMIT*)   end
fun implements (p, submt, supermt) =

(*   (app eprint ["At ", pathString p, "\n  sub:  ", mtString submt, "\n  sup: ", mtString supermt, "\n"]; id)  *)
                                                                        (*OMIT*)
  let val theta = msubsn (submt, p)
      (* val () = app eprint ["substitution ", substString theta, "\n"] *)
                                                                        (*OMIT*)
  in  subtype (submt, mtsubstManifest theta supermt) 
  end
(* Function [[implements]] tells if a subtype implements *)
(* a supertype. What's the path for? It is the first *)
(* argument to functions [[msubsn]] and to      *)
(* [[abstractTypePaths]]. Which means it's used as the *)
(* prefix to produce the correct substitution, and *)
(* that's it.                                   *)
(*                                              *)
(* If you've read \citetleroy:modular-module, function *)
(* [[implements]] is my approximation of Leroy's *)
(* [[modtype_match]]. Instead of placing type equalities *)
(* in an environment, I substitute.             *)
(* <boxed values 247>=                          *)
val _ = op implements : path * modty * modty -> unit error
(* <path-expression lookup>=                    *)
fun asBinding (COMPVAL tau, root) = ENVVAL tau
  | asBinding (COMPABSTY path, root) = ENVMANTY (TYNAME path)
  | asBinding (COMPMANTY tau, root) = ENVMANTY tau
  | asBinding (COMPMOD mt, root) = ENVMOD (mt, root)

fun uproot (ENVVAL tau) = COMPVAL tau
  | uproot (ENVMANTY tau) = COMPMANTY tau
  | uproot (ENVMOD (mt, _)) = COMPMOD mt
  | uproot d = raise InternalError (whatdec d ^ " as component")
(* \qtrim1                                      *)
(*                                              *)
(* Paths and \chaptocsplitenvironments          *)
(*                                              *)
(* Looking up path expressions                  *)
(*                                              *)
(* One complexity of \mcl's type system is that we *)
(* continually find ourselves converting between *)
(* bindings (which appear in environments) and  *)
(* components (which appear in export lists). Module *)
(* components are not rooted, but module bindings are *)
(* rooted—so to convert a component to a binding *)
(* requires a root.                             *)
(* <boxed values 236>=                          *)
val _ = op asBinding : component * path -> binding
val _ = op uproot : binding -> component
(* <path-expression lookup>=                    *)
fun pathfind (PNAME x, Gamma) = find (snd x, Gamma)
  | pathfind (PDOT (path, x), Gamma) =
      let (* <definition of [[mtfind]]>=                  *)
          fun mtfind (x, mt as MTEXPORTS comps) : component option =
                 (SOME (find (x, comps)) handle NotFound _ => NONE)
            | mtfind (x, MTARROW _) =
                 raise TypeError ("tried to select component " ^ x ^
                                  " from generic module " ^ pathexString path)
            | mtfind (x, mt as MTALLOF mts) =
                (case List.mapPartial (fn mt => mtfind (x, mt)) mts
                   of [comp] => SOME comp
                    | [] => NONE
                    | comps =>
                        let val abstract = (fn COMPABSTY _ => true | _ => false)
                            val manifest = (fn COMPMANTY _ => true | _ => false)
                            fun tycomp c = abstract c orelse manifest c
                        in  if not (List.all tycomp comps) then
                              if List.exists tycomp comps then
                                raise BugInTypeChecking
                                        "mixed type and non-type components"
                              else
                                unimp
                              "value or module component in multiple signatures"
                            else
                              case List.filter manifest comps
                                of [comp] => SOME comp
                                 | [] => SOME (hd comps)  (* all abstract *)
                                 | _ :: _ :: _ => 
          ( app (fn c => app eprint ["saw ", ncompString (x, c), "\n"]) comps
                                                                        (*OMIT*)
          ;
                                                                        (*OMIT*)
                                     unimp ("manifest-type component " ^ x ^
                                            " in multiple signatures")
          )
                                                                        (*OMIT*)
                        end)
          (* \qbreak The other piece of [[pathfind]] looks up a *)
          (* name in a module type to find a component. Because *)
          (* I have not worked out the theory of intersection *)
          (* types in its entirety, there are some tricky cases *)
          (* that I have not implemented,                 *)
          (* <boxed values 239>=                          *)
          val _ = op mtfind : name * modty -> component option
      in  case pathfind (path, Gamma)
            of ENVMOD (mt, root) =>
                 (asBinding (valOf (mtfind (x, mt)), root) handle Option =>
                   noComponent (path, x, mt))
             | dec =>
               (* <tried to select [[path]].[[x]] but [[path]] is a [[dec]]>= *)
                      raise TypeError ("Tried to select " ^ pathexString (PDOT (
                                                          path, x)) ^ ", but " ^
                                       pathexString path ^ " is " ^ whatdec dec
                                                         ^ ", which does not " ^
                                       " have components")
      end
  | pathfind (PAPPLY (fpx, actualpxs) : pathex, Gamma) =
     (* Now we can tackle the two big pieces of [[pathfind]]. *)
     (* First, the instantiation of a generic module when it *)
     (* is applied to actual parameters. The instantiation is *)
     (* specified by Leroy's [[Apply]] rule. The idea is *)
     (* summarized as follows: {mathpar} f : PiA:T.B *)
     (*                                              *)
     (* f @@ M : B[A |->M] {mathpar} This rule works even if *)
     (* B is itself an arrow type. Uncurrying, it means that *)
     (* when substituting for the first formal parameter, *)
     (* I substitute in all the remaining formal parameters. *)
     (* The type of the instantiation the result type of *)
     (* module [[fpx]] after substitution.           *)
     (* <instantiation of module [[fpx]] to [[actualpxs]]>= *)
     let fun rootedModtype px = case pathfind (px, Gamma)
                                  of ENVMOD (mt, root) => (mt, root)
                                   | dec => notModule (dec, px)
         and notModule (dcl, px) =
           raise TypeError ("looking for a module, but " ^ pathexString px ^
                            " is a " ^ whatdec dcl)
         val (fmod, actuals) = (rootedModtype fpx, map rootedModtype actualpxs)
         val (formals, result) = case fmod
                                   of (MTARROW fr, _) => fr
                                    | _ =>
                          (* Error messages for instantiation are as follows: *)

                              (* <instantiated exporting module [[fpx]]>=     *)
                                           raise TypeError ("module " ^
                       pathexString fpx ^ " is an exporting module, and only " ^

                                        " a generic module can be instantiated")
         fun resty ( []
                   , []
                   , result) = 
               result
           | resty ( (formalid, formalmt) :: formals
                   , (actmt,    actroot)  :: actuals
                   , result) =
               let val theta = formalid |--> actroot
                   fun fsubst (ident, mt) = (ident, mtsubstRoot theta mt)
                   val mtheta = msubsn (actmt, actroot)
     (*OMIT*)      val () = if true orelse null mtheta then ()
     (*OMIT*)               else app (fn (pi, tau) =>
     (*OMIT*)                           app eprint [ "manifestly ", pathString
                                                                              pi
     (*OMIT*)                                      , " |--> ", typeString tau,
                                                                          "\n"])
     (*OMIT*)                        mtheta
                   val subst = mtsubstManifest mtheta o mtsubstRoot theta
     (*OMIT*)      (* XXX need to substitute manifest types from the actuals? *)
               in  case implements (actroot, actmt, mtsubstRoot theta formalmt)
                     of OK () => resty (map fsubst formals, actuals, subst
                                                                         result)
                      | ERROR msg =>
                      (* <can't pass [[actroot]] as [[formalid]] to [[fpx]]>= *)
                                     raise TypeError ("module " ^ pathString
                                      actroot ^ " cannot be used as argument " ^
                                                      modidentString formalid ^
                                      " to generic module " ^ pathexString fpx ^
                                                      ": " ^ msg)
               end
           | resty _ = (* <wrong number of arguments to [[fpx]]>=      *)
                       raise TypeError ("generic module " ^ pathexString fpx ^
                                                              " is expecting " ^
                                        countString formals "parameter" ^
                                                                  ", but got " ^
                                        countString actuals "actual parameter")
     in  ENVMOD (resty (formals, actuals, result), PAPPLY (root fmod, map root
                                                                       actuals))
     end
and noComponent (path, x, mt) =
  raise TypeError ("module " ^ pathexString path ^ " does not have a component "
                                                                               ^
                   pathexString (PDOT (path, x)) ^ "; its type is " ^ mtString
                                                                             mt)
(* \qtrim1                                      *)
(*                                              *)
(* The fundamental operation on environments is looking *)
(* up a path, not just a name. Internal function *)
(* [[mtfind]] looks up the meaning of a component in a *)
(* module type—which can then be converted to a binding. *)
(* <boxed values 237>=                          *)
val _ = op pathfind : pathex * binding env -> binding
(* <path-expression lookup>=                    *)
local
  val vconLoc = ("unlocated value constructor", ~99)
in
  fun pathexOfVcon (PNAME x) = PNAME (vconLoc, x)
    | pathexOfVcon (PDOT (path, x)) = PDOT (pathexOfVcon path, x)
    | pathexOfVcon (PAPPLY _) = raise InternalError "application vcon"
end
(* Function [[pathfind]] can't look up a value  *)
(* constructor, because a value constructor is a path *)
(* that has only a name, not a location. But a value *)
(* constructor can be converted to a full [[pathex]] by *)
(* adding a meaningless location.               *)
(* <boxed values 238>=                          *)
val _ = op pathexOfVcon : vcon -> pathex
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <path-expression lookup>=                    *)
fun findModule (px, Gamma) =
  case pathfind (px, Gamma)
    of ENVMOD (mt, _) => mt
     | dec => raise TypeError ("looking for a module, but " ^
                               pathexString px ^ " is a " ^ whatdec dec)
(* A common special case of [[pathfind]] is to look up *)
(* the name of a module.                        *)
(* <boxed values 240>=                          *)
val _ = op findModule : pathex * binding env -> modty
(* Processing definitions \chaptocbacksplitand building *)
(* the initial basis                            *)
(*                                              *)
(* A named value, type, or module can also be a *)
(* component.                                   *)
(* <converting bound entities to components>=   *)
fun asComponent (x, ENVVAL tau)     = SOME (x, COMPVAL tau)
  | asComponent (x, ENVMANTY tau)   = SOME (x, COMPMANTY tau)
  | asComponent (m, ENVMOD (mt, _)) = SOME (m, COMPMOD mt)
  | asComponent (_, ENVOVLN _) = NONE
  | asComponent (_, ENVMODTY _) = raise InternalError "module type as component"
(* <elaboration of {\mcl} type syntax into types>= *)
fun elabpath (px, Gamma) =
  let fun elab (PAPPLY (f, args)) = PAPPLY (elab f, map elab args)
        | elab (PDOT (p, x)) = PDOT (elab p, x)
        | elab (PNAME (loc, m)) =
            let fun bad aThing =
                  raise TypeError ("I was expecting " ^ m ^ " to refer to " ^
                                   "a module, but at " ^ srclocString loc ^
                                   ", it's " ^ aThing)
            in  case find (m, Gamma)
                  of ENVMODTY _ => bad "a module type"
                   | ENVMOD (mt, p) => p
                   | c => bad (whatdec c)
            end
  in  elab px
  end
(* Elaboration of syntax into types             *)
(*                                              *)
(* Paths, types, declarations, and module types all have *)
(* two forms: syntactic and internal. Each syntactic *)
(* form is translated into the corresponding internal *)
(* form by an elaboration function.             *)
(* <boxed values 275>=                          *)
val _ = op elabpath : pathex * binding env -> path
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <elaboration of {\mcl} type syntax into types>= *)
fun elabty (t, Gamma) =
  let fun elab (TYNAME px) =
            (case pathfind (px, Gamma)
               of ENVMANTY tau => tau
                | dec => raise TypeError ("I was expecting a type, but " ^
                                           pathexString px ^ " is " ^ whatdec
                                                                           dec))
        | elab (FUNTY (args, res)) = FUNTY (map elab args, elab res)
        | elab ANYTYPE = ANYTYPE
  in  elab t
  end
(* \qbreak                                      *)
(* <boxed values 276>=                          *)
val _ = op elabty : tyex * binding env -> ty
(* <elaboration of {\mcl} type syntax into types>= *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of ENVMODTY mt => mt
     | dec => raise TypeError ("Tried to use " ^ whatdec dec ^ " " ^ x ^
                                " as a module type")
(* When a module type is elaborated, it's easiest to *)
(* separate out the code that looks up named module *)
(* types.                                       *)
(* <boxed values 277>=                          *)
val _ = op findModty : name * binding env -> modty
(* <elaboration of {\mcl} type syntax into types>= *)
fun elabmt ((mtx : modtyx, path), Gamma) =
  let fun elab (MTNAMEDX t) =
            mtsubstRoot (MODTYPLACEHOLDER t |--> path) (findModty (t, Gamma))
        | elab (MTEXPORTSX exports) =
            let val (this', _) = foldl (leftLocated export) ([], Gamma) exports
            in  MTEXPORTS (rev this')
            end
        | elab (MTALLOFX mts) =
            allofAt (map (located elab) mts, path)
        | elab (MTARROWX (args, body)) =
            let val resultName = PNAME (MODTYPLACEHOLDER "functor result")
                fun arrow ([], (loc, body), Gamma : binding env, idents') =
                      let val resultName = PAPPLY (path, reverse idents')
                      in
                      ([], atLoc loc elabmt ((body, resultName), Gamma))
                      end
                  | arrow (((mloc, m), (mtloc, mtx)) :: rest, body, Gamma,
                                                                      idents') =
                      let val modid = genmodident m
                          val modty = atLoc mtloc elabmt ((mtx, PNAME modid),
                                                                          Gamma)
                          val () =
                              (* \qbreak                                      *)

                             (* <if [[modty]] is generic, bleat about [[m]]>= *)
                                   case modty
                                     of MTARROW _ =>
                                       raise TypeError
                                               ("module parameter " ^ m ^
                                                 " is generic, and a generic " ^

                    "module may not take another generic module as a parameter")
                                      | _ => ()
                          val Gamma' = bind (m, ENVMOD (modty, PNAME modid),
                                                                          Gamma)
                             (* XXX check 1st arg to ENVMOD *) (*OMIT*)
                          val (rest', body') =
                            arrow (rest, body, Gamma', PNAME modid :: idents')
                      in  ((modid, modty) :: rest', body')
                      end
            in  MTARROW (arrow (args, body, Gamma, []))
            end
(* <elaboration of {\mcl} type syntax into types>= *)
      and export ((x, ctx : decl), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                               " in module type")
            else
              let val c = elabComp ((ctx, PDOT (path, x)), Gamma)
              in  ((x, c) :: theseDecls, bind (x, asBinding (c, path), Gamma))
              end
(* <boxed values 278>=                          *)
val _ = op elabmt : modtyx rooted * binding env -> modty
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
(* \qbreak                                      *)
(* <boxed values 278>=                          *)
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
  in  elab mtx
  end
(* <elaboration of {\mcl} type syntax into types>= *)
and elabComp ((comp : decl, path), Gamma : binding env) : component =
  let fun ty t = elabty (t, Gamma)
(* A declaration is elaborated into a component. *)
(* <boxed values 279>=                          *)
val _ = op elabComp : decl rooted * binding env -> component
  in  case comp
        of DECVAL tau  => COMPVAL (ty tau)
         | DECABSTY    => COMPABSTY path
         | DECMANTY t  => COMPMANTY (ty t)
         | DECMOD mt   => COMPMOD (elabmt ((mt, path), Gamma))
                              (* XXX is path really OK here??? *) (*OMIT*)
         | DECMODTY mt =>
             raise TypeError ("module type " ^ pathString path ^
                              " may not be a component of another module")
  end
(* I redefine [[elabmt]] to check for bugs in my type *)
(* checker.                                     *)
(* <elaboration of {\mcl} type syntax into types>= *)
val elabmt = fn a =>
  let val mt = elabmt a
  in  if mixedManifestations mt then
        raise BugInTypeChecking
                ("invariant violation (mixed M): " ^ mtString mt)
      else
        mt
  end
(* Types exported by the primitive modules      *)
(*                                              *)
(* When typechecking literal expressions, the   *)
(* interpreter needs access to predefined types like *)
(* [[int]] and [[sym]] (which are synonyms for [[Int.t]] *)
(* and [[Sym.t]] respectively). To keep things simple, *)
(* all those types are defined here, and likewise the *)
(* module identifiers for the primitive modules. *)
(* <primitive module identifiers and types used to type literal expressions>= *)
val arraymodname = "Array"

val intmodident    = genmodident "Int"
val symmodident    = genmodident "Sym"
val boolmodident   = genmodident "Bool"
val unitmodident   = genmodident "Unit"
val arraymodident  = genmodident arraymodname
val uarraymodident = genmodident "UnsafeArray"

val inttype  = TYNAME (PDOT (PNAME intmodident, "t"))
val symtype  = TYNAME (PDOT (PNAME symmodident, "t"))
val booltype = TYNAME (PDOT (PNAME boolmodident, "t"))
val unittype = TYNAME (PDOT (PNAME unitmodident, "t"))
(* An array type is made using a path that instantiates *)
(* the [[Array]] module.                        *)
(* <primitive module identifiers and types used to type literal expressions>= *)
fun arraytype tau =
  case tau
    of TYNAME (PDOT (module, "t")) =>
         TYNAME (PDOT (PAPPLY (PNAME arraymodident, [module]), "t"))
     | _ => raise InternalError "unable to form internal array type"

(* \qvfilbreak3in                               *)
(*                                              *)
(* Type checking for expressions, with overloading *)
(*                                              *)
(* Type checking for expressions is much like type *)
(* checking in Typed Impcore or Typed uScheme, except *)
(* that \mcl supports operator overloading. It works *)
(* like this:                                   *)
(*                                              *)
(*   • Only functions can be overloaded, and the *)
(*  overloading is resolved by consulting the type of *)
(*  the first argument.                         *)
(*   • An overloaded name is associated with a sequence *)
(*  of values: one for each type at which the name is *)
(*  overloaded.                                 *)
(*   • At run time, the sequence is represented by an *)
(*  array of values.                            *)
(*   • At compile time, the sequence is represented by a *)
(*  list of types.                              *)
(*   • Adding an overloading means consing on to the *)
(*  front of the sequence.                      *)
(*   • Using an overloaded name requires an index into *)
(*  the sequence. The first matching type wins. *)
(*   • An overloaded name can be used only in a function *)
(*  application. At every application, therefore, the *)
(*  type checker writes the sequence index into the *)
(*  AST node.                                   *)
(*                                              *)
(* An attempt to overload a name starts with a check for *)
(* the type of the first argument.              *)
(* <utility functions on {\mcl} types>=         *)
fun firstArgType (x, FUNTY (tau :: _, _)) = OK tau
  | firstArgType (x, FUNTY ([], _)) =
      ERROR ("function " ^ x ^ " cannot be overloaded because " ^
             "it does not take any arguments")
  | firstArgType (x, _) =
      ERROR (x ^ " cannot be overloaded because it is not a function")

(* <utility functions on {\mcl} types>=         *)
fun okOrTypeError (OK a) = a
  | okOrTypeError (ERROR msg) = raise TypeError msg

fun ok a = okOrTypeError a
           handle _ => raise InternalError "overloaded non-function?"
(* <utility functions on {\mcl} types>=         *)
fun resolveOverloaded (f, argty, tys) =
  let fun findAt (tau :: taus, i) =
            if eqType (argty, ok (firstArgType (f, tau))) then
              OK (tau, i)
            else
              findAt (taus, i + 1)
        | findAt ([], _) =
            ERROR ("cannot figure out how to resolve overloaded name " ^
                   f ^ " when applied to first argument of type " ^
                   typeString argty ^ " (resolvable: " ^
                   commaSep (map typeString tys) ^ ")")
  in  findAt (tys, 0)
  end
(* Function [[okOrTypeError]] converts an [[ERROR]] *)
(* value to a [[TypeError]] exception. And [[ok]] *)
(* asserts that there should be no [[ERROR]] value. *)
(* <boxed values 252>=                          *)
val _ = op okOrTypeError : 'a error -> 'a
val _ = op ok            : 'a error -> 'a
(* \qbreak To resolve overloaded name [[f]], internal *)
(* function [[findAt]] is given a list of the types at *)
(* which name [[f]] is overloaded. It returns the type *)
(* and index of the overloading in which the first *)
(* argument is [[argty]]. (Because only functions of at *)
(* least one argument can be overloaded, the argument *)
(* type is extracted from [[firstArgType]] using  *)
(* [[ok]].)                                     *)
(* <boxed values 252>=                          *)
val _ = op resolveOverloaded : name * ty * ty list -> (ty * int) error
(* <[[typeof]] a {\mcl} expression ((prototype))>= *)
fun typeof (e, Gamma) =
  let fun ty e = typeof (e, Gamma)  (* replace with your code *)
      (* <definitions of internal functions for [[typeof]]>= *)
      fun typeofFunction (f, []) = (typeof (f, Gamma), notOverloadedIndex)
        | typeofFunction (f, first::_) =
            let fun resolve (EXP_AT (loc, f)) = atLoc loc resolve f
                  | resolve (e as VAR (PNAME (_, f))) =
                      (case find (f, Gamma)
                         of ENVOVLN taus =>
                                 okOrTypeError (resolveOverloaded (f, first,
                                                                          taus))
                          | _ => (typeof (e, Gamma), notOverloadedIndex))
                  | resolve e = (typeof (e, Gamma), notOverloadedIndex)
            in  resolve f
            end
      (* <definitions of internal functions for [[typeof]]>= *)
      fun typeOfApply f actuals index =
        let val atys = map ty actuals
            (* Error checking is hell.                      *)
            (* <definitions of [[maybeNamed]] and [[diagnoseArgs]]>= *)
            val got = (* what we actually got in the way of argument types *)
              case atys
                of [tau] => "argument of type " ^ typeString tau
                 | []    => "no arguments"
                 | _     => "arguments of types " ^ spaceSep (map typeString
                                                                           atys)
            val actuals_are =
              case actuals of [_] => "argument"
                            | _ => "arguments"

            fun ordinal 1 = "first"
              | ordinal 2 = "second"
              | ordinal 3 = "third"
              | ordinal n = intString n ^ "th"

            fun maybeNamed whattype =
              case stripExpAt f of VAR _ => whattype ^ " " ^ expString f
                                 | _ => whattype

            fun diagnoseArgs (formals, actuals) =
              let fun go (_, [], []) = raise InternalError
                                                          "can't find arg fault"
                    | go (k, f::fs, a::a's) =
                        if eqType (f, a) then
                          go (k + 1, fs, a's)
                        else
                          maybeNamed "function" ^ " expects " ^ ordinal k ^
                          " argument of type " ^ typeString f ^ ", but got " ^
                                                                    typeString a
                    | go _ =
                          maybeNamed "function" ^ " expects " ^
                          countString formals "argument" ^ " but got " ^
                          intString (length actuals)
              in  go (1, formals, actuals)
              end

      (* The [[typeof]] function is very much like the one for *)
      (* Typed uScheme, which is a homework exercise. To avoid *)
      (* spoiling that exercise, this appendix includes only *)
      (* the code for typechecking function applications. *)
      (* <boxed values 253>=                          *)
      val _ = op typeof : exp * binding env -> ty
      val _ = op ty     : exp                    -> ty
      (* Function [[typeofFunction]] resolves any overloading. *)
      (* It takes the expression being applied, plus the types *)
      (* of the actual parameters. It returns the type of the *)
      (* expression, plus an index into the overloading table *)
      (* for this [[APPLY]] node. If the function is not *)
      (* overloaded, the index that comes back is     *)
      (* [[notOverloadedIndex]].                      *)
      (* <boxed values 253>=                          *)
      val _ = op typeofFunction : exp * ty list -> ty * int
      (* \qbreak The code for [[APPLY]] checks the argument *)
      (* types as expected. The only oddity is that it also *)
      (* has the side effect of updating the [[index]] field *)
      (* in the abstract-syntax tree. When an overloaded *)
      (* function is applied, that index is used to find its *)
      (* value at this call site.                     *)
      (*                                              *)
      (* Function [[maybeNamed]] adds [[f]]'s name to a *)
      (* string, provided [[f]] has a name. And if things go *)
      (* wrong, [[diagnoseArgs]] compares the types of the *)
      (* formals and the actuals, producing a type-error *)
      (* message.                                     *)
      (* <boxed values 253>=                          *)
      val _ = op maybeNamed : string -> string
      val _ = op diagnoseArgs : ty list * ty list -> string
        in  case typeofFunction (f, atys)
              of (FUNTY (formals, result), i) =>
                   if eqTypes (atys, formals) then
                       result before index := i
                   else
                       raise TypeError (diagnoseArgs (formals, atys))
               | _ => raise TypeError ("tried to apply " ^ maybeNamed
                                                                "non-function" ^
                                       " of type " ^ typeString (ty f))
        end
  in  raise LeftAsExercise "typeof"  (* call ty e *)
  end
(* <principal type of a module>=                *)
fun strengthen (MTEXPORTS comps, p) =
      let fun comp (c as (x, dc)) =
            case dc
              of COMPABSTY _ => (x, COMPMANTY (TYNAME (PDOT (p, x))))
               | COMPMOD mt  => (x, COMPMOD (strengthen (mt, PDOT (p, x))))
               | COMPVAL   _ => c
               | COMPMANTY _ => c
      in  MTEXPORTS (map comp comps)
      end
  | strengthen (MTALLOF mts, p) =
      allofAt (map (fn mt => strengthen (mt, p)) mts, p)
  | strengthen (mt as MTARROW _, p) =
      mt
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* {mathpar} \textstyleE \vdash\apath: \amodtype *)
(*                                              *)
(* \inferrule\envhas\apath:\rootat\amodtype'\apath' E \ *)
(* vdash\apath: \strengthen\amodtype'\apath' {mathpar} *)
(*                                              *)
(* {align*} \str(\amodtype_1 \land\amodtype_2) --- = \ *)
(* str\amodtype_1 \land\str\amodtype_2 --- \str(t = tau) *)
(* --- =(t = tau)                               *)
(* \str\exports(\decls) --- =\exports(\str\decls) --- \ *)
(* str(t :: \rootat\ktype\apath') --- = (t = \apath.t) *)
(* \str\emptylist --- =\emptylist --- \str(x : tau) --- *)
(* =x : tau                                     *)
(* \str(\adecl,\decls) --- = \str\adecl,\str\decls --- \ *)
(* str(M : \amodtype) --- = x : \strengthen\amodtype\ *)
(* apath.M {align*}                             *)
(*                                              *)
(* Strengthening module types and components [*] *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Typechecking modules: strengthening          *)
(*                                              *)
(* Strengthening a module type converts every abstract *)
(* type into a manifest type, which is manifestly equal *)
(* to itself. For ``itself'' to mean anything, we have *)
(* to know the path associated with the module  *)
(* type—that is, the module type has to be rooted. *)
(* <boxed values 254>=                          *)
val _ = op strengthen : modty rooted -> modty
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <elaboration and evaluation of [[data]] definitions for \mcl>= *)
fun typeDataDef ((T, vcons), context, Gamma) =
  let val tau    = TYNAME (contextDot (context, T))
      val Gamma' = bind (T, ENVMANTY tau, Gamma)
      fun translateVcon (K, tx) =
            (K, elabty (tx, Gamma'))
            handle TypeError msg =>
              raise TypeError ("in type of value constructor " ^
                               K ^ ", " ^ msg)
      val Ktaus = map translateVcon vcons
        
      fun validate (K, FUNTY (_, result)) =
            if eqType (result, tau) then
              ()
            else 
              (* <result type of [[K]] should be [[tau]] but is [[result]]>= *)
              raise TypeError ("value constructor " ^ K ^ " should return " ^
                                                                typeString tau ^
                               ", but it returns type " ^ typeString result)
        | validate (K, tau') =
            if eqType (tau', tau) then
              ()
            else 
              (* <type of [[K]] should be [[tau]] but is [[tau']]>= *)
              raise TypeError ("value constructor " ^ K ^ " should have " ^
                                                                typeString tau ^
                              ", but it has type " ^ typeString tau')
      val () = app validate Ktaus
  in  (T, ENVMANTY tau) :: map (fn (K, tau) => (K, ENVVAL tau)) Ktaus
      (* thin ice here: the type component should be abstract? *)  (*OMIT*)
  end
(* \qvfilbreak1.5in                             *)
(*                                              *)
(* Typechecking datatype definitions            *)
(*                                              *)
(* A [[data]] definition is checked more or less as in *)
(* uML (\crefadt.chap). \nwnarrowboxes          *)
(* <boxed values 261>=                          *)
val _ = op typeDataDef : data_def * context * binding env -> (name * binding)
                                                                            list
(* <elaboration and evaluation of [[data]] definitions for \mcl>= *)
fun evalDataDef ((_, typed_vcons), rho) =
  let fun addVcon ((K, t), rho) =
        let val v = if isfuntype t then
                      PRIMITIVE (fn vs => CONVAL (PNAME K, map ref vs))
                    else
                      CONVAL (PNAME K, [])
        in  bind (K, ref v, rho)
        end
(* As in \crefadt.chap, a [[data]] definition is *)
(* evaluated separately from other forms. Function *)
(* [[evalDataDef]] returns not the values of the new *)
(* value constructors, but their names. \nwnarrowboxes *)
(* <boxed values 285>=                          *)
val _ = op evalDataDef : data_def * value ref env -> value ref env * string list
  in  (foldl addVcon rho typed_vcons, map fst typed_vcons)
  end
(* <elaborate a {\mcl} definition>=             *)
type value_printer = 
  interactivity -> (name -> ty -> value -> unit) -> value list -> unit
fun printStrings ss _ _ vs = 
  app print ss
(* \qbreak                                      *)
(*                                              *)
(* Responding to definitions                    *)
(*                                              *)
(* The interpreter issue a response to each top-level *)
(* definition. A response has type [[value_printer]], *)
(* and it may be formed from a list of strings. *)
(* <boxed values 255>=                          *)
type value_printer = value_printer
val _ = op printStrings : string list -> value_printer
(* <elaborate a {\mcl} definition>=             *)
val ppwidth =
  getOpt(Option.mapPartial Int.fromString (OS.Process.getEnv "COLS"), 77)

fun printDoc doc interactivity _ _ =
  let val margin = if prompts interactivity then 2 else 0
  in  print (layout ppwidth (indent (margin, agrp doc)))
  end
(* A response may also be formed from a [[doc]], which *)
(* is a type that contains instructions for     *)
(* prettyprinting (automated indentation and such), as *)
(* described on \cpagerefasdlml.prettyprinter (\cref *)
(* asdlml.chap). Prettyprinting is done 77 columns wide, *)
(* unless environment variable [[COLS]] says otherwise. *)
(* <boxed values 256>=                          *)
val _ = op printDoc : doc -> value_printer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <elaborate a {\mcl} definition>=             *)
fun defName (VAL (x, _)) = x
  | defName (VALREC (x, _, _)) = x
  | defName (EXP _) = "it"
  | defName (QNAME _) = raise InternalError "defName QNAME"
  | defName (DEFINE (x, _, _)) = x
  | defName (TYPE (t, _)) = t
  | defName (DATA (t, _)) = raise InternalError "defName DATA"
  | defName (OVERLOAD _) = raise InternalError "defName OVERLOAD"
  | defName (MODULE (m, _)) = m
  | defName (GMODULE (m, _, _)) = m
  | defName (MODULETYPE (t, _)) = t
(* <elaborate a {\mcl} definition>=             *)
fun printMt what m how mt =
  printDoc (doc (concat [what, " ", m, " ", how]) ^/+ mtDoc mt)

fun defResponse (x, c) =
  case c
    of ENVVAL tau =>
         (fn _ => fn printfun =>
             fn [v] => (printfun x tau v; app print [" : ", typeString tau])
                       (* can't do better b/c printfun only prints *)
              | _ => raise InternalError "value count for val definition")
     | ENVMANTY tau =>
         let val expansion = typeString tau
         in  if x = expansion then
               printStrings ["abstract type ", x]
             else
               printDoc (doc "type" ^^ doc x ^^ doc "=" ^/+ typeDoc tau)
         end
     | ENVMOD (mt as MTARROW _, _) => printMt "generic module" x ":" mt
     | ENVMOD (mt, _)              => printMt "module" x ":" mt
     | ENVMODTY mt                 => printMt "module type" x "=" mt
     | ENVOVLN _ => raise InternalError "defResponse to overloaded name"
(* The response to a definition includes the name of the *)
(* thing defined, which is computed by [[defName]]. *)
(* <boxed values 257>=                          *)
val _ = op defName : baredef -> string
(* Once name and binding are known, a response is *)
(* computed by [[defReponse]].                  *)
(* <boxed values 257>=                          *)
val _ = op defResponse : name * binding -> value_printer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <elaborate a {\mcl} definition>=             *)
fun defPrinter (d, Gamma) =
      let val x = defName d
      in  defResponse (x, find (x, Gamma))
          handle NotFound _ => raise InternalError "defName not found"
      end
(* \qbreak And [[defPrinter]] composes [[defName]] and *)
(* [[defResponse]].                             *)
(* <boxed values 258>=                          *)
val _ = op defPrinter : baredef * binding env -> value_printer
(* <elaborate a {\mcl} definition>=             *)
fun typdef (d : baredef, context, Gamma) =
  let fun toplevel what =
        case context
          of TOPLEVEL => id
           | _ => raise TypeError (what ^ " cannot appear " ^
                                   contextString context)
      (* <definition of [[mtypeof]]>=                 *)
      fun mtypeof ((m, path), Gamma) =
        let fun ty (MPATH p) =
                     strengthen (findModule (p, Gamma), elabpath (p, Gamma))
              | ty (MPATHSEALED (mtx, p)) = sealed (mtx, ty (MPATH p))
              | ty (MUNSEALED defs)       = principal defs
              | ty (MSEALED (mtx, defs))  = sealed (mtx, principal defs)
            and sealed (sealingMtx, sealedMt) =
                  let val sealingMt = elabmt ((sealingMtx, path), Gamma)
                  in  case implements (path, sealedMt, sealingMt)
                        of OK () => sealingMt
                         | ERROR msg => raise TypeError msg
                  end
            and principal ds = MTEXPORTS (elabdefs (ds, INMODULE path, Gamma))
            and elabdefs ([],    c, Gamma) = []
              | elabdefs ((loc, d) :: ds, c, Gamma) =
                  let val bindings = atLoc loc typdef (d, c, Gamma)
                      val comps'   = List.mapPartial asComponent bindings
                      val Gamma'   = Gamma <+> bindings
                      val comps''  = elabdefs (ds, c, Gamma')
                      (* <definition of [[asUnique]]>=                *)
                      fun asUnique following (x, c) =
                        let val c' = find (x, following)
                        in  case (c, c')
                              of (COMPVAL _, COMPVAL _) => NONE
                                                    (* repeated values are OK *)
                               | _ => raise TypeError ("Redefinition of " ^
                                                          whatcomp c ^ " " ^ x ^
                                                       " in module " ^
                                                                pathString path)
                        end handle NotFound _ => SOME (x, c)        

                     (* Function [[asUnique]] ensures that no name is defined *)

                   (* more than once—except for values. Redefining types or *)
                      (* modules would not be sound, but redefining values *)
                      (* is OK. \nwnarrowboxes                        *)
                      (* <boxed values 265>=                          *)
                      val _ = op asUnique : component env -> name * component ->
                                                       (name * component) option
                  in  List.mapPartial (asUnique comps'') comps' @ comps''
                  end
      (* \qvfilbreak3.5in                             *)
      (*                                              *)
      (* Typechecking module definitions              *)
      (*                                              *)
      (* Each form of module definition is checked in its own *)
      (* way. \nwnarrowboxes                          *)
      (* <boxed values 264>=                          *)
      val _ = op mtypeof   : moddef rooted * binding env -> modty
      val _ = op sealed    : modtyex * modty -> modty
      val _ = op principal : def list -> modty
      val _ = op elabdefs  : def list * context * binding env -> (name *
                                                                 component) list
        in  ty m
        end
  in  case d
        of EXP e =>
             let val what = "an expression (like " ^ expString e ^ ")"
             in  toplevel what (typdef (VAL ("it", e), context, Gamma))
             end
         | QNAME px => 
             let val what = "a qualified name (like " ^ pathexString px ^ ")"
             in  toplevel what (typdef (EXP (VAR px), context, Gamma))
             end
         (* \qbreak A module type is elaborated into internal *)
         (* form. Module types may appear only at top level. *)
         (* <named bindings for other forms of definition>= *)
         | MODULETYPE (T, mtx) =>
             let val mt = elabmt ((mtx, PNAME (MODTYPLACEHOLDER T)), Gamma)
             in  toplevel ("a module type (like " ^ T ^ ")")
                 [(T, ENVMODTY mt)]
             end
         (* A newly defined module is rooted at a location *)
         (* depending on its context, then typechecked with *)
         (* [[mtypeof]].                                 *)
         (* <named bindings for other forms of definition>= *)
         | MODULE (name, mx) =>
             let val root = contextDot (context, name)
                 val mt   = mtypeof ((mx, root), Gamma)
             in  [(name, ENVMOD (mt, root))]
             end
         (* When a generic-module definition is typechecked, each *)
         (* formal parameter adds to the environment in which *)
         (* subsequent formal parameters (and the body) are *)
         (* typechecked. A generic module may be defined only at *)
         (* top level.                                   *)
         (* <named bindings for other forms of definition>= *)
         | GMODULE (f, formals, body) =>
             let val () = toplevel ("a generic module (like " ^ f ^ ")") ()
                 val fpath     = contextDot (context, f)
                 val idformals = map (fn (x, mtx) => (genmodident x, (x, mtx)))
                                                                         formals
                 val resultpath = PAPPLY (fpath, map (PNAME o fst) idformals)

                 fun addarg arg (args, res) = (arg :: args, res)

                 fun arrowtype ((mid : modident, (x, mtx)) :: rest, Gamma) =
                       let val mt = elabmt ((mtx, PNAME mid), Gamma)
                           val Gamma' = bind (x, ENVMOD (mt, PNAME mid), Gamma)
                       in  addarg (mid, mt) (arrowtype (rest, Gamma'))
                       end
                   | arrowtype ([], Gamma) = ([], mtypeof ((body, resultpath),
                                                                         Gamma))
                 val mt = MTARROW (arrowtype (idformals, Gamma))
             in  [(f, ENVMOD (mt, fpath))]
             end       
         (* As usual, [[define]] is syntactic sugar for a *)
         (* combination of [[val-rec]] and [[lambda]].   *)
         (* <named bindings for other forms of definition>= *)
         | DEFINE (name, tau, lambda as (formals, body)) =>
             let val funty = FUNTY (map (fn (n, ty) => ty) formals, tau)
             in  typdef (VALREC (name, funty, LAMBDA lambda), context, Gamma)
             end
         (* The [[val]] and [[val-rec]] forms are typechecked as *)
         (* in Typed uScheme.                            *)
         (* <named bindings for other forms of definition>= *)
         | VAL (x, e) =>
             let val tau = typeof (e, Gamma)
             in  [(x, ENVVAL tau)]
             end
         (* \qbreak                                      *)
         (* <named bindings for other forms of definition>= *)
         | VALREC (f, tau, e as LAMBDA _) =>
             let val tau    = elabty (tau, Gamma)
                 val Gamma' = bind (f, ENVVAL tau, Gamma)
                 val tau'   = typeof (e, Gamma')
             in  if not (eqType (tau, tau')) then
                   raise TypeError ("identifier " ^ f ^
                                    " is declared to have type " ^
                                    typeString tau ^ " but has actual type " ^
                                    typeString tau')
                 else
                   [(f, ENVVAL tau)]
             end
         | VALREC (name, tau, _) =>
             raise TypeError ("(val-rec [" ^ name ^ " : " ^ tyexString tau ^
                              "] ...) must use (lambda ...) on right-hand side")
         (* A manifest type definition is added to the   *)
         (* environment.                                 *)
         (* <named bindings for other forms of definition>= *)
         | TYPE (t, tx) =>
             let val tau = elabty (tx, Gamma)
             in  [(t, ENVMANTY tau)]
             end
         (* Each of the remaining forms (algebraic data types, *)
         (* overloading) is typechecked in its own function. *)
         (* <named bindings for other forms of definition>= *)
         | DATA dd => typeDataDef (dd, context, Gamma)
         | OVERLOAD ovl => (* <return bindings from overload list [[ovl]]>= *)
                           let fun overloadBinding (p, Gamma) = 
                                 let val (tau, first) =
                                       case pathfind (p, Gamma)
                                         of ENVVAL tau =>
                                              (tau, okOrTypeError (firstArgType
                                                         (pathexString p, tau)))
                                          | c =>
                              (* <can't overload a [[c]]>=                    *)
                                                 raise TypeError (
                          "only functions can be overloaded, but " ^ whatdec c ^
                                                                  " " ^
                                          pathexString p ^ " is not a function")
                                     val x = plast p

                                     val currentTypes =
                                       (case find (x, Gamma)
                                          of ENVOVLN vals => vals
                                           | _ => []) handle NotFound _ => []
                                 in  (x, ENVOVLN (tau :: currentTypes))
                                 end
                           (* Computing overload bindings                  *)
                           (*                                              *)

                     (* Any path bound to a value that has a [[firstArgType]] *)

                       (* can be overloaded. Its (short) name [[x]] and type  *)

                     (* [[tau]] are added to the list of types at which name  *)

                     (* [[x]] is overloaded (the [[currentTypes]]). (If [[x]] *)

                          (* is not overloaded yet, start with a fresh, empty *)
                           (* list.)                                       *)
                           (* <boxed values 262>=                          *)
                           val _ = op overloadBinding : pathex * binding env ->
                                                                  name * binding
                           (* <return bindings from overload list [[ovl]]>= *)
                               fun overloadBindings (ps, Gamma) =
                                 let fun add (bs', Gamma, []) = bs'
                                       | add (bs', Gamma, p :: ps) =
                                           let val b = overloadBinding (p, Gamma
                                                                               )
                                           in  add (b :: bs', Gamma <+> [b], ps)
                                           end
                                 in  add ([], Gamma, ps)
                                 end

                     (* A sequence of paths is overloaded one at a time. Each *)

                          (* addition extends the environment. \nwnarrowboxes *)
                           (* <boxed values 263>=                          *)
                           val _ = op overloadBindings : pathex list * binding
                                                    env -> (name * binding) list
                           in  overloadBindings (ovl, Gamma)
                           end
  end
(* Each definition appears in a known context and is *)
(* typechecked using a given environment. (Some *)
(* definitions, like a [[module-type]] definition for *)
(* example, may appear only in a top-level context.) *)
(* Typechecking a definition produces a named   *)
(* [[binding]]. is added to the environment by  *)
(* [[typdef]].                                  *)
(* <boxed values 260>=                          *)
val _ = op typdef : baredef * context * binding env -> (name * binding) list


(*****************************************************************)
(*                                                               *)
(*   SUBSTITUTIONS FOR \MCL                                      *)
(*                                                               *)
(*****************************************************************)

(* <substitutions for \mcl>=                    *)
type rootsubst = (modident * path) list
val idsubst = []
(* Substitutions (boring)                       *)
(*                                              *)
(* Substitutions are used to propagate information about *)
(* manifest types. Everything you need to know about *)
(* substitution appears in \creftypesys.chap.   *)
(* <boxed values 266>=                          *)
type rootsubst = rootsubst
val _ = op idsubst : rootsubst
(* <substitutions for \mcl>=                    *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* <boxed values 267>=                          *)
val _ = op |--> : modident * path -> rootsubst
(* <substitutions for \mcl>=                    *)
type tysubst = 
  (path * ty) list
fun associatedWith (x, []) =
      NONE
  | associatedWith (x, (key, value) :: pairs) =
      if x = key then SOME value else associatedWith (x, pairs)

fun hasKey [] x = false
  | hasKey ((key, value) :: pairs) x = x = key orelse hasKey pairs x
(* <boxed values 268>=                          *)
type tysubst = tysubst
val _ = op associatedWith : path * tysubst -> ty option
val _ = op hasKey : tysubst -> path -> bool
(* <substitutions for \mcl>=                    *)
fun pathsubstRoot theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* <boxed values 269>=                          *)
val _ = op pathsubstRoot : rootsubst -> path -> path
(* <substitutions for \mcl>=                    *)
fun tysubstRoot theta (TYNAME p)          = TYNAME (pathsubstRoot theta p)
  | tysubstRoot theta (FUNTY (args, res)) =
      FUNTY (map (tysubstRoot theta) args, tysubstRoot theta res)
  | tysubstRoot theta ANYTYPE = ANYTYPE
(* <boxed values 270>=                          *)
val _ = op tysubstRoot : rootsubst -> ty -> ty
(* <substitutions for \mcl>=                    *)
fun dom theta = 
  map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubstRoot theta2 o pathsubstRoot theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* Functions [[dom]] and [[compose]] may be familiar *)
(* from \crefml.chap.                           *)
(* <boxed values 271>=                          *)
val _ = op dom     : rootsubst -> modident set
val _ = op compose : rootsubst * rootsubst -> rootsubst
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \mcl>=                    *)
fun bsubstRoot s = 
  map (fn (x, a) => (x, s a))

fun mtsubstRoot theta =
  let fun s (MTEXPORTS comps) = MTEXPORTS (bsubstRoot (compsubstRoot theta)
                                                                          comps)
        | s (MTALLOF mts)     = MTALLOF (map s mts)
        | s (MTARROW (args, res)) = MTARROW (bsubstRoot s args, s res)
  in  s
  end
and compsubstRoot theta =
  let fun s (COMPVAL t) = COMPVAL (tysubstRoot theta t)
        | s (COMPABSTY path) = COMPABSTY (pathsubstRoot theta path)
        | s (COMPMANTY t)  = COMPMANTY (tysubstRoot theta t)
        | s (COMPMOD mt)  = COMPMOD (mtsubstRoot theta mt)
  in  s
  end
(* <boxed values 272>=                          *)
val _ = op mtsubstRoot   : rootsubst -> modty      -> modty
val _ = op compsubstRoot : rootsubst -> component -> component
(* <substitutions for \mcl>=                    *)
fun tysubstManifest mantypes =
  let fun r (TYNAME path) = getOpt (associatedWith (path, mantypes), TYNAME path
                                                                               )
        | r (FUNTY (args, res)) = FUNTY (map r args, r res)
        | r (ANYTYPE) = ANYTYPE
  in  r
  end
(* <boxed values 273>=                          *)
val _ = op tysubstManifest : tysubst -> ty -> ty
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \mcl>=                    *)
fun mtsubstManifest mantypes mt =
  let val newty = tysubstManifest mantypes
      fun newmt (MTEXPORTS cs) =
            MTEXPORTS (map (fn (x, c) => (x, newcomp c)) cs)
        | newmt (MTALLOF mts) =
            MTALLOF (map newmt mts)  (* can't violate unmix invariant *)
        | newmt (MTARROW (args, result)) =
            MTARROW (map (fn (x, mt) => (x, newmt mt)) args, newmt result)
      and newcomp (COMPVAL tau) = COMPVAL (newty tau)
        | newcomp (COMPABSTY p) =
           (case associatedWith (p, mantypes)
              of SOME tau => COMPMANTY tau
               | NONE => COMPABSTY p)   (* used to be this on every path *)
        | newcomp (COMPMANTY tau) = COMPMANTY (newty tau)
        | newcomp (COMPMOD mt) = COMPMOD (newmt mt)
  in  newmt mt
  end
(* <boxed values 274>=                          *)
val _ = op mtsubstManifest : tysubst -> modty -> modty


(*****************************************************************)
(*                                                               *)
(*   ELABORATION OF {\MCL} TYPE SYNTAX INTO TYPES                *)
(*                                                               *)
(*****************************************************************)

(* <elaboration of {\mcl} type syntax into types>= *)
fun elabpath (px, Gamma) =
  let fun elab (PAPPLY (f, args)) = PAPPLY (elab f, map elab args)
        | elab (PDOT (p, x)) = PDOT (elab p, x)
        | elab (PNAME (loc, m)) =
            let fun bad aThing =
                  raise TypeError ("I was expecting " ^ m ^ " to refer to " ^
                                   "a module, but at " ^ srclocString loc ^
                                   ", it's " ^ aThing)
            in  case find (m, Gamma)
                  of ENVMODTY _ => bad "a module type"
                   | ENVMOD (mt, p) => p
                   | c => bad (whatdec c)
            end
  in  elab px
  end
(* Elaboration of syntax into types             *)
(*                                              *)
(* Paths, types, declarations, and module types all have *)
(* two forms: syntactic and internal. Each syntactic *)
(* form is translated into the corresponding internal *)
(* form by an elaboration function.             *)
(* <boxed values 275>=                          *)
val _ = op elabpath : pathex * binding env -> path
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <elaboration of {\mcl} type syntax into types>= *)
fun elabty (t, Gamma) =
  let fun elab (TYNAME px) =
            (case pathfind (px, Gamma)
               of ENVMANTY tau => tau
                | dec => raise TypeError ("I was expecting a type, but " ^
                                           pathexString px ^ " is " ^ whatdec
                                                                           dec))
        | elab (FUNTY (args, res)) = FUNTY (map elab args, elab res)
        | elab ANYTYPE = ANYTYPE
  in  elab t
  end
(* \qbreak                                      *)
(* <boxed values 276>=                          *)
val _ = op elabty : tyex * binding env -> ty
(* <elaboration of {\mcl} type syntax into types>= *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of ENVMODTY mt => mt
     | dec => raise TypeError ("Tried to use " ^ whatdec dec ^ " " ^ x ^
                                " as a module type")
(* When a module type is elaborated, it's easiest to *)
(* separate out the code that looks up named module *)
(* types.                                       *)
(* <boxed values 277>=                          *)
val _ = op findModty : name * binding env -> modty
(* <elaboration of {\mcl} type syntax into types>= *)
fun elabmt ((mtx : modtyx, path), Gamma) =
  let fun elab (MTNAMEDX t) =
            mtsubstRoot (MODTYPLACEHOLDER t |--> path) (findModty (t, Gamma))
        | elab (MTEXPORTSX exports) =
            let val (this', _) = foldl (leftLocated export) ([], Gamma) exports
            in  MTEXPORTS (rev this')
            end
        | elab (MTALLOFX mts) =
            allofAt (map (located elab) mts, path)
        | elab (MTARROWX (args, body)) =
            let val resultName = PNAME (MODTYPLACEHOLDER "functor result")
                fun arrow ([], (loc, body), Gamma : binding env, idents') =
                      let val resultName = PAPPLY (path, reverse idents')
                      in
                      ([], atLoc loc elabmt ((body, resultName), Gamma))
                      end
                  | arrow (((mloc, m), (mtloc, mtx)) :: rest, body, Gamma,
                                                                      idents') =
                      let val modid = genmodident m
                          val modty = atLoc mtloc elabmt ((mtx, PNAME modid),
                                                                          Gamma)
                          val () =
                              (* \qbreak                                      *)

                             (* <if [[modty]] is generic, bleat about [[m]]>= *)
                                   case modty
                                     of MTARROW _ =>
                                       raise TypeError
                                               ("module parameter " ^ m ^
                                                 " is generic, and a generic " ^

                    "module may not take another generic module as a parameter")
                                      | _ => ()
                          val Gamma' = bind (m, ENVMOD (modty, PNAME modid),
                                                                          Gamma)
                             (* XXX check 1st arg to ENVMOD *) (*OMIT*)
                          val (rest', body') =
                            arrow (rest, body, Gamma', PNAME modid :: idents')
                      in  ((modid, modty) :: rest', body')
                      end
            in  MTARROW (arrow (args, body, Gamma, []))
            end
(* <elaboration of {\mcl} type syntax into types>= *)
      and export ((x, ctx : decl), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                               " in module type")
            else
              let val c = elabComp ((ctx, PDOT (path, x)), Gamma)
              in  ((x, c) :: theseDecls, bind (x, asBinding (c, path), Gamma))
              end
(* <boxed values 278>=                          *)
val _ = op elabmt : modtyx rooted * binding env -> modty
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
(* \qbreak                                      *)
(* <boxed values 278>=                          *)
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
  in  elab mtx
  end
(* <elaboration of {\mcl} type syntax into types>= *)
and elabComp ((comp : decl, path), Gamma : binding env) : component =
  let fun ty t = elabty (t, Gamma)
(* A declaration is elaborated into a component. *)
(* <boxed values 279>=                          *)
val _ = op elabComp : decl rooted * binding env -> component
  in  case comp
        of DECVAL tau  => COMPVAL (ty tau)
         | DECABSTY    => COMPABSTY path
         | DECMANTY t  => COMPMANTY (ty t)
         | DECMOD mt   => COMPMOD (elabmt ((mt, path), Gamma))
                              (* XXX is path really OK here??? *) (*OMIT*)
         | DECMODTY mt =>
             raise TypeError ("module type " ^ pathString path ^
                              " may not be a component of another module")
  end
(* I redefine [[elabmt]] to check for bugs in my type *)
(* checker.                                     *)
(* <elaboration of {\mcl} type syntax into types>= *)
val elabmt = fn a =>
  let val mt = elabmt a
  in  if mixedManifestations mt then
        raise BugInTypeChecking
                ("invariant violation (mixed M): " ^ mtString mt)
      else
        mt
  end



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \MCL  *)
(*                                                               *)
(*****************************************************************)

(* Evaluation                                   *)
(*                                              *)
(* The components of the evaluator and read-eval-print *)
(* loop are organized as follows:               *)
(* <evaluation, testing, and the read-eval-print loop for \mcl>= *)
fun basename (PDOT (_, x)) = PNAME x
  | basename (PNAME x) = PNAME x
  | basename (instance as PAPPLY _) = instance
(* <definitions of [[matchRef]] and [[Doesn'tMatch]] ((elided))>= *)
exception Doesn'tMatch    (* pattern-match failure *)

fun matchRef (CONPAT (k, ps), CONVAL (k', vs)) =
     if basename k = k' then
       disjointUnion (ListPair.mapEq matchConval (ps, vs))
     else
       raise Doesn'tMatch
  | matchRef (CONPAT _, _) = raise Doesn'tMatch
  | matchRef (WILDCARD, _) = emptyEnv
  | matchRef (PVAR x,   v) = bind (x, ref v, emptyEnv)
and matchConval (PVAR x, vref) = bind (x, vref, emptyEnv)
  | matchConval (p, ref v) = matchRef (p, v)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
fun evalpath (p : pathex, rho) =
  let fun findpath (PNAME (srcloc, x)) = !(find (x, rho))
        | findpath (PDOT (p, x)) =
            (case findpath p
               of MODVAL comps => (!(find (x, comps))
                                   handle NotFound x =>
                                     raise BugInTypeChecking "missing component"
                                                                               )
                | _ => raise BugInTypeChecking "selection from non-module")
        | findpath (PAPPLY (f, args)) = apply (findpath f, map findpath args)
(* Overloading support                          *)
(*                                              *)
(* A qualified name is overloaded based on the last part *)
(* of the name.                                 *)
(* <boxed values 280>=                          *)
val _ = op plast : pathex -> name
(* The [[basename]] function defined here is used in \ *)
(* crefadt.chap to match a value constructor against a *)
(* value-constructor pattern. This computation makes it *)
(* possible to match two references to the same value *)
(* constructor even if the two references refer to *)
(* different modules. (The fact that such references *)
(* denote the same value constructor is guaranteed by *)
(* the type system.)                            *)
(*                                              *)
(* Evaluating paths                             *)
(*                                              *)
(* Since a path may include an instantiation of a *)
(* generic module, [[evalpath]] includes a function to *)
(* apply the generic module to its arguments.   *)
(* <boxed values 280>=                          *)
val _ = op evalpath : pathex * value ref env -> value
val _ = op apply    : value  * value list    -> value
  in  findpath p
  end
and apply (PRIMITIVE prim, vs) = prim vs
  | apply (CLOSURE ((formals, body), rho_c), vs) = 
      (eval (body, bindList (formals, map ref vs, rho_c))
       handle BindListLength => 
         raise BugInTypeChecking ("Wrong number of arguments to closure; " ^
                                  "expected (" ^ spaceSep formals ^ ")"))
  | apply _ = raise BugInTypeChecking "applied non-function"
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and eval (e, rho : value ref env) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let fun ev (LITERAL n) = n
        (* Code for variables is just as in Chapter [->]. *)
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (VAR p) = evalpath (p, rho)
        | ev (SET (n, e)) = 
            let val v = ev e
            in  find (n, rho) := v;
                unitVal
            end
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (VCONX c) = evalpath (pathexOfVcon c, rho)
        | ev (CASE (LITERAL v, (p, e) :: choices)) =
            (let val rho' = matchRef (p, v)
             in  eval (e, rho <+> rho')
             end
             handle Doesn'tMatch => ev (CASE (LITERAL v, choices)))
        | ev (CASE (LITERAL v, [])) =
            raise RuntimeError ("'case' does not match " ^ valueString v)
        | ev (CASE (e, choices)) =
            ev (CASE (LITERAL (ev e), choices))
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (IFX (e1, e2, e3)) = ev (if projectBool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if projectBool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              unitVal
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, unitVal)
            end
        (* Code for a [[lambda]] removes the types from the *)
        (* abstract syntax.                             *)
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (LAMBDA (args, body)) = CLOSURE ((map (fn (x, ty) => x) args, body)
                                                                          , rho)
        (* Each [[APPLY]] node contains a reference to an *)
        (* integer [[i]] that is set by the type checker. If  *)
        (* [[i]] is negative, then what's being applied is not *)
        (* an overloaded name, and the value being applied is *)
        (* computed by [[ev]] as usual. If [[i]] is nonnegative, *)
        (* then name [[f]] evaluates to an array of possible *)
        (* functions, from which the correct function is found *)
        (* at index [[i]]. Once the function to be applied *)
        (* ([[fv]]) has been computed, it is applied just as in *)
        (* \crefmlscheme.chap.                          *)
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (APPLY (f, args, ref i))  =
           let val fv =
                 if i < 0 then
                   ev f
                 else
                   case ev f
                     of ARRAY a =>
                          (Array.sub (a, i)
                           handle Subscript =>
                             raise BugInTypeChecking "overloaded index")
                      | _ => raise BugInTypeChecking
                                                  "overloaded name is not array"
           in  case fv
                 of PRIMITIVE prim => prim (map ev args)
                  | CLOSURE clo =>
                       (* The pattern \monoboxe as APPLY (f, args) matches an *)

                      (* [[APPLY]] node. On the right-hand side, [[e]] stands *)

                     (* for the entire node, and [[f]] and [[args]] stand for *)

                              (* the children.                                *)

                              (*                                              *)

                              (* A closure is applied by first creating fresh *)

                              (* locations to hold the values of the actual   *)

                        (* parameters. In \crefscheme.chap, the locations are *)

                        (* allocated by function [[allocate]]; here, they are *)

                     (* allocated by the built-in function [[ref]]. Calling \ *)

                     (* monoboxref v allocates a new location and initializes *)

                        (* it to v. The ML expression \monoboxmap ref actuals *)

                              (* does half the work of \crefscheme.chap's     *)

                              (* [[bindalloclist]]; the other half is done by *)

                              (* [[bindList]]. \mdbuseschemebindalloclist     *)

                         (* <apply closure [[clo]] to [[args]] ((mlscheme))>= *)
                                   let val ((formals, body), savedrho) = clo
                                       val actuals = map ev args
                                   in  eval (body, bindList (formals, map ref
                                                             actuals, savedrho))
                                       handle BindListLength => 
                                           raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                                               "expected (" ^
                                                         spaceSep formals ^ ")")
                                   end
                  | v => raise BugInTypeChecking "applied non-function"
           end
        (* Code for the [[LETX]] family is as in Chapter [->]. *)
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (LETRECX (bs, body)) =
            let val (lhss, values) = ListPair.unzip bs
                val names = map fst lhss
                fun unspecified () = NUM 42
                val rho' = bindList (names, map (fn _ => ref (unspecified()))
                                                                    values, rho)
                val updates = map (fn (x, e) => (x, eval (e, rho'))) bs
            in  List.app (fn ((x, _), v) => find (x, rho') := v) updates; 
                eval (body, rho')
            end
        (* \qtrim2 Evaluating a module expression produces a *)
        (* module value.                                *)
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (MODEXP components) =
            let fun step ((x, e), (results', rho)) =
                  let val loc = ref (eval (e, rho))
                  in  ((x, loc) :: results', bind (x, loc, rho))
                  end
                val (results', _) = foldl step ([], rho) components
            in  MODVAL results'
            end
        (* <more alternatives for [[ev]] for {\mcl}>=   *)
        | ev (ERRORX es) =
            raise RuntimeError (spaceSep (map (valueString o ev) es))
        | ev (EXP_AT (loc, e)) = atLoc loc ev e
(* \qbreak                                      *)
(*                                              *)
(* Evaluating expressions                       *)
(*                                              *)
(* Most of the implementation of the evaluator is almost *)
(* identical to the implementation in Chapter [->]. But *)
(* there are some significant differences:      *)
(*                                              *)
(*   • Evaluation of [[APPLY]] checks to see if an *)
(*  overloaded name is being applied, and if so, *)
(*  selects its value from an array.            *)
(*   • Before [[LAMBDA]] can be turned into [[CLOSURE]], *)
(*  its types have to be erased.                *)
(*   • The evaluator needs cases for new forms like *)
(*  [[TYAPPLY]] and [[TYLAMBDA]] (\creftypesys.chap), *)
(*  [[VCONX]] and [[CASE]] (\crefadt.chap), and *)
(*  [[MODEXP]] (a module-valued expression).    *)
(*                                              *)
(* And as in \creftypesys.chap,adt.chap, many potential *)
(* run-time errors should be impossible because the *)
(* relevant code would be rejected by the type checker. *)
(* If one of those errors occurs anyway, the evaluator *)
(* raises the exception [[BugInTypeChecking]], not *)
(* [[RuntimeError]] as in \crefmlscheme.chap. \mclflabel *)
(* eval                                         *)
(* <boxed values 281>=                          *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

  in  ev e
  end
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
fun evaldef (d, rho) =
  let val bindings = defbindings (d, rho)
(* Evaluating definitions                       *)
(*                                              *)
(* Definitions may appear either at top level or inside *)
(* a module. Function [[evaldef]] is called only for a *)
(* definition that appears at top level. It returns the *)
(* new environment and the list of values defined. *)
(* (A typical definition defines just one value, but a *)
(* [[data]] definition may define many values, and a *)
(* [[type]] definition doesn't define any.) \mclflabel *)
(* evaldef The real work of evaluating definitions is *)
(* done by [[defbindings]]; it computes a list of *)
(* bindings that may either be added to an environment *)
(* (when the definition appears at top level) or may be *)
(* used to form a module value (when the definition *)
(* appears inside a module).                    *)
(* <boxed values 282>=                          *)
val _ = op evaldef : baredef * value ref env -> value ref env * value list
  in  (rho <+> bindings, map (! o snd) bindings)
  end
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and defbindings (VAL (x, e), rho) =
      [(x, ref (eval (e, rho)))]
  | defbindings (VALREC (x, tau, e), rho) =
      let val this = ref (SYM "placedholder for val rec")
(* Function [[defbindings]] allocates and initializes a *)
(* new mutable reference cell for each name introduced *)
(* by the definition. The evaluation of the classic four *)
(* definition forms from \crefscheme.chap proceeds just *)
(* as in \crefscheme.chap (or \crefmlscheme.chap). Type *)
(* soundness requires a change in the evaluation rule *)
(* for [[VAL]]; as described in Exercise [->] in *)
(* Chapter [->], [[VAL]] must always create a new *)
(* binding. [*]                                 *)
(* <boxed values 283>=                          *)
val _ = op defbindings : baredef * value ref env -> (name * value ref) list
          val rho' = bind (x, this, rho)
          val v    = eval (e, rho')
          val _    = this := v
      in  [(x, this)]
      end
  | defbindings (EXP e, rho) = 
      defbindings (VAL ("it", e), rho)
  | defbindings (DEFINE (f, tau, lambda), rho) =
      defbindings (VALREC (f, tau, LAMBDA lambda), rho)
(* In the [[VALREC]] case, the interpreter evaluates  *)
(* [[e]] while [[name]] is still bound to [[NIL]]---that *)
(* is, before the assignment to [[find (name, rho)]]. *)
(* Therefore, as in Typed uScheme, evaluating [[e]] must *)
(* not evaluate [[name]]---because the mutable cell for *)
(* [[name]] does not yet contain its correct value. *)

(* Evaluating a qualified name doesn't bind [[it]]. *)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
  | defbindings (QNAME _, rho) = 
      []
(* \qbreak The definition of a manifest type or a module *)
(* type doesn't introduce any value at all.     *)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
  | defbindings (TYPE _, _) =
      []
  | defbindings (MODULETYPE (a, _), rho) = 
      []
(* Evaluating a [[data]] definition produces one value *)
(* for each value constructor. If the type of the *)
(* constructor is a function type, the value constructor *)
(* is represented by a function; otherwise it's *)
(* represented by a constructed value.          *)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
  | defbindings (DATA (t, typed_vcons), rho) =
      let fun binding (K, tau) =
            let val v =
                  case tau
                    of FUNTY _ => PRIMITIVE (fn vs => CONVAL (PNAME K, map ref
                                                                            vs))
                     | _ => CONVAL (PNAME K, [])
            in  (K, ref v)
            end
      in  map binding typed_vcons
      end
(* An exporting module is evaluated below by    *)
(* [[evalmod]], which is defined below. A generic module *)
(* evaluates to a closure; the body of the closure is an *)
(* expression constructed by [[modexp]], which is also *)
(* defined below.                               *)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
  | defbindings (MODULE (x, m), rho) =
      [(x, ref (evalmod (m, rho)))]
  | defbindings (GMODULE (f, formals, body), rho) =
      [(f, ref (CLOSURE ((map fst formals, modexp body), rho)))]
(* A declaration of an overloaded name evaluates to an *)
(* array, as used in the case for evaluating an *)
(* [[APPLY]] expression above.                  *)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
  | defbindings (OVERLOAD ps, rho) = 
      let fun overload (p :: ps, rho) =
                let val x = plast p
                    val v = extendOverloadTable (x, evalpath (p, rho), rho)
                    val loc = ref (ARRAY v)
                in  (x, loc) :: overload (ps, bind (x, loc, rho))
                end
            | overload ([], rho) = []
      in  overload (ps, rho)
      end
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and extendOverloadTable (x, v, rho) =
  let val currentVals =
        (case find (x, rho)
           of ref (ARRAY a) => a
            | _ => Array.fromList [])
        handle NotFound _ => Array.fromList []
(* An array is created by extending an existing array, *)
(* or if no array exists yet, by extending the empty *)
(* array.                                       *)
(* <boxed values 284>=                          *)
val _ = op extendOverloadTable : name * value * value ref env -> value array
  in  Array.tabulate (1 + Array.length currentVals,
                      fn 0 => v | i => Array.sub (currentVals, i - 1))
  end
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and evalmod (MSEALED (_, ds), rho) = evalmod (MUNSEALED ds, rho)
  | evalmod (MPATH p, rho) = evalpath (p, rho)
  | evalmod (MPATHSEALED (mtx, p), rho) = evalpath (p, rho)
  | evalmod (MUNSEALED defs, rho) = MODVAL (rev (defsbindings (defs, rho)))
(*OMIT*)   (* XXX type checker should ensure there are no duplicates here *)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and defsbindings ([],   rho) = []
  | defsbindings (d::ds, rho) =
      let val bs   = leftLocated defbindings (d, rho)
          val rho' = foldl (fn ((x, loc), rho) => bind (x, loc, rho)) rho bs
(* Evaluating modules                           *)
(*                                              *)
(* An exporting module is evaluated by evaluating *)
(* whatever form defines it: either a path or a list of *)
(* definitions.                                 *)
(* <boxed values 286>=                          *)
val _ = op evalmod : moddef * value ref env -> value
(* A list of definitions is evaluated by calling *)
(* [[defbindings]] on each definition. Earlier  *)
(* definitions add to the environment used to evaluate *)
(* later definitions. Therefore, bindings are added *)
(* using [[foldl]]; I can't use [[<+>]] here.   *)
(* <boxed values 286>=                          *)
val _ = op defsbindings : def list * value ref env -> (name * value ref) list
      in  bs @ defsbindings (ds, rho')
      end
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and modexp (MPATH px)            = VAR px
  | modexp (MPATHSEALED (_, px)) = VAR px
  | modexp (MSEALED (_, defs))   = MODEXP (concatMap (located defexps) defs)
  | modexp (MUNSEALED defs)      = MODEXP (concatMap (located defexps) defs)
(* <definitions of [[eval]] and [[evaldef]] for {\mcl}>= *)
and defexps (VAL (x, e)) = [(x, e)]
  | defexps (VALREC (x, tau, e)) = 
      let val nullsrc : srcloc = ("translated name in VALREC", ~1)
(* A generic module is evaluated by creating a closure. *)
(* The body of that closure is an expression constructed *)
(* by [[modexp]]. For the common case in which the body *)
(* is defined by a list of definitions, those   *)
(* definitions are converted into named expressions by *)
(* [[defexp]].                                  *)
(* <boxed values 287>=                          *)
val _ = op modexp : moddef -> exp
(* \qbreak Function [[defexps]] has a lot in common with *)
(* [[defbindings]]. I would like to combine them, but *)
(* I have not figured out how to do so. (Shipping is *)
(* also a feature.)                             *)
(* <boxed values 287>=                          *)
val _ = op defexps : baredef -> (name * exp) list
      in  [(x, LETRECX ([((x, tau), e)], VAR (PNAME (nullsrc, x))))]
      end
  | defexps (EXP e) =  [("it", e)]
  | defexps (QNAME _) = []
  | defexps (DEFINE (f, tau, lambda)) = defexps (VALREC (f, tau, LAMBDA lambda))
  | defexps (TYPE _) = []
  | defexps (DATA (t, typed_vcons)) = 
      let fun vconExp (K, t) =
            let val v = if isfuntype t then
                          PRIMITIVE (fn vs => CONVAL (PNAME K, map ref vs))
                        else
                          CONVAL (PNAME K, [])
            in  (K, LITERAL v)
            end
      in  map vconExp typed_vcons
      end
  | defexps (MODULE (x, m)) = [(x, modexp m)]
  | defexps (GMODULE (f, formals, body)) =
      [(f, LAMBDA (map (fn (x, _) => (x, ANYTYPE)) formals, modexp body))]
  | defexps (MODULETYPE (a, _)) = []
  | defexps (OVERLOAD ovls) = unimp "overloadiang within generic module"
(* <definitions of [[basis]] and [[processDef]] for \mcl>= *)
type basis = binding env * value ref env
val emptyBasis : basis = (emptyEnv, emptyEnv)
fun processDataDef (dd, (Gamma, rho), interactivity) =
  let val bindings      = typeDataDef (dd, TOPLEVEL, Gamma)
      val Gamma'        = Gamma <+> bindings
      val comps         = List.mapPartial asComponent bindings
        (* could convert first component to abstract type here XXX *)
      val (rho', vcons) = evalDataDef (dd, rho)
      val _ = if echoes interactivity then

         (* <print the new type and each of its value constructors for \mcl>= *)
                let fun ddString (_, COMPMANTY _) = "*"  (* kind of the type *)
                      | ddString (_, COMPVAL tau) = typeString tau
                      | ddString _ = raise InternalError
                                              "component of algebraic data type"
                    val (kind, vcon_types) =
                      case map ddString comps
                       of s :: ss => (s, ss)
                        | [] => raise InternalError "missing kind string"
                    val (T, _) = dd
                    val tau = (case find (T, Gamma')
                                 of ENVMANTY tau => tau
                                  | _ => raise Match)
                              handle _ => raise InternalError
                                                        "datatype is not a type"
                in  ( println (typeString tau ^ " :: " ^ kind)
                    ; ListPair.appEq (fn (K, tau) => println (K ^ " : " ^ tau))
                                     (vcons, vcon_types)
                    )
                end
              else
                ()
  in  (Gamma', rho')
  end
(* A data definition is processed in much the same was *)
(* as in uML. The major difference is that \mcl uses a *)
(* single environment for all type information. *)
(* <boxed values 225>=                          *)
val _ = op processDataDef : data_def * basis * interactivity -> basis
(* <definitions of [[basis]] and [[processDef]] for \mcl>= *)
fun processOverloading (ps, (Gamma, rho), interactivity) =
  let fun next (p, (Gamma, rho)) =
        let val (tau, first) =
              case pathfind (p, Gamma)
                of ENVVAL tau =>
                     (tau, okOrTypeError (firstArgType (pathexString p, tau)))
                 | c => (* <can't overload a [[c]]>=                    *)
                        raise TypeError (
                          "only functions can be overloaded, but " ^ whatdec c ^
                                         " " ^ pathexString p ^
                                                           " is not a function")
            val x = plast p

            val currentTypes =
              (case find (x, Gamma)
                 of ENVOVLN vals => vals
                  | _ => []) handle NotFound _ => []
            val newTypes = tau :: currentTypes
            val Gamma' = bind (x, ENVOVLN newTypes, Gamma)

            val newVals = extendOverloadTable (x, evalpath (p, rho), rho)
            val rho' = bind (x, ref (ARRAY newVals), rho)

            val _ = if echoes interactivity then
                      app print ["overloaded ", x, " : ", typeString tau, "\n"]
                    else
                      ()
        in  (Gamma', rho')
        end
  in  foldl next (Gamma, rho) ps
  end
(* A basis has just two environments. The [[binding]] *)
(* environment holds compile-time information (mostly *)
(* types) about all named entities. The other   *)
(* environment holds the locations of all the named *)
(* values.                                      *)
(* <definitions of [[basis]] and [[processDef]] for \mcl>= *)
type basis = binding env * value ref env
(* <definitions of [[basis]] and [[processDef]] for \mcl>= *)
fun processDef ((loc, DATA dd), (Gamma, rho), interactivity) =
      atLoc loc processDataDef (dd, (Gamma, rho), interactivity)
  | processDef ((loc, OVERLOAD ps), (Gamma, rho), interactivity) =
      atLoc loc processOverloading (ps, (Gamma, rho), interactivity)
(* <definitions of [[basis]] and [[processDef]] for \mcl>= *)
  | processDef ((loc, QNAME px), (Gamma, rho), interactivity) =
      let val c = pathfind (px, Gamma)
          val x = pathexString px
          fun respond doc = println (layout ppwidth (indent(2, agrp doc)))
          fun typeResponse ty = if x = ty then ["abstract type ", x]
                                else ["type ", x, " = ", ty]
          fun typeResponseDoc ty =
              if x = typeString ty then doc "abstract type " ^^ doc x
              else doc "type " ^^ doc x ^^ doc " = " ^^ typeDoc ty

          infixr 0 $
          fun f $ x = f x

          fun response (ENVVAL _) =
                raise InternalError "ENVVAL reached response"
            | response (ENVMANTY tau) =
                typeResponseDoc tau
            | response (ENVMOD (mt as MTARROW _, _)) = 
                doc "generic module " ^^ doc x ^^ doc " :" ^/ mtDoc mt
            | response (ENVMOD (mt, _)) =
                doc "module " ^^ doc x ^^ doc " :" ^/ mtDoc mt
            | response (ENVMODTY mt) =
                doc "module type " ^^ doc x ^^ doc " =" ^/ mtDoc mt
            | response (ENVOVLN []) =
                raise InternalError "empty overloaded name"
            | response (ENVOVLN (tau :: taus)) =
                let val first_prefix = doc "overloaded "
                    val rest_prefix  = doc "         " 
                          (* rest_prefix drops 2 for indent in `respond` *)
                    fun binding pfx tau =
                      indent (9,
                              agrp (pfx ^^ doc x ^^ doc " :" ^/+ typeDoc tau))
                in  vgrp (brkSep (binding first_prefix tau ::
                                  map (binding rest_prefix) taus))
                end
                  
          val _ =
            if echoes interactivity then
              case c
                of ENVVAL _ =>
                     ignore (processDef ((loc, EXP (VAR px)), (Gamma, rho),
                                                                 interactivity))
                 | _ =>
                     respond (response c)
            else
              ()
      in  (Gamma, rho)
      end
(* \qvfilbreak0.5in                             *)
(*                                              *)
(* All other definition forms are handled by the same *)
(* logic, which invokes [[typdef]] and then [[evaldef]]. *)
(* Most of the logic is devoted to printing the values, *)
(* which uses the overloaded \mcl [[print]] function if *)
(* it's overloaded at the right type, and otherwise uses *)
(* the interpreter function [[valueString]].    *)
(* <definitions of [[basis]] and [[processDef]] for \mcl>= *)
  | processDef ((loc, d), (Gamma, rho), interactivity) =
      let val bindings   = atLoc loc typdef (d, TOPLEVEL, Gamma)
          val Gamma      = Gamma <+> bindings
          val printer    = defPrinter (d, Gamma) interactivity
          val (rho, vs)  = atLoc loc evaldef (d, rho)
          
          fun callPrintExp i v = (* to be passed to eval when needed *)
            APPLY (VAR (PNAME (loc, "print")), [LITERAL v], ref i)

          fun printfun x tau v =
            (* print value v of type tau, which is named x *)
            let val resolved =
                 (case find ("print", Gamma)
                    of ENVOVLN taus => resolveOverloaded ("print", tau, taus)
                     | _ => ERROR "no printer for tau")
                 handle NotFound _ => ERROR "'print' not found"
            in  case resolved
                  of OK (_, i) => ignore (eval (callPrintExp i v, rho))
                   | ERROR _ =>
                       case d
                         of EXP _ => print (valueString v)
                          | _ => case tau
                                   of FUNTY _ => print x
                                    | _       => print (valueString v)
            end

          val _ = if echoes interactivity then
                    (printer printfun vs; print "\n")
                  else
                    ()
      in  (Gamma, rho)
      end
(* And now, all of the definitions. First, the [[DATA]] *)
(* and overload definitions, which are implemented *)
(* above.                                       *)
(* <boxed values 226>=                          *)
val _ = op processDef : def * basis * interactivity -> basis
fun dump_names (Gamma, rho) = app (println o fst) Gamma (*OMIT*)

(* <shared definition of [[withHandlers]]>=     *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn)
                        a
                        (fn s => caught (fillAtLoc (s, loc)))

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]]>= *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
       (* Types should always be checked for equality using *)
       (* [[eqType]], not the built-in [[=]] operator. As shown *)
       (* in \creftuscheme.type-equivalence below, a single *)
       (* type can sometimes have multiple representations, *)
       (* which [[=]] reports as different but should actually *)
       (* be considered the same. Using [[eqType]] gets these *)
       (* cases right; if you use [[=]], you risk introducing *)
       (* bugs that will be hard to find.              *)
       (*                                              *)
       (* Function types are checked for equality using *)
       (* function [[eqFunty]].                        *)

       (* Because [[badParameter]] is called only when the code *)
       (* fails to typecheck, it doesn't need for a base case *)
       (* in which both lists are empty.               *)
       (*                                              *)
       (* Function [[processDef]] is used in the       *)
       (* read-eval-print loop that is defined in \crefpage *)
       (* (mlinterps.repl. The code for the loop can be used *)
       (* unchanged except for one addition: Typed Impcore *)
       (* needs handlers for the new exceptions introduced in \ *)
       (* creftypesys.chap ([[TypeError]] and          *)
       (* [[BugInTypeChecking]]). [[TypeError]] is raised not *)
       (* at parsing time, and not at evaluation time, but at *)
       (* typechecking time (by [[typdef]]).           *)
       (* [[BugInTypeChecking]] should never be raised. *)

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]] ((type-checking))>= *)
       | TypeError         msg => caught ("type error <at loc>: " ^ msg)
       | BugInTypeChecking msg => caught ("bug in type checking: " ^ msg)
(* <shared unit-testing utilities>=             *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* The [[failtest]] used to write the error messages, *)
(* which always returns [[false]], is defined as *)
(* follows:                                     *)
(* <boxed values 133>=                          *)
val _ = op failtest : string list -> bool
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(*  {combinators} \theaderUnit-testing functions *)
(*  provided by each language \combinatoroutcomeexp *)
(*  -> value error \combinatortyexp -> ty error \ *)
(*  combinatortestEqualsvalue * value -> bool \ *)
(*  combinatorasSyntacticValueexp -> value option \ *)
(*  combinatorvalueStringvalue -> string \combinator *)
(*  expStringexp -> string \combinator          *)
(*  testIsGoodunit_test list * basis -> bool \theader *)
(*  Shared functions for unit testing \combinator *)
(*  whatWasExpectedexp * value error -> string \ *)
(*  combinatorcheckExpectPassesexp * exp -> bool \ *)
(*  combinatorcheckErrorPassesexp -> bool \combinator *)
(*  numberOfGoodTestsunit_test list * basis -> int \ *)
(*  combinatorprocessTestsunit_test list * basis -> *)
(*  unit {combinators}                          *)
(*                                              *)
(* Unit-testing functions                       *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* In each bridge language, test results are reported *)
(* the same way. The report's format is stolen from the *)
(* DrRacket programming environment. If there are no *)
(* tests, there is no report.                   *)
(* <shared unit-testing utilities>=             *)
fun reportTestResultsOf what (npassed, nthings) =
  case (npassed, nthings)
    of (_, 0) => ()  (* no report *)
     | (0, 1) => println ("The only " ^ what ^ " failed.")
     | (1, 1) => println ("The only " ^ what ^ " passed.")
     | (0, 2) => println ("Both " ^ what ^ "s failed.")
     | (1, 2) => println ("One of two " ^ what ^ "s passed.")
     | (2, 2) => println ("Both " ^ what ^ "s passed.")
     | _ => if npassed = nthings then
              app print ["All ", intString nthings, " " ^ what ^ "s passed.\n"]
            else if npassed = 0 then
              app print ["All ", intString nthings, " " ^ what ^ "s failed.\n"]
            else
              app print [intString npassed, " of ", intString nthings,
                          " " ^ what ^ "s passed.\n"]
val reportTestResults = reportTestResultsOf "test"
(* <definition of [[testIsGood]] for {\mcl}>=   *)
fun comparisonIndex env tau =
  let val wanted = FUNTY ([tau, tau], booltype)
      fun badType compty = 
        (ERROR o String.concat) ["on type ", typeString tau,
                                 " operation = has type ", typeString compty]
      val index  =
        case find ("=", env)
          of ENVOVLN taus =>
               (case resolveOverloaded ("=", tau, taus)
                  of OK (compty, i) =>
                       if eqType (compty, wanted) then OK i
                       else badType compty
                   | ERROR msg => ERROR msg)
           | _ => ERROR "operator = is not overloaded, so I can't check-expect"
  in  index
  end
(* Unit testing                                 *)
(*                                              *)
(* Testing [[check-expect]] uses the overloaded *)
(* definition of [[=]]. The index of [[=]] overloaded at *)
(* [[tau]] in environment [[env]], if any, is computed *)
(* by function [[comparisonIndex]].             *)
(*                                              *)
(* <boxed values 323>=                          *)
val _ = op comparisonIndex : binding env -> ty -> int error
(* <definition of [[testIsGood]] for {\mcl}>=   *)
fun testIsGood (test, (E, rho)) =
  let fun ty e = typeof (e, E)
                 handle NotFound x =>
                   raise TypeError ("name " ^ x ^ " is not defined")

    (* <shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]]>= *)
      fun checkTypeChecks (e, tau) =
        let val tau' = ty e
        in  true
        end
        handle TypeError msg => 
          failtest ["In (check-type ", expString e, " " ^ typeString tau, "), ",
                                                                            msg]

    (* <shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]]>= *)
      fun checkExpectChecks (e1, e2) = 
        let val tau1 = ty e1
            val tau2 = ty e2
        in  if eqType (tau1, tau2) then
              true
            else
              raise TypeError ("Expressions have types " ^ typeString tau1 ^
                                  " and " ^ typeString tau2)
        end handle TypeError msg =>
        failtest ["In (check-expect ", expString e1, " ", expString e2, "), ",
                                                                            msg]

    (* <shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]]>= *)
      fun checkOneExpChecks inWhat e =
        let val tau1 = ty e
        in  true
        end handle TypeError msg =>
        failtest ["In (", inWhat, " ", expString e, "), ", msg]
      val checkAssertChecks = checkOneExpChecks "check-assert"
      val checkErrorChecks  = checkOneExpChecks "check-error"
      (* \qtrim2                                      *)
      (*                                              *)
      (* When typechecking unit tests, \mcl has a couple of *)
      (* special cases:                               *)
      (*                                              *)
      (*  \tightlist                                  *)
      (*   • A [[check-expect]] checks only if [[=]] is *)
      (*  overloaded at the type we need.             *)
      (*   • A [[check-type]] or [[check-module-types]] checks *)
      (*  only if the type (or module type) given is well *)
      (*  formed.                                     *)
      (*                                              *)
      (* <definition of [[checks]] for \mcl>=         *)
      fun checks (CHECK_EXPECT (e1, e2)) =
            checkExpectChecks (e1, e2) andalso
            (case comparisonIndex E (ty e1)
               of OK i => true
                | ERROR msg =>
                    failtest ["cannot check-expect ", expString e1, ": ", msg])
        | checks (CHECK_ASSERT e)    = checkAssertChecks e
        | checks (CHECK_ERROR  e)    = checkErrorChecks  e
        | checks (CHECK_TYPE (e, t)) = (
            let val _ = elabty (t, E)
            in  true
            end handle TypeError msg => 
              failtest ["In (check-type ", expString e, " " ^ tyexString t,
                                                                    "), ", msg])
        | checks (CHECK_TYPE_ERROR e) = true
        | checks (CHECK_MTYPE (pathx, mt)) =
            let val path = elabpath (pathx, E)
                val _ = elabmt ((mt, path), E)
            in  true
            end handle TypeError msg =>
              failtest ["In (check-module-type ", pathexString pathx, " ",
                        mtxString mt, "), ", msg]
      (* <definition of [[deftystring]] for \mcl>=    *)
      fun deftystring d =
        let val comps = List.mapPartial asComponent (typdef (d, TOPLEVEL, E))
        in  if null comps then
              (case d of OVERLOAD _ => "an overloaded name" 
                       | GMODULE _ => "a generic module"   
                       | MODULETYPE _ => "a module type"
                       | _ => raise InternalError "unrecognized definition")
            else
              commaSep (map (whatcomp o snd) comps)
        end handle NotFound x => raise TypeError ("name " ^ x ^
                                                              " is not defined")
      (* The code for [[check-type-error]] is shared with *)
      (* other interpreters, but in order to show the outcome *)
      (* if a definition does not result in a type error, \mcl *)
      (* needs its own version of [[deftystring]].    *)
      (* <boxed values 326>=                          *)
      val _ = op deftystring : baredef -> string
      (* [[funty]] stand for \tau, [[actualtypes]]    *)
      (* stand for \ldotsntau, and [[rettype]] stand for alpha *)
      (* . The first premise is implemented by a call to *)
      (* [[typesof]] and the second by a call to      *)
      (* [[freshtyvar]]. The constraint is represented just as *)
      (* written in the rule.                         *)


      fun outcome e =
        withHandlers (fn () => OK (eval (e, rho))) () (ERROR o stripAtLoc)

      (* <definition of [[asSyntacticValue]] for \mcl>= *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue (VCONX c)   = SOME (CONVAL (c, []))
        | asSyntacticValue (APPLY (e, es, _)) =
            (case (asSyntacticValue e, optionList (map asSyntacticValue es))
               of (SOME (CONVAL (c, [])), SOME vs) => SOME (CONVAL (c, map ref
                                                                            vs))
                | _ => NONE)
        | asSyntacticValue _ = NONE
      (* To render what was expected if [[check-expect]] *)
      (* fails, function [[whatWasExpected]] (\cref   *)
      (* mlinterps.chap) needs a definition of        *)
      (* [[asSyntacticValue]].                        *)
      (* <boxed values 325>=                          *)
      val _ = op asSyntacticValue : exp -> value option
      (* <shared [[whatWasExpected]]>=                *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (* These functions are used in parsing and elsewhere. *)
      (*                                              *)
      (* Unit testing                                 *)
      (*                                              *)
      (* When running a unit test, each interpreter has to *)
      (* account for the possibility that evaluating an *)
      (* expression causes a run-time error. Just as in *)
      (* Chapters [->] and [->], such an error shouldn't *)
      (* result in an error message; it should just cause the *)
      (* test to fail. (Or if the test expects an error, it *)
      (* should cause the test to succeed.) To manage errors *)
      (* in C, each interpreter had to fool around with *)
      (* [[set_error_mode]]. In ML, things are simpler: the *)
      (* result of an evaluation is converted either to [[OK]] *)
      (*  v, where v is a value, or to [[ERROR]] m, where m is *)
      (* an error message, as described above. To use this *)
      (* representation, I define some utility functions. *)
      (*                                              *)
      (* When a [[check-expect]] fails, function      *)
      (* [[whatWasExpected]] reports what was expected. If the *)
      (* thing expected was a syntactic value,        *)
      (* [[whatWasExpected]] shows just the value. Otherwise *)
      (* it shows the syntax, plus whatever the syntax *)
      (* evaluated to. The definition of [[asSyntacticValue]] *)
      (* is language-dependent.                       *)
      (* <boxed values 129>=                          *)
      val _ = op whatWasExpected  : exp * value error -> string
      val _ = op asSyntacticValue : exp -> value option
      (* <shared [[checkExpectPassesWith]], which calls [[outcome]]>= *)
      val cxfailed = "check-expect failed: "
      fun checkExpectPassesWith equals (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               equals (check, expect) orelse
               failtest [cxfailed, " expected ", expString checkx,
                         " to evaluate to ", whatWasExpected (expectx, OK expect
                                                                              ),
                         ", but it's ", valueString check, "."]
           | (ERROR msg, tried) =>
               failtest [cxfailed, " expected ", expString checkx,
                         " to evaluate to ", whatWasExpected (expectx, tried),
                         ", but evaluating ", expString checkx,
                         " caused this error: ", msg]
           | (_, ERROR msg) =>
               failtest [cxfailed, " expected ", expString checkx,
                         " to evaluate to ", whatWasExpected (expectx, ERROR msg
                                                                              ),
                         ", but evaluating ", expString expectx,
                         " caused this error: ", msg]
      (* <boxed values 130>=                          *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      val _ = op outcome  : exp -> value error
      val _ = op failtest : string list -> bool

(* <shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]]>= *)
      val cafailed = "check-assert failed: "
      fun checkAssertPasses checkx =
            case outcome checkx
              of OK check =>
                   projectBool check orelse
                   failtest [cafailed, " expected assertion ", expString checkx,
                             " to hold, but it doesn't"]
               | ERROR msg =>
                   failtest [cafailed, " expected assertion ", expString checkx,
                             " to hold, but evaluating it caused this error: ",
                                                                            msg]
      (* <boxed values 131>=                          *)
      val _ = op checkAssertPasses : exp -> bool

(* <shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]]>= *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (* Function [[checkErrorPasses]] does the analogous job *)
      (* for [[check-error]].                         *)
      (* <boxed values 132>=                          *)
      val _ = op checkErrorPasses : exp -> bool
      (* To run [[check-expect]], I put the overloaded *)
      (* equality into [[eqfun]], then apply it to the two *)
      (* values from [[checkExpectPassesWith]].       *)
      (* <definition of [[checkExpectPasses]] for \mcl>= *)
      fun checkExpectPasses (c, e) =
        let val i = 
              case comparisonIndex E (ty c)
                of OK i => i
                 | ERROR _ => raise InternalError "overloaded = in check-expect"
            val eqfun =
              case !(find ("=", rho))
                of ARRAY vs =>
                     (Array.sub (vs, i)
                      handle _ => raise InternalError "overloaded subscript")
                 | _ => raise InternalError "overloaded = not array"
                     
            fun testEquals (v1, v2) =
              case eval (APPLY (LITERAL eqfun,
                                [LITERAL v1, LITERAL v2],
                                ref notOverloadedIndex),
                         rho)
                of CONVAL (PNAME "#t", []) => true
                 | _ => false

        in  checkExpectPassesWith testEquals (c, e)
        end
      (* \qbreak The [[check-module-type]] form is unique to \ *)
      (* mcl, but its implementation is similar to that of *)
      (* [[check-type]] from \crefml.chap.            *)
      (* <definition of [[checkMtypePasses]] for \mcl>= *)
      fun checkMtypePasses (pathx, mtx) =
        let val path = elabpath (pathx, E)
            val principal = strengthen (findModule (pathx, E), path)
            val mt = elabmt ((mtx, path), E)
      (*OMIT*) val () = if true then () else
      (*OMIT*)          ( app print ["principal MT   = ", mtString principal,
                                                                           "\n"]
      (*OMIT*)          ; app print ["supertype      = ", mtString mt, "\n"]
      (*OMIT*)          ; app print ["supertype path = ", pathString path, "\n"]
      (*OMIT*)          )
        in  case implements (path, principal, mt)
              of OK () => true
               | ERROR msg => raise TypeError msg
        end handle TypeError msg =>
          failtest ["In (check-module-type ", pathexString pathx, " ",
                    mtxString mtx, "), ", msg]
      (* \qvfilbreak1in                               *)
      (*                                              *)

(* <shared [[checkTypePasses]] and [[checkTypeErrorPasses]], which call [[ty]]>= *)
      fun checkTypePasses (e, tau) =
        let val tau' = ty e
        in  if eqType (tau, tau') then
              true
            else
              failtest ["check-type failed: expected ", expString e,
                        " to have type ", typeString tau,
                        ", but it has type ", typeString tau']
        end handle TypeError msg =>
            failtest ["In (check-type ", expString e,
                      " " ^ typeString tau, "), ", msg]

(* <shared [[checkTypePasses]] and [[checkTypeErrorPasses]], which call [[ty]]>= *)
      fun checkTypeErrorPasses (EXP e) =
            (let val tau = ty e
             in  failtest ["check-type-error failed: expected ", expString e,
                       " not to have a type, but it has type ", typeString tau]
             end handle TypeError msg => true
                      | Located (_, TypeError _) => true)
        | checkTypeErrorPasses d =
            (let val t = deftystring d
             in  failtest ["check-type-error failed: expected ", defString d,

                         " to cause a type error, but it successfully defined ",
                           defName d, " : ", t
                          ] 
             end handle TypeError msg => true
                      | Located (_, TypeError _) => true)

      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
        | passes (CHECK_TYPE (c, t))   = checkTypePasses   (c, elabty (t, E))
        | passes (CHECK_TYPE_ERROR (loc, c))  = atLoc loc checkTypeErrorPasses c
        | passes (CHECK_MTYPE c)       = checkMtypePasses c
  in  checks test andalso passes test
  end
(* \qbreak As we expect at this stage, [[testIsGood]] *)
(* brings together shared code from other bridge *)
(* languages with code written just for \mcl.   *)
(* <boxed values 324>=                          *)
val _ = op testIsGood : unit_test * (binding env * value ref env) -> bool
(* <shared definition of [[processTests]]>=     *)
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
and numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
(* \qbreak Function [[processTests]] is shared among all *)
(* bridge languages. For each test, it calls the *)
(* language-dependent [[testIsGood]], adds up the number *)
(* of good tests, and reports the result. [*]   *)
(* <boxed values 134>=                          *)
val _ = op processTests : unit_test list * basis -> unit
(* <shared read-eval-print loop>=               *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []

(* <definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]]>= *)
      fun processXDef (xd, basis) =
        let (* <definition of [[useFile]], to read from a file>= *)
            fun useFile filename =
              let val fd = TextIO.openIn filename
                  val (_, printing) = interactivity
                  val inter' = (NOT_PROMPTING, printing)
              in  readEvalPrintWith errmsg (filexdefs (filename, fd, noPrompts),
                                                                  basis, inter')
                  before TextIO.closeIn fd
              end
            fun try (USE filename) = useFile filename
              | try (TEST t)       = (unitTests := t :: !unitTests; basis)
              | try (DEF def)      = processDef (def, basis, interactivity)
              | try (DEFS ds)      = foldl processXDef basis (map DEF ds)
                                                                        (*OMIT*)
            fun caught msg = (errmsg (stripAtLoc msg); basis)
            val _ = resetComputationLimits ()     (* OMIT *)
        in  withHandlers try xd caught
        end 
      (* The extended-definition forms [[USE]] and [[TEST]] *)
      (* are implemented in exactly the same way for every *)
      (* language: internal function [[try]] passes each *)
      (* [[USE]] to [[useFile]], and it adds each [[TEST]] to *)
      (* the mutable list [[unitTests]]—just as in the C code *)
      (* in \crefpage(impcore.readevalprint. Function [[try]] *)
      (* passes each true definition [[DEF]] to function *)
      (* [[processDef]], which does the language-dependent *)
      (* work.                                        *)
      (* <boxed values 161>=                          *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* Function [[testIsGood]], which can be shared among *)
(* languages that share the same definition of  *)
(* [[unit_test]], says whether a test passes (or in a *)
(* typed language, whether the test is well-typed and *)
(* passes). Function [[testIsGood]] has a slightly *)
(* different interface from the corresponding C function *)
(* [[test_result]]. The reasons are discussed in \cref *)
(* mlschemea.chap on \cpagerefmlschemea.testIsGood. *)
(* These pieces can be used to define a single version *)
(* of [[processTests]] (\crefpage               *)
(* ,mlinterps.processTests) and a single read-eval-print *)
(* loop, each of which is shared among many bridge *)
(* languages. The pieces are organized as follows: \ *)
(* mdbusemlinterpsprocessTests                  *)
(* <boxed values 160>=                          *)
type basis = basis
val _ = op processDef   : def * basis * interactivity -> basis
val _ = op testIsGood   : unit_test      * basis -> bool
val _ = op processTests : unit_test list * basis -> unit
(* Given [[processDef]] and [[testIsGood]], function *)
(* [[readEvalPrintWith]] processes a stream of extended *)
(* definitions. As in the C version, a stream is created *)
(* using [[filexdefs]] or [[stringsxdefs]].     *)
(*                                              *)
(* Function [[readEvalPrintWith]] has a type that *)
(* resembles the type of the C function         *)
(* [[readevalprint]], but the ML version takes an extra *)
(* parameter [[errmsg]]. Using this parameter, I issue a *)
(* special error message when there's a problem in the *)
(* initial basis (see function [[predefinedError]] on \ *)
(* cpagerefmlinterps.predefinedError). \mdbuse  *)
(* mlinterpspredefinedError The special error message *)
(* helps with some of the exercises in \cref    *)
(* typesys.chap,ml.chap, where if something goes wrong *)
(* with the implementation of types, an interpreter *)
(* could fail while trying to read its initial basis. *)
(* (Failure while reading the basis can manifest in *)
(* mystifying ways; the special message demystifies the *)
(* failure.) \mlsflabelreadEvalPrintWith [*]    *)
(* <boxed values 160>=                          *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
  in  basis
  end
(* Function [[readEvalPrintWith]] executes essentially *)
(* the same imperative actions as the C function *)
(* [[readevalprint]] (\chunkref                 *)
(* scheme.chunk.readevalprint): allocate space for a *)
(* list of pending unit tests; loop through a stream of *)
(* extended definitions, using each one to update the *)
(* environment(s); and process the pending unit tests. *)
(* (The looping action in the ML code is implemented by *)
(* function [[streamFold]], which applies       *)
(* [[processXDef]] to every element of [[xdefs]]. *)
(* Function [[streamFold]] is the stream analog of the *)
(* list function [[foldl]].) Unlike the C       *)
(* [[readevalprint]], which updates the environment *)
(* in place by writing through a pointer, the   *)
(* ML function ends by returning a new basis, which *)
(* contains the updated environment(s).         *)
(*                                              *)
(* Please pause and look at the names of the functions. *)
(* Functions [[eval]] and [[evaldef]] are named after a *)
(* specific, technical action: they evaluate. But *)
(* functions [[processDef]], [[processXDef]], and *)
(* [[processTests]] are named after a vague action: they *)
(* process. I've chosen this vague word deliberately, *)
(* because the ``processing'' is different in different *)
(* languages:                                   *)
(*                                              *)
(*   • In an untyped language like micro-Scheme or *)
(*  uSmalltalk, ``process'' means ``evaluate.'' *)
(*   • In a typed language like Typed Impcore, Typed *)
(*  uScheme, \nml, or uML, ``process'' means ``first *)
(*  typecheck, then evaluate.''                 *)
(*                                              *)
(* Using the vague word ``process'' to cover both *)
(* language families helps me write generic code that *)
(* works with both language families.           *)
(*                                              *)
(* \qbreak Let's see the generic code that ``processes'' *)
(* an extended definition. To process a [[USE]] form, *)
(* [[processXDef]] calls function [[useFile]], which *)
(* reads definitions from a file and recursively passes *)
(* them to [[readEvalPrintWith]].               *)




(*****************************************************************)
(*                                                               *)
(*   FUNCTIONS FOR BUILDING PRIMITIVES WHEN TYPES ARE CHECKED    *)
(*                                                               *)
(*****************************************************************)

(* <functions for building primitives when types are checked>= *)
fun binaryOp f = (fn [a, b] => f (a, b) | _ => raise BugInTypeChecking "arity 2"
                                                                               )
fun unaryOp  f = (fn [a]    => f a      | _ => raise BugInTypeChecking "arity 1"
                                                                               )
(* Primitive functions, predefined functions, and the *)
(* initial basis                                *)
(*                                              *)
(* Code in this section defines Typed Impcore's *)
(* primitive functions and builds its initial basis. As *)
(* in Chapter [->], all primitives are either binary or *)
(* unary operators. But the code below should not reuse *)
(* functions [[unaryOp]] and [[binaryOp]] from \cref *)
(* mlscheme.chap, because when a primitive is called *)
(* with the wrong number of arguments, those versions *)
(* raise the [[RuntimeError]] exception. In a typed *)
(* language, it should not be possible to call a *)
(* primitive with the wrong number of arguments. If it *)
(* happens anyway, these versions of [[unaryOp]] and *)
(* [[binaryOp]] raise [[BugInTypeChecking]].    *)
(* <boxed values 389>=                          *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* <functions for building primitives when types are checked>= *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeChecking "arithmetic on non-numbers")
(* Arithmetic primitives expect and return integers. *)
(* <boxed values 390>=                          *)
val _ = op arithOp : (int * int -> int) -> (value list -> value)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)



(*****************************************************************)
(*                                                               *)
(*   LIST OF (TYPED) PRIMITIVES FOR EACH PRIMITIVE MODULE        *)
(*                                                               *)
(*****************************************************************)

(* <list of (typed) primitives for each primitive module>= *)
type primitive = name * primop * ty
fun compval (x, v, ty) = (x, COMPVAL ty)
fun decval  (x, v, ty) = (x, ENVVAL  ty)
(* Primitive functions, primitive modules, and the *)
(* initial basis                                *)
(*                                              *)
(* Primitive functions and module values        *)
(*                                              *)
(* Primitives for a module are placed into a list. Each *)
(* element has a name, an ML value (of type [[primop]]), *)
(* and a type. The element can be converted into a pair *)
(* to be inserted into a component environment, and also *)
(* a type-checking environment.                 *)
(* <boxed values 227>=                          *)
type primitive = primitive
val _ = op compval : primitive -> name * component
val _ = op decval  : primitive -> name * binding
(* <list of (typed) primitives for each primitive module>= *)
fun eqPrintPrims tau project =
  let val comptype = FUNTY ([tau, tau], booltype)
      val printtype = FUNTY ([tau], unittype)
      val u = unitVal
      fun comparison f =
        binaryOp (embedBool o (fn (x, y) => f (project x, project y)))
  in  ("similar?",  comparison op =,  comptype) ::
      ("dissimilar?",  comparison op =,  comptype) ::
      ("=",  comparison op =,  comptype) ::
      ("!=", comparison op <>, comptype) ::
      ("print",   unaryOp (fn x => (print   (valueString x); u)), printtype) ::
      ("println", unaryOp (fn x => (println (valueString x); u)), printtype) ::
      []
  end
(* If a primitive module exports an atomic type, like *)
(* [[int]] or [[sym]], it also exports operations that *)
(* implement equivalence tests and printing on values of *)
(* that type. These operations are always built by *)
(* [[eqPrintPrims]]. Argument [[project]] converts a \ *)
(* mcl value to a primitive ML value that admits *)
(* equality.                                    *)
(* <boxed values 228>=                          *)
val _ = op eqPrintPrims : ty -> (value -> ''a) -> primitive list
(* <list of (typed) primitives for each primitive module>= *)
val symPrims =
  eqPrintPrims symtype
               (fn SYM s => s
                 | _ => raise BugInTypeChecking "comparing non-symbols")
val boolPrims =
  eqPrintPrims booltype
               (fn CONVAL (K, []) => K
                 | _ => raise BugInTypeChecking "comparing non-Booleans")
(* Modules [[Sym]] and [[Bool]] export only equivalence *)
(* and printing operations.                     *)
(* <boxed values 229>=                          *)
val _ = op symPrims  : primitive list
val _ = op boolPrims : primitive list
(* Module [[Int]] exports all the integer primitives. *)
(* The primitives are built from ML functions, using the *)
(* higher-order functions defined here.         *)
(* <list of (typed) primitives for each primitive module>= *)
fun asInt (NUM n) = n
  | asInt v = raise BugInTypeChecking ("expected a number; got " ^ valueString v
                                                                               )

fun comparison f = binaryOp (embedBool o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeChecking "comparing non-numbers")
fun wordOp f =
  arithOp (fn (n, m) => Word.toInt (f (Word.fromInt n, Word.fromInt m)))
fun unaryIntOp f = unaryOp (NUM o f o asInt)
fun unaryWordOp f = unaryIntOp (Word.toInt o f o Word.fromInt)
(* <list of (typed) primitives for each primitive module>= *)
val arithtype = FUNTY ([inttype, inttype], inttype)
val comptype  = FUNTY ([inttype, inttype], booltype)

val intPrims = 
  ("+", arithOp op +,   arithtype) :: 
  ("-", arithOp op -,   arithtype) :: 
  ("*", arithOp op *,   arithtype) :: 
  ("/", arithOp op div, arithtype) ::

  ("land", wordOp Word.andb, arithtype) ::
  ("lor", wordOp Word.orb,   arithtype) ::
  (">>u", wordOp Word.>>,    arithtype) ::
  (">>s", wordOp Word.~>>,   arithtype) ::
  ("<<",  wordOp Word.<<,    arithtype) ::

  ("of-int", unaryOp id,          FUNTY ([inttype], inttype)) ::
  ("negated", unaryIntOp ~,       FUNTY ([inttype], inttype)) ::
  ("lnot", unaryWordOp Word.notb, FUNTY ([inttype], inttype)) ::

  ("<",  intcompare op <,  comptype) :: 
  (">",  intcompare op >,  comptype) ::
  ("<=", intcompare op <=, comptype) :: 
  (">=", intcompare op >=, comptype) ::
  ("printu"
  , unaryOp (fn n => (printUTF8 (asInt n); unitVal))
  , FUNTY ([inttype], unittype)
  ) ::
  eqPrintPrims inttype
               (fn NUM n => n
                 | _ => raise BugInTypeChecking "comparing non-numbers")
(* \qtrim3.5                                    *)
(*                                              *)
(* The values and types of the integer primitives are as *)
(* follows.                                     *)
(* <boxed values 230>=                          *)
val _ = op intPrims : primitive list
(* <list of (typed) primitives for each primitive module>= *)
local
  val arraypath = PNAME arraymodident
  val arrayarg  = genmodident "Elem"
  val argpath   = PNAME arrayarg
  val resultpath = PAPPLY (arraypath, [argpath])
  val elemtype   = TYNAME (PDOT (argpath, "t"))
  val arraytype  = TYNAME (PDOT (resultpath, "t"))

  val uninitialized = CONVAL (PNAME "uninitialized", [])

  fun protect f x = f x
    handle Size      => raise RuntimeError "array too big"
         | Subscript => raise RuntimeError "array index out of bounds"


  fun asArray (ARRAY a) = a
    | asArray _         = raise BugInTypeChecking "non-array value as array"
  fun arrayLeft f (a, x) = f (asArray a, x)
in
  val arrayPrims = 
    ("size", unaryOp (NUM o Array.length o asArray), FUNTY ([arraytype], inttype
                                                                           )) ::
    ("new", binaryOp (fn (NUM n, a) => ARRAY (protect Array.array (n, a))
                       | _ => raise BugInTypeChecking "array size not a number")
                                                                               ,
            FUNTY ([inttype, elemtype], arraytype)) ::
    ("empty", fn _ => ARRAY (Array.fromList []), FUNTY ([], arraytype)) ::
    ("at", binaryOp (fn (ARRAY a, NUM i) => protect Array.sub (a, i)
                      | _ => raise BugInTypeChecking "Array.at array or index"),
            FUNTY ([arraytype, inttype], elemtype)) ::
    ("at-put", fn [ARRAY a, NUM i, x] => (protect Array.update (a, i, x);
                                                                        unitVal)
                | _ => raise BugInTypeChecking
                                         "number/types of args to Array.at-put",
            FUNTY ([arraytype, inttype, elemtype], unittype)) ::
    []

  val arraymodtype : modty =
    MTARROW ([(arrayarg, MTEXPORTS [("t", COMPABSTY (PDOT (argpath, "t")))]  :
                                                                        modty)],
             MTEXPORTS (("t", COMPABSTY (PDOT (resultpath, "t"))) ::
                        ("elem", COMPMANTY elemtype) ::
                        map compval arrayPrims) : modty)

  val uarrayPrims = 
    ("new", unaryOp (fn (NUM n) => ARRAY (protect Array.array (n, uninitialized)
                                                                               )
                       | _ => raise BugInTypeChecking "array size not a number")
                                                                               ,
            FUNTY ([inttype], arraytype)) ::
    []

  val uarraymodtype : modty =
    MTARROW ([(arrayarg, MTEXPORTS [("t", COMPABSTY (PDOT (argpath, "t")))]  :
                                                                        modty)],
             MTEXPORTS (("t", COMPABSTY (PDOT (resultpath, "t"))) ::
                        map compval uarrayPrims) : modty)
end
(* <boxed values 231>=                          *)
val _ = op arrayPrims  : primitive list
val _ = op uarrayPrims : primitive list
val _ = op arraymodtype  : modty
val _ = op uarraymodtype : modty


(*****************************************************************)
(*                                                               *)
(*   PRIMITIVE MODULES AND DEFINITION OF [[INITIALBASIS]]        *)
(*                                                               *)
(*****************************************************************)

(* <primitive modules and definition of [[initialBasis]]>= *)
local
  fun module id primvals =
    let val typeT = ("t", COMPABSTY (PDOT (PNAME id, "t")))
    in  ENVMOD (MTEXPORTS (typeT :: map compval primvals), PNAME id)
    end
  val unitinfo = ("unit", unitVal, TYNAME (PDOT (PNAME unitmodident, "t")))
(* Building the initial basis                   *)
(*                                              *)
(* Each of the four atomic modules is given an  *)
(* export-list type, which consists of an abstract type  *)
(* [[t]] plus all the module's primitives. Each *)
(* primitive is considered as a component, using *)
(* [[compval]]. Each of the two the array modules has a *)
(* type defined above.                          *)
(* <boxed values 232>=                          *)
val _ = op module : modident -> primitive list -> binding
in
  val intmod  = module intmodident intPrims
  val symmod  = module symmodident symPrims
  val boolmod = module boolmodident boolPrims
  val unitmod = module unitmodident [unitinfo]
  val arraymod  = ENVMOD (arraymodtype,  PNAME arraymodident)
  val uarraymod = ENVMOD (uarraymodtype, PNAME uarraymodident)
end
(* <primitive modules and definition of [[initialBasis]]>= *)
fun addValWith f ((x, v, ty), rho) = bind (x, f v, rho)
val intmodenv    = foldl (addValWith (ref o PRIMITIVE)) emptyEnv intPrims
val arraymodenv  = foldl (addValWith (ref o PRIMITIVE)) emptyEnv arrayPrims
val boolmodenv   = foldl (addValWith (ref o PRIMITIVE)) emptyEnv boolPrims
val unitmodenv = bind ("unit", ref (CONVAL (PNAME "unit", [])), emptyEnv)
val symmodenv  = foldl (addValWith (ref o PRIMITIVE)) emptyEnv symPrims
(* The initial value of each module contains all the *)
(* named components, each of which goes into a [[ref]] *)
(* cell.                                        *)
(* <boxed values 233>=                          *)
val _ = op intmodenv : value ref env
(* <primitive modules and definition of [[initialBasis]]>= *)

val modules = 
  let fun primExp (x, f, _) = (x, LITERAL (PRIMITIVE f))
  in  [ ("Int",  intmod,  MODVAL intmodenv)
      , ("Bool", boolmod, MODVAL boolmodenv)
      , ("Unit", unitmod, MODVAL unitmodenv)
      , ("Sym",  symmod,  MODVAL symmodenv)
      , (arraymodname,  arraymod,
         CLOSURE ((["Elem"], MODEXP (map primExp arrayPrims)), emptyEnv))
      , ("UnsafeArray",  uarraymod,
         CLOSURE ((["Elem"], MODEXP (map primExp uarrayPrims)), emptyEnv))
      , ("ArrayCore",  arraymod,
         CLOSURE ((["Elem"], MODEXP (map primExp arrayPrims)), emptyEnv))

      , ("#t", ENVVAL booltype, CONVAL (PNAME "#t", []))
      , ("#f", ENVVAL booltype, CONVAL (PNAME "#f", []))
      ]
  end
(* And finally the modules themselves. Each has a type *)
(* (expressed as a [[binding]]) and a value.    *)
(* <boxed values 234>=                          *)
val _ = op modules : (name * binding * value) list
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <primitive modules and definition of [[initialBasis]]>= *)
val emptyBasis = (emptyEnv, emptyEnv)
val initialBasis = 
  let fun addmod ((x, dbl, v), (Gamma, rho)) =
        (bind (x, dbl, Gamma), bind (x, ref v, rho))
  in  foldl addmod emptyBasis modules
  end

val initialBasis =
  let val predefinedTypes =
            
             [ ";  <predefined Molecule types, functions, and modules>= "
             , "(type int  Int.t)"
             , "(type bool Bool.t)"
             , "(type unit Unit.t)"
             , "(type sym  Sym.t)"
             , ";  <predefined Molecule types, functions, and modules>= "
             , "(module-type ARRAY"
             , " (exports [abstype t]    ;; an array"
             , "          [abstype elem] ;; one element of the array"
             , "          [new    : (int elem -> t)]"
             , "          [empty  : (         -> t)]"
             , "          [size   : (t -> int)]     "
             , "          [at     : (t int -> elem)]"
             , "          [at-put : (t int elem -> unit)]))"
             ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
             , ";  \\advanceby -0.7pt                            "
             , ";                                               "
             , ";           Definition form    Declaration Expression "
             , ";                              form        form "
             , ";   Module  [[module]]         \\monobox    none! "
             , ";                              (module [M  (2nd class) "
             , ";                              : \\amodtype      "
             , ";                              ])               "
             , ";   Generic [[generic-module]] none (not a none! "
             , ";   module                     component)  (2nd class) "
             , ";   Module  [[module-type]]    none (not a \\monobox "
             , ";   type                       component)  (exports "
             , ";                                          ...) "
             , ";   Type    [[type]], [[data]] \\monobox    (as in uML) "
             , ";                              [abstype t       "
             , ";                              ], \\monobox      "
             , ";                              [type t tau      "
             , ";                              ]                "
             , ";   Value   [[val]],           \\monobox[x  (as in uML) "
             , ";           [[define]]         : tau]           "
             , ";                                               "
             , ";  Some syntactic forms of \\mcl [*]             "
             , ";                                               "
             , ";  \\qvspace-1                                   "
             , ";                                               "
             ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
             , ";                                               "
             , ";  Module introduction: Definition forms for exporting "
             , ";  modules                                      "
             , ";                                               "
             , ";  A module type only makes promises—it doesn't deliver "
             , ";  any values, types, or modules. To deliver on the "
             , ";  promises requires a module, which is defined using "
             , ";  one of the [[module]] forms from \\cref       "
             , ";  mcl.fig.module-syntax. An exporting module is "
             , ";  typically defined by giving its name M, its module "
             , ";  type \\amodty, and a sequence of definitions, like so: "
             , ";  \\monobox(module [M : \\amodty ] \\tightcdotsnd). "
             , ";  The definition of M is well typed only if all the d_i "
             , ";  's are well typed and if M provides all the  "
             , ";  components listed in \\amodty. And the definition "
             , ";  seals module M with type \\amodty. Sealing limits "
             , ";  access to the components defined by definitions \\ "
             , ";  tightcdotsnd: outside of M, the only components that "
             , ";  can be named are those mentioned in \\amodty. And if "
             , ";  any of the type components is declared as an abstract "
             , ";  type, its definition is hidden.              "
             , ";                                               "
             , ";  Exporting modules can also be defined using two other "
             , ";  forms. First, an existing module M' may be sealed "
             , ";  using the form \\monobox(module [M : \\amodty ] M'). "
             , ";  New module M has the same components as existing "
             , ";  module M', but only components exported by module "
             , ";  type \\amodty are visible. And if any type component t "
             , ";  is declared as abstract in \\amodty, then sealing "
             , ";  gives type M.t a new identity that is distinct from "
             , ";  M'.t. (In the language of \\crefadt.chap, sealing is "
             , ";  generative.) Second, an existing module may be "
             , ";  abbreviated, without sealing, using the form \\monobox "
             , ";  (module M M'), as in \\monobox(module IR (@m Ref "
             , ";  Int)). Such abbreviations are used primarily for "
             , ";  instances of generic modules, which are discussed "
             , ";  below.                                       "
             , ";                                               "
             , ";  Module elimination: Qualified names          "
             , ";                                               "
             , ";  Client code can observe or interrogate a module in "
             , ";  only one way: it can name a component. The   "
             , ";  component's qualified name is formed by concatenating "
             , ";  the name of the module, a dot, and the name of the "
             , ";  component. Qualified names are used in many  "
             , ";  languages, including \\ada, CLU, Haskell, \\java, \\ "
             , ";  ocaml, \\modula3, and Standard ML. In \\mcl, as in most "
             , ";  of these languages, a qualified name like    "
             , ";  [[IntArray.at]] selects a component from a module. "
             , ";  Qualified names can also instantiate generic modules, "
             , ";  as described below.                          "
             , ";                                               "
             , ";  Generic modules                              "
             , ";                                               "
             , ";  [*] A good module can embody a lot of craft—think "
             , ";  about the engineering that goes into a balanced "
             , ";  search tree or a good hash table. Such engineering "
             , ";  should be reusable, and you already know a suitable "
             , ";  mechanism: higher-order, polymorphic functions. But "
             , ";  at this scale, [[lambda]] and [[type-lambda]] are "
             , ";  awkward; they work with individual values and types, "
             , ";  and the unit of reuse should be the module.  "
             , ";  To support polymorphism with modules, \\mcl provides "
             , ";  generic modules.                             "
             , ";                                               "
             , ";  A generic module takes one or more other modules as "
             , ";  formal parameters, and it produces a module as its "
             , ";  result. By taking modules as parameters, generic "
             , ";  modules provide, in one mechanism, the capabilities "
             , ";  of higher-order functions (functions as parameters, "
             , ";  as with [[lambda]]) and parametric polymorphism "
             , ";  (types as parameters, as with [[type-lambda]]). "
             , ";                                               "
             , ";  A parameter of a generic module may not itself be "
             , ";  generic. This sort of restriction, which says that "
             , ";  the parameter to a polymorphic thing may not itself "
             , ";  be polymorphic, makes the polymorphism predicative. "
             , ";  Predicativity can simplify a type theory     "
             , ";  considerably, so predicative polymorphism is common. "
             , ";  For example, the Hindley-Milner type system used in "
             , ";  core ML is predicative, and so are systems for other "
             , ";  languages that support generic modules, including "
             , ";  CLU, \\ada, and \\modula-3. Type systems in which type "
             , ";  parameters are not restricted, like Typed uScheme, "
             , ";  are called impredicative.                    "
             , ";                                               "
             , ";  In \\mcl, the type of a generic module is written like "
             , ";  a function type, except that each formal parameter "
             , ";  gets a name (so that later parameters and the result "
             , ";  type may refer to it), and the parameters are "
             , ";  separated from the result type by the module arrow  "
             , ";  [[–m->]]. For example, the type of \\mcl's predefined, "
             , ";  generic [[Array]] module is as follows:      "
             , ";  <predefined Molecule types, functions, and modules>= "
             , "(module-type GENERIC-ARRAY"
             , " ([Elem : (exports [abstype t])] --m->"
             , "     (exports [abstype t]        ;; an array"
             , "              [type elem Elem.t] ;; one element of the array"
             , "              [new    : (int elem -> t)]"
             , "              [empty  : (         -> t)]"
             , "              [size   : (t -> int)]     "
             , "              [at     : (t int -> elem)]"
             , "              [at-put : (t int elem -> unit)])))"
             , ";  The instance is pronounced ``[[Array]] at [[Char]].'' "
             , ";                                               "
             , ";  An instance of a generic module also plays a second "
             , ";  role: an instance of a generic module counts as a "
             , ";  qualified name. This quirk of \\mcl's type system, "
             , ";  which is explained in depth in \\cref         "
             , ";  mcl.instantiation, supports a simple algorithm for "
             , ";  type checking with abstract types. To get the right "
             , ";  intuition, think of the instance as being like the "
             , ";  application of a type constructor, like \\monobox "
             , ";  (array char) in Typed Impcore.               "
             , ";                                               "
             , ";  Intersection types                           "
             , ";                                               "
             , ";  An intersection type combines multiple export-list "
             , ";  module types, forming a refined export-list module "
             , ";  type. The term ``intersection'' can be confusing "
             , ";  because if you focus on the components, it looks like "
             , ";  an intersection type takes their union. Intersection "
             , ";  is happening to the inhabitants; a module inhabits "
             , ";  the intersection type if and only if it inhabits all "
             , ";  of the intersected module types.             "
             , ";                                               "
             , ";  An intersection type is well formed if all of its "
             , ";  constituent module types are well formed. The "
             , ";  constituent module types need not be compatible; the "
             , ";  intersection of incompatible module types is well "
             , ";  formed but uninhabited.                      "
             , ";                                               "
             , ";  Intersection types are most often used to intersect a "
             , ";  named module type with an [[exports]] module type "
             , ";  that reveals the identity of an abstract type. "
             , ";  For example, an intersection type is used to seal the "
             , ";  [[IntArray]] module: [*]                     "
             , ";  <predefined Molecule types, functions, and modules>= "
             , "(module [IntArray : (allof ARRAY (exports [type elem Int.t]))]"
             , "   (@m Array Int))"
             ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
             , ";                                               "
             , ";   + <  =       negated                        "
             , ";   - >  !=                                     "
             , ";   * <= print                                  "
             , ";   / >= println                                "
             , ";                                               "
             , ";  Names that are overloaded in \\mcl's initial basis [*] "
             , ";  [*]                                          "
             ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
             , ";                                               "
             , ";  Overloading                                  "
             , ";                                               "
             , ";  [*] When every operation needs a qualified name, code "
             , ";  can feel bloated and tedious; unqualified names like "
             , ";  [[+]], [[print]], and [[=]] are easier on the eyes. "
             , ";  To enable unqualified names to be used in more "
             , ";  contexts, \\mcl allows the name of a function to be "
             , ";  overloaded. An overloaded name may stand for more "
             , ";  than one value or function; to determine which "
             , ";  function is meant, \\mcl looks at the type of its "
             , ";  first argument.                              "
             , ";                                               "
             , ";  Overloading helps answer three questions that the "
             , ";  designers of any statically typed programming "
             , ";  language ought to address:                   "
             , ";                                               "
             , ";   1. Programmers like to use [[+]] for both integer "
             , ";   addition and floating-point addition. How is the "
             , ";   language to know which is meant, when?      "
             , ";                                               "
             , ";   2. Programmers like to use [[=]] for equality. "
             , ";   How is equality supposed to be decided for values "
             , ";   of different types? Especially abstract types? "
             , ";   After all, equality for something like      "
             , ";   association lists is not always obvious (\\  "
             , ";   cpagerefscheme.alist-equality). And anything like "
             , ";   [[check-expect]] has to use the right equality "
             , ";   for each test.                              "
             , ";                                               "
             , ";   3. Interpreters and debuggers need to be able to "
             , ";   print values. How does an interpreter know what "
             , ";   to print? In some languages, printing the   "
             , ";   representation might be good enough, but if "
             , ";   representation is supposed to be hidden, perhaps "
             , ";   a [[print]] operation shouldn't reveal it.  "
             , ";                                               "
             , ";  Each of these questions is addressed by overloading: "
             , ";  arithmetic, equality, and printing use conventional, "
             , ";  overloaded names, and by overloading these names, "
             , ";  programmers can specify how arithmetic, equality, and "
             , ";  printing should behave.                      "
             , ";                                               "
             , ";  In \\mcl, you overload a name by putting a function's "
             , ";  qualified name in an [[overload]] definition. \\mcl's "
             , ";  initial basis overloads arithmetic, comparisons, and "
             , ";  printing (\\vrefmcl.table.overloaded):        "
             ,
           ";  <predefined Molecule types, functions, and modules ((elided))>= "
             , ";  Characters                                   "
             , ";                                               "
             , ";  The definitions of type [[t]] and values [[space]] "
             , ";  and [[right-curly]] appear in \\crefmcl.chap. "
             , ";  Everything else is here.                     "
             , ";  <definition of module [[Char]]>=             "
             , "(module [Char : (exports [abstype t]"
             , "                         [new : (int -> t)]"
             , "                         [left-curly : t]"
             , "                         [right-curly : t]"
             , "                         [left-round : t]"
             , "                         [right-round : t]"
             , "                         [left-square : t]"
             , "                         [right-square : t]"
             , "                         [newline : t]"
             , "                         [space : t]"
             , "                         [semicolon : t]"
             , "                         [quotemark : t]"
             , "                         [=  : (t t -> bool)]"
             , "                         [!= : (t t -> bool)]"
             , "                         [print : (t -> unit)]"
             , "                         [println : (t -> unit)])]"
             , ""
             , ";    This optimized merge is possible only because "
             , ";    [[merge]] can inspect the representations of both "
             , ";    arguments, [[h1]] and [[h2]].                "
             , ";                                                 "
             , ";    The other operations are left to you (\\cref  "
             , ";    mcl.ex.leftist).                             "
             , ";                                                 "
             , ";    In-depth case study: Arithmetic              "
             , ";                                                 "
             , ";    To get experience writing functions that can inspect "
             , ";    all of their arguments, try implementing arithmetic. "
             , ";    A function that adds two numbers, for example, needs "
             , ";    access to every digit of both numbers.       "
             , ";    By implementing arithmetic on so-called large "
             , ";    integers, where there is no a priori limit to the "
             , ";    number of digits, you can compare the abstract-type "
             , ";    approach to data abstraction (this chapter) with the "
             , ";    object-oriented approach (\\crefsmall.chap).  "
             , ";    Arithmetic was once every schoolchild's introduction "
             , ";    to algorithms, but if you have forgotten the classic "
             , ";    algorithms used to add, subtract, and multiply "
             , ";    numbers of many digits, they are explained in detail "
             , ";    in \\crefarith.chap.                          "
             , ";                                                 "
             , ";    \\qtrim0.5                                    "
             , ";                                                 "
             , ";    Implementing arithmetic will give you insight into "
             , ";    similarities and differences between abstract data "
             , ";    types and objects. Abstract data types and objects "
             , ";    use the same representation (``sequence of digits''), "
             , ";    and they use data abstraction to protect the same "
             , ";    invariants (``the leading digit of a nonzero number "
             , ";    is never zero''), but from there, they diverge: "
             , ";                                                 "
             , ";      • In \\crefmcl.ex.bignat in this chapter, you can "
             , ";     define a function that adds two natural numbers. "
             , ";     Because it is defined in the same module as the "
             , ";     representation of natural numbers, this function "
             , ";     can simply look at the digits of both       "
             , ";     representations, add them pairwise, and return a "
             , ";     result. But this function is limited to the type "
             , ";     on which it is defined; it cannot dream of, for "
             , ";     example, adding a natural number to a machine "
             , ";     integer to get a natural-number result. If client "
             , ";     code wants to add a natural number and a machine "
             , ";     integer, it must first coerce the machine integer "
             , ";     to a natural number. (Such a coercion is a key "
             , ";     step in the algorithm for multiplying two natural "
             , ";     numbers.)                                   "
             , ";      • By contrast, an addition method defined on an "
             , ";     object can see only the digits of the addend on "
             , ";     which it is defined; it must treat the other "
             , ";     addend as an abstraction. To make addition  "
             , ";     possible requires changing the abstraction's API, "
             , ";     so it can include operations like ``tell me your "
             , ";     least-significant digit.'' But because the other "
             , ";     addend is an abstraction, it doesn't have to have "
             , ";     the same representation as the first addend—and "
             , ";     in \\crefrange                               "
             , ";     small.ex.Naturalsmall.ex.transparent-bignum in \\ "
             , ";     crefsmall.chap, you can build an object-oriented "
             , ";     implementation of arithmetic that can seamlessly "
             , ";     add machine integers and large integers. Using "
             , ";     objects, client code can simply add numbers and "
             , ";     not worry about their types.                "
             , ";                                                 "
             , ";    Large integers make a great case study for comparing "
             , ";    abstract data types with objects, but they are also a "
             , ";    vital abstraction in their own right—integers of "
             , ";    practically unlimited size are supported natively in "
             , ";    many programming languages, including \\icon, Haskell, "
             , ";    \\python, and full Scheme.                    "
             , ";                                                 "
             , ";    \\mcl's type system: Enforcing abstraction    "
             , ";                                                 "
             , ";    [*] Access to information about abstract types is "
             , ";    controlled by a type system. \\mcl's type system "
             , ";    supports data abstraction with abstract types and "
             , ";    modules, including generic modules and nested "
             , ";    modules, in much the same way as other languages that "
             , ";    provide these features. \\mcl's type system also "
             , ";    supports function overloading, but \\mcl's overloading "
             , ";    is just a convenience, not a good model of   "
             , ";    overloading as it is found elsewhere. Regardless, \\ "
             , ";    mcl's type system is more ambitious than the type "
             , ";    systems in \\creftypesys.chap,adt.chap,ml.chap. What "
             , ";    the type system does and how it works are explained "
             , ";    in this section. The section begins informally, goes "
             , ";    deep into formal details, and concludes with some "
             , ";    comparisons.                                 "
             , ";                                                 "
             , ";    What the type system does and how            "
             , ";                                                 "
             , ";    The type system's main job is to make modules and "
             , ";    abstract types work:                         "
             , ";                                                 "
             , ";      • When a module is sealed, the type system hides "
             , ";     the representations of its abstract types, and it "
             , ";     gives each abstract type a new identity.    "
             , ";      • When a module is copied or is passed as a "
             , ";     parameter to instantiate a generic module, the "
             , ";     type system preserves the identities of its "
             , ";     abstract types.                             "
             , ";      • When a module type is ascribed to a module, "
             , ";     whether by sealing or by instantiation, the type "
             , ";     system ensures that the module implements the "
             , ";     module type ascribed to it.                 "
             , ";      • When a generic module is defined, the type system "
             , ";     checks it right away, assuming that each    "
             , ";     parameter has the module type claimed for it. "
             , ";     When the module is instantiated, the type system "
             , ";     checks that each actual parameter implements the "
             , ";     corresponding module type, but it needn't check "
             , ";     the generic code again—if the actual parameters "
             , ";     are OK, the instance is guaranteed to be well "
             , ";     typed. This ability to check a generic module "
             , ";     once, rather than have to check each instance, is "
             , ";     an example of modular type checking.        "
             , ";                                                 "
             , ";    Each aspect of the job is illustrated below with an "
             , ";    example.                                     "
             , ";                                                 "
             , ";    \\pbreak[5000]                                "
             , ";                                                 "
             , ";    As an example of sealing, module [[Char]] is sealed "
             , ";    with a signature that declares \\monobox[abstype t]. "
             , ";    Inside [[Char]], type [[Char.t]] is defined as type  "
             , ";    [[int]], and the values exported from module [[Char]] "
             , ";    are just integers:                           "
             , ";    <definitions inside module [[Char]]>=        "
             , "  (type t int)"
             , "  (val space        32)   "
             , "  (val right-curly 125)"
             , "  (define int new ([n : int]) n)"
             , "  (val newline      10)   "
             , "  (val semicolon    59)   "
             , "  (val quotemark    39)   "
             , "  (val left-round    40)"
             , "  (val right-round   41)"
             , "  (val left-curly   123)"
             , "  (val left-square   91)"
             , "  (val right-square  93)"
             , "                          "
             , "  (val = Int.=)"
             , "  (val != Int.!=)"
             , "                          "
             , "  (val print Int.printu)"
             , "  (define unit println ([c : t]) (print c) (print newline))"
             , ")   "
             , ";  A module-arrow type is well formed only if the module "
             , ";  type of each formal parameter describes an exporting "
             , ";  module, not a generic module—that's the predicative "
             , ";  polymorphism. The module type of each parameter must "
             , ";  be well formed, and the module type of the result "
             , ";  must also be well formed. The module type of the "
             , ";  result may refer to formal parameters, like [[Elem]] "
             , ";  in the example above. And the module types of later "
             , ";  parameters may refer to earlier parameters. [ "
             , ";  A~function type with these kinds of references is "
             , ";  called a \\emph{dependent function type}; in~type "
             , ";  theory, it~is often written with a $\\Pi$ symbol, as "
             , ";  in $\\Pi x\\mathord:\\tau \\alldot \\tau'$, where $\\tau'$ "
             , ";  may mention~$x$.\\index{dependent function types}] "
             , ";                                               "
             , ";  A generic module is introduced using the same "
             , ";  definition form as an exporting module, except that "
             , ";  it uses the keyword [[generic-module]], and the "
             , ";  module type used to seal it must be a module-arrow "
             , ";  type. For an example, see the definition of generic "
             , ";  module [[ArrayHeap]] in \\chunkrefmcl.chunk.ArrayHeap. "
             , ";                                               "
             , ";  A generic module is eliminated by instantiating it. "
             , ";  Instantiation creates an exporting instance of a "
             , ";  generic module. The concrete syntax resembles the "
             , ";  instantiation of a polymorphic value in Typed "
             , ";  uScheme, but to remind us of the distinction between "
             , ";  a generic module in \\mcl and a polymorphic value in "
             , ";  Typed uScheme, a generic instantiation is written "
             , ";  using keyword [[@m]]. For example, the predefined "
             , ";  module [[String]] is an instance of the generic "
             , ";  [[Array]] module:                            "
             , ";  <definition of module [[String]]>=           "
             , "(module String (@m Array Char))"
             , ";  <predefined Molecule types, functions, and modules>= "
             , "(overload Int.+ Int.-  Int.* Int./ Int.negated"
             , "          Int.= Int.!= Int.< Int.> Int.<= Int.>="
             , "          Int.print Int.println)"
             , "(overload Bool.= Bool.!= Bool.print Bool.println)"
             , "(overload Sym.=  Sym.!=  Sym.print  Sym.println)"
             , "(overload Char.= Char.!= Char.print Char.println)"
             , ";  \\qvfilbreak2.5in                             "
             , ";                                               "
             , ";  Predefined modules, module types, and functions "
             , ";                                               "
             , ";  Predefined things include not only modules but also "
             , ";  module types and a handful of functions.     "
             , ";  <predefined Molecule types, functions, and modules>= "
             , ";  \\qbreak                                      "
             , ";                                               "
             , ";  Predefined module types                      "
             , ";                                               "
             , ";  In addition to the [[ARRAY]] module type defined in \\ "
             , ";  crefmcl.chap (\\chunkrefmcl.chunk.ARRAY), \\mcl defines "
             , ";  several other module types.                  "
             , ";  <\\mcl's predefined module types>=            "
             , "(module-type PRINTS"
             , "   (exports [abstype t]"
             , "            [print : (t -> unit)]"
             , "            [println : (t -> unit)]))"
             , ";  <\\mcl's predefined module types>=            "
             , "(module-type BOOL"
             , "   (exports [abstype t]"
             , "            [#f : t]"
             , "            [#t : t]))"
             , " ;;;; omitted: and, or, not, similar?, copy, print, println"
             , ";  <\\mcl's predefined module types>=            "
             , "(module-type SYM"
             , "   (exports [abstype t]"
             , "            [=  : (t t -> Bool.t)]"
             , "            [!= : (t t -> Bool.t)]))"
             , " ;;;; omitted: hash, similar?, copy, print, println"
             , ";  Total order and relational predicates        "
             , ";                                               "
             , ";  <\\mcl's predefined module types>=            "
             , "(module-type ORDER"
             , "  (exports [abstype t]"
             , "           [LESS : t]"
             , "           [EQUAL : t]"
             , "           [GREATER : t]))"
             , ""
             , "(module [Order : ORDER]"
             , "  (data t"
             , "    [LESS : t]"
             , "    [EQUAL : t]"
             , "    [GREATER : t]))"
             , ";  <\\mcl's predefined module types>=            "
             , "(module-type RELATIONS"
             , "  (exports [abstype t]"
             , "           [<  : (t t -> Bool.t)]"
             , "           [<= : (t t -> Bool.t)]"
             , "           [>  : (t t -> Bool.t)]"
             , "           [>= : (t t -> Bool.t)]"
             , "           [=  : (t t -> Bool.t)]"
             , "           [!= : (t t -> Bool.t)]))"
             , ";  The generic module [[Relations]] is given a module "
             , ";  with a [[compare]] function, and it implements "
             , ";  relational predicates.                       "
             , ";  <\\mcl's predefined module types>=            "
             , "(generic-module [Relations : ([M : (exports [abstype t]"
             ,
    "                                            [compare : (t t -> Order.t)])]"
             , "                               --m-> (allof RELATIONS"
             ,
         "                                            (exports [type t M.t])))]"
             , "  (type t M.t)"
             , ";    <definitions of the six relational predicates>= "
             , "  (define bool < ([x : t] [y : t])"
             , "     (case (M.compare x y)"
             , "        [Order.LESS #t]"
             , "        [_    #f]))"
             , "  (define bool > ([x : t] [y : t])"
             , "     (case (M.compare y x)"
             , "        [Order.LESS #t]"
             , "        [_    #f]))"
             , ";    <definitions of the six relational predicates>= "
             , "  (define bool <= ([x : t] [y : t])"
             , "     (case (M.compare x y)"
             , "        [Order.GREATER #f]"
             , "        [_       #t]))"
             , "  (define bool >= ([x : t] [y : t])"
             , "     (case (M.compare y x)"
             , "        [Order.GREATER #f]"
             , "        [_       #t]))"
             , ";    <definitions of the six relational predicates>= "
             , "  (define bool = ([x : t] [y : t])"
             , "     (case (M.compare x y)"
             , "        [Order.EQUAL #t]"
             , "        [_     #f]))"
             , "  (define bool != ([x : t] [y : t])"
             , "     (case (M.compare x y)"
             , "        [Order.EQUAL #f]"
             , "        [_     #t]))"
             , ")"
             , ""
             ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
             , ";  <\\mcl's predefined module types>=            "
             , "(module-type INT"
             , "  (exports [abstype t]                 [<  : (t t -> Bool.t)]"
             , "           [+ : (t t -> t)]            [<= : (t t -> Bool.t)]"
             , "           [- : (t t -> t)]            [>  : (t t -> Bool.t)]"
             , "           [* : (t t -> t)]            [>= : (t t -> Bool.t)]"
             , "           [/ : (t t -> t)]            [=  : (t t -> Bool.t)]"
             , "           [negated : (t -> t)]        [!= : (t t -> Bool.t)]"
             , "           [print   : (t -> Unit.t)]"
             , "           [println : (t -> Unit.t)]))"
             , ";  Supporting code \\chapheadsplitfor \\nml       "
             , ";                                               "
             , ";  [*][*] \\invisiblelocaltableofcontents[*]     "
             , ";                                               "
             , ";  [*]                                          "
             , ";                                               "
             , ";  Organizing code chunks \\chaptocbacksplitinto an "
             , ";  interpreter                                  "
             , ";                                               "
             , ";  The overall structure of the \\nml interpreter "
             , ";  resembles the structure of the Typed uScheme* "
             , ";  interpreter, but instead of type checking, it uses "
             , ";  type inference.                              "
             , ""
             , "(define bool and ([b : bool] [c : bool]) (if b c b))"
             , "(define bool or  ([b : bool] [c : bool]) (if b b c))"
             ,
              "(define bool not ([b : bool])            (if b (= 1 0) (= 0 0)))"
             , "(define int mod ([m : int] [n : int]) (- m (* n (/ m n))))"
             , ";  Resizeable arrays                            "
             , ";                                               "
             , ";  [*] To make the [[ArrayList]] module easy to download "
             , ";  and play with, it is defined in its own file, "
             , ";  arraylist.mcl.                               "
             , ";  <predefined Molecule types, functions, and modules>= "
             , ";  \\mcl provides several array abstractions, including "
             , ";  [[ArrayList]], which can grow and shrink at either "
             , ";  end. All array abstractions provide constant-time "
             , ";  access to elements. Arrays are supported by modules "
             , ";  [[UnsafeArray]], [[ArrayCore]], [[Array]],   "
             , ";  [[IntArray]], and [[ArrayList]].             "
             , ";                                               "
             , ";  Predefined module types include [[ARRAY]],   "
             , ";  [[GENERIC-ARRAY]], and [[ARRAYLIST]].        "
             , ";                                               "
             , ";  \\mcl also provides a generic [[Ref]] module, with "
             , ";  operations [[new]], [[!]], and [[:=]]. References "
             , ";  work as they do in ML.                       "
             , ";                                               "
             , ";  Program design: \\chaptocsplitAbstractions    "
             , ";                                               "
             , ";  [*] [*] Because programming at scale is challenging, "
             , ";  learning to use abstract data types and modules can "
             , ";  also be challenging. But you can get started with "
             , ";  just a few techniques:                       "
             , ";                                               "
             , ";    • When designing an interface, you must say what an "
             , ";   abstraction is, whether it is mutable, what "
             , ";   operations are supported, and what they cost. And "
             , ";   you must plan for a representation that can do "
             , ";   the job.                                    "
             , ";                                               "
             , ";   Once an interface is designed, you can classify "
             , ";   each operation as a creator, producer, mutator, "
             , ";   or observer (\\cpageref                      "
             , ";   scheme.constructor-observer-mutator). The   "
             , ";   classification can help you confirm that the "
             , ";   interface is not missing anything obvious and "
             , ";   that its operations will work well together. "
             , ";    • To specify the behaviors of an interface's "
             , ";   operations, you use the metaphorical or     "
             , ";   mathematical language of the interface's    "
             , ";   abstractions, perhaps with some algebraic laws (\\ "
             , ";   crefscheme.chap).                           "
             , ";    • To relate an abstraction to its implementation, "
             , ";   you can use two powerful tools: an abstraction "
             , ";   function and a representation invariant.    "
             , ";                                               "
             , ";  These techniques apply equally well to program design "
             , ";  with objects (\\crefsmall.chap), but in this chapter "
             , ";  they are illustrated with modules and abstract data "
             , ";  types. Description and classification are illustrated "
             , ";  with array lists and sets; abstraction functions are "
             , ";  illustrated with multiple examples, and      "
             , ";  representation invariants are illustrated with "
             , ";  priority queues, both here and in \\cref      "
             , ";  mcl.multiple-rep.                            "
             , ";                                               "
             , ";  Interface-design choices                     "
             , ";                                               "
             , ";  [*] [*]                                      "
             , ";                                               "
             , ";  Every designer must decide whether an abstraction "
             , ";  will be mutable, what operations an interface will "
             , ";  provide, and what those operations might cost. "
             , ";  All three considerations influence (and are  "
             , ";  influenced by) the intended representation.  "
             , ";                                               "
             , ";  If an abstraction has a state that can change over "
             , ";  time, it's mutable. Otherwise, it's immutable. "
             , ";  A mutable abstraction has a mutable representation. "
             , ";  An immutable abstraction usually has an immutable "
             , ";  representation, but it can have a mutable    "
             , ";  representation—a classic example is an immutable data "
             , ";  structure with a mutable cache.              "
             , ";                                               "
             , ";  Mutability isn't arbitrary; for example, atomic "
             , ";  values—like integers, Booleans, characters, and "
             , ";  enumeration literals—are expected to be immutable. "
             , ";  Aggregate values like strings, arrays, and records, "
             , ";  which store other values inside, are often   "
             , ";  mutable—though in functional languages, strings and "
             , ";  records are typically immutable, while arrays "
             , ";  typically appear in both mutable and immutable forms. "
             , ";                                               "
             , ";  Mutable representations normally require less "
             , ";  allocation than immutable representations and so can "
             , ";  be less expensive; for example, a mutable binary "
             , ";  search tree can be updated with at most constant "
             , ";  allocation. But a mutable representation demands a "
             , ";  mutable abstraction, and compared with immutable "
             , ";  abstractions, mutable abstractions are harder to test "
             , ";  and can lead to more bugs. In particular, when "
             , ";  mutable data is shared among different parts of a "
             , ";  program, one part can make a change that another "
             , ";  wasn't expecting. (``Not found? But that key was in "
             , ";  the table!'') With immutable abstractions, these "
             , ";  sorts of bugs can't happen.                  "
             , ";                                               "
             , ";  General-purpose abstractions should often be designed "
             , ";  in both mutable and immutable forms. For example, if "
             , ";  I'm using an association list to represent an "
             , ";  environment in an interpreter, I want an immutable "
             , ";  [[alist]]—that's going to make it easy to implement "
             , ";  [[let]] expressions and closures. But if I'm using an "
             , ";  association list to implement a sparse array, I want "
             , ";  a mutable [[alist]]—that's going to reduce the "
             , ";  allocation cost of an update from linear to constant. "
             , ";  If you design mutable and immutable abstractions in "
             , ";  tandem, you'll always have the one you want when you "
             , ";  want it.                                     "
             , ";                                               "
             , ";  Abstractions can be designed in tandem in part "
             , ";  because mutability is surprisingly independent of the "
             , ";  designer's other big choice: what operations "
             , ";  to provide. Mutability rarely affects what operations "
             , ";  are implemented; for example, whether it is mutable "
             , ";  or immutable, a dictionary abstraction needs to "
             , ";  implement the same operations: insert, lookup, "
             , ";  update, and delete. The same is true of a stack, "
             , ";  a priority queue, an array, and many other   "
             , ";  abstractions. Mutability does affect costs; the cost "
             , ";  of each operation, called the cost model, often "
             , ";  depends both on mutability and on the representation "
             , ";  the designer has in mind.                    "
             , ";                                               "
             , ";  A cost model determines what operations will be "
             , ";  cheap, what operations will be expensive, and what "
             , ";  operations won't be implemented at all. Identifying "
             , ";  the right operations at the right cost calls for "
             , ";  techniques from beyond the world of programming "
             , ";  languages. But once you have a set of operations, "
             , ";  programming-language techniques can help you see if "
             , ";  it's a good one: You can analyze the operations "
             , ";  according to their classification (\\chaprefpage "
             , ";  ,scheme.constructor-observer-mutator), which is "
             , ";  closely related to classification of rules in type "
             , ";  theory (sidebar, \\cpageref                   "
             , ";  typesys.sidebar.intro-elim).                 "
             , ";                                               "
             , ";    • Every interface needs an operation that creates a "
             , ";   new value of abstract type: a creator.      "
             , ";    • Most interfaces need operations that can take an "
             , ";   existing value of abstract type and do something "
             , ";   with it. An operation that updates a value in "
             , ";   place is a mutator; one that uses an existing "
             , ";   value to make a new value is a producer. One "
             , ";   example is insertion into a dictionary: a mutable "
             , ";   dictionary would implement insertion as a   "
             , ";   mutator, and an immutable dictionary would  "
             , ";   implement it as a producer.                 "
             , ";    • Every interface needs operations that get "
             , ";   information out of a value of abstract type:  "
             , ";   observers. As examples, observers in my digital "
             , ";   video recorder tell me whether a game has   "
             , ";   started, whether it's over, and who's playing. \\ "
             , ";   pbreak                                      "
             , ";                                               "
             , ";  A classification of operations can be used to ask if "
             , ";  a design is complete:                        "
             , ";                                               "
             , ";   \\tightlist                                  "
             , ";    • Can every part of the abstraction be observed? "
             , ";    • Can every mutation be observed?        "
             , ";    • Can the effect of every producer be observed? "
             , ";    • Can every observable be mutated (or changed by a "
             , ";   producer)?                                  "
             , ";                                               "
             , ";  A design with these properties probably meets "
             , ";  clients' needs—and can be tested without having to "
             , ";  violate abstraction.                         "
             , ";                                               "
             , ";  Case study in design \\chaptocsplitand specification: "
             , ";  \\chaptocsplitThe array list                  "
             , ";                                               "
             , ";  [*] Once you've chosen your abstraction and its "
             , ";  operations, it's time to write the complete API. The "
             , ";  abstraction is central: it dictates the language you "
             , ";  use to specify the behavior of each operation. When "
             , ";  precision is important, the abstraction and the "
             , ";  language should be mathematical, so operations can be "
             , ";  described with mathematical precision. As an example, "
             , ";  I present what \\java calls an [[ArrayList]]: "
             , ";  a mutable sequence that provides constant-time "
             , ";  indexing but can also grow and shrink.       "
             , ";                                               "
             , ";  The abstraction is a sequence of elements, each of "
             , ";  type [[elem]]. The elements are numbered     "
             , ";  sequentially, and the number of the first element is "
             , ";  part of the abstraction. I write the abstraction as \\ "
             , ";  arrayabsk \\vs, where \\vs is a sequence of values and "
             , ";  k is the index of the first value.           "
             , ";                                               "
             , ";  The abstraction looks like an array, but it can grow "
             , ";  or shrink at either end, in constant amortized time. "
             , ";  I plan a single creator, operation [[from]], which "
             , ";  takes one argument k and returns \\arrayabsk \\ "
             , ";  emptylist, the empty array starting at k. I don't "
             , ";  plan any producers; rather than produce a new array "
             , ";  from an existing array, I plan to update arrays in "
             , ";  place.                                       "
             , ";                                               "
             , ";  Update is implemented by mutator [[at-put]]. Growth "
             , ";  and shrinkage are implemented by mutators [[addlo]], "
             , ";  [[addhi]], [[remlo]], and [[remhi]]. Individual "
             , ";  elements are observed by [[at]], and the size and "
             , ";  bounds of the array are observed by [[size]], [[lo]], "
             , ";  and [[nexthi]].[*]                           "
             , ";  <arraylist.mcl>=                             "
             , "(module-type ARRAYLIST"
             , "  (exports [abstype t]"
             , "           [abstype elem]"
             ,
     "           [from   : (int -> t)]           ; creator (from initial index)"
             , "           [size   : (t -> int)]           ; observer"
             , "           [at     : (t int -> elem)]      ; observer"
             , "           [at-put : (t int elem -> unit)] ; mutator"
             , ""
             , "           [lo     : (t -> int)]       ; observer"
             , "           [nexthi : (t -> int)]       ; observer"
             , "           [addlo  : (t elem -> unit)] ; mutator"
             , "           [addhi  : (t elem -> unit)] ; mutator"
             , "           [remlo  : (t -> elem)]      ; mutator"
             , "           [remhi  : (t -> elem)]))    ; mutator"
             , ";  <arraylist.mcl>=                             "
             , "(generic-module"
             ,
        "   [ArrayList : ([Elem : (exports [abstype t])] --m-> (allof ARRAYLIST"
             ,
"                                                               (exports [type elem Elem.t])))]"
             , "   (module A (@m Array Elem))"
             , "   (module U (@m UnsafeArray Elem))"
             , "   (record-module Rep t ([elems : A.t]"
             , "                         [low-index : int]"
             , "                         [population : int]"
             , "                         [low-stored : int]))"
             , "   (type t Rep.t)"
             , "   (type elem Elem.t)"
             , ""
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define t from ([i : int])"
             , "     (Rep.make (U.new 3) i 0 0))"
             , ""
             , "   (define int size ([a : t]) (Rep.population a))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define bool in-bounds? ([a : t] [i : int])"
             , "     (if (>= i (Rep.low-index a))"
             , "         (< (- i (Rep.low-index a)) (Rep.population a))"
             , "         #f))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define int internal-index ([a : t] [i : int])"
             , "     (let* ([k (+ (Rep.low-stored a) (- i (Rep.low-index a)))]"
             ,
          "            [_ (when (< k 0) (error 'internal-error: 'array-index))]"
             , "            [n (A.size (Rep.elems a))]"
             , "            [idx (if (< k n) k (- k n))])"
             , "       idx))"
             , ""
             , "   (define elem at ([a : t] [i : int])"
             , "     (if (in-bounds? a i)"
             , "         (A.at (Rep.elems a) (internal-index a i))"
             , "         (error 'array-index-out-of-bounds)))"
             , ""
             , "   (define unit at-put ([a : t] [i : int] [v : elem])"
             , "     (if (in-bounds? a i)"
             , "         (A.at-put (Rep.elems a) (internal-index a i) v)"
             , "         (error 'array-index-out-of-bounds)))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define int lo     ([a : t]) (Rep.low-index a))   "
             ,
     "   (define int nexthi ([a : t]) (+ (Rep.low-index a) (Rep.population a)))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define unit maybe-grow ([a : t])"
             , "     (when (>= (size a) (A.size (Rep.elems a)))"
             , "       (let* ([n  (A.size (Rep.elems a))]"
             , "              [n' (if (Int.= n 0) 8 (Int.* 2 n))]"
             , "              [new-elems (U.new n')]"
             , "              [start (lo a)]"
             , "              [limit (nexthi a)]"
             , "              [i 0]"
             ,
              "              [_ (while (< start limit)      ; copy the elements"
             , "                   (A.at-put new-elems i (at a start))"
             , "                   (set i (+ i 1))"
             , "                   (set start (+ start 1)))])"
             , "         (Rep.set-elems! a new-elems)"
             , "         (Rep.set-low-stored! a 0))))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define unit addhi ([a : t] [v : elem])"
             , "     (maybe-grow a)"
             , "     (let ([i (nexthi a)])"
             , "        (Rep.set-population! a (+ (Rep.population a) 1))"
             , "        (at-put a i v)))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define unit addlo ([a : t] [v : elem])"
             , "     (maybe-grow a)"
             , "     (Rep.set-population! a (+ (Rep.population a) 1))"
             , "     (Rep.set-low-index!  a (- (Rep.low-index a)  1))"
             , "     (Rep.set-low-stored! a (- (Rep.low-stored a) 1))"
             , "     (when (< (Rep.low-stored a) 0)"
             ,
 "       (Rep.set-low-stored! a (+ (Rep.low-stored a) (A.size (Rep.elems a)))))"
             , "     (at-put a (Rep.low-index a) v))"
             , ";     \\qtrim1                                      "
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define elem remhi ([a : t])"
             , "     (if (<= (Rep.population a) 0)"
             , "         (error 'removal-from-empty-array)"
             , "         (let* ([v (at a (- (nexthi a) 1))]"
             ,
         "                [_ (Rep.set-population! a (- (Rep.population a) 1))])"
             , "           v)))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define elem remlo ([a : t])"
             , "     (if (<= (Rep.population a) 0)"
             , "         (error 'removal-from-empty-array)"
             , "         (let* ([v (at a (lo a))]"
             ,
          "                [_ (Rep.set-population! a (- (Rep.population a) 1))]"
             , "                [_ (Rep.set-low-index! a (+ (lo a) 1))]"
             ,
          "                [_ (Rep.set-low-stored! a (+ (Rep.low-stored a) 1))]"
             ,
    "                [_ (when (Int.= (Rep.low-stored a) (A.size (Rep.elems a)))"
             , "                       (Rep.set-low-stored! a 0))])"
             , "           v)))"
             , ";     <definitions of operations in [[ArrayList]]>= "
             , "   (define unit setlo ([a : t] [i : int])"
             , "     (Rep.set-low-index! a i))"
             , ""
             , ")"
             , ";  \\qbreak The full [[Array]] module is built from "
             , ";  [[ArrayCore]].                               "
             , ";  <predefined Molecule types, functions, and modules>= "
             , "(generic-module"
             , "   [Array : ([M : (exports [abstype t])] --m-> "
             , "                (allof ARRAY (exports (type elem M.t))))]"
             , "   (module A (@m ArrayCore M))"
             , "   (type t A.t)"
             , "   (type elem M.t)"
             , "   (val new A.new)"
             , "   (val empty A.empty)"
             , "   (val at A.at)"
             , "   (val size A.size)"
             , "   (val at-put A.at-put))"
             , ";  \\qbreak Module [[Ref]] is implemented using an array "
             , ";  of size 1.                                   "
             , ";  <predefined Molecule types, functions, and modules>= "
             , "(generic-module"
             , "   [Ref : ([M : (exports [abstype t])] --m->"
             , "                  (exports [abstype t]"
             , "                           [new : (M.t -> t)]"
             , "                           [!   : (t -> M.t)]"
             , "                           [:=  : (t M.t -> unit)]))]"
             , "  (module A (@m ArrayCore M))"
             , "  (type t A.t)"
             , "  (define t    new ([x : M.t])  (A.new 1 x))"
             , "  (define M.t  !   ([cell : t]) (A.at cell 0))"
             , "  (define unit :=  ([cell : t] [x : M.t]) (A.at-put cell 0 x)))"
              ] 
      val xdefs = stringsxdefs ("predefined types etc", predefinedTypes)
  in  readEvalPrintWith predefinedFunctionError
                        (xdefs, initialBasis, noninteractive)
  end
(* \qbreak The initial basis is defined in two steps. *)
(* First, all the primitive modules are added to an *)
(* empty basis. Then the predefined things are added. *)
(* <boxed values 235>=                          *)
val _ = op initialBasis : basis
(* \makenowebnotdef (from chunk [->])           *)

(* <primitive modules and definition of [[initialBasis]] ((elided))>= *)
(* This code cheats the typical command-line options that are
   used to dump information about the initial basis, primitives, etc *)
val primitiveBasis = emptyBasis (* lies *)
val predefs = [] (* damn lies *)
val () =
  if hasOption "basis" then
    let fun show (x, c) = app print [whatdec c, " ", x, "\n"]
    in  app show (fst initialBasis)
    end
  else
    ()


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNSTREAM]], WHICH EVALUATES INPUT GIVEN [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* <function [[runStream]], which evaluates input given [[initialBasis]]>= *)
fun runStream inputName input interactivity basis = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs (inputName, input, prompts)
  in  readEvalPrintWith eprintln (xdefs, basis, interactivity)
  end 
(* <boxed values 162>=                          *)
val _ = op withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
(* A bridge-language interpreter can be run on standard *)
(* input or on a named file. Either one can be converted *)
(* to a stream, so the code that runs an interpreter is *)
(* defined on a stream, by function [[runStream]]. This *)
(* runs the code found in a given, named input, using a *)
(* given interactivity mode. The interactivity mode *)
(* determines both the error format and the prompts. *)
(* Function [[runStream]] then starts the       *)
(* read-eval-print loop, using the initial basis. [*] \ *)
(* nwnarrowboxes                                *)
(* <boxed values 162>=                          *)
val _ = op runStream : string -> TextIO.instream -> interactivity -> basis ->
                                                                           basis


(*****************************************************************)
(*                                                               *)
(*   LOOK AT COMMAND-LINE ARGUMENTS, THEN RUN                    *)
(*                                                               *)
(*****************************************************************)

(* <look at command-line arguments, then run>=  *)
fun runPathWith interactivity ("-", basis) =
      runStream "standard input" TextIO.stdIn interactivity basis
  | runPathWith interactivity (path, basis) =
      let val fd = TextIO.openIn path
      in  runStream path fd interactivity basis
          before TextIO.closeIn fd
      end 
(* (Function [[runStream]] gets its own code chunk *)
(* because the \usm interpreter needs a slightly *)
(* different version.)                          *)
(*                                              *)
(* If files are named on a command line, each file is *)
(* passed to function [[runPathWith]]. This function *)
(* opens the named file and calls [[runStream]]. And in *)
(* a special hack, relatively common on Unix systems, *)
(* the name [[-]] stands for standard input.    *)
(*                                              *)
(* <boxed values 163>=                          *)
val _ = op runPathWith : interactivity -> (string * basis -> basis)
(* <look at command-line arguments, then run>=  *)
val usage = ref (fn () => ())
(* If an interpreter doesn't recognize a command-line *)
(* option, it can print a usage message. A usage-message *)
(* function needs to know the available options, but *)
(* each available option is associated with a function *)
(* that performs an action, and if something goes wrong, *)
(* the action function might need to call the usage *)
(* function. I resolve this mutual recursion by first *)
(* allocating a mutual cell to hold the usage function, *)
(* then updating it later. This is also how [[letrec]] *)
(* is implemented in micro-Scheme.              *)
(* <boxed values 164>=                          *)
val _ = op usage : (unit -> unit) ref
(* \qbreak To represent actions that might be called for *)
(* by command-line options, I define type [[action]]. *)
(* <look at command-line arguments, then run>=  *)
datatype action
  = RUN_WITH of interactivity  (* call runPathWith on remaining arguments *)
  | DUMP     of unit -> unit   (* dump information *)
  | FAIL     of string         (* signal a bad command line *)
  | DEFAULT                    (* no command-line options were given *)
(* The default action is to run the interpreter in its *)
(* most interactive mode.                       *)
(* <look at command-line arguments, then run>=  *)
val default_action = RUN_WITH (PROMPTING, ECHOING)
(* <look at command-line arguments, then run>=  *)
fun perform (RUN_WITH interactivity, []) =
      perform (RUN_WITH interactivity, ["-"])
  | perform (RUN_WITH interactivity, args) =
      ignore (foldl (runPathWith interactivity) initialBasis args)
  | perform (DUMP go, [])     = go ()
  | perform (DUMP go, _ :: _) = perform (FAIL "Dump options take no files", [])
  | perform (FAIL msg, _)     = (eprintln msg; !usage())
  | perform (DEFAULT, args)   = perform (default_action, args)
(* An action is performed by function [[perform]]. *)
(* Not every action makes sense with arguments. *)
(* <boxed values 165>=                          *)
val _ = op perform: action * string list -> unit
(* <look at command-line arguments, then run>=  *)
fun merge (_, a as FAIL _) = a
  | merge (a as FAIL _, _) = a
  | merge (DEFAULT, a) = a
  | merge (_, DEFAULT) = raise InternalError "DEFAULT on the right in MERGE"
  | merge (RUN_WITH _, right as RUN_WITH _) = right
  | merge (DUMP f, DUMP g) = DUMP (g o f)
  | merge (_, r) = FAIL "Interpret or dump, but don't try to do both"
(* When command-line options call for multiple actions, *)
(* those actions are merged by function [[merge]]. *)
(* Options are processed left to right, and actions are *)
(* merged in the same order. The initial action is *)
(* always [[DEFAULT]], which can appear only on the *)
(* left. For most actions, the rightmost action takes *)
(* precedence, but merging two [[DUMP]] actions performs *)
(* them both.                                   *)
(*                                              *)
(* <boxed values 166>=                          *)
val _ = op merge: action * action -> action
(* <look at command-line arguments, then run>=  *)
val actions =
  [ ("",    RUN_WITH (PROMPTING,     ECHOING))
  , ("-q",  RUN_WITH (NOT_PROMPTING, ECHOING))
  , ("-qq", RUN_WITH (NOT_PROMPTING, NOT_ECHOING))
  , ("-names",      DUMP (fn () => dump_names initialBasis))
  , ("-primitives", DUMP (fn () => dump_names primitiveBasis))
  , ("-help",       DUMP (fn () => !usage ()))
  ]
                                                          (*OMIT*)
val unusedActions =  (* reveals answers to homeworks *)   (*OMIT*)
  [ ("-predef",     DUMP (fn () => app println predefs))  (*OMIT*)
  ]                                                       (*OMIT*)
(* Each possible command-line option is associated with *)
(* an action. Options [[-q]] and [[-qq]] suppress *)
(* prompts and echos. Options [[-names]] and    *)
(* [[-primitives]] dump information found in the initial *)
(* basis.                                       *)
(* <boxed values 167>=                          *)
val _ = op actions : (string * action) list
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <look at command-line arguments, then run>=  *)
val _ = usage := (fn () =>
  ( app eprint ["Usage:\n"]
  ; app (fn (option, action) =>
         app eprint ["       ", CommandLine.name (), " ", option, "\n"]) actions
  ))
(* <look at command-line arguments, then run>=  *)
fun action option =
  case List.find (curry op = option o fst) actions
    of SOME (_, action) => action
     | NONE => FAIL ("Unknown option " ^ option)
(* <boxed values 168>=                          *)
val _ = op action : string -> action
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <look at command-line arguments, then run>=  *)
fun strip_options a [] = (a, [])
  | strip_options a (arg :: args) =
      if String.isPrefix "-" arg andalso arg <> "-" then
          strip_options (merge (a, action arg)) args
      else
          (a, arg :: args)

val _ = if hasOption "NORUN" then ()
        else perform (strip_options DEFAULT (CommandLine.arguments ()))
(* A complete command-line is processed by computing the *)
(* action associated with the command-line options, then *)
(* performing that action with the remaining    *)
(* command-line arguments. Unless option [[NORUN]] is *)
(* present in the [[BPCOPTIONS]] environment variable. *)
(* <boxed values 169>=                          *)
val _ = op strip_options : action -> string list -> action * string list
(* User-defined, algebraic types \unsecsplit(and pattern *)
(* matching)                                    *)
(*                                              *)
(* [*][*] \invisiblelocaltableofcontents[*]     *)
(*                                              *)
(* \typesystemuml                               *)
(*                                              *)
(* {epigraph}Fred Brooks, The Mythical Man-Month, *)
(* Chapter 9 Representation is the essence of   *)
(* programming. ... \penalty-200 Much more often, *)
(* strategic breakthrough will come from redoing the *)
(* representation of the data or the tables. This is *)
(* where the heart of a program lies. Show me your *)
(* flowcharts and conceal your tables, and I shall *)
(* continue to be mystified. Show me your tables, and I *)
(* won't usually need your flowcharts; they'll be *)
(* obvious. {epigraph}                          *)
(*                                              *)
(* \Crefrangeimpcore.chapml.chap don't give us many ways *)
(* to organize data. S-expressions are great, but you *)
(* might have noticed that they serve as a kind of *)
(* high-level assembly language on top of which you have *)
(* to craft your own data structures. For programming at *)
(* scale, that's not good enough—programmers need to *)
(* define proper data structures whose shapes and *)
(* contents are known. Proper data-definition mechanisms *)
(* must be able to express these possibilities: *)
(*                                              *)
(*  \tightlist                                  *)
(*   • Data can take multiple forms, like a C [[union]]. *)
(*   • Data can have multiple parts, like a C  *)
(*  [[struct]].                                 *)
(*   • One or more parts may be like the whole, that is, *)
(*  data can be recursive.                      *)
(*                                              *)
(* All these possibilities can be expressed using *)
(* algebraic data types. Algebraic data types,  *)
(* supplemented by the base types, function types, and *)
(* array types shown in previous chapters, suffice to *)
(* describe and typecheck representations of data at any *)
(* scale. They are ubiquitous in the ML family and in *)
(* languages derived from it, including Standard ML, \ *)
(* ocaml, Haskell, \agda, \gallina, and \idris. *)
(*                                              *)
(* Algebraic data types can be added to any language; *)
(* this chapter adds them to \nml, making the new *)
(* language uML. To add algebraic data types requires a *)
(* new species of value, a new expression form for *)
(* looking at the values, and a new definition form for *)
(* introducing the types and values.            *)
(*                                              *)
(* The new species of value is a constructed value. *)
(* A constructed value is made by applying some value *)
(* constructor to zero or more other values. In the *)
(* syntax, however, zero-argument value constructors *)
(* aren't applied; a zero-argument value constructor is *)
(* a value all by itself. For example, [['()]] is a *)
(* value constructor for lists, and it expects no *)
(* arguments, so it is a constructed value. And [[cons]] *)
(* is also a value constructor for lists, but it expects *)
(* arguments, so to make a constructed value, [[cons]] *)
(* must be applied to two other values: an element and a *)
(* list.                                        *)
(*                                              *)
(* A constructed value is interrogated, observed, or *)
(* eliminated scrutinized by a case expression. A case *)
(* expression provides concise, readable syntax for *)
(* asking a key question about any datum: how was it *)
(* formed, and from what parts? A case expression gets *)
(* the answer by using patterns: a pattern can match a *)
(* particular value constructor, and when it does, it *)
(* can name each of the values to which the constructor *)
(* was applied. For example, the pattern \monobox(cons y *)
(* ys) matches any cons cell, and when it matches, it *)
(* binds the name [[y]] to the [[car]] and [[ys]] to the *)
(* [[cdr]].                                     *)
(*                                              *)
(* Case expressions and pattern matching eliminate the *)
(* clutter associated with functions like [[null?]], *)
(* [[car]], [[cdr]], [[fst]], and [[snd]]. Instead of *)
(* using such functions, you lay out the possible forms *)
(* of the data, and for each form, you name the parts *)
(* directly. The resulting code is short and clear, and *)
(* it operates at a higher level of abstraction than *)
(* Scheme code or C code. With the right syntactic *)
(* sugar, your code can look a lot like algebraic laws ( *)
(* \crefadt.sugar).                             *)
(*                                              *)
(* A case expression inspects a scrutinee, and it *)
(* includes a sequence of choices, each of which *)
(* associates a pattern with a right-hand side. And if *)
(* the choices don't cover all possible cases,  *)
(* a compiler can tell you what case you left out (\cref *)
(* adt.ex.exhaustiveness-check). As an example, a case *)
(* expression can be used to see if a list is empty; the *)
(* scrutinee is the formal parameter [[xs]], and there *)
(* are two choices: one for each form of list. \ *)
(* addboxumlnull? : (forall ['a] ((list 'a) -> bool)) *)

