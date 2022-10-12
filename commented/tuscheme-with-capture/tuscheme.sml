(* <tuscheme.sml>=                              *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE CHECKING             *)
(*                                                               *)
(*****************************************************************)

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
(* <boxed values 87>=                           *)
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

(* Each micro-Scheme name is bound to a location that *)
(* contains a value. In C, such a location is   *)
(* represented by a pointer of C type \monoboxValue *. *)
(* In ML, such a pointer has type \monoboxvalue ref. *)
(* Like a C pointer, an ML [[ref]] can be read from and *)
(* written to, but unlike a C pointer, it can't be added *)
(* to or subtracted from.                       *)
(*                                              *)
(* In C, the code that looks up or binds a name has to *)
(* know what kind of thing a name stands for; that's why *)
(* the Impcore interpreter uses one set of environment *)
(* functions for value environments xi and rho and *)
(* another set for a function environment phi. In ML, *)
(* the code that looks up or binds a name is independent *)
(* of what a name stands for; it is naturally   *)
(* polymorphic. One set of polymorphic functions *)
(* suffices to implement environments that hold *)
(* locations, values, or types.                 *)
(*                                              *)
(* ML has a static type system, and polymorphism is *)
(* reflected in the types. An environment has type \ *)
(* monobox'a env; such an environment binds each name in *)
(* its domain to a value of type [['a]]. The [['a]] is *)
(* called a type parameter or type variable; it stands *)
(* for an unknown type. (Type parameters are explained *)
(* in detail in \creftypesys.tuscheme, where they have *)
(* an entire language devoted to them.) Type \monobox'a *)
(* env, like any type that takes a type parameter, can *)
(* be instantiated at any type; instantiation   *)
(* substitutes a known type for every occurrence of  *)
(* [['a]]. micro-Scheme's environment binds each name to *)
(* a mutable location, and it is obtained by    *)
(* instantiating type \monobox'a env using \nomathbreak\ *)
(* monobox'a = \monoboxvalue ref; the resulting type is *)
(* \monoboxvalue ref env.                       *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \advanceby 0.8pt \newskip\myskip \myskip=6pt *)
(*                                              *)
(*    Semantics   Concept        Interpreter    *)
(*        d       Definition     def (\cpageref *)
(*                               mlscheme.type.def) *)
(*        e       Expression     \mlstypeexp    *)
(*        x       Name           \mlstypename   *)
(*   [\myskip] v  Value          \mlstypevalue  *)
(*        l       Location       \monovalue ref (ref is *)
(*                               built into ML) *)
(*       rho      Environment    \monovalue ref env (\ *)
(*                               cpagerefmlscheme.type.env) *)
(*      sigma     Store          Machine memory (the *)
(*                               ML heap)       *)
(*                    Expression     \monoboxeval(e, rho) = *)
(*   [\myskip] \      evaluation     v, \break with sigma *)
(*   evale ==>\                      updated to sigma' \ *)
(*   evalr['] v                      mlsfunpageeval *)
(*                                              *)
(*                    Definition     \monoboxevaldef(d, rho *)
(*  <d,rho,sigma>     evaluation     ) = (rho', s), \break *)
(*   --><rho',                       with sigma updated to  *)
(*     sigma'>                       sigma' \mlsfunpage *)
(*                                   evaldef    *)
(*                                              *)
(*                Definedness    \monofind (x, rho) *)
(*   [\myskip] x                 terminates without raising *)
(*   in dom rho                  an exception (\cpageref *)
(*                               mlscheme.fun.find) *)
(*     rho(x)     Location       \monofind (x, rho) (\ *)
(*                lookup         cpagerefmlscheme.fun.find) *)
(*  sigma(rho(x)) Value lookup   \mono!(find (x, rho)) (\ *)
(*                               cpagerefmlscheme.fun.find) *)
(*   rho{x |->l}  Binding        \monobind (x, l, rho) (\ *)
(*                               cpagerefmlscheme.fun.bind) *)
(*          \     Allocation     call \monoboxref v; the *)
(*      centering                result is l    *)
(*      sigma{l|                                *)
(*       ->v}, \                                *)
(*        break                                 *)
(*      where l\                                *)
(*      notindom                                *)
(*        sigma                                 *)
(*                                              *)
(*          \     Store update   \monol := v    *)
(*      centering                               *)
(*      sigma{l|                                *)
(*       ->v}, \                                *)
(*        break                                 *)
(*      where lin                               *)
(*      dom sigma                               *)
(*                                              *)
(*                                              *)
(* Correspondence between micro-Scheme semantics and ML *)
(* code [*]                                     *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* My environments are implemented using ML's native *)
(* support for lists and pairs. Although my C code *)
(* represents an environment as a pair of lists, in ML, *)
(* it's easier and simpler to use a list of pairs. The *)
(* type of the list is \monobox(name * 'a) list; *)
(* the type of a single pair is \monoboxname * 'a. *)
(* A pair is created by an ML expression of the form \ *)
(* monobox(e_1, e_2); this pair contains the value of  *)
(* e_1 and the value of e_2. The pair \monobox(e_1, e_2) *)
(* has type \monoboxname * 'a if e_1 has type [[name]] *)
(* and e_2 has type [['a]]. \mlslabelenv        *)
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
(* <boxed values 13>=                           *)
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
(* <boxed values 13>=                           *)
val _ = op find : name * 'a env -> 'a
(* \mlsflabelfind                               *)

(* Again using [[::]], function [[bind]] adds a new *)
(* binding to an existing environment. Unlike \cref *)
(* scheme.chap's [[bind]], it does not allocate a *)
(* mutable reference cell.                      *)
(* <boxed values 13>=                           *)
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
(* <boxed values 13>=                           *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* <boxed values 13>=                           *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* <support for names and environments>=        *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* <boxed values 37>=                           *)
val _ = op duplicatename : name list -> name option
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
(* <boxed values 35>=                           *)
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
(* <boxed values 36>=                           *)
val _ = op optionList : 'a option list -> 'a list option
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

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
(* <boxed values 27>=                           *)
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
(* To separate items by spaces or commas, interpreters *)
(* use [[spaceSep]] and [[commaSep]], which are special *)
(* cases of the basis-library function          *)
(* [[String.concatWith]].                       *)
(*                                              *)
(* <boxed values 28>=                           *)
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
(* <boxed values 29>=                           *)
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
(* <boxed values 30>=                           *)
val _ = op fnvHash : string -> int
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

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
(* <boxed values 31>=                           *)
val _ = op withXprinter : (string -> unit) -> ('a -> 'b) -> ('a -> 'b)
val _ = op tryFinally   : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b
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

(* Representing error outcomes as values        *)
(*                                              *)
(* When an error occurs, especially during evaluation, *)
(* the best and most convenient thing to do is often to *)
(* raise an ML exception, which can be caught in a *)
(* handler. But it's not always easy to put a handler *)
(* exactly where it's needed. To get the code right, it *)
(* may be better to represent an error outcome as a *)
(* value. Like any other value, such a value can be *)
(* passed and returned until it reaches a place where a *)
(* decision is made.                            *)
(*                                              *)
(*   • When representing the outcome of a unit test, an *)
(*  error means failure for [[check-expect]] but *)
(*  success for [[check-error]]. Rather than juggle *)
(*  ``exception'' versus ``non-exception,'' I treat *)
(*  both outcomes on the same footing, as values. *)
(*  Successful evaluation to produce bridge-language *)
(*  value v is represented as ML value \monoOK v. *)
(*  Evaluation that signals an error with message m *)
(*  is represented as ML value \monoERROR m.    *)
(*  Constructors [[OK]] and [[ERROR]] are the value *)
(*  constructors of the algebraic data type     *)
(*  [[error]], defined here:                    *)
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
(* <boxed values 39>=                           *)
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
(* <boxed values 40>=                           *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* <support for representing errors as \ml\ values>= *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* Sometimes I map an error-producing function over a *)
(* list of values to get a list of [['a error]] results. *)
(* Such a list is hard to work with, and the right thing *)
(* to do with it is to convert it to a single value *)
(* that's either an [['a list]] or an error. In my code, *)
(* the conversion operation is called [[errorList]]. [ *)
(* Haskell calls it [[sequence]].] It is implemented by *)
(* folding over the list of possibly erroneous results, *)
(* concatenating all error messages.            *)
(* <boxed values 41>=                           *)
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
(* <boxed values 72>=                           *)
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
(* <boxed values 32>=                           *)
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
(* <boxed values 38>=                           *)
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
(* <boxed values 33>=                           *)
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
(* <boxed values 34>=                           *)
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
(* <boxed values 48>=                           *)
type 'a susp = 'a susp
(* <suspensions>=                               *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* <boxed values 49>=                           *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
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
(* Even though its representation uses mutable state *)
(* (the suspension), the stream is an immutable *)
(* abstraction. [When debugging, I~sometimes violate the *)
(* abstraction and look at the state of a [[SUSPENDED]] *)
(* stream.] To observe that abstraction, call   *)
(* [[streamGet]]. This function performs whatever *)
(* actions are needed either to produce a pair holding *)
(* an element an a stream (represented as \monoSOME (x, *)
(* xs)) or to decide that the stream is empty and no *)
(* more elements can be produced (represented as *)
(* [[NONE]]).                                   *)
(* <boxed values 50>=                           *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* The simplest way to create a stream is by using the *)
(* [[:::]] or [[EOS]] constructor. A stream can also be *)
(* created from a list. When such a stream is read, no *)
(* new actions are performed.                   *)
(* <boxed values 50>=                           *)
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
(* <boxed values 51>=                           *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 51>=                           *)
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
(* <boxed values 52>=                           *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* Function [[streamOfEffects]] can be used to produce a *)
(* stream of lines from an input file:          *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 53>=                           *)
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
(* <boxed values 54>=                           *)
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
(* <boxed values 55>=                           *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* Primitive functions of \tuschemeheader       *)
(*                                              *)
(* [*] The primitives resemble the primitives in *)
(* Chapter [->], except that each primitive comes with a *)
(* type as well as a value.                     *)
(*                                              *)
(* A comparison takes two arguments. Most comparisons *)
(* (but not equality) apply only to integers.   *)

(* <streams>=                                   *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* <boxed values 56>=                           *)
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
(* <boxed values 57>=                           *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 57>=                           *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* <boxed values 58>=                           *)
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
(* <boxed values 59>=                           *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 60>=                           *)
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
(* <boxed values 61>=                           *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* Concatenation turns a stream of streams of tau's into *)
(* a single stream of tau's. I define it using a *)
(* [[streamOfUnfold]] with a two-part state: the first *)
(* element of the state holds an initial [[xs]], \qbreak *)
(* and the second part holds the stream of all remaining *)
(* streams, [[xss]]. To concatenate the stream of *)
(* streams [[xss]], I use an initial state of [[(EOS, *)
(* xss)]].                                      *)
(* <boxed values 61>=                           *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* In list and stream processing, [[concat]] is very *)
(* often composed with [[map f]]. The composition is *)
(* usually called [[concatMap]].                *)
(* <boxed values 62>=                           *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* The code used to append two streams is much like the *)
(* code used to concatenate arbitrarily many streams. *)
(* To avoid duplicating the tricky manipulation of *)
(* states, I implement append using concatenation. *)
(* <boxed values 63>=                           *)
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
(* <boxed values 64>=                           *)
val _ = op streamTake : int * 'a stream -> 'a list
(* <streams>=                                   *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* Once I've used [[streamTake]], I get the rest of the *)
(* stream with [[streamDrop]] (\chunkref        *)
(* mlinterps.chunk.use-streamDrop).             *)
(* <boxed values 65>=                           *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* <stream transformers and their combinators>= *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* <boxed values 83>=                           *)
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
(* <boxed values 84>=                           *)
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
(* <boxed values 85>=                           *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 86>=                           *)
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
(* <boxed values 88>=                           *)
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
(* <boxed values 89>=                           *)
val _ = op pzero : ('a, 'b) xformer
(* This parser obeys the algebraic law          *)
(*                                              *)
(*  \monoboxt <|> pzero = \monoboxpzero <|> t = \ *)
(*  monoboxt\text.                              *)
(*                                              *)
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)
(* <boxed values 89>=                           *)
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
(* <boxed values 90>=                           *)
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
(* <boxed values 91>=                           *)
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
(* <boxed values 92>=                           *)
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
(* <boxed values 93>=                           *)
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
(* <boxed values 94>=                           *)
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
(* <boxed values 95>=                           *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 96>=                           *)
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
(* <boxed values 97>=                           *)
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
(* <boxed values 98>=                           *)
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
(* <boxed values 99>=                           *)
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
(* <boxed values 100>=                          *)
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
(* <boxed values 101>=                          *)
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
(* <boxed values 102>=                          *)
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
(* <boxed values 103>=                          *)
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
(* <boxed values 67>=                           *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 67>=                           *)
type 'a located = 'a located
(* <support for source-code locations and located streams>= *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* In addition to exceptions that I have defined, *)
           (* [[atLoc]] also recognizes and wraps some of Standard *)
           (* ML's predefined exceptions. Handlers for even more *)
           (* exceptions, like [[TypeError]], can be added using *)
           (* Noweb.                                       *)
           (* <more handlers for [[atLoc]]>=               *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
           (* <more handlers for [[atLoc]] ((type-checking))>= *)
           | e as TypeError _         => raise Located (loc, e)
           | e as BugInTypeChecking _ => raise Located (loc, e)
(* <boxed values 68>=                           *)
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
(* <boxed values 69>=                           *)
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
(* <boxed values 70>=                           *)
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
(* <boxed values 71>=                           *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 71>=                           *)
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
(* <boxed values 112>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* <boxed values 112>=                          *)
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
(* <boxed values 104>=                          *)
type 'a lexer = 'a lexer
(* The type [['a lexer]] should be pronounced ``lexer *)
(* returning [['a]].''                          *)

(* <support for lexical analysis>=              *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* <boxed values 105>=                          *)
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
(* <boxed values 106>=                          *)
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
(* <boxed values 107>=                          *)
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
(* <boxed values 108>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 109>=                          *)
val _ = op intToken : (char -> bool) -> int lexer
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

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
(* <boxed values 110>=                          *)
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
(* <boxed values 111>=                          *)
val _ = op plusBracketsString : ('a -> string) -> ('a plus_brackets -> string)
(* <common parsing code>=                       *)
(* <combinators and utilities for parsing located streams>= *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* <combinators and utilities for parsing located streams>= *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* <boxed values 113>=                          *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* <combinators and utilities for parsing located streams>= *)
fun @@ p = pair <$> srcloc <*> p
(* <boxed values 114>=                          *)
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
(* \qvfilbreak3in                               *)
(*                                              *)
(* Unit testing                                 *)
(*                                              *)
(* Unit testing in uSmalltalk looks a little different *)
(* from unit testing in micro-Scheme or uML, but a *)
(* little more like unit testing in \mcl: not only does *)
(* testing for equality require a call to [[eval]], but *)
(* also printing is different. If a value needs to be *)
(* printed, the testing code can't first convert it to a *)
(* string, because in general, a uSmalltalk object *)
(* doesn't know how to convert itself to a string. But a *)
(* value can be asked to print itself, so when a value *)
(* needs to be printed, the testing code sends it a *)
(* [[print]] message. Values are printed by function *)
(* [[printsAs]], which sends a [[print]] message to an *)
(* object and places the results in a buffer, the *)
(* contents of which it then returns.           *)
(* <boxed values 115>=                          *)
val _ = op asAscii : ('t, string) polyparser -> ('t, string) polyparser
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
(* <boxed values 115>=                          *)
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
(* <boxed values 116>=                          *)
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
(* <boxed values 125>=                          *)
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
(* <boxed values 126>=                          *)
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
(* <boxed values 118>=                          *)
val _ = op left  : ('t plus_brackets, bracket_shape located) polyparser
val _ = op right : ('t plus_brackets, bracket_shape located) polyparser
val _ = op pretoken : ('t plus_brackets, 't) polyparser
(* <transformers for interchangeable brackets>= *)
fun badRight msg =
  (fn (loc, shape) => synerrorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* <boxed values 119>=                          *)
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
(* <boxed values 117>=                          *)
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
(* <boxed values 120>=                          *)
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
(* <boxed values 120>=                          *)
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
(* <boxed values 121>=                          *)
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
(* <boxed values 122>=                          *)
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
(* <boxed values 123>=                          *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* <boxed values 123>=                          *)
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
(* <boxed values 124>=                          *)
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
(* <boxed values 127>=                          *)
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
(* <boxed values 128>=                          *)
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
(* <boxed values 129>=                          *)
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
(* <boxed values 130>=                          *)
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
(* <boxed values 131>=                          *)
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
(* <boxed values 132>=                          *)
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
(* <boxed values 133>=                          *)
val _ = op parseWithErrors : ('t, 'a) polyparser ->                     't
                                    located eol_marked stream -> 'a error stream
(* <streams that issue two forms of prompts>=   *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* <boxed values 134>=                          *)
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
(* <boxed values 135>=                          *)
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
(*   KINDS FOR TYPED LANGUAGES                                   *)
(*                                                               *)
(*****************************************************************)

(* Polymorphic type systems \chaptocbacksplit\  *)
(* maintocsplitand \tuschemeheader              *)
(*                                              *)
(* \typesystemtuscheme                          *)
(*                                              *)
(* [*] The full benefits of types aren't provided by *)
(* Typed Impcore, which is both too complicated and not *)
(* powerful enough. Typed Impcore is too complicated *)
(* because of its multiple type environments \vgam, \ *)
(* fgam, and \rgam. It is not powerful enough because *)
(* each operation that works with values of more than *)
(* one type, like [[=]] or [[println]], has to be built *)
(* into its abstract syntax. A function defined by a *)
(* user can operate only on values of a single type, *)
(* which is to say it is monomorphic. For example, a *)
(* user can't define a reusable array-reversal function *)
(* that could operate on both an array of Booleans and *)
(* an array of integers. This limitation is shared by *)
(* such languages as C and Pascal.              *)
(*                                              *)
(* Monomorphism handicaps programmers. Many primitive *)
(* structures, including arrays, lists, tables, *)
(* pointers, products, sums, and objects, inherently *)
(* work with multiple types: they are polymorphic. But *)
(* when user-defined functions are monomorphic, *)
(* a computation like the length of a list has to be *)
(* coded anew for each type of list element, as in \cref *)
(* impcore.chap.                                *)
(*                                              *)
(* And monomorphic languages are hard to extend with new *)
(* type constructors. A language designer can do it, *)
(* provided they are willing to add new rules to a type *)
(* system and to revisit its proof of type soundness. *)
(* But a programmer can't; unless there is some sort of *)
(* template or macro system, no programmer can add a *)
(* user-defined, polymorphic type constructor such as *)
(* the [[env]] type constructor used for environments in *)
(* \crefrangemlscheme.chapsmall.chap. At best, a *)
(* programmer can add a new base type, not a new type *)
(* constructor.                                 *)
(*                                              *)
(* These problems are solved by polymorphic type systems *)
(* . Such type systems enable a programmer to write *)
(* polymorphic functions and to add new type    *)
(* constructors. Our first polymorphic type system is *)
(* part of a language called \proglangTyped micro-Scheme *)
(* , or Typed uScheme for short. Typed uScheme is *)
(* patterned after micro-Scheme: it uses the same values *)
(* as micro-Scheme and similar abstract syntax. *)
(*                                              *)
(* Our study of Typed uScheme begins with concrete *)
(* syntax. It continues with kinds and quantified types; *)
(* these are the two ideas at the core of the type *)
(* system. Kinds are used to ensure that every type *)
(* written in the source code is well formed and *)
(* meaningful; kinds classify types in much the same way *)
(* that types classify terms. Quantified types express *)
(* polymorphism; they make it possible to implement *)
(* polymorphic operations using ordinary functions *)
(* instead of special-purpose abstract syntax. Building *)
(* on these ideas, the rest of the chapter presents *)
(* technical details needed to make a polymorphic type *)
(* system work: type equivalence and substitution. *)
(* Type equivalence is a relation that shows when two *)
(* types cannot be distinguished by any program, even if *)
(* they don't look identical. And substitution is the *)
(* mechanism by which a polymorphic value is    *)
(* instantiated so it can be used.              *)
(*                                              *)
(* Concrete syntax of \tuschemeheader           *)
(*                                              *)
(* Typed uScheme is much like micro-Scheme, except as *)
(* follows:                                     *)
(*                                              *)
(*   • Function definitions and [[lambda]] abstractions *)
(*  require type annotations for parameters.\stdbreak *)
(*   • Function definitions require explicit result *)
(*  types. \stdbreak                            *)
(*   • All letrec expressions require type annotations *)
(*  for bound names—and each name may be bound only *)
(*  to a [[lambda]] abstraction. \stdbreak      *)
(*   • Instead of micro-Scheme's single [[val]] form, *)
(*  Typed uScheme provides two forms: [[val-rec]], *)
(*  which is recursive and defines only functions, *)
(*  and [[val]], which is non-recursive and can *)
(*  define any type of value. Only the [[val-rec]] *)
(*  form requires a type annotation. Typed uScheme's *)
(*  [[val]] and [[val-rec]] forms resemble the  *)
(*  corresponding forms in Standard ML.         *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \productionwidthtype-formals \csynlabeltuscheme [*] *)
(* {production}def [[(val ]]\termvariable-name exp[[)]] *)
(* | [[(val-rec]] [[[]]\termvariable-name [[:]] type-exp *)
(* [[]]] exp[[)]] | [[(define ]]type-exp \term  *)
(* function-name (formals) exp[[)]] | exp | [[(use ]]\ *)
(* termfile-name[[)]] | unit-test \newruleunit-test *)
(* [[(check-expect ]]exp exp[[)]] | [[(check-assert ]] *)
(* exp[[)]] | [[(check-error ]]exp[[)]] |       *)
(* [[(check-type]] exp type-exp[[)]] |          *)
(* [[(check-type-error ]]def[[)]] \newruleexp literal | *)
(* \termvariable-name | [[(set ]]\termvariable-name exp *)
(* [[)]] | [[(if ]]exp exp exp[[)]] | [[(while ]]exp exp *)
(* [[)]] | [[(begin ]]\sequenceexp[[)]] | [[(]]exp \ *)
(* sequenceexp[[)]] | [[(]]let-keyword [[(]]\sequence *)
(* [[[]]\termvariable-name exp[[]]][[) ]]exp[[)]] | *)
(* [[(letrec]] [[[]]\sequence[[([]]\termvariable-name *)
(* [[:]] type-exp[[]]] exp[[)]][[] ]]exp[[)]] | *)
(* [[(lambda (]]formals[[) ]]exp[[)]] | [[(type-lambda *)
(* []]type-formals[[] ]]exp[[)]] | [[[@ ]]exp[[ ]]\ *)
(* sequencetype-exp[[]]] \newrulelet-keyword \alternate *)
(* *let | let* \newruleformals \sequence[\term  *)
(* variable-name : type-exp] \newruletype-formals \ *)
(* sequence'\termtype-variable-name \newruletype-exp \ *)
(* termtype-constructor-name | '\termtype-variable-name *)
(* | [[(forall (]]\sequence'\termtype-variable-name[[) *)
(* ]]type-exp[[)]] | [[(]]\sequencetype-exp[[ -> ]] *)
(* type-exp[[)]] | [[(]]type-exp \sequencetype-exp[[)]] *)
(* \newruleliteral \alternate*\termnumeral | [[#t]] | *)
(* [[#f]] | [[']]S-exp | [[(quote ]]S-exp[[)]] \newrule *)
(* S-exp \alternate*literal | \termsymbol-name | [[(]]\ *)
(* sequenceS-exp[[)]] \newrulenumeral \grammarboxtoken *)
(* composed only of digits, possibly prefixed with a *)
(* plus or minus sign \newrulename@*-name \grammarbox *)
(* token that is not a bracket, a numeral, or one of the *)
(* ``reserved'' words shown in typewriter font  *)
(* {production}                                 *)
(*                                              *)
(* Concrete syntax of Typed uScheme [*]         *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* The type system of Typed uScheme is more powerful *)
(* than that of Typed Impcore:                  *)
(*                                              *)
(*   • Typed uScheme adds quantified types, which are *)
(*  written with [[forall]]. Values of quantified *)
(*  type are introduced by a new syntactic form of *)
(*  expression: [[type-lambda]]. They are eliminated *)
(*  by the new syntactic form [[@]].            *)
(*   • Syntactically, Typed uScheme does not distinguish *)
(*  a ``type'' from a ``type constructor''; both can *)
(*  be called ``types,'' and both are in the    *)
(*  syntactic category type-exp. The category, which *)
(*  is called ``type-level expression,'' also   *)
(*  includes ill-formed nonsense that is neither type *)
(*  nor type constructor, like \monobox(int int). *)
(*   • In Typed uScheme, only the type constructor for *)
(*  functions requires special-purpose syntax;  *)
(*  a function is introduced by [[lambda]] and  *)
(*  eliminated by function application. Other type *)
(*  constructors, like pairs and arrays, require no *)
(*  new syntax or new typing rules. They go into the *)
(*  initial basis, where their operations are   *)
(*  implemented as ordinary (primitive) functions. *)
(*                                              *)
(* The syntax of Typed uScheme is shown in \vref *)
(* tuscheme.fig.syntax.                         *)
(*                                              *)
(* A replacement for type-formation rules: \    *)
(* rlapheaderKinds                              *)
(*                                              *)
(* \realsubsectionmarkA replacement for type-formation *)
(* rules: Kinds                                 *)
(*                                              *)
(* [*] Types in source code are written by programmers, *)
(* and they can't be trusted. ``Types'' like \monobox *)
(* (int int) are ill formed and must be rejected. *)
(* In Typed Impcore, types are determined to be well *)
(* formed or ill formed by type-formation rules. And for *)
(* a language with a fixed set of types and type *)
(* constructors, that's fine. But in Typed uScheme, *)
(* we want to be able to add new type constructors *)
(* without adding new rules. So Typed uScheme uses just *)
(* a few rules to encompass arbitrarily many type *)
(* constructors. The rules rely on each type constructor *)
(* having a kind.                               *)
(*                                              *)
(* Kinds classify types (and type constructors) in much *)
(* the same way that types classify terms. A kind shows *)
(* how a type constructor may be used. For example, both *)
(* [[int]] and [[array]] are type constructors of Typed *)
(* uScheme, but they must be used differently. The *)
(* [[int]] constructor is a kind of constructor that is *)
(* a type all by itself; the [[array]] constructor is a *)
(* kind of constructor that has to be applied to an *)
(* element type in order to make another type. \notation *)
(* [kappa]\kinda kind, which classifies types The first *)
(* kind is written \ktype and pronounced ``type'';\ *)
(* notation [type]\ktypethe kind ascribed to types that *)
(* classify terms the second kind is written \ktype\ *)
(* karrow\ktype and pronounced ``type arrow type.''\ *)
(* notation [arrow]\karrowused to form kinds of type  *)
(* constructors A kind is attributed to a type by a *)
(* formal judgment: tau:: \kind\notation [tau has kind \ *)
(* kind]tau:: \kindascribes kind \kind to type tau says *)
(* that type constructor tau has kind kappa. As a *)
(* special case, the judgment ``tau:: \ktype'' is *)
(* equivalent to the judgment ``tau is a type'' used in *)
(* Typed Impcore.                               *)
(*                                              *)
(* Like types, kinds are defined inductively: there is *)
(* one base kind, ``type'' (\ktype), and other kinds are *)
(* made using arrows (\karrow). As concrete notation, we *)
(* write {production}k@\kind \alternate*\ktype | \ *)
(* crossdotsn\kind\karrow\kind. {production} Types that *)
(* are inhabited by values, like [[int]] or \monobox *)
(* (list bool), have kind \ktype. Types of other kinds, *)
(* like [[list]] and [[array]], are ultimately used to *)
(* make types of kind \ktype.                   *)
(*                                              *)
(* Some common kinds, with example type constructors of *)
(* those kinds, are as follows: {indented}      *)
(*                                              *)
(* \ktype                [[int]], [[bool]], [[unit]] *)
(* \ktype\karrow\ktype   [[list]], [[array]], [[option]] *)
(* \ktype*\ktype\karrow\ [[pair]], [[sum]], Standard *)
(* ktype                 ML's [[->]]            *)
(*                                              *)
(* {indented} More exotic kinds can be found in *)
(* languages like Haskell, which includes not only *)
(* ``monads,'' which are all types of kind \ktype\karrow *)
(* \ktype, but also ``monad transformers,'' which are *)
(* types of kind (\ktype\karrow\ktype) \karrow(\ktype\ *)
(* karrow\ktype).                               *)
(*                                              *)
(* Every syntactically expressible kind \kind is well *)
(* formed: \jlabeltuscheme.good.kind\isakind\kind \ *)
(* tyrule,KindFormationType \isakind\ktype \    *)
(* tyrule.KindFormationArrow \twoquad \arekinds\ldotsn\ *)
(* kind \isakind\kind \isakind\crossdotsn\kind\karrow\ *)
(* kind                                         *)
(*                                              *)
(* How do we know which type constructors have which *)
(* kinds? The kind of each type constructor is stored in *)
(* a kind environment, written Delta.\notation [delta] *)
(* Deltaa kind environment The example environment  *)
(* Delta_0 below shows the kinds of the primitive type *)
(* constructors of Typed uScheme. Each binding is *)
(* written using the :: symbol, which is used instead *)
(* of |->; it is pronounced ``has kind.''       *)
(*                                              *)
(*  Delta_0 int:: \ktype, bool:: \ktype, unit:: \ *)
(*  = {     ktype, pair :: \ktype*\ktype\karrow\ktype *)
(*          ,                                   *)
(*          sum :: \ktype*\ktype\karrow\ktype, array *)
(*          :: \ktype\karrow\ktype, list :: \ktype\ *)
(*          karrow\ktype }                      *)
(*                                              *)
(* The kind environment determines how both [[int]] and *)
(* [[array]] may be used. New type constructors can be *)
(* added to Typed uScheme just by adding them to Delta_0 *)
(* (\                                           *)
(* creftypesys.ex.tuscheme-sums,typesys.ex.tuscheme-pairs,typesys.ex.tuscheme-queues,typesys.ex.polyrefs). *)
(*                                              *)
(* A kind environment is used to tell what types are *)
(* well formed. No matter how many type constructors are *)
(* defined, they are handled using just three   *)
(* type-formation rules:                        *)
(*                                              *)
(*   • A type can be formed by writing a type *)
(*  constructor. In abstract syntax, a type     *)
(*  constructor is written \asttycon(\tycon), where \ *)
(*  tycon is the name of the constructor. In concrete *)
(*  syntax it is written just using its name, like *)
(*  [[int]] or [[list]]. A type constructor is well *)
(*  formed if and only if it is bound in Delta. *)
(*                                              *)
(*   • A type can be formed by applying a type to other *)
(*  types. In abstract syntax, type application is *)
(*  written \astconapp(tau, [\ldotsntau]), where tau *)
(*  and \ldotsntau are type-level expressions. In *)
(*  concrete syntax, application of a type      *)
(*  constructor is written using the same concrete *)
(*  syntax as application of a function. For example, *)
(*  \monobox(list int) is the type ``list of    *)
(*  integer.'' A constructor application is well *)
(*  formed if its arguments have the kinds it   *)
(*  expects, as formalized in the \rulenameKindApp *)
(*  rule below.                                 *)
(*                                              *)
(*   • A type can be a function type. In abstract *)
(*  syntax, it is \nomathbreak\crossdotsntau-->tau, *)
(*  where \ldotsntau are the argument types and tau *)
(*   is the result type. In concrete syntax,    *)
(*  a function type is \monobox(\cdotsntau -> tau). *)
(*  [The arrow that signifies a function occurs in *)
(*  the \emph{middle} of the parentheses, between *)
(*  types. In~other words, the function arrow [[->]] *)
(*  is an \emph{infix} operator. This infix syntax *)
(*  violates \lisp's \emph{prefix} convention, in *)
(*  which keywords, type constructors, and operators *)
(*  always come first, immediately after an open *)
(*  parenthesis. Prefix syntax might look like ``\ *)
(*  monobox{(function ($\tau_1~\ldots~\tau_n$) $\ *)
(*  tau$)}.'' But when functions take or return other *)
(*  functions, prefix syntax is too hard to read. ] *)
(*  A function type is well formed if and only if *)
(*  types tau_1 to tau_n and tau all have kind \ *)
(*  ktype.                                      *)
(*                                              *)
(* These rules are formalized using the kinding judgment *)
(* \kindistau\kind. [*]\jlabeltuscheme.good.type\ *)
(* kindistau\kind This judgment says that in kind *)
(* environment Delta, type-level expression tau has *)
(* kind \kind. Kinds classify types in much the same way *)
(* that types classify expressions. \tyrule KindIntroCon *)
(* \tyconin dom Delta \kindis\asttycon(\tycon) Delta(\ *)
(* tycon) \tyrule KindApp \twoquad \kindistau\crossdotsn *)
(* \kind\karrow\kind \repeati\kindistau_i \kind_i \ *)
(* kindis\astconapp(tau, [\ldotsntau]) \kind \tyrule *)
(* KindFunction \twoquad \repeati\kindistau_i \ktype \ *)
(* kindistau\ktype \kindis\crossdotsntau-->tau \ktype *)
(* No matter how many type constructors we may add to *)
(* Typed uScheme, these kinding rules tell us everything *)
(* we will ever need to know about the formation of *)
(* types. Compare this situation with the situation in *)
(* Typed Impcore. In Typed Impcore, we need the \ *)
(* rulenameBaseTypes rule for [[int]] and [[bool]]. \ *)
(* stdbreak To add arrays we need the \         *)
(* rulenameArrayFormation rule. To add lists we would *)
(* need a list-formation rule (\                *)
(* crefpage,typesys.ex.timpcore-lists). And so on. *)
(* Unlike Typed Impcore's type system, Typed uScheme's *)
(* type system can easily be extended with new type *)
(* constructors (\                              *)
(* creftypesys.ex.tuscheme-queues,typesys.ex.tuscheme-pairs,typesys.ex.tuscheme-sums,typesys.ex.polyrefs). *)
(* Similar ideas are used in languages in which  *)
(* programmers can define new type constructors, *)
(* including uML and \mcl (\crefadt.chap,mcl.chap). *)
(*                                              *)
(* Implementing kinds                           *)
(*                                              *)
(* A kind is represented using the datatype [[kind]]. \ *)
(* tuslabelkind                                 *)
(* <kinds for typed languages>=                 *)
datatype kind = TYPE                          (* kind of all types *)
              | ARROW of kind list * kind     (* kind of many constructors *)
(* Kinds are equal if and only if they are identical. *)

(* <kinds for typed languages>=                 *)
fun eqKind (TYPE, TYPE) = true
  | eqKind (ARROW (args, result), ARROW (args', result')) =
      eqKinds (args, args') andalso eqKind (result, result')
  | eqKind (_, _) = false
and eqKinds (ks, ks') = ListPair.allEq eqKind (ks, ks')


(*****************************************************************)
(*                                                               *)
(*   TYPES FOR {\TUSCHEME}                                       *)
(*                                                               *)
(*****************************************************************)

(* The kind system and the type-formation rules shown *)
(* above replace the type-formation rules of Typed *)
(* Impcore. To get polymorphism, however, we need *)
(* something more: quantified types.            *)
(*                                              *)
(* The heart of polymorphism: \                 *)
(* chaptocbacksplitQuantified types             *)
(*                                              *)
(* \realsubsectionmarkThe heart of polymorphism: *)
(* Quantified types                             *)
(*                                              *)
(* [*] Polymorphic functions aplenty can be found in \ *)
(* crefscheme.chap; one of the simplest is [[length]]. *)
(* As defined in (untyped) micro-Scheme, [[length]] can *)
(* be applied to any list of values, no matter what the *)
(* types of its elements: {smallverbatim} (define length *)
(* (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))) *)
(* {smallverbatim} Suppose [[length]] could be defined *)
(* in Typed Impcore; what would its type be? In a *)
(* monomorphic language like Typed Impcore or C, *)
(* a function can have at most one type, so the *)
(* definition would have to designate an element type. *)
(* To use [[length]] with different types of lists would *)
(* require different versions: {smallverbatim} (define *)
(* int lengthI ([xs : (list int)]) (if (null? xs) 0 (+ 1 *)
(* (lengthI (cdr xs))))) (define int lengthB ([xs : *)
(* (list bool)]) (if (null? xs) 0 (+ 1 (lengthB (cdr *)
(* xs))))) (define int lengthS ([xs : (list sym)]) (if *)
(* (null? xs) 0 (+ 1 (lengthS (cdr xs)))))      *)
(* {smallverbatim} Such duplication wastes effort; *)
(* except for the types, the functions are identical. *)
(* but Typed Impcore's type system cannot express the *)
(* idea that [[length]] works with any list, independent *)
(* of the element type. To express the idea that *)
(* [[length]] could work with any element type, we need  *)
(* type variables and quantified types.         *)
(*                                              *)
(* A type variable stands for an unknown type;  *)
(* a quantified type grants permission to substitute any *)
(* type for a type variable. In this book, type *)
(* variables are written using the Greek letters\ *)
(* notation [alpha, beta, gamma]alpha, beta, gammatype *)
(* variables alpha, beta, and gamma; quantified types *)
(* are written using \/.\notation [for all]\/used to *)
(* write quantified, polymorphic types For example, the *)
(* type of a polymorphic [[length]] function is \/alpha\ *)
(* alldotlistalpha -->int. Greek letters and math *)
(* symbols can be awkward in code, so in Typed uScheme* *)
(* this type is written \monobox(forall ['a] ((list 'a) *)
(* -> int)).                                    *)
(*                                              *)
(* A [[forall]] type is not a function type; the *)
(* [[length]] function can't be used on a list of *)
(* Booleans, for example, until it is instantiated. *)
(* The instantiation \monobox(@ length bool) strips ``\/ *)
(* alpha.'' from the front of [[length]]'s type, and in *)
(* what remains, substitutes bool for alpha. The type of *)
(* the resulting instance is \nomathbreaklistbool --> *)
(* bool, or in Typed uScheme, \monobox((list bool) -> *)
(* int). This instance can be applied to a list of *)
(* Booleans.                                    *)
(*                                              *)
(* Like [[lambda]], \/ is a binding construct, and the *)
(* variable alpha is sometimes called a type parameter. *)
(* Like the name of a formal parameter, the name of a *)
(* type parameter doesn't matter; for example, the type *)
(* of the [[length]] function could also be written \/ *)
(* beta\alldotlistbeta -->int, and its meaning would be *)
(* unchanged. That's because the meaning of a quantified *)
(* type is determined by how it behaves when we strip *)
(* the quantifier and substitute for the bound type *)
(* variable.                                    *)
(*                                              *)
(* \qbreak In abstract syntax, type variables and *)
(* quantified types are written using \asttyvar and \ *)
(* astforall. And like \asttycon and \astconapp, \ *)
(* asttyvar and \astforall are governed by kinding *)
(* rules. (The kind system replaces the type-formation *)
(* rules used in Typed Impcore; remember the slogan *)
(* ``just as types classify terms, kinds classify *)
(* types.'')                                    *)
(*                                              *)
(* The kind of a type variable, like the kind of a type *)
(* constructor, is looked up in the environment Delta. \ *)
(* tyrule KindIntroVar alphain dom Delta \kindis\ *)
(* asttyvar(alpha) Delta(alpha)                 *)
(*                                              *)
(* The kind of a quantified type is always \ktype, and *)
(* the \astforall quantifier may be used only over types *)
(* of kind \ktype. Within the body of the \astforall, *)
(* the quantified variables stand for types. So above *)
(* the line, they are introduced into the kind  *)
(* environment with kind \ktype. \tyrule KindAll \kindis *)
(* [{alpha_1 :: \ktype, ..., alpha_n :: \ktype}] tau\ *)
(* ktype \kindis\astforall(<\ldotsnalpha>, tau) \ktype *)
(* In some polymorphic type systems, including the *)
(* functional language Haskell, type variables may have *)
(* other kinds.                                 *)
(*                                              *)
(* In Typed uScheme, every type is written using a *)
(* type-level expression (nonterminal type-exp in \ *)
(* crefpage,tuscheme.fig.syntax). In the interpreter, a *)
(* type-level expression is represented by a value of *)
(* the ML type [[tyex]]; its forms include not only \ *)
(* asttyvar and \astforall but also \asttycon, \ *)
(* astconapp, and a function-type form. \tuslabeltyex *)
(* <types for {\tuscheme}>=                     *)
datatype tyex = TYCON  of name                (* type constructor *)
              | CONAPP of tyex * tyex list    (* type-level application *)
              | FUNTY  of tyex list * tyex    (* function type *)
              | FORALL of name list * tyex    (* quantified type *)
              | TYVAR  of name                (* type variable *)
(* Even though not every [[tyex]] represents a  *)
(* well-formed type, it's easier to call them all *)
(* ``types''—except when we have to be careful. *)

(* <types for {\tuscheme}>=                     *)
val inttype  = TYCON "int"
val booltype = TYCON "bool"
val symtype  = TYCON "sym"
val unittype = TYCON "unit"
val tvA      = TYVAR "'a"
fun listtype ty = CONAPP (TYCON "list",[ty])
(* Definitions are evaluated slightly differently than *)
(* in untyped micro-Scheme. As in Typed Impcore, the *)
(* type system and operational semantics cooperate to *)
(* ensure that no definition ever changes the type of an *)
(* existing name. In Typed Impcore, the assurance is *)
(* provided by the type system: it permits a name to be *)
(* redefined only when the existing type is preserved. *)
(* In Typed uScheme, the assurance is provided by the *)
(* operational semantics: as in Exercise [->] from *)
(* Chapter [->], evaluating a definition always creates *)
(* a new binding. In a \astval binding, the right-hand *)
(* side is evaluated in the old environment; in a \ *)
(* rulenameVal-Rec binding, the right-hand side is *)
(* evaluated in the new environment. The type system *)
(* guarantees that the result of evaluation does not *)
(* depend on the \unspec value with which \aloc is *)
(* initialized.                                 *)
(*                                              *)
(* \ops Val \twoline l\notin dom sigma \sthreeerhosigma *)
(* ==>\evalr[']v \topeval\xval(x, e) -->\stworho{x|->l} *)
(* sigma'{l|->v}                                *)
(*                                              *)
(* \ops Val-Rec \twoline l\notin dom sigma \sthreeerho{x *)
(* |->l}sigma{l|->unspecified} ==>\evalr[']v \topeval\ *)
(* xvalrec(x, tau, e) --> \stworho{x|->l}sigma'{l|->v} *)
(* These rules are implemented in \crefapp:tuscheme. *)
(*                                              *)
(* Primitive type constructors of \tuschemeheader *)
(*                                              *)
(* The types of the primitive functions have to be *)
(* written using ML code inside the interpreter, but the *)
(* raw representation isn't easy to write. For example, *)
(* the type of [[cons]] is represented by this enormous *)
(* constructed value: {smallverbatim} FORALL (["'a"], *)
(* FUNTY ([TYVAR "'a", CONAPP (TYCON "list", [TYVAR *)
(* "'a"])]), CONAPP (TYCON "list", [TYVAR "'a"]))) *)
(* {smallverbatim} To make such values easier to *)
(* construct, I provide these representations: \ *)
(* tusflabelinttype,booltype                    *)
(* <boxed values 9>=                            *)
val _ = op inttype   : tyex
val _ = op booltype  : tyex
val _ = op symtype   : tyex
val _ = op unittype  : tyex
val _ = op tvA       : tyex
val _ = op listtype  : tyex -> tyex
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <types for {\tuscheme}>=                     *)
fun typeString (TYCON c) = c
  | typeString (TYVAR a) = a
  | typeString (FUNTY (args, result)) =
      "(" ^ spaceSep (map typeString args) ^ " -> " ^ typeString result ^ ")"
  | typeString (CONAPP (tau, [])) = "(" ^ typeString tau ^ ")"
  | typeString (CONAPP (tau, tys)) =
      "(" ^ typeString tau ^ " " ^ spaceSep (map typeString tys) ^ ")"
  | typeString (FORALL (tyvars, tau)) =
      "(forall [" ^ spaceSep tyvars ^ "] " ^ typeString tau ^ ")"


(*****************************************************************)
(*                                                               *)
(*   SETS OF FREE TYPE VARIABLES IN \TUSCHEME                    *)
(*                                                               *)
(*****************************************************************)

(* <sets of free type variables in \tuscheme>=  *)
fun freetyvars t =
  let fun free (TYVAR v,          ftvs) = insert (v, ftvs)
        | free (TYCON _,          ftvs) = ftvs
        | free (CONAPP (ty, tys), ftvs) = foldl free (free (ty, ftvs)) tys
        | free (FUNTY  (tys, ty), ftvs) = foldl free (free (ty, ftvs)) tys
        | free (FORALL (alphas, tau), ftvs) =
               union (diff (free (tau, emptyset), alphas), ftvs)
  in  reverse (free (t, emptyset))
  end  
(* \qtrim1                                      *)
(*                                              *)
(* Function [[eqType]] can be used in the implementation *)
(* of any typing rule that requires two types to be the *)
(* same. To formalize the use of equivalence instead of *)
(* identity, I extend the type system with the following *)
(* rule, which says that if e has a type, it also has *)
(* any equivalent type. [*] \tyruleEquiv \twoquad\ *)
(* ptypeise tau tau===tau' \ptypeise tau'       *)
(*                                              *)
(* Instantiation and renaming by capture-avoiding *)
(* substitution                                 *)
(*                                              *)
(* \realsubsectionmarkInstantiation and renaming by *)
(* capture-avoiding substitution                *)
(*                                              *)
(* [*] [*] Capture-avoiding substitution is tricky—even *)
(* eminent professors sometimes get it wrong. To study *)
(* it, we first need to get precise about free and bound *)
(* type variables.                              *)
(*                                              *)
(* Free and bound type variables                *)
(*                                              *)
(* [*] Type variables may occur free or bound, and we *)
(* substitute only for free occurrences. A free type *)
(* variable acts like a global variable; a bound type *)
(* variable acts like a formal parameter. And a binding *)
(* occurrence is an appearance next to a [[forall]]. All *)
(* three kinds of occurrences are shown in this example *)
(* type: {indented}                             *)
(*                                              *)
(* Example type A         \monobox('c -> (forall ['a] *)
(*                     ('a -> 'c)))             *)
(* Free occurrence of     \monobox(\high'c -> (forall *)
(* [['c]] in A            ['a] ('a -> 'c)))     *)
(* Binding occurrence of  \monobox('c -> (forall (\high *)
(* [['a]] in A            'a) ('a -> 'c)))      *)
(* Bound occurrence of    \monobox('c -> (forall ('a) (\ *)
(* [['a]] in A            high'a -> 'c)))       *)
(* Free occurrence of     \monobox('c -> (forall ('a) *)
(* [['c]] in A            ('a -> \high'c)))     *)
(*                                              *)
(* {indented} As the wording suggests, ``free'' and *)
(* ``bound'' are not absolute properties; they are *)
(* relative to a particular type. For example, type *)
(* variable [['a]] occurs bound in type \monobox(forall *)
(* ['a] ('a -> 'c)), but it occurs free in type \monobox *)
(* ('a -> 'c).                                  *)
(*                                              *)
(* Free type variables can be specified by a proof *)
(* system. The judgment of the system is \mathboxalphain *)
(* \ftv(tau)\jlabeltuscheme.ftvalphain \ftv(tau), which *)
(* means ``alpha is free in tau.'' [``Binding   *)
(* occurrence'' doesn't need a proof system; binding *)
(* occurrences are those introduced by~$\forall$.] The *)
(* proof system resembles the proof system for free term *)
(* variables in \crefpage(mlscheme.freein: \jlabel *)
(* tuscheme.free.tyalphain \ftv(tau) {mathpar} \ *)
(* inferrule \freealphaalpha                    *)
(*                                              *)
(* \inferrule\freealphatau_i \freealpha(\ldotsntau) tau *)
(*                                              *)
(* \inferrule\freealphatau \freealpha(\ldotsntau) tau\ *)
(* fracsuffix,                                  *)
(*                                              *)
(* \inferrule\freealphatau_i \freealpha\crossdotsntau--> *)
(* tau                                          *)
(*                                              *)
(* \inferrule\freealphatau \freealpha\crossdotsntau--> *)
(* tau\fracsuffix,                              *)
(* \inferrule\freealpha tau \andalso\repeatialpha!=alpha *)
(* _i \freealpha \/\ldotsnalpha\alldottau\fracsuffix. *)
(* {mathpar} Also, if \freealpha_i tau, then alpha_i is *)
(* bound in \mathbox\/\ldotsnalpha\alldottau.   *)
(*                                              *)
(* The free type variables of a type are computed by *)
(* function [[freetyvars]]. Bound type variables are *)
(* removed using [[diff]]. Using [[foldl]], [[union]], *)
(* [[diff]], and [[reverse]] puts type variables in the *)
(* set in the order of their first appearance. \ *)
(* tusflabelfreetyvars                          *)
(* <boxed values 3>=                            *)
val _ = op freetyvars : tyex -> name set
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <sets of free type variables in \tuscheme>=  *)
fun freetyvarsGamma Gamma =
  foldl (fn ((x, tau), ftvs) => union (ftvs, freetyvars tau)) emptyset Gamma


(*****************************************************************)
(*                                                               *)
(*   SHARED UTILITY FUNCTIONS ON SETS OF TYPE VARIABLES          *)
(*                                                               *)
(*****************************************************************)

(* <shared utility functions on sets of type variables>= *)
fun freshName (alpha, avoid) =
  let val basename = stripNumericSuffix alpha
      val candidates =
        streamMap (fn n => basename ^ "-" ^ intString n) naturals
      fun ok beta = not (member beta avoid)
  in  case streamGet (streamFilter ok candidates)
        of SOME (beta, _) => beta
         | NONE => raise InternalError "ran out of natural numbers"
  end
(* Calling renameForallAvoiding([\ldotsnalpha], tau, C) *)
(* must choose variables beta_i not in C and return a *)
(* type \nomathbreak\/\ldotsnbeta\alldottau' with these *)
(* properties: {gather*} \/\ldotsnbeta\alldottau' ===\/\ *)
(* ldotsnalpha\alldottau\text,                  *)
(* {\ldotsnbeta}\capC = \emptyset\text. {gather*} \ *)
(* stdbreak For each alpha_i, there are two cases: *)
(*                                              *)
(*   • If alpha_i \notinC, then it doesn't need to be *)
(*  renamed, and to maximize readability of the *)
(*  resulting type, let beta_i = alpha_i.       *)
(*   • If alpha_i in C, then beta_i must be a new *)
(*  variable that does not appear in C, is not free *)
(*  in tau, and is different from every alpha_i. *)
(*                                              *)
(* To find a beta_i, use function [[freshName]], which *)
(* returns a name based on alpha but not in a given set *)
(* of type variables. [*]                       *)
(* <boxed values 181>=                          *)
val _ = op freshName : name * name set -> name
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)



(*****************************************************************)
(*                                                               *)
(*   KIND CHECKING FOR {\TUSCHEME}                               *)
(*                                                               *)
(*****************************************************************)

(* <kind checking for {\tuscheme}>=             *)
fun kindof (tau, Delta) =
  let (* The internal function [[kind]] computes the kind of  *)
      (* [[tau]]; the environment [[Delta]] is assumed. *)
      (* Function [[kind]] implements the kinding rules in the *)
      (* same way that [[typeof]] implements the typing rules *)
      (* and [[eval]] implements the operational semantics. *)
      (*                                              *)
      (* The kind of a type variable is looked up in the *)
      (* environment. \usetyKindIntroVar Thanks to the parser *)
      (* in \creftuschemea.parser, the name of a type variable *)
      (* always begins with a quote mark, so it is distinct *)
      (* from any type constructor. \tusflabelkind    *)
      (* <definition of internal function [[kind]]>=  *)
      fun kind (TYVAR a) =
            (find (a, Delta)
             handle NotFound _ => raise TypeError ("unknown type variable " ^ a)
                                                                               )
      (* <definition of internal function [[kind]]>=  *)
        | kind (TYCON c) =
            (find (c, Delta)
             handle NotFound _ => raise TypeError ("unknown type constructor " ^
                                                                             c))
      (* The kind of a function type is \ktype, provided that *)
      (* the argument types and result type also have kind \ *)
      (* ktype. \usetyKindFunction                    *)
      (* <definition of internal function [[kind]]>=  *)
        | kind (FUNTY (args, result)) =
            let fun badKind tau = not (eqKind (kind tau, TYPE))
            in  if badKind result then
                  raise TypeError "function result is not a type"
                else if List.exists badKind args then
                  raise TypeError "argument list includes a non-type"
                else
                  TYPE
            end
      (* <definition of internal function [[kind]]>=  *)
        | kind (CONAPP (tau, actuals)) =
            (case kind tau
               of ARROW (formal_kinds, result_kind) =>
                    if eqKinds (formal_kinds, map kind actuals) then
                        result_kind
                    else
                        raise TypeError ("type constructor " ^ typeString tau ^
                                         " applied to the wrong arguments")
                | TYPE =>
                    raise TypeError ("tried to apply type " ^ typeString tau ^
                                     " as type constructor"))
      (* The kind of a quantified type is always \ktype, *)
      (* provided its body also has kind \ktype. \usetyKindAll *)
      (* The quantified variables \ldotsnalpha may be used in  *)
      (* tau, so they are added to Delta before the kind of  *)
      (* tau is computed.                             *)
      (* <definition of internal function [[kind]]>=  *)
        | kind (FORALL (alphas, tau)) =
            let val Delta' =
                  foldl (fn (a, Delta) => bind (a, TYPE, Delta)) Delta alphas
            in  case kindof (tau, Delta')
                  of TYPE    => TYPE
                   | ARROW _ =>
                       raise TypeError
                                      "quantifed a non-nullary type constructor"
            end
(* By using a similar trick, you can make a value of any *)
(* type masquerade as a value of any other type (\cref *)
(* tuscheme.ex.evil-type-lambda,tuscheme.ex.forall-a-a). *)
(*                                              *)
(* Other building blocks of a type checker      *)
(*                                              *)
(* \realsubsectionmarkOther building blocks of a type *)
(* checker                                      *)
(*                                              *)
(* The functions for equivalence, substitution, and *)
(* instantiation, which are presented above, are all key *)
(* elements of a type checker, which I hope you will *)
(* write (\creftypesys.ex.tuscheme). When you do, you *)
(* can take advantage of some more useful functions, *)
(* which are presented below.                   *)
(*                                              *)
(* Ensuring well-formed types: Kind checking    *)
(*                                              *)
(* A type in the syntax, like the type of a parameter in *)
(* a [[lambda]] abstraction, can't be trusted—it has to *)
(* be checked to make sure it is well formed. In Typed *)
(* uScheme*, a [[tyex]] is well formed if it has a kind *)
(* (\crefpage,tuscheme.kindis). The kind is computed by *)
(* function [[kindof]], which implements the kinding *)
(* judgment \nomathbreak\kindistau\kind. This judgment *)
(* says that given kind environment Delta, type-level *)
(* expression tau is well formed and has kind \kind. *)
(* Given Delta and tau, \monoboxkindof(tau, Delta) *)
(* returns a \kind such that \kindistau\kind, or if no *)
(* such kind exists, it raises the exception    *)
(* [[TypeError]]. \tusflabelkindof              *)
(* <boxed values 7>=                            *)
val _ = op kindof : tyex * kind env -> kind
val _ = op kind   : tyex            -> kind
  in  kind tau
  end
(* <kind checking for {\tuscheme}>=             *)
fun asType (ty, Delta) =
  case kindof (ty, Delta)
    of TYPE    => ty
     | ARROW _ => raise TypeError ("used type constructor `" ^
                                   typeString ty ^ "' as a type")
(* <boxed values 8>=                            *)
val _ = op asType : tyex * kind env -> tyex



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR {\TUSCHEME}                  *)
(*                                                               *)
(*****************************************************************)

(* <abstract syntax and values for {\tuscheme}>= *)
(* As shown by these examples, explicit types and *)
(* polymorphism impose a notational burden. At the cost *)
(* of a little expressive power, that burden can be *)
(* lifted by type inference, as in \nml (\crefml.chap). *)
(* Or the burden can be lightened by instantiating an *)
(* entire module at once, as in \mcl (\crefmcl.chap). *)
(*                                              *)
(* To summarize this section, the essential new idea in *)
(* a polymorphic type system is the quantified type. *)
(* It comes with its own special-purpose syntax: *)
(* [[forall]] to form a quantified type, [[type-lambda]] *)
(* to introduce a quantified type, and [[@]] to *)
(* eliminate a quantified type. The rest of this chapter *)
(* shows how quantified types are combined with *)
(* micro-Scheme to produce Typed uScheme.       *)
(*                                              *)
(* Abstract syntax, values, and evaluation \    *)
(* chaptocsplitof \tuschemeheader               *)
(*                                              *)
(* \realsubsectionmarkAbstract syntax, values, and *)
(* evaluation of \tuschemeheader                *)
(*                                              *)
(* Like the concrete syntax, the abstract syntax of *)
(* Typed uScheme resembles the abstract syntax of *)
(* micro-Scheme. \stdbreak Typed uScheme adds two new *)
(* expressions, \asttylambda and \asttyapply, which *)
(* introduce and eliminate quantified types. \stdbreak *)
(* And it requires that names bound by [[letrec]] or *)
(* [[lambda]] (internal recursive functions and the *)
(* parameters of every function) be annotated with *)
(* explicit types. \tuslabelexp                 *)
(* <definitions of [[exp]] and [[value]] for {\tuscheme}>= *)
datatype exp   = LITERAL  of value
               | VAR      of name
               | SET      of name * exp
               | IFX      of exp * exp * exp
               | WHILEX   of exp * exp
               | BEGIN    of exp list
               | APPLY    of exp * exp list
               | LETX     of let_flavor * (name * exp) list * exp
               | LETRECX  of ((name * tyex) * exp) list * exp
               | LAMBDA   of lambda_exp
               | TYLAMBDA of name list * exp
               | TYAPPLY  of exp * tyex list
and let_flavor = LET | LETSTAR
(* <definitions of [[exp]] and [[value]] for {\tuscheme}>= *)
and    value = NIL
             | BOOLV     of bool   
             | NUM       of int
             | SYM       of name
             | PAIR      of value * value
             | CLOSURE   of lambda_value * value ref env
             | PRIMITIVE of primitive
             | ARRAY     of value array                    (*OMIT*)
withtype primitive    = value list -> value (* raises RuntimeError *)
     and lambda_exp   = (name * tyex) list * exp
     and lambda_value = name          list * exp
(* The definitions of Typed uScheme are like those of *)
(* Typed Impcore, plus the recursive binding form *)
(* [[VALREC]] (see sidebar \vpageref            *)
(* tuscheme.sidebar.valrec). \tuslabeldef       *)
(* <definition of [[def]] for {\tuscheme}>=     *)
datatype def  = VAL    of name * exp
              | VALREC of name * tyex * exp
              | EXP    of exp
              | DEFINE of name * tyex * lambda_exp
(* \qbreak The new unit-test forms [[check-type]] and *)
(* [[check-type-error]], which are meaningful only in *)
(* typed languages, demand new abstract syntax. \ *)
(* tuslabelunit_test                            *)
(* <definition of [[unit_test]] for explicitly typed languages>= *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_TYPE_ERROR  of def
(* And these forms of extended definition are used by *)
(* all languages in \crefrangemlscheme.chapsmall.chap.  *)
(* [*][*]                                       *)
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* <definition of [[valueString]] for \uscheme, \tuscheme, and \nml>= *)
fun valueString (SYM v)   = v
  | valueString (NUM n)   = intString n
  | valueString (BOOLV b) = if b then "#t" else "#f"
  | valueString (NIL)     = "()"
  | valueString (PAIR (car, cdr))  = 
      let fun tail (PAIR (car, cdr)) = " " ^ valueString car ^ tail cdr
            | tail NIL = ")"
            | tail v = " . " ^ valueString v ^ ")"
(* The rest of this section defines utility functions on *)
(* values.                                      *)
(*                                              *)
(* String conversion                            *)
(*                                              *)
(* Instead of [[printf]], ML provides functions that can *)
(* create, manipulate, and combine strings. So instead *)
(* using something like \crefimpcore.chap's extensible *)
(* [[print]] function, this chapter builds strings using *)
(* string-conversion functions. One example,    *)
(* [[valueString]], which converts an ML [[value]] to a *)
(* string, is shown here. The other string-conversion *)
(* functions are relegated to the Supplement.   *)
(*                                              *)
(* Function [[valueString]] is primarily concerned with *)
(* S-expressions. An atom is easily converted, but a *)
(* list made up of cons cells ([[PAIR]]s) requires care; *)
(* the [[cdr]] is converted by a recursive function, *)
(* [[tail]], which implements the same list-printing *)
(* algorithm as the C code. (The algorithm, which goes *)
(* back to McCarthy, is implemented by C function *)
(* [[printtail]] on \cpagerefschemea.printtail.imp.) *)
(* Function [[tail]] is defined inside [[valueString]], *)
(* with which it is mutually recursive. [*]     *)
(* <boxed values 14>=                           *)
val _ = op valueString : value -> string
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

      in  "(" ^ valueString car ^ tail cdr
      end
  | valueString (CLOSURE   _) = "<function>"
  | valueString (PRIMITIVE _) = "<function>"
(* Function [[valueString]] demonstrates pattern *)
(* matching. It takes one argument and does a case *)
(* analysis on its form. Each case corresponds to a *)
(* clause in the definition of [[valueString]]; there is *)
(* one clause for each datatype constructor of the *)
(* [[value]] type. On the left of the [[=]], each clause *)
(* contains a pattern that applies a datatype   *)
(* constructor to a variable, to a pair of variables, or *)
(* to the special ``wildcard'' pattern [[_]]    *)
(* (the underscore). Each variable, but not the *)
(* wildcard, is introduced into the environment and is *)
(* available for use on the right-hand side of  *)
(* the clause, just as if it had been bound by a *)
(* micro-Scheme [[let]] or ML [[val]].          *)
(*                                              *)
(* From the point of view of a C programmer, a pattern *)
(* match combines a [[switch]] statement with assignment *)
(* to local variables. The notation is sweet; for *)
(* example, in the matches for [[BOOLV]] and [[PAIR]], *)
(* I like [[b]] and [[car]] much better than the *)
(* [[v.u.boolv]] and [[v.u.pair.car]] that I have to *)
(* write in C. And I really like that the variables *)
(* [[b]] and [[car]] can be used only where they are *)
(* meaningful—in C, [[v.u.pair.car]] is accepted *)
(* whenever [[v]] has type [[Value]], but if [[v]] isn't *)
(* a pair, the reference to its [[car]] is meaningless *)
(* (and to evaluate it is an unchecked run-time error). *)
(* In ML, only meaningful references are accepted. *)
(*                                              *)
(* Embedding and projection                     *)
(*                                              *)
(* [*] Inside the interpreter, micro-Scheme values *)
(* sometimes need to be converted to or from ML values *)
(* of other types.                              *)
(*                                              *)
(*   • When it sees a quote mark and brackets, the *)
(*  parser defined in the Supplement produces a *)
(*  native ML list of S-expressions represented by a *)
(*  combination of [[[]]] and [[::]]. But the   *)
(*  evaluator needs a micro-Scheme list represented *)
(*  by a combination of [[NIL]] and [[PAIR]].   *)
(*   • When it evaluates a condition in a micro-Scheme *)
(*  [[if]] expression, the evaluator produces a *)
(*  micro-Scheme value. But to test that value with *)
(*  a native ML [[if]] expression, the evaluator *)
(*  needs a native ML Boolean.                  *)
(*                                              *)
(* Such needs are met by using functions that convert *)
(* values from one language to another. Similar needs *)
(* arise whenever one language is used to implement or *)
(* describe another, and to keep two such languages *)
(* straight, we typically resort to jargon:     *)
(*                                              *)
(*   • The language being implemented or described—in *)
(*  our case, micro-Scheme—is called the object *)
(*  language.                                   *)
(*   • The language doing the describing or   *)
(*  implementation—in our case, ML—is called the *)
(*  metalanguage. (The name ML actually stands for *)
(*  ``metalanguage.'')                          *)
(*                                              *)
(* To convert, say, an integer between object language *)
(* and metalanguage, we use a pair of functions called *)
(* embedding and projection. The embedding puts a *)
(* metalanguage integer into the object language, *)
(* converting an [[int]] into a [[value]].      *)
(* The projection extracts a metalanguage integer from *)
(* the object language, converting a [[value]] into an  *)
(* [[int]]. If the [[value]] can't be interpreted as an *)
(* integer, the projection fails. [In general, we embed *)
(* a smaller set into a larger set. Embeddings don't *)
(* fail, but projections might. A mathematician would *)
(* say that an embedding e of S into S' is an injection *)
(* from S-->S'. The corresponding projection pi_e is a *)
(* left inverse of the embedding; that is pi_e oe is the *)
(* identity function on S. There is no corresponding *)
(* guarantee for e opi_e; for example, pi_e may be *)
(* undefined (_|_) on some elements of S', or e(pi_e(x)) *)
(*  may not equal x. ] The embedding/projection pair for *)
(* integers is defined as follows:              *)

  | valueString (ARRAY vs) =
                                                                        (*OMIT*)
      "[" ^ spaceSep (map valueString (Array.foldr op :: [] vs)) ^ "]" (*OMIT*)
(* \qvspace-0.4                                 *)
(*                                              *)
(* <definition of [[expString]] for {\tuscheme}>= *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun formal (x, tau) = bracketSpace [typeString tau, x]
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)

      fun tybinding ((x, ty), e) = bracketSpace [formal (x, ty), expString e]
      and tybindings bs = bracket (spaceSep (map tybinding bs))
      val letkind = fn LET => "let" | LETSTAR => "let*"
  in  case e
        of LITERAL v         => valueString v
         | VAR name          => name
         | SET (x, e)        => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3)  => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) => 
                       bracketSpace ["while", expString cond, expString body]
         | BEGIN es          => bracketSpace ("begin" :: exps es)
         | APPLY (e, es)     => bracketSpace (exps (e::es))
         | LETX (lk, bs, e)  => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LETRECX (bs, e)  => bracketSpace ["letrec", tybindings bs, expString
                                                                              e]
         | LAMBDA (xs, e)    =>
             bracketSpace ["lambda", bracketSpace (map formal xs), expString e]
         | TYLAMBDA (alphas, e) =>
             bracketSpace ["type-lambda", bracketSpace alphas, expString e]
         | TYAPPLY (e, taus) =>
             bracketSpace ("@" :: expString e :: map typeString taus)
  end
(* <definitions of [[defString]] and [[defName]] for {\tuscheme}>= *)
fun defString d =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun formal (x, t) = "[" ^ x ^ " : " ^ typeString t ^ "]"
  in  case d
        of EXP e => expString e
         | VAL (x, e) => bracketSpace ["val", x, expString e]
         | VALREC (x, tau, e) =>
             bracketSpace ["val-rec", formal (x, tau), expString e]
         | DEFINE (f, rtau, (formals, body)) =>
             bracketSpace ["define", typeString rtau, f,
                           bracketSpace (map formal formals), expString body]
  end
(* <definitions of [[defString]] and [[defName]] for {\tuscheme}>= *)
fun defName (VAL (x, _))       = x
  | defName (VALREC (x, _, _)) = x
  | defName (DEFINE (x, _, _)) = x
  | defName (EXP _) = raise InternalError "asked for name defined by expression"


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON VALUES ({\FOOTNOTESIZE \USCHEME, \TUSCHEME, \NML}) *)
(*                                                               *)
(*****************************************************************)

(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
val unitVal = NIL
(* Each of these type constructors creates a type or *)
(* types that are inhabited by certain forms of *)
(* [[value]]. For example, types [[int]] and [[bool]] *)
(* are inhabited by [[value]]s of the form \monoboxNUM n *)
(* and \monoboxBOOLV b; that's what [[eval]] returns *)
(* when interpreting an expression of type [[int]] or  *)
(* [[bool]]. What about type [[unit]]? That type also *)
(* needs an inhabitant, which is defined here:  *)
(* <boxed values 10>=                           *)
val _ = op unitVal : value
(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
fun embedInt n = NUM n
fun projectInt (NUM n) = n
  | projectInt v =
      raise RuntimeError ("value " ^ valueString v ^ " is not an integer")
(* <boxed values 15>=                           *)
val _ = op embedInt   : int   -> value
val _ = op projectInt : value -> int
(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
fun embedBool b = BOOLV b
fun projectBool (BOOLV false) = false
  | projectBool _             = true
(* Embedding and projection for Booleans is a little *)
(* different; unlike some projection functions, *)
(* [[projectBool]] is total: it always succeeds. *)
(* Function [[projectBool]] reflects the operational *)
(* semantics of micro-Scheme, which treats any value *)
(* other than [[#f]] as a true value. [A~Boolean *)
(* projection function formalizes the concepts of *)
(* ``truthy'' and ``falsy'' found in languages like \js: *)
(* a~value is truthy if it projects to~[[true]] and *)
(* falsy if it projects to~[[false]].] [*]      *)
(* <boxed values 16>=                           *)
val _ = op embedBool   : bool  -> value
val _ = op projectBool : value -> bool
(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
fun equalatoms (NIL,      NIL    )  = true
  | equalatoms (NUM  n1,  NUM  n2)  = (n1 = n2)
  | equalatoms (SYM  v1,  SYM  v2)  = (v1 = v2)
  | equalatoms (BOOLV b1, BOOLV b2) = (b1 = b2)
  | equalatoms  _                   = false
(* <boxed values 137>=                          *)
val _ = op equalatoms : value * value -> bool
(* In a unit test written with [[check-expect]], lists *)
(* are compared for equality structurally, the way the *)
(* micro-Scheme function [[equal?]] does.       *)

(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)
(* <boxed values 138>=                          *)
val _ = op equalpairs : value * value -> bool
(* Each of these type constructors creates a type or *)
(* types that are inhabited by certain forms of *)
(* [[value]]. For example, types [[int]] and [[bool]] *)
(* are inhabited by [[value]]s of the form \monoboxNUM n *)
(* and \monoboxBOOLV b; that's what [[eval]] returns *)
(* when interpreting an expression of type [[int]] or  *)
(* [[bool]]. What about type [[unit]]? That type also *)
(* needs an inhabitant, which is defined here:  *)

(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
val testEquals = equalpairs
(* <boxed values 139>=                          *)
val _ = op testEquals : value * value -> bool
(* <utility functions on values ({\footnotesize \uscheme, \tuscheme, \nml})>= *)
fun cycleThrough xs =
  let val remaining = ref xs
      fun next () = case !remaining
                      of [] => (remaining := xs; next ())
                       | x :: xs => (remaining := xs; x)
  in  if null xs then
        raise InternalError "empty list given to cycleThrough"
      else
        next
  end
val unspecified =
  cycleThrough [ BOOLV true, NUM 39, SYM "this value is unspecified", NIL
               , PRIMITIVE (fn _ => raise RuntimeError "unspecified primitive")
               ]
(* Unspecified values                           *)
(*                                              *)
(* In a [[val]] or [[letrec]] binding, the operational *)
(* semantics of micro-Scheme call for the allocation of *)
(* a location containing an unspecified value. My C code *)
(* chooses a value at random, but the initial basis of *)
(* Standard ML has no random-number generator. So unlike *)
(* the C [[unspecified]] function in \chunkref  *)
(* schemea.chunk.unspecified, the ML version just cycles *)
(* through a few different values. It's probably enough *)
(* to prevent careless people from assuming that such a *)
(* value is always [[NIL]].                     *)
(* <boxed values 161>=                          *)
val _ = op cycleThrough : 'a list -> (unit -> 'a)
val _ = op unspecified  : unit -> value
(* Supporting code for Typed Impcore            *)
(*                                              *)
(* [*][*] [*] \invisiblelocaltableofcontents[*] *)
(*                                              *)
(* Organizing code chunks \chaptocbacksplitinto an *)
(* interpreter                                  *)
(*                                              *)
(* Like all the interpreters from \crefmlscheme.chap *)
(* onward, the Typed Impcore interpreter is defined by *)
(* laying down Noweb chunks in the right order, as *)
(* discussed in \crefmlschemea.chap. The layout is *)
(* similar to that of micro-Scheme, but Typed Impcore *)
(* has two additional chunks which are related to type *)
(* checking: [[]] and                           *)
(* [[]].                                        *)




(*****************************************************************)
(*                                                               *)
(*   CAPTURE-AVOIDING SUBSTITUTION FOR {\TUSCHEME}               *)
(*                                                               *)
(*****************************************************************)

(* <capture-avoiding substitution for {\tuscheme}>= *)
fun tysubst (tau, varenv) =
  let
   (* <definition of [[renameForallAvoiding]] for {\tuscheme} ((prototype))>= *)
      fun renameForallAvoiding (alphas, tau, captured) =
        raise LeftAsExercise "renameForallAvoiding"
      (* <boxed values 180>=                          *)
      val _ = op renameForallAvoiding : name list * tyex * name set -> tyex
      fun subst (TYVAR a) = (find (a, varenv) handle NotFound _ => TYVAR a)
        | subst (TYCON c) = (TYCON c)
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
        | subst (FUNTY  (taus, tau)) = FUNTY  (map subst taus, subst tau)
        | subst (FORALL (alphas, tau)) =
           (* To avoid capture, [[tysubst]] identifies and renames *)
           (* bindings that might capture a variable. The scenario *)
           (* has three parts:                             *)
           (*                                              *)
           (*   • A type tau_new is substituted for a variable that *)
           (*  appears free in \nomathbreak\/\ldotsnalpha\alldot *)
           (*  tau.                                        *)
           (*   • Among the free variables of type tau_new is one *)
           (*  of the very type variables alpha_i that appears *)
           (*  under the \/.                               *)
           (*   • To avoid capturing alpha_i, the bound alpha_i has *)
           (*  to be renamed.                              *)
           (*                                              *)
           (* Below, alpha_i's that have to be renamed are put in a *)
           (* set called [[actual_captures]]. If the set is empty, *)
           (* the code above works. Otherwise, the variables in *)
           (* [[actual_captures]] are renamed by function  *)
           (* [[renameForallAvoiding]]. [*] [*]            *)

(* <use [[varenv]] to substitute in [[tau]]; don't capture or substitute for any [[alphas]]>= *)
           let val free               = freetyvars (FORALL (alphas, tau))
               val new_taus           = map (subst o TYVAR) free
               val potential_captures = foldl union emptyset (map freetyvars
                                                                       new_taus)
               val actual_captures    = inter (potential_captures, alphas)
           in  if true then

(* <substitute [[varenv]] in \monobox{FORALL (alphas, tau)} (OK only if there is no capture)>= *)
                 let val varenv' = varenv <+> mkEnv (alphas, map TYVAR alphas)
                 in  FORALL (alphas, tysubst (tau, varenv'))
                 end
               else
                 subst (renameForallAvoiding (alphas, tau, potential_captures))
           end
           (* When capture may occur, function             *)
           (* [[renameForallAvoiding]] renames the [[alphas]] to *)
           (* avoid potentially captured variables. It must return *)
           (* a type that is equivalent to \monoboxFORALL (alphas, *)
           (* tau) but that does not result in variable capture. *)
           (* In detail, renameForallAvoiding([\ldotsnalpha], tau, *)
           (* C) returns a type \/\ldotsnbeta\alldottau' that has *)
           (* these properties: {gather*} \/\ldotsnbeta\alldottau' *)
           (* ===\/\ldotsnalpha\alldottau\text,            *)
           (* {\ldotsnbeta}\capC = \emptyset\text. {gather*} The *)
           (* implementation of [[renameForallAvoiding]] is left to *)
           (* you (\creftypesys.ex.renameForallAvoiding).  *)

(* The free type variables of a type environment, which *)
(* are needed to enforce the side condition in rule \ *)
(* rulenameTylambda \cpagereftuscheme.rule.Tylambda, are *)
(* computed by calling function [[freetyvarsGamma]]. \ *)
(* tusflabelfreetyvarsGamma                     *)
(* <boxed values 4>=                            *)
val _ = op freetyvarsGamma : tyex env -> name set
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Substitution avoids capturing variables      *)
(*                                              *)
(* Substitution must not only avoid substituting for *)
(* bound occurrences; it must also avoid changing a free *)
(* occurrence to a bound occurrence. That is, supposing *)
(* tau is substituted for [['c]], every type variable *)
(* that is free in tau must also be free in the result. *)
(* As an example, let us substitute \monobox(list 'b) *)
(* for [['c]] in example types A and B: {smallverbatim} *)
(* ('c -> (forall ['a] ('a -> 'c))) ; example A ('c -> *)
(* (forall ['b] ('b -> 'c))) ; example B ((list 'b) -> *)
(* (forall ['a] ('a -> (list 'b)))) ; A substituted *)
(* ((list 'b) -> (forall ['b] ('b -> (list 'b)))) ; B *)
(* substituted WRONG {smallverbatim} In both examples, *)
(* we substitute for two free occurrences of [['c]]. *)
(* The type we substitute has one free occurrence of  *)
(* [['b]]. In example A, the result has, as expected, *)
(* two free occurrences of [['b]], But in example B, the *)
(* second free occurrence of [['b]] has become a bound *)
(* occurrence. We say variable [['b]] is captured. *)
(*                                              *)
(* Capture is a problem in any computation that involves *)
(* substitutions—think macros—and the problem is one we *)
(* have solved before: it's the problem of the faulty *)
(* [[let]] sugar for [[||]] in \cref            *)
(* scheme.or-sugar-capture-let (\cpageref       *)
(* scheme.or-sugar-capture-let). The faulty sugar *)
(* suggests that \monobox(|| e_1 e_2) be implemented by *)
(* substituting for e_1 and e_2 in this template: *)
(*                                              *)
(*  \mono(let ([x e_1]) (if x x e_2)).          *)
(*                                              *)
(* The substitution fails if [[x]] appears as a free *)
(* variable in e_2—when e_2 is substituted into the *)
(* template for [[||]], variable [[x]] is captured and *)
(* its meaning is changed. Capture is avoided by *)
(* renaming the bound variable [[x]] to something that *)
(* is not free in e_2.                          *)
(*                                              *)
(* A polymorphic type system avoids capture in the same *)
(* way: by renaming a bound type variable. And in the *)
(* type system, renaming is easy to justify: when we *)
(* rename, instead of substituting into example type B, *)
(* we are substituting into an equivalent type. In the *)
(* example above, the only type variable bound in B is  *)
(* [['b]], and we rename it to [['z]]: {smallverbatim} *)
(* ('c -> (forall ('z) ('z -> 'c))) ; equivalent to B *)
(* ((list 'b) -> (forall ('z) ('z -> (list 'b)))) ; and *)
(* now substituted {smallverbatim} Now the substitution *)
(* is correct, and the result is equivalent to  *)
(* A substituted.                               *)
(*                                              *)
(* Specifying and implementing substitution     *)
(*                                              *)
(* Let's generalize from the example to a specification. *)
(* To substitute one type tau for free occurrences of *)
(* type variable alpha, without allowing any variable to *)
(* be captured, the judgment form is \jform     *)
(* [tuscheme.substitution]\alphasubsttau' tau''. It is *)
(* pronounced ``tau' with alpha going to tau is *)
(* equivalent to tau''.''                       *)
(*                                              *)
(* Substitution for alpha changes only alpha. And it *)
(* preserves the structure of constructors, constructor *)
(* applications, and function types. {mathpar} \ *)
(* inferrule \alphasubstalpha tau               *)
(*                                              *)
(* \inferrulealpha!=alpha' \alphasubstalpha' alpha' *)
(*                                              *)
(* \inferrule \alphasubstµµ \inferrule\alphasubsttau'tau *)
(* '' \alphasubst((\bareldotsntau') tau') (\asfuntau'_1, *)
(* ..., \asfuntau'_n) tau''                     *)
(*                                              *)
(* \inferrule \alphasubst(\crossdotsntau' -->tau') \ *)
(* asfuntau'_1 *...*\asfuntau'_n --> \asfuntau' *)
(* {mathpar} Substitution into a quantified type may *)
(* substitute for free variables only, and it may not *)
(* capture a free type variable of tau:\notation \capset *)
(* intersection\notation[empty]\emptysetthe empty set *)
(* {mathpar} \inferrule alpha\notin{\ldotsnalpha} \ *)
(* andalso \ftv(tau) \cap{\ldotsnalpha}= \emptyset \ *)
(* alphasubst(\/\ldotsnalpha\alldottau') \/\ldotsnalpha\ *)
(* alldot(\asfuntau') \fracsuffix. {mathpar} The second *)
(* premise prevents variable capture. Substitution can *)
(* proceed without capture by substituting into an *)
(* equivalent type: {mathpar} \inferrule tau' ===tau'' \ *)
(* alphasubsttau' \asfuntau''\fracsuffix. {mathpar} *)
(* Substitution for a bound variable has no effect: *)
(* {mathpar} \inferrule (\/\ldotsnalpha\alldottau')[ *)
(* alpha_i |->tau] === (\/\ldotsnalpha\alldottau') \ *)
(* fracsuffix. {mathpar}                        *)
(*                                              *)
(* In Typed uScheme, substituting for a single type *)
(* variable isn't enough; instantiation substitutes for *)
(* multiple type variables simultaneously.      *)
(* A substitution is represented by an environment of *)
(* type \monoboxtyex env, which is passed to function *)
(* [[tysubst]] as parameter [[varenv]]. This environment *)
(* maps each type variable to the type that should be *)
(* substituted for it. If a type variable is not mapped, *)
(* substitution leaves it unchanged. [*] \tusflabel *)
(* tysubst                                      *)
(* <boxed values 4>=                            *)
val _ = op tysubst : tyex * tyex env -> tyex
val _ = op subst   : tyex            -> tyex
(* \makenowebnotdef(left as an exercise)        *)

  in  subst tau
  end
(* <capture-avoiding substitution for {\tuscheme}>= *)
fun rename (alphas, betas, tau) =
  tysubst (tau, mkEnv (alphas, map TYVAR betas))
(* Renaming and instantiation                   *)
(*                                              *)
(* Renaming is a special case of substitution.  *)
(* It substitutes one set of variables for another. *)
(* <boxed values 5>=                            *)
val _ = op rename : name list * name list * tyex -> tyex
(* <capture-avoiding substitution for {\tuscheme}>= *)
fun instantiate (FORALL (formals, tau), actuals, Delta) =
      (case List.find (fn t => not (eqKind (kindof (t, Delta), TYPE)))
                      actuals
         of SOME t => raise TypeError
                                ("instantiated at type constructor `" ^
                                 typeString t ^ "', which is not a type")
          | NONE =>
              (tysubst (tau, mkEnv (formals, actuals))
               handle BindListLength =>
                 raise TypeError
                   "instantiated polymorphic term at wrong number of types"))
  | instantiate (tau, _, _) =
       raise TypeError ("tried to instantiate term " ^
                        "of non-quantified type " ^ typeString tau)
(* Instantiation is also implemented by substitution. *)
(* It builds a type environment that maps formal type *)
(* parameters to actual type parameters. Most of the *)
(* code enforces restrictions: only quantified types may *)
(* be instantiated, only at actual types of kind *)
(* [[TYPE]], and only with the right number of types. \ *)
(* tusflabelinstantiate                         *)
(* <boxed values 6>=                            *)
val _ = op instantiate : tyex * tyex list * kind env -> tyex
val _ = List.find : ('a -> bool) -> 'a list -> 'a option
(* The Standard ML function [[List.find]] takes a *)
(* predicate and searches a list for an element *)
(* satisfying that predicate.                   *)
(*                                              *)
(* \stdbreak                                    *)



(*****************************************************************)
(*                                                               *)
(*   TYPE EQUIVALENCE FOR {\TUSCHEME}                            *)
(*                                                               *)
(*****************************************************************)

(* <type equivalence for {\tuscheme}>=          *)
(* <infinite supply of type variables>=         *)
val infiniteTyvars = 
  streamMap (fn n => "'b-" ^ intString n) naturals
(* An infinite stream of type variables         *)
(*                                              *)
(* The stream [[infiniteTyvars]] is used to rename type *)
(* variables when checking type equality (\cref *)
(* typesys.chap). It is built from stream [[naturals]], *)
(* which contains the natural numbers; [[naturals]] is *)
(* defined in \chunkrefmlinterps.chunk.naturals in \cref *)
(* mlinterps.chap.                              *)
(* <boxed values 194>=                          *)
val _ = op naturals       : int stream
val _ = op infiniteTyvars : name stream
(* Complete implementation of \tuschemeheader type *)
(* checking                                     *)
(*                                              *)
(* [*] This Appendix presents the solutions to some of *)
(* the implementation problems in Chapter [->]. *)
(*                                              *)
(* Capture with type-lambda                     *)
(*                                              *)
(* Answers to \crefpage                         *)
(* (tuscheme.ex.forall-a-a,tuscheme.ex.evil-type-lambda. *)
(*                                              *)
(* The hole in my type system is not so little. *)

fun eqType (TYVAR a, TYVAR a') = a = a'
  | eqType (TYCON c, TYCON c') = c = c'
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (FUNTY (taus, tau), FUNTY (taus', tau')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (FORALL (alphas, tau), FORALL (alphas', tau')) =
      (* Given types formed with [[FORALL]], say \/\ldotsn *)
      (* alpha\alldottau and \/\ldotsnalpha' \alldottau', *)
      (* [[eqType]] first renames the bound type variables on *)
      (* both sides to \ldotsnbeta. It then compares the *)
      (* renamed types: {align*} --- \/\ldotsnbeta\alldottau[\ *)
      (* ldotsmapstonalphabeta]\text and              *)
      (* --- \/\ldotsnbeta\alldottau'[\mapdotsnalpha'beta]\ *)
      (* text. {align*} According to rule \rulename   *)
      (* EquivQuantifieds, the comparison succeeds if the *)
      (* first body type, \nomathbreak tau[\ldotsmapstonalpha *)
      (* beta] , is equivalent to \nomathbreaktau'[\mapdotsn *)
      (* alpha'beta].                                 *)
      (*                                              *)
      (* Because all the alpha_i's and alpha_i''s are renamed, *)
      (* no beta_j can collide with an existing alpha_i or  *)
      (* alpha'_i.                                    *)

(* <Boolean saying if \monobox{FORALL (alphas, tau)} $\equiv$ \monobox{FORALL (alphas', tau')}>= *)
      let fun ok a  =
            not (member a (freetyvars tau) orelse member a (freetyvars tau'))
          val betas = streamTake (length alphas, streamFilter ok infiniteTyvars)
      in  length alphas = length alphas' andalso
          eqType (rename (alphas, betas, tau), rename (alphas', betas, tau'))
      end
  | eqType _ = false
and eqTypes (taus, taus') = ListPair.allEq eqType (taus, taus')
(* When can't we rename a bound type variable? Imagine *)
(* a function that takes a value of any type and returns *)
(* a value of type [['c]]; that is, imagine a function *)
(* of type \monobox(forall ['a] ('a -> 'c)). Renaming *)
(* [['a]] to [['b]] doesn't change the type:    *)
(* {smallverbatim} (forall ['a] ('a -> 'c)) ; equivalent *)
(* types (forall ['b] ('b -> 'c)) {smallverbatim} But *)
(* renaming [['a]] to [['c]] does change the type: *)
(* {smallverbatim} (forall ['c] ('c -> 'c)) ; not *)
(* equivalent to the first two {smallverbatim}  *)
(* As illustrated above, type \monobox(forall ['c] ('c *)
(* -> 'c)) is the type of the identity function, and *)
(* it's not the same as \monobox(forall ['a] ('a -> *)
(* 'c)). \qbreak Functions of these types have to behave *)
(* differently: a function of type \monobox(forall ['a] *)
(* ('a -> 'c)) ignores its argument, and a function of *)
(* type \monobox(forall ['c] ('c -> 'c)) returns its *)
(* argument.                                    *)
(*                                              *)
(* That last renaming is invalid because it captures *)
(* type variable [['c]]: [['c]] is free in the original *)
(* type but bound in the new type, so its meaning has *)
(* been changed. Whenever we rename a bound type *)
(* variable, whether it is bound by [[forall]] or *)
(* [[type-lambda]], we must not capture any free type *)
(* variables. (The same restriction applies to the *)
(* formal parameters of a [[lambda]] expression; *)
(* for example, [[x]] can be renamed to [[y]] in \ *)
(* monobox(lambda (x) (+ x n)), but [[x]] can't be *)
(* renamed to [[n]]; \monobox(lambda (n) (+ n n)) is not *)
(* the same function!) Also, when we substitute a type  *)
(* tau for a free type variable, we must not capture any *)
(* free type variables of tau.                  *)
(*                                              *)
(* Soundness of type equivalence in Typed uScheme *)
(*                                              *)
(* Why is it sound to consider types equivalent if one *)
(* can be obtained from the other by renaming bound type *)
(* variables? Because if two types differ only in the *)
(* names of their bound type variables, no combination *)
(* of instantiations and substitutions can distinguish *)
(* them. To show what it means to distinguish types by *)
(* instantiation and substitution, let's compare the *)
(* three types above. First I instantiate each type at *)
(* tau_1, then I substitute tau_2 for free occurrences *)
(* of [['c]]:                                   *)
(*                                              *)
(*  Original type       After          After    *)
(*                      instantiation  substitution *)
(*  \monobox(forall     \monobox(tau_1 \monobox(tau_1 *)
(*  ['a] ('a -> 'c))    -> 'c)         -> tau_2) *)
(*  \monobox(forall     \monobox(tau_1 \monobox(tau_1 *)
(*  ['b] ('b -> 'c))    -> 'c)         -> tau_2) *)
(*  \monobox(forall     \monobox(tau_1 \monobox(tau_1 *)
(*  ['c] ('c -> 'c))    -> tau_1)      -> tau_1) *)
(*                                              *)
(* No matter how tau_1 and tau_2 are chosen, the first *)
(* two [[forall]] types produce identical results. But *)
(* when tau_1 and tau_2 are chosen intelligently—[[int]] *)
(* and [[bool]] will do—the first two [[forall]] types *)
(* become \monobox(int -> bool), but the third one *)
(* becomes \monobox(int -> int), which is different. *)
(*                                              *)
(* Rules and code for type equivalence          *)
(*                                              *)
(* [*] Typed uScheme's type equivalence tau ===tau' is *)
(* defined by a proof system. A type variable or type *)
(* constructor is equivalent to itself, and type *)
(* equivalence is structural through function types, *)
(* constructor applications and quantifications.\jlabel *)
(* tuscheme.equiv.typetau===tau'\notation ===type *)
(* equivalence {opspar} \tyruleEqualVariables alpha=== *)
(* alpha                                        *)
(*                                              *)
(* \tyruleEqualConstructors µ===µ             *)
(*                                              *)
(* \tyruleEquivFuns \repeatitau_i ===tau_i' \andalsotau= *)
(* ==tau' \crossdotsntau-->tau===\crossdotsntau' -->tau' *)
(* \usetyEquivApplications[*]                   *)
(*                                              *)
(* \tyruleEquivQuantifieds tau===tau' \/\ldotsnalpha\ *)
(* alldottau===\/\ldotsnalpha\alldottau' {opspar} These *)
(* five rules make syntactically identical types *)
(* equivalent. The next rule makes two types equivalent *)
(* if one is obtained from the other by renaming a bound *)
(* type variable. Provided new type variable beta is not *)
(* free in tau, any alpha_i can be renamed to beta: \ *)
(* tyrule.EquivRenamed \twoquad beta\notin\ftv(tau) beta *)
(* \notin{\ldotsnalpha} \/\ldotsnalpha\alldottau===\/ *)
(* alpha, ..., alpha_i-1, beta, alpha_i+1, \ldotsnalpha\ *)
(* alldottau[alpha_i |->beta] The second premise beta\ *)
(* notin{\ldotsnalpha} ensures that even after the *)
(* renaming, the bound type variables are all distinct. *)
(*                                              *)
(* \qpenalty-500                                *)
(*                                              *)
(* Like any equivalence relation, type equivalence is *)
(* symmetric. (Symmetry permits variables to be renamed *)
(* on the left side of the === sign, for example.) \ *)
(* tyruleSymmetry tau===tau' tau' ===tau Type   *)
(* equivalence is also reflexive and transitive (\cref *)
(* typesys.ex.equiv-refl-trans).                *)
(*                                              *)
(* Typed uScheme's type-equivalence relation is *)
(* implemented by function [[eqType]]. Given types *)
(* formed with [[TYVAR]], [[TYCON]], [[CONAPP]], or *)
(* [[FUNTY]], function [[eqType]] implements the unique *)
(* rule that applies to the form. \tusflabel    *)
(* eqType,eqTypes [*]                           *)
(* <boxed values 2>=                            *)
val _ = op eqType  : tyex      * tyex      -> bool
val _ = op eqTypes : tyex list * tyex list -> bool


(*****************************************************************)
(*                                                               *)
(*   TYPE CHECKING FOR {\TUSCHEME}                               *)
(*                                                               *)
(*****************************************************************)

(* <type checking for {\tuscheme} ((prototype))>= *)
fun typeof _ = raise LeftAsExercise "typeof"
fun typdef _ = raise LeftAsExercise "typdef"
(* <boxed values 1>=                            *)
val _ = op eqKind  : kind      * kind      -> bool
val _ = op eqKinds : kind list * kind list -> bool
(* Typing rules for definitions                 *)
(*                                              *)
(* Just as in the operational semantics, a definition *)
(* can produce a new environment. The new environment is *)
(* a type environment, not a value environment: *)
(* it contains the types of the names introduced by the *)
(* definition. As in Typed Impcore, the new environment *)
(* is produced by typing the definition. \jlabel *)
(* tuscheme.type.def\toptd -->Gamma' The relevant *)
(* judgment has the form \toptd -->Gamma', which says *)
(* that when definition d is typed in kind environment  *)
(* Delta and type environment Gamma, the new type *)
(* environment is Gamma'. In Typed uScheme, a definition *)
(* does not introduce any new types, so typing a *)
(* definition leaves Delta unchanged.           *)
(*                                              *)
(* A \xval binding is not recursive, so the name being *)
(* bound is not visible to the right-hand side. \tyrule *)
(* Val \ptypeise tau \topt\xval(x, e) -->Gamma{x |->tau} *)
(*                                              *)
(* A \xvalrec binding, by contrast, is recursive, and it *)
(* requires an explicit type tau. Type tau must be well *)
(* formed and have kind \ktype, and it must be the type *)
(* of the right-hand side. \tyrule ValRec \threeline \ *)
(* kindistau \ktype \ptypeis[{x |->tau}] e tau e has the *)
(* form \xlambda(\mskip0.5mu...\mskip-0.3mu) \topt\ *)
(* xvalrec(x, tau, e) -->Gamma{x |->tau} The bound name  *)
(* x is visible during the typechecking of the  *)
(* right-hand side e, but for safety, x must not be *)
(* evaluated within e until after e's value has been *)
(* stored in x. Restricting e to have the form of a \ *)
(* xlambda prevents e from evaluating x, even if the *)
(* body of the \xlambda mentions x. [In a lazy language *)
(* like Haskell, a right-hand side is not evaluated *)
(* until its value is needed, so a definition like \ *)
(* monobox{(val-rec [x : int] x)} is legal, but *)
(* evaluating~[[x]] produces an infinite loop (sometimes *)
(* called a ``black hole.'')]                   *)
(*                                              *)
(* A top-level expression is syntactic sugar for a *)
(* binding to [[it]]. \tyrule Exp \topt\xval(it, e) --> *)
(* Gamma' \topt\xexp(e) -->Gamma'               *)
(*                                              *)
(* A \xdefine is syntactic sugar for a suitable \xvalrec *)
(* . Indeed, Typed uScheme has \xvalrec only because it *)
(* is easier to typecheck \xvalrec and \xlambda *)
(* independently than to typecheck \xdefine directly. \ *)
(* tyrule Define \topt\xvalrec(f, \crossdotsntau-->tau, *)
(* \xlambda(<x_1 : tau_1, ..., x_n : tau_n>, e)) --> *)
(* Gamma' \topt\xdefine(f, tau, <x_1 : tau_1, ..., x_n : *)
(* tau_n>, e) -->Gamma'                         *)
(*                                              *)
(* Type checking                                *)
(*                                              *)
(* The rules above are to be implemented by a type *)
(* checker, which I hope you will write (\cref  *)
(* typesys.ex.tuscheme). Type checking requires an *)
(* expression or definition, a type environment, and a *)
(* kind environment. Calling \monoboxtypeof(e, Delta, *)
(* Gamma) should return a tau such that \typeise tau, or *)
(* if no such tau exists, it should raise the exception *)
(* [[TypeError]]. Calling \monoboxtypdef(d, Delta, Gamma *)
(* ) should return a pair (Gamma', s), where \toptd --> *)
(* Gamma' and s is a string that represents the type of *)
(* the thing defined. \tusflabeltypeof,typdef [*] *)
(* <boxed values 1>=                            *)
val _ = op typeof : exp * kind env * tyex env -> tyex
val _ = op typdef : def * kind env * tyex env -> tyex env * string
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




(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \TUSCHEME, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* <lexical analysis and parsing for \tuscheme, providing [[filexdefs]] and [[stringsxdefs]]>= *)
(* <lexical analysis for \uscheme\ and related languages>= *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* <boxed values 144>=                          *)
type pretoken = pretoken
type token = token
(* <lexical analysis for \uscheme\ and related languages>= *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* For debugging, code in \creflazyparse.chap needs to *)
(* be able to render a [[token]] as a string.   *)
(* <boxed values 145>=                          *)
val _ = op pretokenString : pretoken -> string
val _ = op tokenString    : token    -> string
(* <lexical analysis for \uscheme\ and related languages>= *)
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
  (* <boxed values 147>=                          *)
  val _ = op noneIfLineEnds : 'a lexer
  (* The [[atom]] function identifies the special literals *)
  (* [[#t]] and [[#f]]; all other atoms are names. *)
  (* <functions used in the lexer for \uscheme>=  *)
  fun atom "#t" = SHARP true
    | atom "#f" = SHARP false
    | atom x    = NAME x
in
  val schemeToken =
    whitespace *>
    bracketLexer   (  QUOTE   <$  eqx #"'" one
                  <|> INT     <$> intToken isDelim
                  <|> (atom o implode) <$> many1 (sat (not o isDelim) one)
                  <|> noneIfLineEnds
                   )
(* \qbreak                                      *)
(*                                              *)
(* Lexical analysis for micro-Scheme            *)
(*                                              *)
(* Lexical analysis turns a character stream into a *)
(* token stream. The [[schemeToken]] function tries each *)
(* alternative in turn: the two brackets, a quote mark, *)
(* an integer literal, an atom, or end of line. An atom *)
(* may be a [[SHARP]] name or a normal name.    *)
(*                                              *)
(* Before each micro-Scheme token, whitespace is *)
(* ignored. [*]                                 *)
(* <boxed values 146>=                          *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> pretoken
(* [[checkExpectPasses]] runs a                 *)
(* [[check-expect]] test and tells if the test passes. *)
(* If the test does not pass, [[checkExpectPasses]] also *)
(* writes an error message. Error messages are written *)
(* using [[failtest]], which, after writing the error *)
(* message, indicates failure by returning [[false]]. *)

end
(* <parsers for single tokens for \uscheme-like languages>= *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val namelike  = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val namelike  = asAscii namelike
(* <boxed values 148>=                          *)
val _ = op booltok  : bool parser
val _ = op int      : int  parser
val _ = op namelike : name parser
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

(* Typed uScheme reuses most of micro-Scheme's lexical *)
(* analysis, but to simplify the parsing of function *)
(* types, the arrow [[->]] is treated as a reserved *)
(* word, not as a name.                         *)
(*                                              *)
(* To help with error messages, the parser tracks which *)
(* keywords are used to write expressions and which are *)
(* used to write types.                         *)
(* <parsers for \tuscheme\ tokens>=             *)
val expKeywords = [ "if", "while", "set", "begin", "lambda"
                  , "type-lambda", "let", "let*", "letrec", "quote", "@"
                  ]
val tyKeywords  = ["forall", "->"]

val defKeywords = [ "val", "define", "use"
                  , "check-expect", "check-assert", "check-error"
                  , "check-type", "check-type-error"
                  ]

val reserved = expKeywords @ tyKeywords @ defKeywords

fun keyword words =
  let fun isKeyword s = List.exists (fn s' => s = s') words
  in  sat isKeyword namelike
  end

val expKeyword = keyword expKeywords
val tyKeyword  = keyword tyKeywords
val name =
  rejectReserved reserved <$>! sat (curry op <> "->") namelike
(* <parsers for \tuscheme\ tokens>=             *)
val arrow = (fn (NAME "->") => SOME () | _ => NONE) <$>? pretoken
val tyvar = 
  quote *> (curry op ^ "'" <$> name <?> "type variable (got quote mark)")
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
(* Building on the single tokens, I define parsers that *)
(* handle syntactic elements used in multiple   *)
(* Scheme-like languages. Function [[formals]] parses a *)
(* list of formal parameters. In a list of formal *)
(* parameters, if not all parameter names are mutually *)
(* distinct, it's treated as a syntax error. Function *)
(* [[bindings]] produces a list of bindings suitable for *)
(* use in [[let*]] expressions. \nwnarrowboxes  *)
(* <boxed values 149>=                          *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
(* <boxed values 149>=                          *)
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* <parsers and parser builders for formal parameters and bindings ((higher-order))>= *)
fun asLambda inWhat (loc, e as LAMBDA _) = OK e
  | asLambda inWhat (loc, e) = 
      synerrorAt ("in " ^ inWhat ^ ", expression " ^ expString e ^ 
                  " is not a lambda")
                 loc

val asLambda = fn what => fn eparser => asLambda what <$>! @@ eparser
(* A [[letrec]] may bind only lambda expressions. *)
(* <boxed values 150>=                          *)
val _ = op asLambda : string -> exp parser -> exp parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* <boxed values 151>=                          *)
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
(* <boxed values 153>=                          *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* <parsers and parser builders for \scheme-like syntax>= *)
fun sexp tokens = (
     SYM       <$> (notDot <$>! @@ namelike)
 <|> NUM       <$> int
 <|> embedBool <$> booltok
 <|> leftCurly <!> "curly brackets may not be used in S-expressions"
 <|> embedList <$> bracket ("list of S-expressions", many sexp)
 <|> (fn v => embedList [SYM "quote", v]) 
               <$> (quote *> sexp)
) tokens
and notDot (loc, ".") =
      synerrorAt "this interpreter cannot handle . in quoted S-expressions" loc
  | notDot (_,   s)   = OK s
(* With this machinery I can define a parser for quoted *)
(* S-expressions. A quoted S-expression is a symbol, *)
(* a number, a Boolean, a list of S-expressions, or a *)
(* quoted S-expression.                         *)
(* <boxed values 152>=                          *)
val _ = op sexp : value parser
(* Full Scheme allows programmers to notate arbitrary *)
(* cons cells using a dot in a quoted S-expression. *)
(* micro-Scheme doesn't.                        *)

(* <parsers and parser builders for \scheme-like syntax>= *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> NUM       <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* <parsers and parser builders for \scheme-like syntax>= *)
fun fullSchemeExpOf atomic bracketedOf =
  let val exp = fn tokens => fullSchemeExpOf atomic bracketedOf tokens
  in      atomic
      <|> bracketedOf exp
      <|> quote *> (LITERAL <$> sexp)
      <|> quote *> badRight "quote ' followed by right bracket"
      <|> leftCurly <!> "curly brackets are not supported"
      <|> left *> right <!> "(): unquoted empty parentheses"
      <|> bracket("function application", curry APPLY <$> exp <*> many exp)
  end
(* <boxed values 155>=                          *)
val _ = op fullSchemeExpOf : exp parser -> (exp parser -> exp parser) -> exp
                                                                          parser
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
(* <boxed values 173>=                          *)
val _ = op typedFormalsOf : string parser -> 'b parser -> 'a parser  -> string
                                                    -> (string * 'a) list parser
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
(* <boxed values 187>=                          *)
val _ = op tyvar : name parser
(* <boxed values 187>=                          *)
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
(* <boxed values 187>=                          *)
val _ = op arrowsOf : ('ty * 'ty list -> 'ty) -> ('ty list * 'ty -> 'ty) -> 'ty
                                              list -> 'ty list list -> 'ty error
(* <parser builders for typed languages>=       *)
fun distinctTBsIn tbindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map (fst o fst) bs) >>=+ (fn _ =>
                                                                             bs)
  in  check <$>! @@ tbindings
  end
(* <boxed values 191>=                          *)
val _ = op distinctTBsIn : ((name * 't) * 'e) list parser -> string -> ((name *
                                                           't) * 'e) list parser
(* Formal parameters, whether to [[lambda]] or  *)
(* [[type-lambda]], must not have duplicates.   *)
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
val tlformals =
  nodups ("formal type parameter", "type-lambda") <$>! @@ (many name)

fun nodupsty what (loc, xts) =
  nodups what (loc, map fst xts) >>=+ (fn _ => xts)
                  (* error on duplicate names *)

fun letDups LETSTAR (_, bindings) = OK bindings
  | letDups LET bindings = nodupsty ("bound variable", "let") bindings
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
val arrows = arrowsOf CONAPP FUNTY
fun ty tokens =
  let fun badExpKeyword (loc, bad) =
        synerrorAt ("looking for type but found `" ^ bad ^ "'") loc
  in     TYCON <$> name
     <|> TYVAR <$> tyvar
     <|> bracketKeyword (kw "forall", "(forall [tyvars] type)",
                         curry FORALL <$> bracket ("('a ...)", distinctTyvars)
                                      <*> ty)
     <|> badExpKeyword <$>! (left *> @@ expKeyword <* matchingRight)
     <|> bracket ("type application or function type",
                  arrows <$> many ty <*>! many (arrow *> many ty))
     <|> int     <!> "expected type; found integer"
     <|> booltok <!> "expected type; found Boolean literal"
  end tokens
(* \qbreak The type parser rejects any keyword that is *)
(* normally used to write expressions.          *)
(* <boxed values 188>=                          *)
val _ = op ty : tyex parser
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
fun endsColon s =
  if size s > 1 andalso String.sub (s, size s - 1) = #":" then
    SOME (String.substring (s, 0, size s - 1))
  else
    NONE

val badColonParm =
 (endsColon <$>? name <* sexp)
 errorAtEnd
 (fn s => ["there must be a space between parameter ", s,
           " and the colon that follows it"]) 
(* The parser for formal parameters detects two common *)
(* mistakes: forgetting to put a space before the colon *)
(* that separates name from type, and forgetting to wrap *)
(* a one-element parameter list in brackets. First the *)
(* colon.                                       *)
(* <boxed values 189>=                          *)
val _ = op endsColon    : name -> name option
val _ = op badColonParm : (name * tyex) parser
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
val formal = 
  bracket ("[x : ty]", badColonParm <|> pair <$> name <* kw ":" <*> ty)

val unbracketedFormal =
  (name <* kw ":" <* ty)
  errorAtEnd
  (fn x => ["the formal parameter ", x, " and its type must be ",
            "wrapped in brackets to make a list of length 1"])

val lformals = bracket ("([x : ty] ...)", unbracketedFormal <|> many formal)
val tformals = bracket ("('a ...)", many tyvar)
(* And now the formal parameters. Function [[lformals]] *)
(* parses the formal parameters to [[lambda]], and *)
(* [[tformals]] parses the formal parameters to *)
(* [[type-lambda]].                             *)
(* <boxed values 190>=                          *)
val _ = op formal : (name * tyex) parser
val _ = op unbracketedFormal : (name * tyex) list parser
val _ = op lformals : (name * tyex) list parser
val _ = op tformals : name list parser
(* \qbreak The expression parser rejects any keyword *)
(* that is normally used to write types.        *)
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
fun lambda xs exp =
      nodupsty ("formal parameter", "lambda") xs >>=+ (fn xs => 
      LAMBDA (xs, exp))
fun tylambda a's exp =
      nodups ("formal type parameter", "type-lambda") a's >>=+ (fn a's =>
      TYLAMBDA (a's, exp))

fun cb key usage parser = bracketKeyword (eqx key namelike, usage, parser)

fun exp tokens = (
     VAR               <$> name
 <|> LITERAL <$> NUM   <$> int
 <|> LITERAL <$> BOOLV <$> booltok
 <|> quote *> (LITERAL <$> sexp)
 <|> quote *> badRight "quote mark ' followed by right bracket"
 <|> cb "quote"  "(quote sx)"               (       LITERAL <$> sexp)
 <|> cb "if"     "(if e1 e2 e3)"            (curry3 IFX     <$> exp  <*> exp <*>
                                                                            exp)
 <|> cb "while"  "(while e1 e2)"            (curry  WHILEX  <$> exp  <*> exp)
 <|> cb "set"    "(set x e)"                (curry  SET     <$> name <*> exp)
 <|> cb "begin"  ""                         (       BEGIN   <$> many exp)
 <|> cb "lambda" "(lambda (formals) body)"  (       lambda  <$> @@ lformals <*>!
                                                                            exp)
 <|> cb "type-lambda" "(type-lambda (tyvars) body)"
                                            (       tylambda <$> @@ tformals
                                                                       <*>! exp)
 <|> cb "let"    "(let (bindings) body)"    (letx   LET     <$> @@ bindings <*>!
                                                                            exp)
 <|> cb "letrec" "(letrec (bindings) body)" (curry  LETRECX <$> letrecbs <*> exp
                                                                               )
 <|> cb "let*"   "(let* (bindings) body)"   (letx   LETSTAR <$> @@ bindings <*>!
                                                                            exp)
 <|> cb "@"      "(@ exp types)"            (curry  TYAPPLY <$> exp <*> many1 ty
                                                                               )
 <|> badTyKeyword <$>! left *> @@ tyKeyword <* matchingRight
 <|> leftCurly <!> "curly brackets are not supported"
 <|> left *> right <!> "empty application"
 <|> bracket ("function application", curry APPLY <$> exp <*> many exp)
) tokens

and letx kind bs exp = letDups kind bs >>=+ (fn bs => LETX (kind, bs, exp))
and tybindings ts = bindingsOf "([x : ty] e)" formal exp ts
and letrecbs ts = distinctTBsIn (bindingsOf "([x : ty] e)" formal (asLambda
                                                                  "letrec" exp))
                                "letrec"
                                ts
and bindings ts = bindingsOf "(x e)" name exp ts

and badTyKeyword (loc, bad) =
      synerrorAt ("looking for expression but found `" ^ bad ^ "'") loc
(* \qbreak The [[def]] parser handles the       *)
(* true-definition syntactic forms [[define]], [[val]], *)
(* and [[val-rec]].                             *)
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
fun define tau f formals body =
  nodupsty ("formal parameter", "definition of function " ^ f) formals >>=+ 
  (fn xts =>
    DEFINE (f, tau, (xts, body)))

fun valrec (x, tau) e = VALREC (x, tau, e)

val def =
     cb "define" "(define type f (args) body)"
                          (define <$> ty <*> name <*> @@ lformals <*>! exp)
 <|> cb "val" "(val x e)" (curry VAL <$> name <*> exp)
 <|> cb "val-rec" "(val-rec [x : type] e)"
                          (valrec <$> formal <*> asLambda "val-rec" exp)
(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
val unit_test =
      cb "check-assert" "(check-assert e)"     (CHECK_ASSERT <$> exp)
  <|> cb "check-error"  "(check-error e)"      (CHECK_ERROR  <$> exp)
  <|> cb "check-expect" "(check-expect e1 e2)" 
                                  (curry CHECK_EXPECT <$> exp <*> exp)
  <|> cb "check-type"   "(check-type e tau)"
                                  (curry CHECK_TYPE   <$> exp <*> ty)
  <|> cb "check-type-error" "(check-type-error e)"
                                  (CHECK_TYPE_ERROR <$> (def <|> EXP <$> exp))
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
(* <boxed values 192>=                          *)
val _ = op unit_test : unit_test parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
val xdef = 
     DEF <$> def
 <|> cb "use" "(use filename)" (USE <$> name)
 <|> TEST <$> unit_test
 <|> badRight "unexpected right bracket"
 <|> DEF <$> EXP <$> exp
 <?> "definition"
(* And the [[xdef]] parser handles the extended *)
(* definitions.                                 *)
(* <boxed values 193>=                          *)
val _ = op xdef : xdef parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and [[xdef]] streams for {\tuscheme}>= *)
val xdefstream = interactiveParsedStream (schemeToken, xdef)
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
(* <boxed values 66>=                           *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR {\TUSCHEME} *)
(*                                                               *)
(*****************************************************************)

(* <evaluation, testing, and the read-eval-print loop for {\tuscheme}>= *)
(* <definition of [[namedValueString]] for functional bridge languages>= *)
fun namedValueString x v =
  case v of CLOSURE _ => x
          | PRIMITIVE _ => x
          | _ => valueString v
(* <boxed values 186>=                          *)
val _ = op namedValueString : name -> value -> string
(* <definitions of [[eval]] and [[evaldef]] for {\tuscheme}>= *)
fun eval (e, rho) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let fun ev (LITERAL n) = n
        (* Evaluation in the presence of polymorphism   *)
        (*                                              *)
        (* This chapter is about types, but the code does *)
        (* eventually have to be evaluated. In Typed uScheme*, *)
        (* types have no effect at run time; expressions are *)
        (* therefore evaluated using the same rules as for *)
        (* untyped micro-Scheme. And there are new rules for *)
        (* evaluating type abstraction and application. These *)
        (* rules specify that the evaluator behaves as if these *)
        (* type abstraction and application aren't there. \ops *)
        (* Tyapply \evale ==>\evalr['] v \eval\asttyapply(e, \ *)
        (* ldotsntau) ==>\evalr['] v \jlabeltuscheme.eval.exp<e, *)
        (* rho, sigma> ==><v, sigma'>                   *)
        (*                                              *)
        (* \ops Tylambda \evale ==>\evalr['] v \eval\asttylambda *)
        (* (<\ldotsnalpha>, e) ==>\evalr['] v This semantics is *)
        (* related to a program transformation called type *)
        (* erasure: if you start with a program written in Typed *)
        (* uScheme, and you remove all the [[TYAPPLY]]s and the *)
        (* [[TYLAMBDA]]s, and you remove the types from the *)
        (* [[LAMBDA]]s and the definitions, and you rewrite *)
        (* [[VALREC]] to [[VAL]], then what's left is a *)
        (* micro-Scheme program.                        *)
        (*                                              *)
        (* The evaluator for Typed uScheme resembles the *)
        (* evaluator for micro-Scheme in Chapter [->]. The code *)
        (* for the new forms acts as if [[TYAPPLY]] and *)
        (* [[TYLAMBDA]] aren't there.                   *)
        (* <alternatives for [[ev]] for [[TYAPPLY]] and [[TYLAMBDA]]>= *)
        | ev (TYAPPLY  (e, _)) = ev e
        | ev (TYLAMBDA (_, e)) = ev e
        (* The rest of the evaluator can be found in \cref *)
        (* app:tuscheme.                                *)

        (* Code for variables is just as in Chapter [->]. *)
        (* <more alternatives for [[ev]] for {\tuscheme}>= *)
        | ev (VAR v) = !(find (v, rho))
        | ev (SET (n, e)) = 
            let val v = ev e
            in  find (n, rho) := v;
                v
            end
        (* <more alternatives for [[ev]] for {\tuscheme}>= *)
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
        (* <more alternatives for [[ev]] for {\tuscheme}>= *)
        | ev (LAMBDA (args, body)) = CLOSURE ((map (fn (x, ty) => x) args, body)
                                                                          , rho)
        (* \qbreak Code for application is almost as in Chapter  *)
        (* [->], except if the program tries to apply a *)
        (* non-function, the evaluator raises           *)
        (* [[BugInTypeChecking]], not [[RuntimeError]], because *)
        (* the type checker should reject any program that could *)
        (* apply a non-function.                        *)
        (* <more alternatives for [[ev]] for {\tuscheme}>= *)
        | ev (APPLY (f, args))  = 
               (case ev f
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
               )
        (* Code for the [[LETX]] family is as in Chapter [->]. *)
        (* <more alternatives for [[ev]] for {\tuscheme}>= *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, rho <+> mkEnv (names, map (ref o ev) values))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((n, e), rho) = bind (n, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* <more alternatives for [[ev]] for {\tuscheme}>= *)
        | ev (LETRECX (bs, body)) = 
            let val (tynames, values) = ListPair.unzip bs
                val names = map fst tynames
                val rho' = rho <+> mkEnv (names, map (fn _ => ref (unspecified()
                                                                      )) values)
                val updates = map (fn ((x, _), e) => (x, eval (e, rho'))) bs
            in  List.app (fn (x, v) => find (x, rho') := v) updates; 
                eval (body, rho')
            end
(* Evaluation                                   *)
(*                                              *)
(* The implementation of the evaluator is almost *)
(* identical to the implementation in Chapter [->]. *)
(* There are only two significant differences:  *)
(*                                              *)
(*  \tightlist                                  *)
(*   • The abstract syntax [[LAMBDA]] has types, but the *)
(*  value [[CLOSURE]] does not.                 *)
(*   • The evaluator needs cases for [[TYAPPLY]] and *)
(*  [[TYLAMBDA]].                               *)
(*                                              *)
(* Another difference is that many potential run-time *)
(* errors should be impossible because the relevant code *)
(* would be rejected by the type checker. If one of *)
(* those errors occurs anyway, the evaluator raises the *)
(* exception [[BugInTypeChecking]], not         *)
(* [[RuntimeError]]. \tusflabeleval             *)
(* <boxed values 184>=                          *)
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
(* <definitions of [[eval]] and [[evaldef]] for {\tuscheme}>= *)
fun evaldef (VAL (x, e), rho) =
      let val v   = eval (e, rho)
          val rho = bind (x, ref v, rho)
      in  (rho, namedValueString x v)
      end
  | evaldef (VALREC (x, tau, e), rho) =
      let val this = ref NIL
          val rho' = bind (x, this, rho)
          val v    = eval (e, rho')
          val _    = this := v
      in  (rho', namedValueString x v)
      end
  | evaldef (EXP e, rho) = (* differs from VAL ("it", e) only in its response *)
      let val v   = eval (e, rho)
          val rho = bind ("it", ref v, rho)
      in  (rho, valueString v)
      end
  | evaldef (DEFINE (f, tau, lambda), rho) =
      evaldef (VALREC (f, tau, LAMBDA lambda), rho)
(* Evaluating a definition can produce a new    *)
(* environment. The function [[evaldef]] also returns a *)
(* string which, if nonempty, should be printed to show *)
(* the value of the item. Type soundness requires a *)
(* change in the evaluation rule for [[VAL]]; as *)
(* described in Exercise [->] in Chapter [->], [[VAL]] *)
(* must always create a new binding. \tusflabelevaldef  *)
(* [*]                                          *)
(* <boxed values 185>=                          *)
val _ = op evaldef : def * value ref env -> value ref env * string
(* In the [[VALREC]] case, the interpreter evaluates  *)
(* [[e]] while name [[x]] is still bound to a location *)
(* that contains [[NIL]]—that is, before the assignment *)
(* to [[this]]. Therefore, as described on page [->], *)
(* evaluating [[e]] must not evaluate [[x]]—because the *)
(* mutable cell for [[x]] does not yet contain its *)
(* correct value.[*] Evaluation is prevented in the *)
(* parser, which issues a syntax error unless [[e]] is a *)
(* [[LAMBDA]].                                  *)
(*                                              *)
(* Function [[evaldef]] returns a string identifying *)
(* what was just defined. If the thing just defined is a *)
(* function, [[evaldef]] identifies it by its name. *)
(* Otherwise, it shows the actual value.        *)

(* <definitions of [[basis]] and [[processDef]] for {\tuscheme}>= *)
(* <definition of [[basis]] for {\tuscheme}>=   *)
type basis = kind env * tyex env * value ref env
fun processDef (d, (Delta, Gamma, rho), interactivity) =
  let val (Gamma, tystring)  = typdef (d, Delta, Gamma)
      val (rho,   valstring) = evaldef (d, rho)
      val _ = if echoes interactivity then
                println (valstring ^ " : " ^ tystring)
              else
                ()
(* Function [[processDef]] is very similar to the *)
(* [[processDef]] used in Typed Impcore, but it accounts *)
(* for the different environments used in the basis. *)
(* Instead of two environments to store the types of *)
(* variables and one to store the types of functions, *)
(* Typed uScheme has just the one environment Gamma *)
(* ([[Gamma]]), which stores the type of each   *)
(* identifier, whether it is a variable or a function. *)
(* And Typed uScheme's basis also includes a kind *)
(* environment Delta ([[Delta]]), which stores the kind *)
(* of each type constructor.                    *)
(* <boxed values 182>=                          *)
val _ = op processDef : def * basis * interactivity -> basis
  in  (Delta, Gamma, rho)
  end
fun dump_names (kinds, types, values) = app (println o fst) values  (*OMIT*)
(* <shared definition of [[withHandlers]]>=     *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn)
                        a
                        (fn s => caught (fillAtLoc (s, loc)))
       (* In addition to [[RuntimeError]], [[NotFound]], and *)
       (* [[Located]], [[withHandlers]] catches many exceptions *)
       (* that are predefined ML's Standard Basis Library. *)
       (* These exceptions signal things that can go wrong when *)
       (* evaluating an expression or reading a file.  *)

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]]>= *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
       (* These exception handlers are used in all the *)
       (* bridge-language interpreters.                *)

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
(* <boxed values 46>=                           *)
val _ = op failtest : string list -> bool
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
(* <definition of [[testIsGood]] for {\tuscheme}>= *)
fun testIsGood (test, (Delta, Gamma, rho)) =
  let fun ty e = typeof (e, Delta, Gamma)
                 handle NotFound x =>
                   raise TypeError ("name " ^ x ^ " is not defined")

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
      (* Function [[checks]] confirms that forms      *)
      (* [[check-expect]], [[check-error]], and [[check-type]] *)
      (* contain only expressions that typecheck. But not *)
      (* [[check-type-error]]. The whole point of     *)
      (* [[check-type-error]] is that its expression doesn't *)
      (* typecheck. Thus, it is not typechecked by function *)
      (* [[check]]. Instead, it is typechecked by function *)
      (* [[passes]]—if it has a type, it fails.     *)
      (*                                              *)
      (* Most of the checking functions are implemented in \ *)
      (* crefmlschemea.chap,typesysa.chap. But        *)
      (* [[checkTypeChecks]] is implemented here.     *)

    (* <shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]]>= *)
      fun checkTypeChecks (e, tau) =
        let val tau' = ty e
        in  true
        end
        handle TypeError msg => 
          failtest ["In (check-type ", expString e, " " ^ typeString tau, "), ",
                                                                            msg]
      fun checks (CHECK_EXPECT (e1, e2)) = checkExpectChecks (e1, e2)
        | checks (CHECK_ASSERT e)        = checkAssertChecks e
        | checks (CHECK_ERROR e)         = checkErrorChecks e
        | checks (CHECK_TYPE (e, tau))   = checkTypeChecks (e, tau)
        | checks (CHECK_TYPE_ERROR e)    = true

      fun outcome e =
        withHandlers (fn () => OK (eval (e, rho))) () (ERROR o stripAtLoc)
      (* <[[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml>= *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (* <boxed values 143>=                          *)
      val _ = op asSyntacticValue : exp -> value option
      (* All three functions are stitched together, using a *)
      (* language-dependent [[testEquals]].           *)

    (* <shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]]>= *)
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
      (* <boxed values 42>=                           *)
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
      (* \qbreak Function [[checkExpectPassesWith]] runs a *)
      (* [[check-expect]] test and uses the given [[equals]] *)
      (* to tell if the test passes. If the test does not *)
      (* pass, [[checkExpectPasses]] also writes an error *)
      (* message. Error messages are written using    *)
      (* [[failtest]], which, after writing the error message, *)
      (* indicates failure by returning [[false]].    *)
      (* <boxed values 43>=                           *)
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
      (* <boxed values 44>=                           *)
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
      (* <boxed values 45>=                           *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEquals (cx, ex)
      fun deftystring d =
        snd (typdef (d, Delta, Gamma))
        handle NotFound x =>
          raise TypeError ("name " ^ x ^ " is not defined")
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
        | passes (CHECK_TYPE (c, tau)) = checkTypePasses   (c, tau)
        | passes (CHECK_TYPE_ERROR d)  = checkTypeErrorPasses d

  in  checks test andalso passes test
  end
(* <shared definition of [[processTests]]>=     *)
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
and numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
(* \qbreak Function [[processTests]] is shared among all *)
(* bridge languages. For each test, it calls the *)
(* language-dependent [[testIsGood]], adds up the number *)
(* of good tests, and reports the result. [*]   *)
(* <boxed values 47>=                           *)
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
      (* <boxed values 74>=                           *)
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
(* <boxed values 73>=                           *)
type basis = basis
val _ = op processDef   : def * basis * interactivity -> basis
val _ = op testIsGood   : unit_test      * basis -> bool
val _ = op processTests : unit_test list * basis -> unit
(* <boxed values 73>=                           *)
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
(*   IMPLEMENTATIONS OF \TUSCHEME\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* \qtrim0.5                                    *)
(*                                              *)
(* Primitive functions, predefined functions, and the *)
(* initial basis                                *)
(*                                              *)
(* The initial basis is built from primitives and from *)
(* predefined functions. The [[<<functions for building *)
(* primitives when types are checked>>]] (from \cref *)
(* typesysa.chap) are reused. \makenowebnotdef (from *)
(* chunk [->])                                  *)
(* <implementations of \tuscheme\ primitives and definition of [[initialBasis]]>= *)
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
(* <boxed values 164>=                          *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* <functions for building primitives when types are checked>= *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeChecking "arithmetic on non-numbers")
(* <boxed values 165>=                          *)
val _ = op arithOp : (int * int -> int) -> (value list -> value)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <functions for building primitives when types are checked>= *)
fun makeArray (n, v) = ARRAY (Array.tabulate (n, (fn _ => v)))
fun arrayLength a = NUM (Array.length a)
fun arrayAt (a, i) = 
  Array.sub (a, i) handle Subscript => raise RuntimeError
                                                 "array subscript out of bounds"
fun arrayAtPut (a, i, v) = 
  Array.update (a, i, v) handle Subscript => raise RuntimeError
                                                 "array subscript out of bounds"
fun singletonArray v = ARRAY (Array.tabulate (1, (fn _ => v)))
(* <utility functions and types for making \tuscheme\ primitives>= *)
val arithtype =
  FUNTY ([inttype, inttype], inttype)
(* <boxed values 11>=                           *)
val _ = op unaryOp   : (value         -> value) -> (value list -> value)
val _ = op binaryOp  : (value * value -> value) -> (value list -> value)
val _ = op arithOp   : (int * int -> int)       -> (value list -> value)
val _ = op arithtype : tyex
(* <utility functions and types for making \tuscheme\ primitives>= *)
fun comparison f = binaryOp (BOOLV o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeChecking "comparing non-numbers")
val comptype = FUNTY ([inttype, inttype], booltype)
(* Primitive functions of \tuschemeheader       *)
(*                                              *)
(* [*] The primitives resemble the primitives in *)
(* Chapter [->], except that each primitive comes with a *)
(* type as well as a value.                     *)
(*                                              *)
(* A comparison takes two arguments. Most comparisons *)
(* (but not equality) apply only to integers.   *)
(* <boxed values 183>=                          *)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
val _ = op comptype   : tyex
(* <definition of [[primBasis]] for \tuscheme>= *)
val primBasis =
  let fun addKind ((name, kind), kinds) =
            bind (name, kind, kinds)
      val kinds   = foldl addKind emptyEnv
                    ((* The kinds of the primitive type constructors, which *)
                     (* populate the initial kind environment Delta_0, are *)
                     (* represented as follows. [*]                  *)
                     (* <primitive type constructors for \tuscheme\ [[::]]>= *)
                     ("int",  TYPE) ::
                     ("bool", TYPE) ::
                     ("sym",  TYPE) ::
                     ("unit", TYPE) ::
                     ("list", ARROW ([TYPE], TYPE)) :: [])
      fun addPrim ((name, prim, funty), (types, values)) = 
        ( bind (name, funty, types)
        , bind (name, ref (PRIMITIVE prim), values)
        )
      val (types, values) = foldl addPrim (emptyEnv, emptyEnv)
                            (
                       (* As in Chapter [->], the names, values, and types of *)

                      (* the primitives are written in one long list in chunk *)
                             (* [[]].                                        *)
                             (* That list is used to build the initial basis. *)
                             (* <primitive functions for \tuscheme\ [[::]]>= *)
                             ("+", arithOp op +,   arithtype) :: 
                             ("-", arithOp op -,   arithtype) :: 
                             ("*", arithOp op *,   arithtype) :: 
                             ("/", arithOp op div, arithtype) ::
                             (* The list primitives have polymorphic types.  *)
                             (* <primitive functions for \tuscheme\ [[::]]>= *)
                             ("null?", unaryOp (BOOLV o (fn (NIL   ) => true | _
                                                                      => false))
                                     , FORALL (["'a"], FUNTY ([listtype tvA],
                                                                  booltype))) ::
                             ("cons", binaryOp (fn (a, b) => PAIR (a, b))
                                    , FORALL (["'a"], FUNTY ([tvA, listtype tvA]
                                                            , listtype tvA))) ::
                             ("car",  unaryOp  (fn (PAIR (car, _)) => car 
                                                 | v => raise RuntimeError
                                                           (
                                    "car applied to non-list " ^ valueString v))
                                   ,  FORALL (["'a"], FUNTY ([listtype tvA], tvA
                                                                          ))) ::
                             ("cdr",  unaryOp  (fn (PAIR (_, cdr)) => cdr 
                                                 | v => raise RuntimeError
                                                           (
                                    "cdr applied to non-list " ^ valueString v))
                                   ,  FORALL (["'a"], FUNTY ([listtype tvA],
                                                              listtype tvA))) ::
                             (* <primitive functions for \tuscheme\ [[::]]>= *)
                             ("<", intcompare op <, comptype) :: 
                             (">", intcompare op >, comptype) ::
                             ("=", comparison equalatoms,
                                                    FORALL (["'a"], FUNTY ([tvA,
                                                            tvA], booltype))) ::

                       (* Two of the print primitives have polymorphic types. *)
                             (* <primitive functions for \tuscheme\ [[::]]>= *)
                             ("println", unaryOp (fn x => (print (valueString x^
                                                               "\n"); unitVal)),
                                 FORALL (["'a"], FUNTY ([tvA], unittype))) ::
                             ("print", unaryOp (fn x => (print (valueString x);
                                                                      unitVal)),
                                 FORALL (["'a"], FUNTY ([tvA], unittype))) ::
                             ("printu",  unaryOp (fn NUM n => (printUTF8 n;
                                                                        unitVal)
                                                   | v => raise
                                      BugInTypeChecking "printu of non-number"),
                                 FUNTY ([inttype], unittype)) :: [])
      fun addVal ((name, v, ty), (types, values)) = 
        ( bind (name, ty, types)
        , bind (name, ref v, values)
        )
      val (types, values) =
        foldl addVal (types, values)
        ((* In plain Typed uScheme, all the primitives are *)
         (* functions, so this chunk is empty. But in case you *)
         (* want to define a non-function primitive for one of *)
         (* the exercises in \creftypesys.chap, I leave an *)
         (* (empty) place for primitives that aren't functions. *)
         (* <primitives that aren't functions, for \tuscheme\ [[::]]>= *)
         (* fill in non-function primitives here *)

(* if this space is completely empty, something goes wrong with the software OMIT *)
                                                                             [])
val primBasis = (kinds, types, values) (*OMIT*)      
(* Error-detecting transformers and their composition *)
(*                                              *)
(* Sometimes an error is detected not by a parser but by *)
(* a function that is applied to the results of parsing. *)
(* A classic example is a function definition: if the *)
(* formal parameters are syntactically correct but *)
(* contain duplicate name, an error should be signalled. *)
(* We would transform the input into a value of type *)
(* [[name list error]]. But the transformer type already *)
(* includes the possibility of error, and we would *)
(* prefer that errors detected by functions be on the *)
(* same footing as errors detected by parsers, and that *)
(* they be handled by the same mechanisms. To enable *)
(* such handling, I define [[<*>!]] and [[<>!]] *)
(* combinators that merge function-detected errors with *)
(* parser-detected errors.                      *)
(* <boxed values 12>=                           *)
val _ = op kinds     : kind      env
val _ = op types     : tyex      env
val _ = op values    : value ref env
val _ = op primBasis : basis
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

  in  (kinds, types, values)
  end
val primitiveBasis = primBasis
val predefs   = 
                 [ ";  <predefined {\\tuscheme} functions>=          "
                 , "(val list1 (type-lambda ['a] (lambda ([x : 'a])"
                 ,
                  "                               ([@ cons 'a] x [@ '() 'a]))))"
                 , "(val list2 (type-lambda ['a] (lambda ([x : 'a] [y : 'a])"
                 ,
            "                               ([@ cons 'a] x ([@ list1 'a] y)))))"
                 ,
             "(val list3 (type-lambda ['a] (lambda ([x : 'a] [y : 'a] [z : 'a])"
                 ,
          "                               ([@ cons 'a] x ([@ list2 'a] y z)))))"
                 , ";  As another example, [[type-lambda]] is used to define "
                 , ";  some of the higher-order functions in Chapter [->]. "
                 , ";  Their types are as follows:                  "
                 , ";                                               "
                 , ";   o       \\/alpha, beta, gamma\\alldot(beta-->gamma) "
                 , ";           *(alpha-->beta) -->(alpha-->gamma)  "
                 , ";   curry   \\/alpha, beta, gamma\\alldot(alpha*beta--> "
                 , ";           gamma) -->(alpha-->(beta-->gamma))  "
                 , ";   uncurry \\/alpha, beta, gamma\\alldot(alpha-->(beta "
                 , ";           -->gamma)) -->(alpha*beta-->gamma)  "
                 , ";                                               "
                 , ";  Their implementations use only [[type-lambda]], "
                 , ";  [[lambda]], and function application:        "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val o (type-lambda ['a 'b 'c]"
                 , "  (lambda ([f : ('b -> 'c)] [g : ('a -> 'b)])"
                 , "     (lambda ([x : 'a]) (f (g x))))))"
                 , ""
                 , "(val curry (type-lambda ['a 'b 'c]"
                 , "   (lambda ([f : ('a 'b -> 'c)])"
                 , "      (lambda ([x : 'a]) (lambda ([y : 'b]) (f x y))))))"
                 , ""
                 , "(val uncurry (type-lambda ['a 'b 'c]"
                 , "   (lambda ([f : ('a -> ('b -> 'c))])"
                 , "      (lambda ([x : 'a] [y : 'b]) ((f x) y)))))"
                 , ";  Other higher-order functions are not only polymorphic "
                 , ";  but also recursive. Such functions are defined by "
                 , ";  nesting [[letrec]] (for recursion) inside    "
                 , ";  [[type-lambda]] (for polymorphism). [*]      "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val length"
                 , "  (type-lambda ['a]"
                 , "    (letrec"
                 , "       [([length-mono : ((list 'a) -> int)]"
                 , "            (lambda ([xs : (list 'a)])"
                 , "              (if ([@ null? 'a] xs)"
                 , "                  0"
                 , "                  (+ 1 (length-mono ([@ cdr 'a] xs))))))]"
                 , "       length-mono)))"
                 , ";  The inner function is called [[length-mono]] because "
                 , ";  it—like any value introduced with [[lambda]]—is "
                 , ";  monomorphic, operating only on lists of the given "
                 , ";  element type [['a]]: the recursive call to   "
                 , ";  [[length-mono]] does not require an instantiation. "
                 , ";                                               "
                 , ";  Every polymorphic, recursive function is defined "
                 , ";  using the same pattern: [[val]] to [[type-lambda]] to "
                 , ";  [[letrec]]. Another example is an explicitly typed "
                 , ";  version of the reverse-append function:      "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val revapp"
                 , "  (type-lambda ['a]"
                 ,
              "    (letrec [([revapp-mono : ((list 'a) (list 'a) -> (list 'a))]"
                 ,
                  "                 (lambda ([xs : (list 'a)] [ys : (list 'a)])"
                 , "                   (if ([@ null? 'a] xs)"
                 , "                     ys"
                 , "                     (revapp-mono ([@ cdr 'a] xs)"
                 ,
       "                                  ([@ cons 'a] ([@ car 'a] xs) ys)))))]"
                 , "       revapp-mono)))"
                 , ";  As another example, [[filter]] is defined as follows: "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val filter"
                 , "  (type-lambda ('a)"
                 , "    (letrec"
                 ,
                 "      [([filter-mono : (('a -> bool) (list 'a) -> (list 'a))]"
                 , "           (lambda ([p? : ('a -> bool)] [xs : (list 'a)])"
                 , "             (if ([@ null? 'a] xs)"
                 , "                 [@ '() 'a]"
                 , "                 (if (p? ([@ car 'a] xs))"
                 , "                     ([@ cons 'a] ([@ car 'a] xs)"
                 ,
           "                                  (filter-mono p? ([@ cdr 'a] xs)))"
                 , "                     (filter-mono p? ([@ cdr 'a] xs))))))]"
                 , "      filter-mono)))"
                 , ";  And likewise [[map]]:                        "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val map"
                 , "  (type-lambda ('a 'b)"
                 , "    (letrec"
                 , "      [([map-mono : (('a -> 'b) (list 'a) -> (list 'b))]"
                 , "           (lambda ([f : ('a -> 'b)] [xs : (list 'a)])"
                 , "              (if ([@ null? 'a] xs)"
                 , "                  [@ '() 'b]"
                 , "                  ([@ cons 'b] (f ([@ car 'a] xs))"
                 ,
              "                               (map-mono f ([@ cdr 'a] xs))))))]"
                 , "      map-mono)))"
                 , ";  Predefined functions of \\tuschemeheader      "
                 , ";                                               "
                 , ";  Because programming in Typed uScheme is a hassle, "
                 , ";  Typed uScheme has fewer predefined functions than "
                 , ";  micro-Scheme. Some of these functions are defined in "
                 , ";  Chapter [->]. The rest are here.             "
                 , ";                                               "
                 , ";  Becauses lists in Typed uScheme are homogeneous, the "
                 , ";  funny list functions built from [[car]] and [[cdr]] "
                 , ";  are much less useful than in micro-Scheme.   "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val caar"
                 , "   (type-lambda ('a)"
                 , "      (lambda ([xs : (list (list 'a))])"
                 , "          ((@ car 'a) ((@ car (list 'a)) xs)))))"
                 , "(val cadr"
                 , "   (type-lambda ('a)"
                 , "      (lambda ([xs : (list (list 'a))])"
                 , "          ((@ car (list 'a)) ((@ cdr (list 'a)) xs)))))"
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(define bool and ([b : bool] [c : bool]) (if b  c  b))"
                 , "(define bool or  ([b : bool] [c : bool]) (if b  b  c))"
                 , "(define bool not ([b : bool])            (if b #f #t))"
                 , ";  Integer comparisons are defined as in Typed Impcore, "
                 , ";  but to define [[!=]] requires a type abstraction. "
                 , ";  This is progress! In Typed Impcore, a polymorphic [[! "
                 , ";  =]] couldn't be defined as as function; it would have "
                 , ";  to be a syntactic form.                      "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(define bool <= ([x : int] [y : int]) (not (> x y)))"
                 , "(define bool >= ([x : int] [y : int]) (not (< x y)))"
                 ,
 "(val != (type-lambda ('a) (lambda ([x : 'a] [y : 'a]) (not ((@ = 'a) x y)))))"
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(define int max ([m : int] [n : int]) (if (> m n) m n))"
                 , "(define int min ([m : int] [n : int]) (if (< m n) m n))"
                 , "(define int mod ([m : int] [n : int]) (- m (* n (/ m n))))"
                 ,
"(define int gcd ([m : int] [n : int]) (if ((@ = int) n 0) m (gcd n (mod m n))))"
                 ,
                  "(define int lcm ([m : int] [n : int]) (* m (/ n (gcd m n))))"
                 , ";  As an example, if a name is parsed by [[name]] and an "
                 , ";  expression is parsed by [[exp]], then a name followed "
                 , ";  by an expression, such as might appear in a [[let]] "
                 , ";  binding, can be turned into (name, expression) pair "
                 , ";  by the parser {nwverbatim} pair <> name <*> exp  "
                 , ";  {nwverbatim} (Parsing the actual micro-Scheme syntax "
                 , ";  would also require a parser to handle the surrounding "
                 , ";  parentheses.) As another example, if a micro-Scheme "
                 , ";  parser has seen a left bracket followed by the "
                 , ";  keyword [[if]], it can call the parser {nwverbatim} "
                 , ";  curry3 IFX <> exp <*> exp <*> exp {nwverbatim} which "
                 , ";  creates the abstract-syntax tree for an [[if]] "
                 , ";  expression.                                  "
                 , ";                                               "
                 , ";  The combinator [[<*>]] creates parsers that read "
                 , ";  things in sequence; but it can't make a choice. "
                 , ";  If any parser in the sequence fails, the whole "
                 , ";  sequence fails. A choice, as in ``[[val]] or "
                 , ";  expression or [[define]] or [[use]],'' is made by a "
                 , ";  choice operator. The choice operator is written "
                 , ";  [[<|>]] and pronounced ``or.'' If [[t1]] and [[t2]] "
                 , ";  are both \\atob transformers, then \\monoboxt1 <|> t2 "
                 , ";  is an \\atob transformer that first tries [[t1]], then "
                 , ";  tries [[t2]]. Transformer \\monoboxt1 <|> t2 succeeeds "
                 , ";  if either [[t1]] or [[t2]] succeeds, detects an error "
                 , ";  if either [[t1]] or [[t2]] detects an error, and "
                 , ";  fails only if both [[t1]] and [[t2]] fail. To assure "
                 , ";  that \\monoboxt1 <|> t2 has a predictable type no "
                 , ";  matter which transformer is chosen, both [[t1]] and  "
                 , ";  [[t2]] have to have the same type.           "
                 , ";  <predefined {\\tuscheme} functions>=          "
                 , "(val append"
                 , "  (type-lambda ('a)"
                 ,
             "     (letrec [([append-mono : ((list 'a) (list 'a) -> (list 'a))]"
                 ,
                  "                 (lambda ([xs : (list 'a)] [ys : (list 'a)])"
                 , "                   (if ((@ null? 'a) xs)"
                 , "                      ys"
                 , "                      ((@ cons 'a)"
                 , "                          ((@ car 'a) xs)"
                 ,
               "                          (append-mono ((@ cdr 'a) xs) ys)))))]"
                 , "        append-mono)))"
                  ]
val initialBasis =
  let val xdefs = stringsxdefs ("predefined functions", predefs)
  in  readEvalPrintWith predefinedFunctionError (xdefs, primBasis,
                                                                 noninteractive)
  end


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
(* A last word about function [[readEvalPrintWith]]: you *)
(* might be wondering, ``where does it read, evaluate, *)
(* and print?'' It has helpers for that: reading is a *)
(* side effect of [[streamGet]], which is called by *)
(* [[streamFold]], and evaluating and printing are done *)
(* by [[processDef]]. But the function is called *)
(* [[readEvalPrintWith]] because when you want reading, *)
(* evaluating, and printing to happen, you call \monobox *)
(* readEvalPrintWith eprintln, passing your extended *)
(* definitions and your environments.           *)
(*                                              *)
(* Handling exceptions                          *)
(*                                              *)
(* When an exception is raised, a bridge-language *)
(* interpreter must ``catch'' or ``handle'' it. *)
(* An exception is caught using a syntactic form written *)
(* with the keyword [[handle]]. (This form resembles a *)
(* combination of a [[case]] expression with the *)
(* [[try-catch]] form from \crefschemes.chap.) Within *)
(* the [[handle]], every exception that the interpreter *)
(* recognizes is mapped to an error message tailored for *)
(* that exception. To be sure that every exception is *)
(* responded to in the same way, no matter where it is *)
(* handled, I write just a single [[handle]] form, and I *)
(* deploy it in a higher-order, continuation-passing *)
(* function: [[withHandlers]].                  *)
(*                                              *)
(* In normal execution, calling \monoboxwithHandlers f a *)
(* caught applies function [[f]] to argument [[a]] and *)
(* returns the result. But when the application f a *)
(* raises an exception, [[withHandlers]] uses [[handle]] *)
(* to recover from the exception and to pass an error *)
(* message to [[caught]], which acts as a failure *)
(* continuation (\crefpage,scheme.cps). Each error *)
(* message contains the string [["<at loc>"]], which can *)
(* be removed (by [[stripAtLoc]]) or can be filled in *)
(* with an appropriate source-code location (by  *)
(* [[fillAtLoc]]).                              *)
(*                                              *)
(* The most important exceptions are [[NotFound]], *)
(* [[RuntimeError]], and [[Located]]. Exception *)
(* [[NotFound]] is defined in \crefmlscheme.chap; the *)
(* others are defined in this appendix. Exceptions *)
(* [[NotFound]] and [[RuntimeError]] signal problems *)
(* with an environment or with evaluation, respectively. *)
(* Exception [[Located]] wraps another exception [[exn]] *)
(* in a source-code location. When [[Located]] is *)
(* caught, [[withHandlers]] calls itself recursively *)
(* with a function that ``re-raises'' exception [[exn]] *)
(* and with a failure continuation that fills in the *)
(* source location in [[exn]]'s error message.  *)
(* <boxed values 75>=                           *)
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
(* <boxed values 75>=                           *)
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
(* <boxed values 76>=                           *)
val _ = op runPathWith : interactivity -> (string * basis -> basis)
(* <look at command-line arguments, then run>=  *)
val usage = ref (fn () => ())
(* <boxed values 77>=                           *)
val _ = op usage : (unit -> unit) ref
(* <look at command-line arguments, then run>=  *)
datatype action
  = RUN_WITH of interactivity  (* call runPathWith on remaining arguments *)
  | DUMP     of unit -> unit   (* dump information *)
  | FAIL     of string         (* signal a bad command line *)
  | DEFAULT                    (* no command-line options were given *)
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
(* <boxed values 78>=                           *)
val _ = op perform: action * string list -> unit
(* <look at command-line arguments, then run>=  *)
fun merge (_, a as FAIL _) = a
  | merge (a as FAIL _, _) = a
  | merge (DEFAULT, a) = a
  | merge (_, DEFAULT) = raise InternalError "DEFAULT on the right in MERGE"
  | merge (RUN_WITH _, right as RUN_WITH _) = right
  | merge (DUMP f, DUMP g) = DUMP (g o f)
  | merge (_, r) = FAIL "Interpret or dump, but don't try to do both"
(* <boxed values 79>=                           *)
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
(* <boxed values 80>=                           *)
val _ = op actions : (string * action) list
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* A transformer can be complemented, turning success *)
(* into failure and vice versa. Transformer \monobox *)
(* notFollowedBy t succeeds if and only if [[t]] fails. *)
(* Transformer \monoboxnotFollowedBy t may look at *)
(* input, but it never consumes any input. This *)
(* transformer is used when trying to read an integer *)
(* literal, to make sure that the digits are not *)
(* followed by a letter or other non-delimiting symbol. *)
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
(* Options are parsed by function [[action]].   *)
(* <boxed values 81>=                           *)
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
(* <boxed values 82>=                           *)
val _ = op strip_options : action -> string list -> action * string list
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

