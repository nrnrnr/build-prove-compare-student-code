(* Putting all the pieces together              *)
(*                                              *)
(* We stitch together the parts of the implementation in *)
(* this order:                                  *)
(* <haskell.sml>=                               *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE INFERENCE            *)
(*                                                               *)
(*****************************************************************)

(* <exceptions used in languages with type inference>= *)
exception TypeError of string
exception BugInTypeInference of string


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH LAZY EVALUATION           *)
(*                                                               *)
(*****************************************************************)

(* If we ever try to force a thunk whose forcing is *)
(* already in progress, we have hit a special kind of *)
(* infinite recursion called a ``black hole.''  *)
(* <exceptions used in languages with lazy evaluation>= *)
exception BlackHole


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
(* <boxed values 111>=                          *)
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
(* The representation guarantees that there is an [['a]] *)
(* for every [[name]].                          *)
(*                                              *)
(* \setcodemargin7pt                            *)
(*                                              *)
(* The empty environment is represented by the empty *)
(* list. In ML, that's written using square brackets. *)
(* The [[val]] form is like micro-Scheme's [[val]] form. *)
(* <boxed values 10>=                           *)
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
(* <boxed values 10>=                           *)
val _ = op find : name * 'a env -> 'a
(* \mlsflabelfind                               *)

(* Again using [[::]], function [[bind]] adds a new *)
(* binding to an existing environment. Unlike \cref *)
(* scheme.chap's [[bind]], it does not allocate a *)
(* mutable reference cell.                      *)
(* <boxed values 10>=                           *)
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
(* <boxed values 10>=                           *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* Finally, environments can be composed using the + *)
(*  operator. In my ML code, this operator is   *)
(* implemented by function [[<+>]], which I declare to *)
(* be [[infix]]. It uses the predefined infix function  *)
(* [[@]], which is ML's way of writing [[append]]. \ *)
(* mlsflabel<+>                                 *)
(* <boxed values 10>=                           *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* <support for names and environments>=        *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* Duplicate names and internal errors          *)
(*                                              *)
(* In bridge-language binding constructs, like  *)
(* [[lambda]], duplicate names are treated as errors. *)
(* Such names are detected by function          *)
(* [[duplicatename]]. If a name x occurs more than twice *)
(* on a list, [[duplicatename]] returns \monoSOME x; *)
(* otherwise it returns [[NONE]].               *)
(* <boxed values 34>=                           *)
val _ = op duplicatename : name list -> name option
(* All interpreters incorporate these two exceptions: *)
(* <exceptions used in every interpreter>=      *)
exception RuntimeError of string (* error message *)
exception LeftAsExercise of string (* string identifying code *)
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
(* <boxed values 32>=                           *)
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
(* <boxed values 33>=                           *)
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
(* <boxed values 24>=                           *)
val _ = op intString : int -> string
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* To characterize a list by its length and contents, *)
(* interpreter messages use strings like        *)
(* ``3 arguments,'' which come from functions [[plural]] *)
(* and [[countString]].                         *)
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
(* <boxed values 25>=                           *)
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
(* <boxed values 26>=                           *)
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
(* <boxed values 27>=                           *)
val _ = op fnvHash : string -> int
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Utility functions for printing               *)
(*                                              *)
(* For writing values and other information to standard *)
(* output, Standard ML provides a simple [[print]] *)
(* primitive, which writes a string. Anything more *)
(* sophisticated, such as writing to standard error, *)
(* requires using the the [[TextIO]] module, which is *)
(* roughly analogous to C's [[<stdio.h>]]. Using *)
(* [[TextIO]] can be awkward, so I define three *)
(* convenience functions. Function [[println]] is like *)
(* [[print]], but it writes a string followed by a *)
(* newline. Functions [[eprint]] and [[eprintln]] are *)
(* analogous to [[print]] and [[println]], but they *)
(* write to standard error. \qbreak More sophisticated *)
(* printing functions (\crefpage,sec:print-interface) *)
(* would be lovely, but making such functions type-safe *)
(* requires code that beginning ML programmers would *)
(* find baffling.                               *)
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
(* <boxed values 28>=                           *)
val _ = op withXprinter : (string -> unit) -> ('a -> 'b) -> ('a -> 'b)
val _ = op tryFinally   : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b
(* And the function stored in [[xprinter]] might be *)
(* [[bprint]], which ``prints'' by appending a string to *)
(* a buffer. Function [[bprinter]] returns a pair that *)
(* contains both [[bprint]] and a function used to *)
(* recover the contents of the buffer.          *)
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
(* <boxed values 36>=                           *)
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
(* <boxed values 37>=                           *)
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
(* <boxed values 38>=                           *)
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
(* <boxed values 69>=                           *)
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
(* <boxed values 29>=                           *)
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
(* <boxed values 35>=                           *)
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
(* <boxed values 30>=                           *)
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
(* <boxed values 31>=                           *)
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
(* <boxed values 45>=                           *)
type 'a susp = 'a susp
(* <suspensions>=                               *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* <boxed values 46>=                           *)
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
(* <boxed values 47>=                           *)
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
(* <boxed values 47>=                           *)
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
(* <boxed values 48>=                           *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 48>=                           *)
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
(* <boxed values 49>=                           *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* In list and stream processing, [[concat]] is very *)
(* often composed with [[map f]]. The composition is *)
(* usually called [[concatMap]].                *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 50>=                           *)
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
(* <boxed values 51>=                           *)
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
(* <boxed values 52>=                           *)
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
(* <boxed values 53>=                           *)
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
(* <boxed values 54>=                           *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 54>=                           *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* <boxed values 55>=                           *)
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
(* <boxed values 56>=                           *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 57>=                           *)
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
(* <boxed values 58>=                           *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* <boxed values 58>=                           *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* <boxed values 59>=                           *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* The code used to append two streams is much like the *)
(* code used to concatenate arbitrarily many streams. *)
(* To avoid duplicating the tricky manipulation of *)
(* states, I implement append using concatenation. *)
(* <boxed values 60>=                           *)
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
(* <boxed values 61>=                           *)
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
(* <boxed values 62>=                           *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* <stream transformers and their combinators>= *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* <boxed values 107>=                          *)
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
(* <boxed values 108>=                          *)
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
(* <boxed values 109>=                          *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 110>=                          *)
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
(* <boxed values 112>=                          *)
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
(* <boxed values 113>=                          *)
val _ = op pzero : ('a, 'b) xformer
(* This parser obeys the algebraic law          *)
(*                                              *)
(*  \monoboxt <|> pzero = \monoboxpzero <|> t = \ *)
(*  monoboxt\text.                              *)
(*                                              *)
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)
(* <boxed values 113>=                          *)
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
(* <boxed values 114>=                          *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* <boxed values 115>=                          *)
val _ = op one : ('a, 'a) xformer
(* <stream transformers and their combinators>= *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* <boxed values 116>=                          *)
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
(* <boxed values 117>=                          *)
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
(* <boxed values 118>=                          *)
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
(* <boxed values 119>=                          *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 120>=                          *)
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
(* <boxed values 121>=                          *)
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
(* <boxed values 122>=                          *)
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
(* <boxed values 123>=                          *)
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
(* <boxed values 124>=                          *)
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
(* <boxed values 125>=                          *)
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
(* <boxed values 126>=                          *)
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
(* <boxed values 127>=                          *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)                 * ('a, 'b) xformer -> ('a, 'c
                                                                       ) xformer
(* <support for source-code locations and located streams>= *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
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
(* <boxed values 64>=                           *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 64>=                           *)
type 'a located = 'a located
(* <support for source-code locations and located streams>= *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* <more handlers for [[atLoc]]>=               *)
           | e as BlackHole => raise Located (loc, e)
           (* <more handlers for [[atLoc]]>=               *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
(* <boxed values 65>=                           *)
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
(* <boxed values 66>=                           *)
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
(* <boxed values 67>=                           *)
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
(* <boxed values 68>=                           *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 68>=                           *)
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
(* <boxed values 136>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* \qbreak To support a stream of marked lines—possibly *)
(* marked, located lines—I define transformers [[eol]], *)
(* [[inline]], and [[srcloc]]. The [[eol]] transformer *)
(* returns the number of the line just ended.   *)
(* <boxed values 136>=                          *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* <support for lexical analysis>=              *)
type 'a lexer = (char, 'a) xformer
(* <boxed values 128>=                          *)
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
(* <boxed values 129>=                          *)
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
(* <boxed values 130>=                          *)
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
(* <boxed values 131>=                          *)
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
(* <boxed values 132>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 133>=                          *)
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
(* Bracket tokens are added to a language-specific *)
(* ``pre-token'' type by using the type constructor *)
(* [[plus_brackets]].[*] Function [[bracketLexer]] takes *)
(* as an argument a lexer for pre-tokens, and it returns *)
(* a lexer for tokens:                          *)
(* <boxed values 134>=                          *)
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
(* <boxed values 135>=                          *)
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
(* <boxed values 137>=                          *)
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
(* <boxed values 138>=                          *)
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
(* <boxed values 139>=                          *)
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
(* <boxed values 139>=                          *)
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
(* <boxed values 140>=                          *)
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
(* <boxed values 149>=                          *)
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
(* <boxed values 150>=                          *)
val _ = op rejectReserved : name list -> name -> name error
(* <transformers for interchangeable brackets>= *)
fun left  tokens = ((fn (loc, LEFT  s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun right tokens = ((fn (loc, RIGHT s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* <boxed values 142>=                          *)
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
(* <boxed values 143>=                          *)
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
(* <boxed values 141>=                          *)
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
(* <boxed values 144>=                          *)
val _ = op notCurly  : bracket_shape located -> bool
val _ = op leftCurly : ('t plus_brackets, bracket_shape located) polyparser
(* <boxed values 144>=                          *)
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
(* <boxed values 145>=                          *)
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
(* <boxed values 146>=                          *)
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
(* <boxed values 147>=                          *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* <boxed values 147>=                          *)
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
(* <boxed values 148>=                          *)
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
(* <boxed values 151>=                          *)
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
(* <boxed values 152>=                          *)
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
(* \qbreak Function [[checkExpectPassesWith]] runs a *)
(* [[check-expect]] test and uses the given [[equals]] *)
(* to tell if the test passes. If the test does not *)
(* pass, [[checkExpectPasses]] also writes an error *)
(* message. Error messages are written using    *)
(* [[failtest]], which, after writing the error message, *)
(* indicates failure by returning [[false]].    *)
(* <boxed values 153>=                          *)
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
(* <boxed values 154>=                          *)
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
(* <boxed values 155>=                          *)
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
(* <boxed values 156>=                          *)
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
(* <boxed values 157>=                          *)
val _ = op parseWithErrors : ('t, 'a) polyparser ->                     't
                                    located eol_marked stream -> 'a error stream
(* <streams that issue two forms of prompts>=   *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* <boxed values 158>=                          *)
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
(* <boxed values 159>=                          *)
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
(* Supporting code for the ML interpreter for \uscheme *)
(*                                              *)
(* [*] [*] \invisiblelocaltableofcontents[*] This *)
(* appendix describes language-specific code that that *)
(* is not interesting enough to include in \cref *)
(* mlscheme.chap, but that is needed to implement the ML *)
(* version of micro-Scheme. Code in this appendix *)
(* defines some primitive functions, builds the initial *)
(* basis, runs unit tests, and does lexical analysis and *)
(* parsing. There is also code that implements the *)
(* ``unspecified'' values in micro-Scheme's operational *)
(* semantics.                                   *)
(*                                              *)
(* For every bridge language that is implemented in ML, *)
(* this Supplement includes a similar appendix. *)
(*                                              *)
(* Organizing code chunks \chaptocbacksplitinto an *)
(* interpreter                                  *)
(*                                              *)
(* My interpreter for micro-Scheme takes up close to *)
(* 2,000 lines of ML code. In the book, that code is *)
(* broken up into small chunks. But those chunks are *)
(* written into a single program—and according to the *)
(* semantics of ML, every type and function must be *)
(* defined before it is used. Definitions come not only *)
(* from this appendix and \crefmlscheme.chap but also *)
(* from \creflazyparse.chap,mlinterps.chap. To get all *)
(* the definitions in the right order, I use Noweb code *)
(* chunks. [This technique is evidence of the biggest *)
(* mistake I made in creating this software: I~used *)
(* Noweb code chunks, but I~should have used ML~modules. *)
(* I~knew I~didn't want to expose readers to ML~modules *)
(* starting in \cref{mlscheme.chap}, and that was a good *)
(* decision. (Covering modules at all was a late change *)
(* to the book's design.) But I wish I had used modules *)
(* secretly, behind the scenes. Had I done so, the *)
(* software would have been {so much} better.] Each *)
(* interpreter is built from a different set of chunks, *)
(* but they are all organized along the same lines: *)
(* shared infrastructure; abstract syntax and values, *)
(* with utility functions; lexical analysis and parsing; *)
(* evaluation (including unit testing and the   *)
(* read-eval-print loop); and initialization.   *)
(* (Interpreters for typed languages also have chunks *)
(* devoted to types and to type checking or type *)
(* inference.) [*]                              *)

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
(*   HINDLEY-MILNER TYPES WITH NAMED TYPE CONSTRUCTORS           *)
(*                                                               *)
(*****************************************************************)

(* The code chunks that make up type inference are *)
(* divided into two groups. The first group contains a *)
(* simpler infrastructure for Hindley-Milner types; this *)
(* infrastructure works only under the assumption that *)
(* each type constructor is represented by its name. *)
(* This assumption holds for \nml, but in uML,  *)
(* a programmer can define two distinct type    *)
(* constructors that have the same name, so uML uses a *)
(* less simple infrastructure that is written in a *)
(* different group of code chunks.              *)
(* <Hindley-Milner types with named type constructors>= *)
(* <[[tycon]], [[eqTycon]], and [[tyconString]] for named type constructors>= *)
type tycon = name
fun eqTycon (mu, mu') = mu = mu'
fun tyconString mu = mu
(* The set of type schemes sigma in which the \ldotsn *)
(* alpha is empty is isomorphic to the set of types tau. *)
(* The isomorphism relates each type [[tau]] to the type *)
(* scheme [[FORALL([], tau)]]. A type without \/ or a *)
(* type scheme in which the \ldotsnalpha is empty is *)
(* sometimes called a monotype or ground type. A type *)
(* scheme in which the \ldotsnalpha is not empty is *)
(* sometimes called a polytype.                 *)
(*                                              *)
(* When a type application appears in code, the type *)
(* constructor goes before its arguments, as in {alltt} *)
(* CONAPP(TYCON "list", [TYCON "int"]). {alltt} But in *)
(* the text, the type constructor goes after its *)
(* arguments, as in \monoboxint list. This is the way *)
(* types are written in ML source code.         *)
(*                                              *)
(* In \nml, all type constructors are predefined; *)
(* no program can add new ones. New type constructors *)
(* can be added in the more advanced bridge languages *)
(* uML (\crefadt.chap) and \mcl (\crefmcl.chap). In \nml *)
(* , the predefined constructors [[arguments]] and *)
(* [[function]] appear in the typing rules for  *)
(* functions. Other constructors, like [[bool]], *)
(* [[int]], [[sym]], and so on, give types to literals *)
(* or primitives.                               *)
(*                                              *)
(* To write types made with [[arguments]] and   *)
(* [[function]], I use ML's abbreviations.      *)
(*                                              *)
(*  Type                        Abbreviation    *)
(*  (tau_1, tau_2) function     tau_1 -->tau_2  *)
(*  (tau_1, ..., tau_n)         tau_1 *tau_2 ...*tau *)
(*  arguments                   _n              *)
(*                                              *)
(* In \nml, as in Typed uScheme, a type environment is *)
(* written using the Greek letter \tyenv. In \nml, a *)
(* type environment \tyenv maps a term variable [Term *)
(* variables, which appear in terms (expressions) and *)
(* are bound by [[let]] or [[lambda]], stand for values. *)
(* Don't confuse them with type variables, which stand *)
(* for types. The name of a term variable begins with a *)
(* letter or symbol; the name of a type variable begins *)
(* with the ASCII quote~([[']]) character.] to a type *)
(* scheme. Type environments are used only during type *)
(* inference, not at run time.                  *)
(*                                              *)
(* In \nml, unlike in Typed uScheme, types don't appear *)
(* in code. And types inferred by the system are *)
(* guaranteed to be well formed, so the type system *)
(* doesn't need formation rules or kinds. (An ill-formed *)
(* type may appear in a unit test, but in that case the *)
(* test just fails; no other checking is needed.) Kinds *)
(* are used again in uML (\crefadt.chap) and in full ML. *)
(*                                              *)
(* Simple type constructors                     *)
(*                                              *)
(* Because all type constructors are predefined, a type *)
(* constructor can be represented simply by its name. *)
(* Because type names cannot be redefined, a name like *)
(* [[int]] always means ``integer,'' and two type *)
(* constructors are the same if and only if they have *)
(* the same name.                               *)
(* <boxed values 87>=                           *)
type tycon = tycon
val _ = op eqTycon : tycon * tycon -> bool
val _ = op tyconString : tycon -> string
(* <representation of Hindley-Milner types>=    *)
type tyvar  = name
datatype ty = TYVAR  of tyvar               (* type variable alpha *)
            | TYCON  of tycon               (* type constructor mu *)
            | CONAPP of ty * ty list        (* type-level application *)

datatype type_scheme = FORALL of tyvar list * ty
(* <sets of free type variables in Hindley-Milner types>= *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
  in  reverse (f (t, emptyset))
  end  
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \newskip\myskip \myskip=6pt                  *)
(*                                              *)
(*    Type system    Concept       Interpreter  *)
(*         d         Definition    def (\cpagerefml.type.def) *)
(*         e         Expression    \nmltypeexp  *)
(*         x         Variable      \mlstypename *)
(*  [\myskip] alpha  Type variable \nmltypetyvar *)
(*        tau        Type          \nmltypety   *)
(*  sigma, \/alpha.  Type scheme   [[type_scheme]] \nmltypepage *)
(*        tau                      type_scheme  *)
(*  [\myskip] tau=== Type          \monoboxeqType(tau, tau') \ *)
(*        tau'       equivalence   nmlfunpageeqType *)
(*  [\myskip] Gamma  Type          [[type_env]] \nmltypepage *)
(*                   environment   type_env     *)
(*  Gamma(x) = sigma Type lookup   \monofindtyscheme(x, Gamma) =  *)
(*                                 sigma \nmlfunpagefindtyscheme *)
(*  Gamma{x |->sigma Type binding  \monobindtyscheme(x, sigma, *)
(*         }                       Gamma) \nmlfunpagebindtyscheme *)
(*   [\myskip] \tyc  Constraint    \nmltypecon  *)
(*  tau_1 \eqtytau_2 Equality      tau_1 [[ ]] tau_2 \nmlfunpage *)
(*                   constraint    tilde        *)
(*  \tyc_1 \land\tyc Conjunction   \tyc_1 [[/] \tyc_2 \nmlfunpage *)
(*         _2                      cand         *)
(*       \trivc      Trivial       \nmlfunTRIVIAL *)
(*                   constraint                 *)
(*     \bigwedge\    Conjunction   \monoconjoinConstraints [\ *)
(*  nolimits_i \tyc                ldotsn\tyc] \nmlfunpage *)
(*         _i                      conjoinConstraints *)
(*  [\myskip] \ftv(  Free type     freetyvars tau \nmlfunpage *)
(*        tau)       variables     freetyvars   *)
(*    \ftv(Gamma)    Free type     freetyvarsGamma Gamma \ *)
(*                   variables     nmlfunpagefreetyvarsGamma *)
(*     \ftv(\tyc)    Free type     freetyvarsConstraint \tyc \ *)
(*                   variables     nmlfunpagefreetyvarsConstraint *)
(*                   Type          \monoboxtypeof(e, Gamma) = ( *)
(*    [\myskip] \    inference     tau, \tyc), also \monoboxty e *)
(*  typeisc\tyce tau               = (tau, \tyc) (\cpageref *)
(*                                 ml.fun.typeof; some parts left *)
(*                                 as an exercise) *)
(*   \nmltoptd -->   Type          \monoboxtypdef(d, Gamma) = \ *)
(*       Gamma'      inference     monobox(Gamma', s) \nmlfunpage *)
(*                                 typdef       *)
(*  [\myskip] \subsn A             \nmltypesubst *)
(*                   substitution               *)
(*      \idsubst     Identity      \nmlfunidsubst *)
(*                   substitution               *)
(*   [alpha|->tau]   Substitution  \monoalpha |–> tau \nmlfunpage *)
(*                   for alpha     |–>        *)
(*     \subsntau     Substitution  \monotysubst \subsn tau \ *)
(*                                 nmlfunpagetysubst *)
(*    \subsnalpha    Substitution  \monovarsubst \subsn alpha \ *)
(*                                 nmlfunpagetysubst *)
(*     \subsn\tyc    Substitution  \monoconsubst \subsn \tyc \ *)
(*                                 nmlfunpageconsubst *)
(*  \subsn_2 o\subsn Composition   \nmlfuncompose *)
(*         _1                                   *)
(*     dom \subsn    Domain        \nmlfundom   *)
(*  [\myskip] \subsn Constraint    \subsn= \monoboxsolve \tyc *)
(*   \tyc===\trivc\  solving       (left as an exercise, \ *)
(*    notation===                  cpagerefml.fun.solve) *)
(*   equivalence of                             *)
(*    constraints                               *)
(*   \tyc===\trivc   Solved        \monoisSolved \tyc \nmlfunpage *)
(*                   constraint    isSolved     *)
(*     [\myskip]     Instantiation \monoboxinstantiate(\/alpha. *)
(*                                 tau, [tau']) \nmlfunpage *)
(*      \centering \               instantiate  *)
(*       /alpha.tau                             *)
(*      becomes tau[                            *)
(*      alpha|->tau                             *)
(*           ']                                 *)
(*                                              *)
(*     [\myskip]     Base types    [[inttype]], [[booltype]], ... *)
(*      [[int]],                   \nmlfunpageinttype *)
(*   [[bool]], ...                              *)
(*   \crossdotsntau  Function type \monoboxfuntype([\ldotsntau], *)
(*       -->tau                    tau) \nmlfunpagefuntype *)
(*                                              *)
(* Correspondence between \nml's type system and code  *)
(* [*]                                          *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* The interpreter                              *)
(*                                              *)
(* [*] In most respects, the interpreter for \nml is the *)
(* interpreter for micro-Scheme (\crefmlscheme.chap), *)
(* plus type inference. Significant parts of type *)
(* inference don't appear here, however, because they *)
(* are meant to be exercises.                   *)
(*                                              *)
(* Functions on types and type schemes          *)
(*                                              *)
(* This section defines functions that are used *)
(* throughout type inference.                   *)
(*                                              *)
(* Function [[freetyvars]] returns a set containing the *)
(* free type variables of a type. For readability, *)
(* it builds the set so type variables appear in the *)
(* order of their first appearance in the type, when *)
(* reading from left to right. \nmlflabelfreetyvars *)
(* <boxed values 95>=                           *)
val _ = op freetyvars : ty -> name set
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

val funtycon = "function"
(* <creation and comparison of Hindley-Milner types with named type constructors>= *)
val inttype  = TYCON "int"
val booltype = TYCON "bool"
val symtype  = TYCON "sym"
val alpha    = TYVAR "a"
val beta     = TYVAR "b"
val unittype = TYCON "unit"
fun listtype ty = 
  CONAPP (TYCON "list", [ty])
fun pairtype (x, y) =
  CONAPP (TYCON "pair", [x, y])
fun funtype (args, result) = 
  CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args), result])
fun asFuntype (CONAPP (TYCON "function",
                       [CONAPP (TYCON "arguments", args), result])) =
      SOME (args, result)
  | asFuntype _ = NONE
(* <boxed values 94>=                           *)
val _ = op inttype   : ty
val _ = op booltype  : ty
val _ = op symtype   : ty
val _ = op alpha     : ty
val _ = op beta      : ty
val _ = op unittype  : ty
val _ = op listtype  : ty -> ty
val _ = op pairtype  : ty * ty -> ty
val _ = op funtype   : ty list * ty -> ty
val _ = op asFuntype : ty -> (ty list * ty) option
(* [*]                                          *)

(* <creation and comparison of Hindley-Milner types with named type constructors>= *)
fun iotype ty  = CONAPP (TYCON "io", [ty])
(* String conversion                            *)
(*                                              *)
(* Function types are printed infix, and other  *)
(* constructor applications are printed prefix. *)
(* <definition of [[typeString]] for Hindley-Milner types>= *)
fun typeString tau =
  case asFuntype tau
    of SOME (args, result) => 
         "(" ^ spaceSep (map typeString args) ^ " -> " ^
               typeString result ^ ")"
     | NONE =>
         case tau
           of TYCON c => tyconString c
            | TYVAR a => a
            | CONAPP (tau, []) => "(" ^ typeString tau ^ ")"
            | CONAPP (tau, taus) =>
                "(" ^ typeString tau ^ " " ^
                      spaceSep (map typeString taus) ^ ")"
(* <shared utility functions on Hindley-Milner types>= *)
type subst = ty env
fun dom theta = map (fn (a, _) => a) theta
(* <shared utility functions on Hindley-Milner types>= *)
fun varsubst theta = 
  (fn a => find (a, theta) handle NotFound _ => TYVAR a)
(* In Typed uScheme, [[empty-list]] would have to be *)
(* instantiated explicitly, using expressions \monobox(@ *)
(* empty-list int) and \monobox(@ empty-list (list *)
(* sym)). Likewise, primitives [[pair]] and [[cons]] *)
(* would have to be instantiated explicitly. These *)
(* instantiations are what Professor Milner found *)
(* intolerable. In ML, no [[@]] form is needed; every *)
(* polymorphic name is instantiated automatically by *)
(* Milner's inference algorithm. The algorithm works by *)
(* computing an appropriate substitution.       *)
(*                                              *)
(* A substitution is a finite map from type variables to *)
(* types; one is written using the Greek letter \subsn *)
(* (pronounced ``THAYT-uh'').\notation[THAYT-uh]\subsn *)
(* a substitution A \subsn has many interpretations: *)
(*                                              *)
(*  \tightlist                                  *)
(*   • As a function from type variables to types *)
(*   • As a function from types to types      *)
(*   • As a function from type schemes to type schemes *)
(*   • As a function from type environments to type *)
(*  environments                                *)
(*   • As a function from type-equality constraints to *)
(*  type-equality constraints                   *)
(*   • As a function from typing judgments to typing *)
(*  judgments                                   *)
(*   • As a function from typing derivations to typing *)
(*  derivations                                 *)
(*                                              *)
(* These interpretations are all related and mutually *)
(* consistent. They all appear in the math, and some *)
(* appear in my code. \stdbreak The interpretation I use *)
(* most is the function from types to types. Such a *)
(* function \subsn is a substitution if it preserves *)
(* type constructors and constructor application: [*] *)
(*                                              *)
(*   • For any type constructor \tycon, \subsn\tycon= \ *)
(*  tycon.                                      *)
(*                                              *)
(*   • For any constructor application \astconapp(tau, < *)
(*  \ldotsntau>), [*]                           *)
(*                                              *)
(*      \subsn(\astconapp(tau, <\ldotsntau>)) = \ast *)
(*      conapp(\subsntau, <\subsntau_1, ..., \subsn *)
(*      tau_n>).                                *)
(*                                              *)
(*  \stdbreak Or, using informal ML-like notation, *)
(*                                              *)
(*      \subsn((tau_1, ..., tau_n) tau) = (\subsntau *)
(*      _1, ..., \subsntau_n) (\subsntau).      *)
(*                                              *)
(*  \stdbreak In the common case where the tau being *)
(*  applied is a simple constructor µ, \subsnµ=µ and *)
(*                                              *)
(*      \subsn((tau_1, ..., tau_n) µ) = (\subsntau_1, *)
(*      ..., \subsntau_n) µ.                   *)
(*                                              *)
(* To be a substitution, a function from types to types *)
(* must meet one other condition:               *)
(*                                              *)
(*   • The set {alpha\mid\subsnalpha!=alpha} must be *)
(*  finite. This set is the set of variables    *)
(*  substituted for. It is called the domain of the *)
(*  substitution, and it is written dom \subsn.[*] *)
(*                                              *)
(* Such a function is defined by [[tysubst]] in \cref *)
(* typesys.chap on \cpagereftuscheme.substitution; its *)
(* inner function [[subst]] has all the properties *)
(* claimed above (\crefml.ex.verify-tuscheme-subst). *)
(*                                              *)
(* Substitution determines when tau' is an instance of  *)
(* tau:\notation[instance of]<:the instance relation *)
(* like \citetmilner:theory, we write tau' <:tau if and *)
(* only if there exists a substitution \subsn such that *)
(* tau' = \subsntau.[*] The instance relation tau' <:tau *)
(* is pronounced in two ways: not only ``tau' is an *)
(* instance of tau'' but also ``tau is at least as *)
(* general as tau'.''                           *)
(*                                              *)
(* The instance relation is extended to type schemes: \ *)
(* nomathbreaktau' <:\/\ldotsnalpha\alldottau if and *)
(* only if there exists a substitution \subsn such that *)
(* \nomathbreakdom \subsn\subseteq{\ldotsnalpha} and \ *)
(* nomathbreak\subsntau= tau' . The first condition says *)
(* that the instantiating substitution \subsn may *)
(* substitute only for type variables that are bound by *)
(* the \/.                                      *)
(*                                              *)
(* To instantiate a type scheme sigma= \/\ldotsnalpha\ *)
(* alldottau is to choose a tau' <:sigma. An instance of *)
(* sigma is obtained by substituting for the type *)
(* variables \ldotsnalpha, and only for those type *)
(* variables. It's like instantiation in Typed uScheme, *)
(* except in ML, the system instantiates each sigma *)
(* automatically.                               *)
(*                                              *)
(* In my code, a substitution is represented as a finite *)
(* map from type variables to types: an environment of *)
(* type \monoboxty env. A substitution's domain is *)
(* computed by function [[dom]]. \nmllabelsubst *)
(* <boxed values 88>=                           *)
type subst = subst
val _ = op dom : subst -> name set
(* To interpret a substitution as a function from type *)
(* variables to types, we apply [[varsubst]] to it: \ *)
(* nmlflabelvarsubst                            *)
(* <boxed values 88>=                           *)
val _ = op varsubst : subst -> (name -> ty)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <shared utility functions on Hindley-Milner types>= *)
fun tysubst theta =
  let fun subst (TYVAR a) = varsubst theta a
        | subst (TYCON c) = TYCON c
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
(* <boxed values 89>=                           *)
val _ = op tysubst : subst -> (ty -> ty)
val _ = op subst   :           ty -> ty
  in  subst
  end
(* <shared utility functions on Hindley-Milner types>= *)
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  mkEnv (domain, map replace domain)
  end
(* A function produced by [[tysubst]] has type \monobox *)
(* ty -> ty and so can be composed with any other *)
(* function of the same type, \qbreak including all *)
(* functions that correspond to substitutions. To be *)
(* precise, if \subsn_1 and \subsn_2 are substitutions, *)
(* then the composition \nomathbreaktysubst \subsn_2 *)
(* otysubst \subsn_1 is a function from types to types *)
(* (and also corresponds to a substitution). Composition *)
(* is really useful, but a substitution data structure \ *)
(* subsn is strictly more useful than the corresponding *)
(* function tysubst \subsn. For one thing, \subsn can be *)
(* asked about its domain. To compose substitutions in a *)
(* way that lets me ask about the domain of the *)
(* composition, I define a function [[compose]], which *)
(* obeys these algebraic laws: {align*} tysubst(compose( *)
(* \subsn_2, \subsn_1)) --- = tysubst \subsn_2 otysubst *)
(* \subsn_1\text,                               *)
(* dom(compose(\subsn_2, \subsn_1)) --- = dom \subsn_1 \ *)
(* cupdom \subsn_2\text. {align*} \nmlflabeldom,compose *)
(* <boxed values 90>=                           *)
val _ = op compose : subst * subst -> subst
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <shared utility functions on Hindley-Milner types ((elided))>= *)
fun eqsubst (theta1, theta2) =
  let val domain  = union (dom theta2, dom theta1)
      fun eqOn a = (varsubst theta1 a = varsubst theta2 a)
  in  List.all eqOn domain
  end
(* <shared utility functions on Hindley-Milner types>= *)
fun instantiate (FORALL (formals, tau), actuals) =
  tysubst (mkEnv (formals, actuals)) tau
  handle BindListLength =>
    raise BugInTypeInference "number of types in instantiation"
(* Instantiation is as in Chapter [->], except no kind *)
(* environment is needed. Because instantiations are *)
(* computed by the system, instantiating a type scheme *)
(* with the wrong number of arguments indicates an *)
(* internal error. Such an error is signaled by raising *)
(* the exception [[BugInTypeInference]], which is raised *)
(* only when there is a fault in the interpreter; *)
(* it should never be triggered by a faulty \nml *)
(* program. [*] \nmlflabelinstantiate           *)
(* <boxed values 91>=                           *)
val _ = op instantiate : type_scheme * ty list -> ty
(* <shared utility functions on Hindley-Milner types>= *)
infix 7 |-->
fun a |--> (TYVAR a') = if a = a' then emptyEnv
                        else bind (a, TYVAR a', emptyEnv)
  | a |--> tau        = bind (a, tau, emptyEnv)
(* All substitutions can be built with [[mkEnv]], but *)
(* that's not how Milner's algorithm creates them. *)
(* Milner's algorithm substitutes for one type variable *)
(* at a time, then composes those substitutions. *)
(* To create a substitution that substitutes for a *)
(* single variable, I define an infix function [[|–>]]. *)
(* The expression \monoboxalpha |–> tau is the *)
(* substitution that substitutes [[tau]] for [[alpha]]. *)
(* In math, that substitution is written \xsubsnalphatau *)
(* . \nmlflabel|–>                            *)
(* <boxed values 92>=                           *)
val _ = op |--> : name * ty -> subst
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <shared utility functions on Hindley-Milner types>= *)
val idsubst = emptyEnv
(* <shared utility functions on Hindley-Milner types>= *)
fun eqType (TYVAR a, TYVAR a') = a = a'
  | eqType (TYCON c, TYCON c') = eqTycon (c, c')
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType _ = false
and eqTypes (taus, taus') = ListPair.allEq eqType (taus, taus')
(* The [[|–>]] function accepts any combination of alpha *)
(*  and tau. But if alpha appears free in tau   *)
(* (for example, if tau=listalpha), then the resulting *)
(* substitution \subsn is not idempotent. If \subsn is *)
(* not idempotent, then \subsno\subsn!=\subsn, and *)
(* moreover, \subsnalpha!=\subsntau. But type inference *)
(* is all about using substitutions to guarantee *)
(* equality of types, and we must be sure that every *)
(* substitution we create is idempotent, so if \subsn=( *)
(* alpha|->tau), then \subsnalpha= \subsntau. If this *)
(* equality does not hold, there is a bug in type *)
(* inference (\crefml.ex.idempotence).          *)
(*                                              *)
(* A final useful substitution is the identity  *)
(* substitution, which is represented by an empty *)
(* environment. \nmlflabelidsubst               *)
(* <boxed values 93>=                           *)
val _ = op idsubst : subst
(* In math, the identity substitution is written \ *)
(* idsubst,\notation \idsubstthe identity substitution *)
(* and it is a left and right identity of composition: \ *)
(* nomathbreak\idsubsno\subsn= \subsno\idsubsn= \subsn. *)
(*                                              *)
(* My representation of substitutions is simple but not *)
(* efficient. Efficient implementations of type *)
(* inference represent each type variable as a mutable *)
(* cell, and they apply and compose substitutions by *)
(* mutating those cells.                        *)
(*                                              *)
(* Functions that compare, create, and print types *)
(*                                              *)
(* Because a Hindley-Milner type contains no    *)
(* quantifiers, type equivalence is easier to define *)
(* than in Typed uScheme (chunk [->]). \nmlflabeleqType  *)
(* [*]                                          *)
(* <boxed values 93>=                           *)
val _ = op eqType : ty * ty -> bool
(* <shared utility functions on Hindley-Milner types>= *)
fun canonicalize (FORALL (bound, ty)) =
  let fun canonicalTyvarName n =
        if n < 26 then "'" ^ str (chr (ord #"a" + n))
        else "'v" ^ intString (n - 25)
      val free = diff (freetyvars ty, bound)
      fun unusedIndex n =
        if member (canonicalTyvarName n) free then unusedIndex (n+1) else n
      fun newBoundVars (index, [])                = []
        | newBoundVars (index, oldvar :: oldvars) =
            let val n = unusedIndex index
            in  canonicalTyvarName n :: newBoundVars (n+1, oldvars)
            end
      val newBound = newBoundVars (0, bound)
(* \setcodemargin0pt \advance\nwdefspaceby -8pt \ *)
(* umlflabelpattype                             *)
(* <boxed values 96>=                           *)
val _ = op canonicalize : type_scheme -> type_scheme
val _ = op newBoundVars : int * name list -> name list
  in  FORALL (newBound,
              tysubst (mkEnv (bound, map TYVAR newBound)) ty)
  end
(* <shared utility functions on Hindley-Milner types>= *)
local
  val n = ref 1
in
  fun freshtyvar _ = TYVAR ("'t" ^ intString (!n) before n := !n + 1)
(* Fresh type variables                         *)
(*                                              *)
(* A type variable that does not appear in any type *)
(* environment or substitution is called fresh. When a *)
(* function is introduced, fresh type variables are used *)
(* as the (unknown) types of its arguments. When a *)
(* polytype is instantiated, fresh type variables are *)
(* used as the unknown types that are substituted for *)
(* its bound type variables. And when a function is *)
(* applied, a fresh type variable is used as its *)
(* (unknown) result type.                       *)
(*                                              *)
(* Fresh type variables are created by the      *)
(* [[freshtyvar]] function. The function uses a private *)
(* mutable counter to supply an arbitrary number of type *)
(* variables of the form t n. Because a \nml expression *)
(* or definition never contains any explicit type *)
(* variables, the names don't collide with other names. *)
(* <boxed values 97>=                           *)
val _ = op freshtyvar : 'a -> ty
end
(* <shared utility functions on Hindley-Milner types>= *)
fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))
(* <boxed values 98>=                           *)
val _ = op generalize : ty * name set -> type_scheme
(* <shared utility functions on Hindley-Milner types>= *)
fun freshInstance (FORALL (bound, tau)) =
  instantiate (FORALL (bound, tau), map freshtyvar bound)
(* <boxed values 99>=                           *)
val _ = op freshInstance : type_scheme -> ty
(* <shared utility functions on Hindley-Milner types>= *)
fun typeSchemeString (FORALL ([], tau)) =
      typeString tau
  | typeSchemeString (FORALL (a's, tau)) =
      "(forall [" ^ spaceSep a's ^ "] " ^ typeString tau ^ ")"
(* A degenerate type scheme is printed as if it were a *)
(* type. But when a nondegenerate polytype is printed, *)
(* the forall is explicit, and all the quantified *)
(* variables are shown. [It~is not strictly necessary to *)
(* show the quantified variables, because in any *)
(* top-level type, \emph{all} type variables are *)
(* quantified by the~$\forall$. For this reason, *)
(* Standard~ML leaves out quantifiers and type  *)
(* variables. But when you're learning about parametric *)
(* polymorphism, explicit \texttt{forall}s are better. *)
(* In~my experience, \sml's implicit \texttt{forall}s *)
(* make it hard to new learners to understand what's *)
(* going~on. ]                                  *)
(* <boxed values 222>=                          *)
val _ = op typeString       : ty          -> string
val _ = op typeSchemeString : type_scheme -> string
(* <shared utility functions on Hindley-Milner types ((elided))>= *)
fun substString [] = "idsubst"
  | substString pairs =
      String.concatWith " o " 
      (map (fn (a, t) => a ^ " |--> " ^ typeString t) pairs)
(* {sidebar}[t]Why generalize and instantiate? \ *)
(* abovedisplayskip=0.8\abovedisplayskip \      *)
(* abovedisplayshortskip=0.8\abovedisplayshortskip \ *)
(* belowdisplayskip=0.8\belowdisplayskip [*] We use *)
(* quantified types (i.e., type schemes) so we can *)
(* instantiate them when we look them up in an  *)
(* environment. Instantiation gives us the full effect *)
(* of polymorphism. Without instantiation, we wouldn't *)
(* be able to type such ML terms as \monobox(1::nil, *)
(* true::nil). Suppose we had an environment Gamma with *)
(* only types, not type schemes:                *)
(*                                              *)
(*  Gamma= { 1 : int, true : bool, nil : listalpha, *)
(*  :: : alpha*listalpha-->listalpha}\text.     *)
(*                                              *)
(* When typechecking [[1::nil]], we would get the *)
(* constraint alpha\eqtyint. And when typechecking *)
(* [[true::nil]], we would get the constraint alpha\eqty *)
(* bool. But the conjunction \mathboxalpha\eqtyint\land *)
(* alpha\eqtybool has no solution, and type checking *)
(* would fail.                                  *)
(*                                              *)
(* Instead, we use [[freshInstance]] to make sure that *)
(* every use of a polymorphic value (here [[::]] and *)
(* [[nil]]) has a type different from any other *)
(* instance. In order to make that work, the environment *)
(* has to contain polytypes:                    *)
(*                                              *)
(*  Gamma= { 1 : \/.int, true : \/.bool, nil : \/ *)
(*  alpha\alldotlistalpha, :: : \/alpha\alldotalpha *)
(*  *listalpha-->listalpha}\text.               *)
(*                                              *)
(* Now, we can imagine our sample term like this, *)
(* writing [[::]] as a \csname@minipagefalse\endcsname \ *)
(* topsep=0pt \partopsep=0pt prefix operator so as to *)
(* show the types: {smallverbatimx} (op :: : 't121 * *)
(* 't121 list -> 't121 list (1 : int, nil : 't122 list), *)
(* op :: : 't123 * 't123 list -> 't123 list (true : *)
(* bool, nil : 't124 list)) {smallverbatimx} The *)
(* constraint [['t121]] \eqtyint\land int\eqty[['t122]]  *)
(* \land [['t123]] \eqtybool \land bool\eqty[['t124]]  *)
(* does have a solution, and the whole term has the type *)
(* \monoboxint list * bool list, as desired. {sidebar} *)
(*                                              *)
(* Type environments                            *)
(*                                              *)
(* Function [[generalize]] is called with the free type *)
(* variables of some type environment. And a type *)
(* environment contains the type of every defined name, *)
(* so it can get big. To reduce the cost of searching a *)
(* large environment for free type variables, a type *)
(* environment is represented in a way that enables the *)
(* type checker to find free type variables in constant *)
(* time.                                        *)
(*                                              *)
(* A representation of type environments must support *)
(* these functions:                             *)
(*                                              *)
(*   • Function [[bindtyscheme]] adds a binding x :  *)
(*  sigma to the environment Gamma. It is used to *)
(*  implement the \rulenameLambda rule and the  *)
(*  various \xlet rules.                        *)
(*                                              *)
(*   • Function [[findtyscheme]] looks up a variable x *)
(*  to find sigma such that Gamma(x) = sigma. It is *)
(*  used to implement the \rulenameVar rule.    *)
(*                                              *)
(*   • Function [[freetyvarsGamma]] finds the type *)
(*  variables free in Gamma, i.e., the type variables *)
(*  free in any sigma in Gamma. It is used to get a *)
(*  set of free type variables to use in        *)
(*  [[generalize]]; when a type scheme is assigned to *)
(*  a let-bound variable, only those type variables *)
(*  not free in Gamma may be \/-bound.          *)
(*                                              *)
(* If [[freetyvarsGamma]] used a representation of type *)
(* \monobox[[type_scheme]] env, it would visit every *)
(* type scheme in every binding in the environment. *)
(* Because most bindings contribute no free type *)
(* variables, most visits would be unnecessary. Instead, *)
(* all functions use a representation that includes a *)
(* cache of the type environment's free type variables. *)
(* \nmllabeltype_env                            *)
(* <specialized environments for type schemes>= *)
type type_env = type_scheme env * name set
(* <specialized environments for type schemes>= *)
val emptyTypeEnv = 
      (emptyEnv, emptyset)
fun findtyscheme (x, (Gamma, free)) = find (x, Gamma)
(* <boxed values 100>=                          *)
val _ = op emptyTypeEnv : type_env
val _ = op findtyscheme : name * type_env -> type_scheme
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <specialized environments for type schemes>= *)
fun bindtyscheme (x, sigma as FORALL (bound, tau), (Gamma, free)) = 
  (bind (x, sigma, Gamma), union (diff (freetyvars tau, bound), free))
(* <specialized environments for type schemes>= *)
fun freetyvarsGamma (_, free) = free



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \UHASKELL                    *)
(*                                                               *)
(*****************************************************************)

(* Values and abstract syntax                   *)
(*                                              *)
(* <abstract syntax and values for \uhaskell>=  *)
(* Unlike an eager language, Haskell does not always *)
(* compute with values. What a name stands for, whether *)
(* it is a let-bound name or a formal parameter, is a *)
(* mutable location [[loc]], which may contain a *)
(* [[value]] or an unevaluated [[thunk]]. If a [[loc]] *)
(* does contain a thunk, we can evaluate the thunk and *)
(* then replace the thunk with a [[value]]. Almost *)
(* everything that we were used to thinking of as a *)
(* value is now a [[loc]], except:              *)
(*                                              *)
(*   • Primitive operators and [[eval]] always return a *)
(*  [[value]], never a thunk.                   *)
(*   • Literal values are never lazy, so [[LITERAL]] *)
(*  contains [[value]].                         *)
(*                                              *)
(* <definitions of [[exp]], [[value]], and [[loc]] for \uhaskell>= *)
datatype thunk_contents 
  = VALUE       of value
  | IN_PROGRESS
  | UNEVAL       of thunk
and value (* a value in weak head normal form *)
  = CONVAL of name * loc list   (* fully saturated constructor app *)
  | SYM  of name
  | NUM  of int
  | CLOSURE   of lambda * loc env
  | PRIMITIVE of primitive
  | IO        of unit -> loc  (* monadic action *)
and exp 
  = LITERAL of value
  | VAR     of name
  | APPLY   of exp * exp list
  | LETX    of let_flavor * (name * exp) list * exp
  | DO      of              (name * exp) list * exp
  | LAMBDA  of lambda
  | CASE    of exp * match list
  | IFX     of exp * exp * exp
and let_flavor = LET | LETREC | LETSTAR
 withtype loc       = thunk_contents ref
      and lambda    = name list * exp
      and thunk     = exp * thunk_contents ref env
      and match     = (name * name list) * exp
      and primitive = thunk_contents ref list -> value
(* <definition of [[def]] for \uml\ and \uhaskell>= *)
datatype def  = VAL    of name * exp
              | VALREC of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
              | CONDEF of name * type_scheme
(* Unit tests resemble the unit tests for Typed uScheme, *)
(* but as explained in \crefpage                *)
(* (ml.check-principal-type, the typing tests are subtly *)
(* different. These unit tests are shared with other *)
(* languages that use Hindley-Milner types. \nmllabel *)
(* unit_test                                    *)
(* <definition of [[unit_test]] for languages with Hindley-Milner types>= *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * type_scheme
                   | CHECK_PTYPE       of exp * type_scheme
                   | CHECK_TYPE_ERROR  of def
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* <definitions of [[locString]] and [[valueString]] for \uhaskell>= *)
fun locString (ref IN_PROGRESS) = "_|_"
  | locString (ref (UNEVAL _))  = "..."
  | locString (ref (VALUE v))  = valueString v
and valueString (CONVAL ("cons", [l, ls])) =
                        (* As in other interpreters, we have a special way of *)

                              (* printing applications of [[cons]].           *)

                              (* <convert [[(cons l ls)]] to string>=         *)
                                             let fun tail (CONVAL ("cons", [l,
                                          ls])) = " " ^ locString l ^ tailLoc ls
                                                   | tail (CONVAL ("()", []))
                                                                           = ")"
                                                   | tail v
                                                   = " . " ^ valueString v ^ ")"
                                                 and tailLoc (ref (VALUE v))   =
                                                                          tail v
                                                   | tailLoc (ref IN_PROGRESS) =
                                                                         " _|_)"
                                                   | tailLoc (ref (UNEVAL _))  =
                                                                       " . ...)"
                                             in  "(" ^ locString l ^ tailLoc ls
                                             end
  | valueString (CONVAL (c, []))  = c
  | valueString (CONVAL (c, ls))  = "(" ^ c ^ " " ^ spaceSep (map locString ls)
                                                                           ^ ")"
  | valueString (NUM n      )   = intString n
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<procedure>"
  | valueString (PRIMITIVE _)   = "<procedure>"
  | valueString (IO _       )   = "<I/O action>"
(* And here is where we print the contents of a *)
(* location.                                    *)
(* <boxed values 3>=                            *)
val _ = op locString   : loc   -> string
val _ = op valueString : value -> string
(* Here's the printing code:                    *)
(* <definition of [[expString]] for \uhaskell>= *)
val rec expString = fn LITERAL v => valueString v
                     | VAR name => name
                     | APPLY (e, es) => prexps (e::es)
                     | LETX (lk, bs, e) => prWithBindings (prLetKind lk, bs, e)
                     | DO (bs, e) => prWithBindings ("do", bs, e)
                     | LAMBDA (xs, body) =>
                          "(lambda " ^ prnames xs ^ " " ^ expString body ^ ")"
                     | IFX (e1, e2, e3) => prexps [VAR "if", e1, e2, e3]
                     | CASE (e, ms) => foldl addMatch ("(case " ^ expString e)
                                                                              ms
and addMatch = fn ((m, b), pfx) => pfx ^ " (" ^ prApp m ^ " " ^ expString b ^
                                                                             ")"
and prApp = fn (c, []) => c
             | (c, xs) => prnames (c::xs)
and prexps  = fn l => "(" ^ spaceSep (map expString l) ^ ")"
and prnames = fn xs => "(" ^ spaceSep xs ^ ")"
and prWithBindings = fn (keyword, bs, e) =>
      "(" ^ keyword ^ " (" ^ prBindings bs ^ ") " ^ expString e ^ ")"
and prBindings = fn bs => spaceSep (map prBinding bs)
and prBinding = fn (x, e) => "(" ^ x ^ " " ^ expString e ^ ")"
and prLetKind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
(* Strings                                      *)
(*                                              *)
(* Not ready and will never be ready.           *)
(* <definitions of [[defString]] and [[defName]] for \uhaskell>= *)
fun defString _ = "<def>"
fun defName _ = "?"
(* Complete implementation of micro-Haskell     *)
(*                                              *)
(* [*] This Appendix presents the solutions to some of *)
(* the implementation problems in Chapter [->]. *)



(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \UHASKELL\ VALUES                      *)
(*                                                               *)
(*****************************************************************)

(* <utility functions on \uhaskell\ values>=    *)
fun allocThunk (e, rho) = ref (UNEVAL (e, rho))
fun allocValue v        = ref (VALUE v)
(* Here are two convenience functions for allocating new *)
(* locations:                                   *)
(* <boxed values 1>=                            *)
val _ = op allocThunk : exp * loc env -> loc
(* <utility functions on \uhaskell\ values>=    *)
fun embedList []     = CONVAL ("()", [])
  | embedList (h::t) = CONVAL ("cons", [allocValue h, allocValue (embedList t)])
fun embedBool b      = CONVAL (if b then "#t" else "#f", [])
(* When parsing a list of values, we need to convert it *)
(* to a uHaskell list.                          *)
(* <boxed values 2>=                            *)
val _ = op embedList : value list -> value
val _ = op embedBool : bool       -> value
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)





(*****************************************************************)
(*                                                               *)
(*   TYPE INFERENCE FOR \UHASKELL                                *)
(*                                                               *)
(*****************************************************************)

(* Placeholder type checker                     *)
(*                                              *)
(* <type inference for \uhaskell>=              *)
(* \qbreak                                      *)
(*                                              *)
(* Constraints and constraint solving           *)
(*                                              *)
(* In the interpreter, constraints are represented in a *)
(* way that resembles the math: the \eqty operator is  *)
(* [[ ]]; the \land operator is [[/             *)
(* ]; and the \trivc constraint is [[TRIVIAL]]. \ *)
(* nmllabelcon \nmlflabeltilde,cand,TRIVIAL [*] *)
(* <representation of type constraints>=        *)
datatype con = ~  of ty  * ty
             | /\ of con * con
             | TRIVIAL
infix 4 ~
infix 3 /\
(* (The name [[ ]] normally stands for ML's negation *)
(* function. An unqualified [[ ]] is redefined by this *)
(* [[datatype]] definition, but negation can still be *)
(* referred to by its qualified name [[Int. ]].) *)
(*                                              *)
(* Utility functions on constraints             *)
(*                                              *)
(* Many of the utility functions defined on types have *)
(* counterparts on constraints. For example, we can find *)
(* free type variables in a constraint, and we can *)
(* substitute for free type variables.\nmlflabel *)
(* freetyvarsConstraint                         *)
(* <utility functions on type constraints>=     *)
fun freetyvarsConstraint (t ~  t') = union (freetyvars t, freetyvars t')
  | freetyvarsConstraint (c /\ c') = union (freetyvarsConstraint c,
                                             freetyvarsConstraint c')
  | freetyvarsConstraint TRIVIAL   = emptyset
(* <utility functions on type constraints>=     *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)    = subst c1 /\ subst c2
        | subst TRIVIAL       = TRIVIAL
  in  subst
  end
(* <boxed values 101>=                          *)
val _ = op bindtyscheme : name * type_scheme * type_env -> type_env
(* Free type variables are found in the cache in *)
(* constant time. \nmlflabelfreetyvarsGamma \   *)
(* nwnarrowboxes                                *)
(* <boxed values 101>=                          *)
val _ = op freetyvarsGamma : type_env -> name set
(* A substitution is applied to a constraint using the *)
(* following laws: {mathpar} \subsn(tau_1\eqtytau_2) = \ *)
(* subsntau_1 \eqty\subsntau_2\text,            *)
(*                                              *)
(* \subsn(\tyc_1 \land\tyc_2) = \subsn\tyc_1 \land\subsn *)
(* \tyc_2\text,                                 *)
(*                                              *)
(* \subsn\trivc= \trivc\text. {mathpar} The code *)
(* resembles the code for [[tysubst]] in chunk [->]. [*] *)
(* \nmlflabelconsubst                           *)
(* <boxed values 101>=                          *)
val _ = op consubst : subst -> con -> con
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <utility functions on type constraints>=     *)
fun conjoinConstraints []      = TRIVIAL
  | conjoinConstraints [c]     = c
  | conjoinConstraints (c::cs) = c /\ conjoinConstraints cs
(* <boxed values 102>=                          *)
val _ = op conjoinConstraints : con list -> con
(* <utility functions on type constraints>=     *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau,tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)
(* For debugging, it can be useful to see if a  *)
(* substitution solves a constraint. \nmlflabelisSolved *)
(* <boxed values 104>=                          *)
val _ = op isSolved : con -> bool
val _ = op solves : subst * con -> bool
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <utility functions on type constraints>=     *)
(* A constraint can be printed in full, but it's easier *)
(* to read if its first passed to [[untriviate]], which *)
(* removes as many [[TRIVIAL]] sub-constraints as *)
(* possible.                                    *)
(* <definitions of [[constraintString]] and [[untriviate]]>= *)
fun constraintString (c /\ c') =
      constraintString c ^ " /\\ " ^ constraintString c'
  | constraintString (t ~  t') = typeString t ^ " ~ " ^ typeString t'
  | constraintString TRIVIAL = "TRIVIAL"

fun untriviate (c /\ c') = (case (untriviate c, untriviate c')
                              of (TRIVIAL, c) => c
                               | (c, TRIVIAL) => c
                               | (c, c') => c /\ c')
  | untriviate atomic = atomic
(* \crefml.chap defines a handful of utility functions *)
(* that operate on type-equality constraints. Two more *)
(* such functions, [[constraintString]] and     *)
(* [[untriviate]], are defined in this appendix. *)
(* <boxed values 213>=                          *)
val _ = op constraintString : con -> string
val _ = op untriviate       : con -> con
(* Two more utility functions are defined in \cref *)
(* app:ml: [[constraintString]] can be used to print *)
(* constraints, and [[untriviate]], whose type is \ *)
(* monoboxcon -> con, removes trivial conjuncts from a *)
(* constraint.                                  *)
(*                                              *)
(* Constraint solving                           *)
(*                                              *)
(* If type inference is given an ill-typed program, *)
(* it produces an unsolvable constraint. Examples of *)
(* unsolvable constraints include int\eqtybool and list *)
(* alpha\eqtyalpha. Given an unsolvable constraint, the *)
(* type checker should issue a readable error message, *)
(* not one full of machine-generated type variables. *)
(* To do so, function [[unsatisfiableEquality]] takes *)
(* the pair of types that can't be made equal, puts the *)
(* pair into canonical form, and raises the     *)
(* [[TypeError]] exception. [*]                 *)
(* <constraint solving>=                        *)
fun unsatisfiableEquality (t1, t2) =
  let val t1_arrow_t2 = funtype ([t1], t2)
      val FORALL (_, canonical) =
            canonicalize (FORALL (freetyvars t1_arrow_t2, t1_arrow_t2))
  in  case asFuntype canonical
        of SOME ([t1'], t2') =>
             raise TypeError ("cannot make " ^ typeString t1' ^
                              " equal to " ^ typeString t2')
         | _ => raise InternalError "failed to synthesize canonical type"
  end
(* <constraint solving ((prototype))>=          *)
fun solve c = raise LeftAsExercise "solve"
(* The mechanism is a little weird. To make a single *)
(* type out of tau_1 and tau_2, so their variables can *)
(* be canonicalized together, I make the type tau_1 --> *)
(* tau_2. What's weird is that there's no function—it's *)
(* just a device to make one type out of two. When I get *)
(* the canonical version, I take it apart to get back *)
(* canonical types [[t1']] and [[t2']].         *)
(*                                              *)
(* I don't provide a solver; I hope you will    *)
(* implement one. \nmlflabelsolve               *)
(* <boxed values 103>=                          *)
val _ = op solve : con -> subst
(* <constraint solving>=                        *)
(*asdf*)
(* Complete implementation of \nml type inference *)
(*                                              *)
(* [*] This Answer Set presents the solutions to most of *)
(* the implementation problems in Chapter [->]. *)
(*                                              *)

(* <constraint solving ((elided))>=             *)
fun hasNoSolution c = (solve c; false) handle TypeError _ => true
fun hasGoodSolution c = solves (solve c, c) handle TypeError _ => false
val hasSolution = not o hasNoSolution : con -> bool
fun solutionEquivalentTo (c, theta) = eqsubst (solve c, theta)
val valtype = TYCON "value"
(* <definitions of [[typeof]] and [[typdef]] for \uhaskell>= *)
fun typeof (e, Gamma) =
  let (* Calling typesof(<\ldotsne>, Gamma) returns (<\ldotsn *)
      (* tau>, \tyc) such that for every i from 1 to n, \ *)
      (* nomathbreak\typeisc\tyce_i tau_i. The base case is *)
      (* trivial; the inductive case uses this rule from *)
      (* Section [->]:[*] \usety.TypesOfC Both cases are *)
      (* implemented by function [[typesof]].         *)

(* <shared definition of [[typesof]], to infer the types of a list of expressions>= *)
      fun typesof ([],    Gamma) = ([], TRIVIAL)
        | typesof (e::es, Gamma) =
            let val (tau,  c)  = typeof  (e,  Gamma)
                val (taus, c') = typesof (es, Gamma)
            in  (tau :: taus, c /\ c')
            end
      (* To infer the type of a literal value, we call *)
      (* [[literal]], which is left as \crefml.ex.typeof. *)

(* <function [[literal]], to infer the type of a literal constant ((prototype))>= *)
      fun literal _ = raise LeftAsExercise "literal"

(* <function [[literal]], to infer the type of a literal constant ((prototype))>= *)
      fun literal _ = (valtype, idsubst)

(* <function [[ty]], to infer the type of an expression, given [[Gamma]] ((prototype))>= *)
      fun addvar (x, g) = bindtyscheme (x, FORALL([], valtype), g)
      val valtyc = (valtype, TRIVIAL)
      fun ty (LITERAL v) = valtyc
        | ty (VAR x) = (findtyscheme (x, Gamma); valtyc)
        | ty (APPLY (e, es)) = check (e::es)
        | ty (LETX (LETSTAR, [], body)) = ty body
        | ty (LETX (LETSTAR, (b :: bs), body)) =
                         ty (LETX (LET, [b], LETX (LETSTAR, bs, body)))
        | ty (LETX (LET, bs, body)) =
             let val (xs, es) = ListPair.unzip bs
             in  ty (APPLY (LAMBDA (xs, body), es))
             end
        | ty (LETX (LETREC, bs, body)) =
             let val (xs, es) = ListPair.unzip bs
                 val Gamma' = foldl addvar Gamma xs
             in  (map (fn e => typeof(e, Gamma')) es; typeof(body, Gamma'))
             end
        | ty (DO (bs, body)) = ty (LETX (LETSTAR, bs, body))
        | ty (LAMBDA (xs, e)) = typeof(e, foldl addvar Gamma xs)
        | ty (IFX (e1, e2, e3)) =
             ty (CASE (e1, [(("#t", []), e2), (("#f", []), e3)]))
        | ty (CASE (e, matches)) =
             let val _ = map (matchtype (ty e)) matches
             in  valtyc
             end
      and check es = (map ty es; valtyc)
      and matchtype tau_case ((con, xs), e) = typeof (e, foldl addvar Gamma xs)
  in  ty e
  end
(* <definitions of [[typeof]] and [[typdef]] for \uhaskell>= *)
fun typdef (d, Gamma) =
  case d
    of VALREC (x, e)      =>
                (* <infer and bind type for [[VALREC (x, e)]] for \uhaskell>= *)
                             let val alpha    = freshtyvar ()
                                 val Gamma'   = bindtyscheme (x, FORALL ([],
                                                                  alpha), Gamma)
                                 val (tau, c) = typeof (e, Gamma')
                                 val theta    = solve (c /\ alpha ~ tau)
                                 val sigma    = generalize (tysubst theta alpha,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | VAL    (x, e)      =>
                         (* Forms [[EXP]] and [[DEFINE]] are syntactic sugar. *)
                             (*                                              *)

                        (* The cases for [[VAL]] and [[VALREC]] resemble each *)

                      (* other. A [[VAL]] computes a type and generalizes it. *)
                             (* \usetyValC                                   *)

                     (* <infer and bind type for [[VAL    (x, e)]] for \nml>= *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | EXP e              =>
                          (* <infer and bind type for [[EXP e]] ((haskell))>= *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme ("it", sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | DEFINE (x, lambda) => typdef (VALREC (x, LAMBDA lambda), Gamma)
     | CONDEF (c, sigma)  => (* <bind constructor [[c]] to type [[sigma]]>=  *)
                             (* XXX missing kind check on sigma *)
                             (bindtyscheme (c, sigma, Gamma), typeSchemeString
                                                                          sigma)



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UHASKELL, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* <lexical analysis and parsing for \uhaskell, providing [[filexdefs]] and [[stringsxdefs]]>= *)
(* <lexical analysis for \uscheme\ and related languages>= *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* Tokens of the micro-Scheme language          *)
(*                                              *)
(* [*] The general parsing mechanism described in *)
(* Appendix [->] requires a language-specific   *)
(* [[pretoken]] type, which adds tokens to the brackets *)
(* described in \creflazyparse.chap. In addition to *)
(* a bracket, a micro-Scheme token may be a quote mark, *)
(* an integer literal, a Boolean literal, or a name. *)
(* <boxed values 168>=                          *)
type pretoken = pretoken
type token = token
(* <lexical analysis for \uscheme\ and related languages>= *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* This code takes a big shortcut: it assumes that \ *)
(* subsnGamma=Gamma. That assumption is sound because a *)
(* top-level Gamma never contains a free type variable ( *)
(* \exrefml.ex.no-free-tyvars-at-top-level). This *)
(* property guarantees that \subsnGamma=Gamma for any \ *)
(* subsn.                                       *)
(*                                              *)
(* A [[VALREC]] is a bit more complicated.      *)
(* The nondeterministic rule calls for an environment *)
(* that binds x to tau, but tau isn't known until e is *)
(* typechecked: \usety.ValRec The rule is made  *)
(* deterministic by initially using a fresh alpha to *)
(* stand for tau, then once tau is known, adding the *)
(* constraint alpha\eqtytau\mskip1mu: \punctrule. \ *)
(* tyrule[ValRecC]ValRec \upshapewith constraints \ *)
(* threeline \typeisc[{x |->alpha}] \tyce tau\qquadalpha *)
(* is fresh \twoquad\trivsat\subsn(\tyc\landalpha\eqty *)
(* tau) \subsnGamma=Gamma sigma= \generalize\subsnalpha *)
(* \ftv(Gamma) \nmltopt\xvalrec(x, e) -->Gamma{x |-> *)
(* sigma}                                       *)
(* <boxed values 169>=                          *)
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
  (* The other primitive worth showing here is [[error]]. *)
  (* Its type, \nomathbreak\/alpha,beta\alldotalpha-->beta *)
  (* , tells us something interesting about its behavior. *)
  (* The type suggests that [[error]] can produce an *)
  (* arbitrary beta without ever consuming one. Such a *)
  (* miracle is impossible; what the type tells us is that *)
  (* the [[error]] function never returns normally. In \ *)
  (* nml, a function of this type either halts the *)
  (* interpreter or fails to terminate; in full ML, a *)
  (* function of this type could also raise an exception. *)
  (* <boxed values 171>=                          *)
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
(* <boxed values 170>=                          *)
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
(* Parsers for micro-Scheme                     *)
(*                                              *)
(* A parser consumes a stream of tokens and produces an *)
(* abstract-syntax tree. My parsers begin with code for *)
(* parsing the smallest things and finish with the code *)
(* for parsing the biggest things. I define parsers for *)
(* tokens, literal S-expressions, micro-Scheme  *)
(* expressions, and finally micro-Scheme definitions. *)
(*                                              *)
(* Parsers for single tokens and common idioms  *)
(*                                              *)
(* Usually a parser knows what kind of token it is *)
(* looking for. To make such a parser easier to write, *)
(* I define a special parsing combinator for each kind *)
(* of token. Each one succeeds when given a token of the *)
(* kind it expects; when given any other token, it *)
(* fails.                                       *)
(* <boxed values 172>=                          *)
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

(* \Nml shares Typed uScheme's requirements for names. *)
(* It even uses the same reserved words. (Although they *)
(* aren't valid keywords in \nml, the parser reserves *)
(* the words [[set]] and [[while]]. That reservation *)
(* ensures that every grammatical \nml program is also a *)
(* grammatical micro-Scheme program.)           *)
(* <parsers for \nml\ tokens>=                  *)
val reserved = [ "if", "while", "set", "begin", "lambda", "let"
               , "letrec", "let*", "quote", "val", "define", "use"
               , "check-expect", "check-assert", "check-error"
               , "check-type", "check-principal-type", "check-type-error"
               ]

val arrow = eqx "->" namelike
val name  = rejectReserved reserved <$>! sat (curry op <> "->") namelike
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
(* <boxed values 173>=                          *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
(* \qbreak For [[let]] and [[letrec]] expressions, which *)
(* do not permit multiple bindings to the same name, *)
(* I define function [[distinctBsIn]], which enforces *)
(* the constraint that all bound names are mutually *)
(* distinct. \nwnarrowboxes                     *)
(* <boxed values 173>=                          *)
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* <parsers and parser builders for formal parameters and bindings ((higher-order))>= *)
fun asLambda inWhat (loc, e as LAMBDA _) = OK e
  | asLambda inWhat (loc, e) = 
      synerrorAt ("in " ^ inWhat ^ ", expression " ^ expString e ^ 
                  " is not a lambda")
                 loc

val asLambda = fn what => fn eparser => asLambda what <$>! @@ eparser
(* <boxed values 174>=                          *)
val _ = op asLambda : string -> exp parser -> exp parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* <boxed values 175>=                          *)
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
(* <boxed values 177>=                          *)
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
(* <boxed values 176>=                          *)
val _ = op sexp : value parser
(* Full Scheme allows programmers to notate arbitrary *)
(* cons cells using a dot in a quoted S-expression. *)
(* micro-Scheme doesn't.                        *)

(* Parsers for micro-Scheme expressions         *)
(*                                              *)
(* I define distinct parses for atomic expressions *)
(* (which aren't recursively defined) and bracketed *)
(* expressions (which are recursively defined). *)
(* An atomic expression is a variable or a literal. *)
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
(* \qbreak The full expression parser handles atomic *)
(* expressions, quoted S-expressions, the table of *)
(* bracketed expressions, a couple of error cases, and *)
(* function application, which uses parentheses but no *)
(* keyword. It is parameterized by the parsers for *)
(* atomic expressions and bracketed expressions. This *)
(* parameterization enables me to reuse it in other *)
(* interpreters. \nwnarrowboxes                 *)
(* <boxed values 179>=                          *)
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
(* <boxed values 197>=                          *)
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
(* \qbreak A type variable begins with a quote mark. *)
(* <boxed values 204>=                          *)
val _ = op tyvar : name parser
(* <boxed values 204>=                          *)
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
(* <boxed values 204>=                          *)
val _ = op arrowsOf : ('ty * 'ty list -> 'ty) -> ('ty list * 'ty -> 'ty) -> 'ty
                                              list -> 'ty list list -> 'ty error
(* <parser builders for typed languages>=       *)
fun distinctTBsIn tbindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map (fst o fst) bs) >>=+ (fn _ =>
                                                                             bs)
  in  check <$>! @@ tbindings
  end
(* <boxed values 208>=                          *)
val _ = op distinctTBsIn : ((name * 't) * 'e) list parser -> string -> ((name *
                                                           't) * 'e) list parser
(* <parsers for Hindley-Milner types with named type constructors>= *)
val arrows = arrowsOf CONAPP funtype

fun ty tokens = (
     TYCON <$> name
 <|> TYVAR <$> tyvar
 <|> usageParsers 
     [("(forall (tyvars) type)", bracket ("('a ...)", many tyvar) *> ty)]
     <!> "nested 'forall' type is not a Hindley-Milner type"
 <|> bracket ("constructor application",
              arrows <$> many ty <*>! many (arrow *> many ty))
) tokens
(* <parsers for Hindley-Milner types with named type constructors>= *)
val tyscheme =
      usageParsers [("(forall (tyvars) type)",
                     curry FORALL <$> bracket ("['a ...]", distinctTyvars) <*>
                                                                            ty)]
  <|> curry FORALL [] <$> ty
  <?> "type"
(* Types are parsed much as in \creftuschemea.chap, from *)
(* which the [[arrowsOf]] function is reused. But a  *)
(* [[forall]] type is not accepted as a type; it's a *)
(* type scheme.                                 *)
(* <boxed values 224>=                          *)
val _ = op tyvar    : string      parser
val _ = op ty       : ty          parser
val _ = op tyscheme : type_scheme parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* micro-Haskell can use micro-Scheme's lexical *)
(* analysis, so all we have here is a parser. As with *)
(* micro-Scheme, we begin with error-detection  *)
(* functions.                                   *)
(* <parsers and [[xdef]] streams for \uhaskell>= *)
val lambdaDups   = nodups ("formal parameter", "lambda")
val conDups      = nodups ("argument of constructor", "case expression")

fun letDups LETSTAR (loc, bindings) = OK bindings
  | letDups kind    (loc, bindings) =
      let val names    = map (fn (n, _) => n) bindings
          val kindName = case kind of LET => "let" | LETREC => "letrec" | _ =>
                                                                            "??"
      in  nodups ("bound name", kindName) (loc, names) >>=+ (fn _ => bindings)
      end
(* <parsers and [[xdef]] streams for \uhaskell>= *)
val atomicExp =  VAR              <$> name
             <|> LITERAL <$> NUM  <$> int
             <|> LITERAL <$> embedBool <$> booltok

val formalsIn = formalsOf "(x1 x2 ...)" name


fun exptable exp = 
  let val bindings = bindingsOf "(x e)" name exp
      val dbs      = distinctBsIn bindings
  
      val matchlhs =
            (fn n => (n, [])) <$> name
        <|> ("()", []) <$ left *> right
        <|> bracket ("(K pat ...)", pair <$> name <*> (conDups <$>! @@ (many
                                                                         name)))
      val match = bracket ("(pat exp)", pair <$> matchlhs <*> exp)
      val letx = curry3 LETX

  in  usageParsers
      [ ("(if e1 e2 e3)",            curry3 IFX <$> exp <*> exp <*> exp)
      , ("(lambda (names) body)",    curry LAMBDA <$> formalsIn "lambda" <*> exp
                                                                               )
      , ("(let (bindings) body)",    letx LET     <$> dbs "let"    <*> exp)
      , ("(letrec (bindings) body)", letx LETREC  <$> dbs "letrec" <*> exp)
      , ("(let* (bindings) body)",   letx LETSTAR <$> bindings     <*> exp)
      , ("(case exp matches)",       curry CASE   <$> exp <*> many match)
      ]
  end

fun exp tokens = (
     atomicExp
 <|> LITERAL          <$> (quote *> sexp)
 <|> exptable exp
 <|> leftCurly <!> "curly brackets are not supported"
 <|> left *> right <!> "empty application"
 <|> bracket("function application", curry APPLY <$> exp <*> many exp)
) tokens
(*                
and sexp tokens = (   
     SYM <$> (notDot <$>! name)
 <|> NUM          <$> int
 <|> boolcon      <$> booltok
 <|> (fn v => embedList [SYM "quote", v]) <$> (quote *> sexp)
 <|> embedList    <$> "(" >-- many sexp --< ")"
) tokens
and notDot "." = ERROR
                      "this interpreter cannot handle . in quoted S-expressions"
  | notDot s   = OK s
*)
(* <parsers and [[xdef]] streams for \uhaskell>= *)
fun unexpected msg (loc, _) = synerrorAt loc msg
val tyvar = curry op ^ "'" <$> (quote *> (name <?> "type variable"))

fun keyword syntax words =
  let fun isKeyword s = List.exists (fn s' => s = s') words
  in  (fn (NAME n) => if isKeyword n then SOME n else NONE | _ => NONE) <$>?
                                                                        pretoken
  end

val expKeyword = keyword "type"       ["if", "lambda",
                                       "type-lambda", "let", "let*", "letrec"]

fun ty tokens = (
     TYCON <$> name
 <|> TYVAR <$> tyvar
 <|> usageParsers [("(function (types) type)",
                    curry funtype <$> bracket ("(ty ...)", many ty) <*> ty)]
 <|> badExpKeyword <$>! left *> @@ expKeyword <* matchingRight
 <|> bracket ("(ty ty ... -> ty)",
              arrowsOf CONAPP funtype <$> many ty <*>! many (arrow *> many ty))
 <|> left *> right <!> "empty type ()"
 <|> int     <!> "expected type; found integer"
 <|> booltok <!> "expected type; found Boolean literal"
) tokens
and badExpKeyword (loc, bad) =
      synerrorAt ("looking for type but found `" ^ bad ^ "'") loc

val tyscheme =
     usageParsers [("(forall [tyvars] type)",
                     curry FORALL <$> bracket ("('a ...)", distinctTyvars) <*>
                                                                            ty)]
 <|> curry FORALL [] <$> ty
(* <parsers and [[xdef]] streams for \uhaskell>= *)
fun define f xs body = DEFINE (f, (xs, body))

val deftable = usageParsers
  [ ("(define f (args) body)", define       <$> name <*> formalsIn "define" <*>
                                                                            exp)
  , ("(val x e)",              curry VAL    <$> name <*> exp)
  , ("(val-rec x e)",          curry VALREC <$> name <*> exp)
  , ("(vcon C ty)",            curry CONDEF <$> name <*> tyscheme)
  ]

val xdef = 
     DEF <$> deftable
 <|> usageParsers [("(use filename)", USE <$> name)]
 <|> badRight "unexpected right bracket"
 <|> DEF <$> EXP <$> exp
 <?> "definition"
(* <parsers and [[xdef]] streams for \uhaskell>= *)
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
(* <boxed values 63>=                           *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UHASKELL *)
(*                                                               *)
(*****************************************************************)

(* Forcing and evaluation                       *)
(*                                              *)
(* <evaluation, testing, and the read-eval-print loop for \uhaskell>= *)
(* Tracking lazy evaluation                     *)
(*                                              *)
(* To see what's happening, we can print the transitions *)
(* <diagnostic code for lazy evaluation, including [[showforce]]>= *)
val showingforce = ref false
val forceindent = ref 0
fun indent 0 = ()
  | indent n = (print "  "; indent (n-1))
fun startforce exp =
  if !showingforce then
     ( indent (!forceindent)
     ; app print ["Forcing <|", expString exp, ", rho|>\n"]
     ; forceindent := !forceindent + 1
     )
  else ()
    
fun showforce(exp, v) =
  if !showingforce then
     ( forceindent := !forceindent - 1
     ; indent (!forceindent)
     ; app print ["Forced <|", expString exp, ", rho|> to ", valueString  v,
                                                                           "\n"]
     )
  else ()
(* <definitions of [[force]], [[eval]], and [[evaldef]] for \uhaskell>= *)
fun force (ref (VALUE v)) = v
  | force (ref IN_PROGRESS) =
              (* <tried to evaluate a thunk whose evaluation is in progress>= *)
                              raise BlackHole
  | force (thunk as ref (UNEVAL (exp, rho))) =
       let val () = thunk := IN_PROGRESS
           val () = startforce exp
           val v  = eval (exp, rho)
           val () = thunk := VALUE v
           val () = showforce (exp, v)
(* The hard part is getting from a thunk back to a weak *)
(* head normal form, which is done by evaluating *)
(* abstract syntax. The two are mutually recursive. *)
(* <boxed values 4>=                            *)
val _ = op eval  : thunk -> value
val _ = op force : loc   -> value
       in  v
       end
(* Function [[eval]] forces a thunk. We show a couple of *)
(* simple cases: forcing a literal produces that *)
(* literal, and forcing a variable forces the location *)
(* the variable stands for. We also show one more *)
(* complex case: when forcing a [[case]] expression, we *)
(* insist that                                  *)
(*                                              *)
(*  1. The scrutinee must be a constructor application. *)
(*  2. There must be exactly exactly one case match *)
(*  which mentions the constructor being applied  *)
(*  ([[c]]).                                    *)
(*  3. The number of variables in the match must equal *)
(*  the number of arguments to the constructor. *)
(*                                              *)
(* <definitions of [[force]], [[eval]], and [[evaldef]] for \uhaskell>= *)
and eval (e, rho) =
  let fun toThunk e = allocThunk (e, rho)
      fun ev (LITERAL v) = v
        | ev (VAR     x) = force (find (x, rho))
        | ev (CASE (e, arms)) =
            (case ev e
               of CONVAL (c, args) =>
                    (case List.filter (fn ((c', _), _) => c = c') arms
                       of [((_, formals), body)] =>
                            (eval(body, bindList (formals, args, rho))
                             handle BindListLength =>

                        (* <bleat about [[c]] with [[args]] and [[formals]]>= *)
                                 raise RuntimeError ("Constructor " ^ c ^
                                                            " was applied to " ^
                                                     countString args "argument"
                                                    ^ ", but case expression " ^
                                                     "expected " ^ countString
                                                            formals "argument"))
                        | [] => raise RuntimeError ("Case expression had no " ^

                                              "alternative for constructor " ^ c
 ^ "; alternatives were: " ^ nullOrCommaSep "<none>" (map (fn ((c, _), _) => c)
                                                                          arms))
                        | _ :: _ :: _ =>
                            raise RuntimeError ("Case expression had multiple "
                                                                               ^
                                                "alternatives for constructor "
                                                                            ^ c)
                    )
                | v => raise RuntimeError (
                                     "Case discrimination on non-constructor " ^
                                           valueString v)
             )
        (* Monadic primitives and [[do]] notation       *)
        (*                                              *)
        (* <cases for evaluation of [[do]] notation>=   *)
        | ev (DO ([], body)) = ev body
        | ev (DO ((x, e) :: bindings, body)) =
           ev (APPLY (LITERAL (PRIMITIVE monadicBindList),
                      [e, LAMBDA([x], DO (bindings, body))]))
        (* Forcing other kinds of thunks is \exref      *)
        (* haskell.ex.eval.                             *)
        (* <other cases for internal function [[ev]] ((prototype))>= *)
        | ev _ = raise LeftAsExercise "evaluating thunks"
  in  ev e
  end
(* <[[and]] definitions of monadic functions>=  *)
and monadicBind (mLoc, kLoc) =
  IO (fn () => let val a = runIO (force mLoc)
               in  runIO (force (apply (kLoc, [a])))
               end)
and runIO (IO f) = f ()
  | runIO _      = raise BugInTypeInference "expected I/O action"
and apply (f, args) =
  allocThunk (APPLY (LITERAL (force f), map (LITERAL o force) args), emptyEnv)
and monadicBindList [m, k] = monadicBind (m, k)
  | monadicBindList _ = raise InternalError
                                           "`do` notation desugared incorrectly"
(* <definitions of [[force]], [[eval]], and [[evaldef]] for \uhaskell>= *)
fun evaldef (d, rho : loc env) =
  case d
    of VALREC (name, e)      => let val cell = ref IN_PROGRESS
                                    val rho  = bind (name, cell, rho)
                                    val _    = cell := UNEVAL (e, rho)
                                in  (rho, locString cell)
                                end
     | VAL    (name, e)      => let val cell = ref IN_PROGRESS
                                    val _    = cell := UNEVAL (e, rho)
                                    val rho  = bind (name, cell, rho)
                                in  (rho, locString cell)
                                end
     | EXP e                 =>
                              (* <evaluate [[e]] and run or bind the result>= *)
                                (case eval (e, rho)
                                   of IO action => (action () ; (rho, ""))
                                    | v => let val cell = allocValue v
                                           in  (bind ("it", cell, rho),
                                                                 locString cell)
                                           end)
     | DEFINE (name, lambda) => evaldef (VALREC (name, LAMBDA lambda), rho)
     | CONDEF (name, ty)     => let val cell = allocValue (vconValue (name, ty))
                                    val rho  = bind (name, cell, rho)
                                in  (rho, name)
                                end
and vconValue (name, sigma) =
  if takesArgs sigma then
    PRIMITIVE (fn args => CONVAL (name, args))
  else
    CONVAL (name, [])
and takesArgs (FORALL (_, CONAPP (TYCON "function", _))) = true
  | takesArgs (FORALL (_, _))                            = false
(* Equality for testing                         *)
(*                                              *)
(* Only atomic values and applications of value *)
(* constructors can test equal. And testing forces the *)
(* arguments of value constructors.             *)
(* <utility functions that use [[force]]>=      *)
fun testEquals (CONVAL (c, locs), CONVAL (c', locs')) =
      c = c' andalso ListPair.allEq testEquals (map force locs, map force locs')
  | testEquals (SYM n, SYM n') = (n = n')
  | testEquals (NUM n, NUM n') = (n = n')
  | testEquals _ = false
(* <utility functions that use [[force]]>=      *)
fun strict f = f o map force
fun arityError n args =
  raise RuntimeError ("primitive function expected " ^ intString n ^
                      "arguments; got " ^ intString (length args))
fun binary_valop f = strict (fn [a, b] => f(a, b) | args => arityError 2 args)
fun unary_valop  f = strict (fn [a]    => f a     | args => arityError 1 args)
(* Primitives                                   *)
(*                                              *)
(* The distinction between strict and non-strict primops *)
(* returns! But the semantics is slightly different this *)
(* time---environment are never involved. [[cons]] is *)
(* the only non-strict primop.                  *)
(* <boxed values 6>=                            *)
val _ = op + : int * int -> int
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Arithmetic primitives expect and return integers. *)
(* <utility functions that use [[force]]>=      *)
fun arithop f = binary_valop (fn (NUM n1, NUM n2) => NUM (f (n1, n2))
                               | _ => raise RuntimeError "integers expected")
val arithtype    = funtype ([inttype, inttype], inttype)
fun comptype tau = funtype ([tau, tau], booltype)
(* We use the chunk [[<<primitives for micro-Haskell *)
(* [[::]]>>]] to cons up all the primitives and their *)
(* names into one giant list. [One advantage of literate *)
(* programming is we can distribute this definition *)
(* throughout the source file, so we can put the parts *)
(* where they make the most sense. In this case, we put *)
(* the pieces of the list of primitives near those *)
(* higher-order functions we use to define them.] We'll *)
(* use that list to build the initial environment for *)
(* the read-eval-print loop.                    *)

(* <utility functions that use [[force]]>=      *)
fun inject_bool x =
      CONVAL (if x then "#t" else "#f", [])
fun project_bool (CONVAL ("#t", [])) = true
  | project_bool (CONVAL ("#f", [])) = false
  | project_bool _ = raise RuntimeError "projected non-boolean"

val projectBool = project_bool  (* I'm just here to pass my regression tests *)


fun inject_predicate f = fn x => inject_bool (f x)
fun predop f = unary_valop  (inject_predicate f)

fun comparison f = binary_valop (inject_predicate f)
fun intcompare f = comparison (
                     fn (NUM n1, NUM n2) => f (n1, n2)
                      | _ => raise BugInTypeInference "integers expected")
(* We have two kinds of predicates: ordinary predicates *)
(* take one argument, and comparisons take two. Some *)
(* comparisons apply only to integers. (From here on, *)
(* you can figure out the types for yourself---or get *)
(* the ML compiler to tell you.)                *)
(* <boxed values 7>=                            *)
val _ = op inject_bool  : bool -> value
val _ = op project_bool : value -> bool
(* And here come the predicates. Equality comparison *)
(* succeeds only on symbols and numbers. The empty list *)
(* is dealt with through [[case]] expressions.  *)

(* <definitions of [[basis]] and [[processDef]] for \uhaskell>= *)
type basis = type_env * loc env
fun processDef (d, (Gamma, rho), interactivity) =
  let val (Gamma, tystring)  = typdef  (d, Gamma)
      val (rho,   valstring) = evaldef (d, rho)
      val _ = if echoes interactivity andalso size valstring > 0 then
                (* don't print when running an IO action *)
                println (valstring ^ " : " ^ tystring)
              else
                ()
(* <boxed values 9>=                            *)
val _ = op evaldef : def * loc env -> loc env * string
(* <boxed values 9>=                            *)
val _ = op processDef : def * basis * interactivity -> basis
        in  (Gamma, rho)
        end
fun dump_names (types, values) = app (println o fst) values  (*OMIT*)
(* As in Typed uScheme, [[elabEvalDef]] preserves the *)
(* phase distinction: type inference is independent of *)
(* [[rho]] and [[evaldef]].                     *)

(* <shared definition of [[withHandlers]]>=     *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn)
                        a
                        (fn s => caught (fillAtLoc (s, loc)))
       (* The [[BlackHole]] exception needs handlers.  *)

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]]>= *)
       | BlackHole => caught
                          "Run-time error <at loc>: evaluation hit a black hole"

(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]]>= *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)
       (* These exception handlers are used in all the *)
       (* bridge-language interpreters.                *)

(* <shared unit-testing utilities>=             *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* <boxed values 43>=                           *)
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
(* \qvfilbreak2.5in                             *)
(*                                              *)
(* Unit testing                                 *)
(*                                              *)
(* [*] Overall, function [[testIsGood]] is structured in *)
(* the same way as in the interpreters for Typed Impcore *)
(* and Typed uScheme*, but the details of the testing *)
(* are quite different: \nml offers not only    *)
(* [[check-type]] but also [[check-principal-type]]. *)
(* <definition of [[testIsGood]] for \nml>=     *)
(* <definition of [[skolemTypes]] for languages with named type constructors>= *)
val skolemTypes = 
  streamMap (fn n => TYCON ("skolem type " ^ intString n)) naturals
(* [*] The logic needed to implement [[check-type]] is *)
(* not so simple. A [[check-type]] needs to pass if the *)
(* type given in the test is an instance of the *)
(* principal type of the thing tested—that is, the type *)
(* of the thing tested can be more polymorphic than what *)
(* the test is asking for. The instance property is not *)
(* so easy to check directly—searching for permutations *)
(* is tedious—but the idea is simple: no matter what *)
(* types are used to instantiate sigma_i, sigma_g can be *)
(* instantiated to the same type. To help myself *)
(* implement this idea, I define a supply of skolem *)
(* types that cannot possibly be part of any type in any *)
(* \nml program.                                *)
(* <boxed values 218>=                          *)
val _ = op skolemTypes : ty stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
fun asGeneralAs (sigma_g, sigma_i as FORALL (a's, tau)) =
  let val theta = mkEnv (a's, streamTake (length a's, skolemTypes))
      val skolemized = tysubst theta tau
      val tau_g = freshInstance sigma_g
  in  (solve (tau_g ~ skolemized); true) handle _ => false
  end
(* Skolem types are used to create an ``arbitrary'' *)
(* instance of type scheme sigma_i. If the constraint *)
(* solver can make that instance equal to a fresh *)
(* instance of sigma_g, then sigma_g is as general as  *)
(* sigma_i.                                     *)
(* <boxed values 219>=                          *)
val _ = op asGeneralAs : type_scheme * type_scheme -> bool
(* Function [[asGeneralAs]] suffices to implement the *)
(* [[check-type]] test. The test passes if the type of  *)
(* [[e]] is as general as the type being claimed for  *)
(* [[e]].                                       *)
(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
fun typeSchemeIsAscribable (e, sigma_e, sigma) =
  if asGeneralAs (sigma_e, sigma) then
    true
  else
    failtest ["check-type failed: expected ", expString e,
              " to have type ", typeSchemeString sigma,
              ", but it has type ", typeSchemeString sigma_e]
(* \qbreak And [[asGeneralAs]] is also sufficient to *)
(* implement [[check-principal-type]], which checks for *)
(* equivalence. Two type schemes are equivalent if each *)
(* is as general as the other. To avoid having to write *)
(* error messages twice, I implement one of the *)
(* generality checks using [[typeSchemeIsAscribable]]. *)
(*                                              *)
(* <shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]]>= *)
fun typeSchemeIsEquivalent (e, sigma_e, sigma) =
  if typeSchemeIsAscribable (e, sigma_e, sigma) then
    if asGeneralAs (sigma, sigma_e) then
      true
    else
      failtest ["check-principal-type failed: expected ", expString e,
                " to have principal type ", typeSchemeString sigma,
                ", but it has the more general type ", typeSchemeString sigma_e]
  else
    false  (* error message already issued *)
fun testIsGood (test, (Gamma, rho)) =
  let fun ty e = typeof (e, Gamma)
                 handle NotFound x =>
                   raise TypeError ("name " ^ x ^ " is not defined")
      fun deftystring d =
        snd (typdef (d, Gamma))
        handle NotFound x => raise TypeError ("name " ^ x ^ " is not defined")
      (* \qtrim1                                      *)
      (*                                              *)

(* <definitions of [[check{Expect,Assert,Error}Checks]] that use type inference>= *)
      fun checkExpectChecks (e1, e2) = 
        let val (tau1, c1) = ty e1
            val (tau2, c2) = ty e2
            val c = tau1 ~ tau2
            val theta = solve (c1 /\ c2 /\ c)
        in  true
        end handle TypeError msg =>
            failtest ["In (check-expect ", expString e1, " ", expString e2,
                                                                     "), ", msg]

(* <definitions of [[check{Expect,Assert,Error}Checks]] that use type inference>= *)
      fun checkExpChecksIn what e =
        let val (tau, c) = ty e
            val theta = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", what, " ", expString e, "), ", msg]
      val checkAssertChecks = checkExpChecksIn "check-assert"
      val checkErrorChecks  = checkExpChecksIn "check-error"
      (* \qtrim1                                      *)
      (*                                              *)

(* <definitions of [[check{Expect,Assert,Error}Checks]] that use type inference>= *)
      fun checkExpectChecks (e1, e2) = 
        let val (tau1, c1) = ty e1
            val (tau2, c2) = ty e2
            val c = tau1 ~ tau2
            val theta = solve (c1 /\ c2 /\ c)
        in  true
        end handle TypeError msg =>
            failtest ["In (check-expect ", expString e1, " ", expString e2,
                                                                     "), ", msg]

(* <definitions of [[check{Expect,Assert,Error}Checks]] that use type inference>= *)
      fun checkExpChecksIn what e =
        let val (tau, c) = ty e
            val theta = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", what, " ", expString e, "), ", msg]
      val checkAssertChecks = checkExpChecksIn "check-assert"
      val checkErrorChecks  = checkExpChecksIn "check-error"
      (* <definition of [[checkTypeChecks]] using type inference>= *)
      fun checkTypeChecks form (e, sigma) = 
        let fun fail msg =
              failtest ["In (", form, " ", expString e, " " ^ typeSchemeString
                                                              sigma, "), ", msg]
            fun freevars (FORALL (alphas, tau)) = diff (freetyvars tau, alphas)
            fun unused   (FORALL (alphas, tau)) = diff (alphas, freetyvars tau)
        in  let val (tau, c) = ty e
                val theta  = solve c
            in  case (freevars sigma, unused sigma)
                  of ([], []) => true
                   | (alpha :: _, _) => fail ("type variable " ^ alpha ^
                                                          " must be quantified")
                   | (_, alpha :: _) => fail ("quantified type variable " ^
                                                         alpha ^ " is not used")
            end handle TypeError msg => fail msg
        end
      fun checks (CHECK_EXPECT (e1, e2)) = checkExpectChecks (e1, e2)
        | checks (CHECK_ASSERT e)        = checkAssertChecks e
        | checks (CHECK_ERROR e)         = checkErrorChecks e
        | checks (CHECK_TYPE  (e, tau))  = checkTypeChecks "check-type" (e, tau)
        | checks (CHECK_PTYPE (e, tau))  = checkTypeChecks
                                                          "check-principal-type"
                                                                        (e, tau)
        | checks (CHECK_TYPE_ERROR e)    = true

      fun outcome e = withHandlers (fn () => OK (eval (e, rho))) () (ERROR o
                                                                     stripAtLoc)
      (* <[[asSyntacticValue]] for \uscheme, \timpcore, \tuscheme, and \nml>= *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue _           = NONE
      (* In most languages, only literal expressions are *)
      (* considered syntactic values.                 *)
      (* <boxed values 167>=                          *)
      val _ = op asSyntacticValue : exp -> value option

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
      (* <boxed values 39>=                           *)
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
      (* <boxed values 40>=                           *)
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
      (* \qtrim1 Function [[checkAssertPasses]] does the *)
      (* analogous job for [[check-assert]].          *)
      (* <boxed values 41>=                           *)
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
      (* <boxed values 42>=                           *)
      val _ = op checkErrorPasses : exp -> bool
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEquals (cx, ex)
      (* Each unit test first computes [[sigma_e]], then calls *)
      (* the appropriate function.                    *)
      (* <definitions of [[check*Type*Passes]] using type inference>= *)
      fun checkTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsAscribable (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-type ", expString e,
                      " ", typeSchemeString sigma, "), ", msg]
      (* <definitions of [[check*Type*Passes]] using type inference>= *)
      fun checkPrincipalTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsEquivalent (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-principal-type ", expString e, " ",
                      typeSchemeString sigma, "), ", msg]
      (* The [[check-type-error]] tests expects a type error *)
      (* while computing [[sigma_e]].                 *)
      (* <definitions of [[check*Type*Passes]] using type inference>= *)
      fun checkTypeErrorPasses (EXP e) =
            (let val (tau, c) = ty e
                 val theta  = solve c
                 val sigma' = generalize (tysubst theta tau, freetyvarsGamma
                                                                          Gamma)
             in  failtest ["check-type-error failed: expected ", expString e,
                           " not to have a type, but it has type ",
                           typeSchemeString sigma']
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
      fun passes (CHECK_EXPECT (c, e))  = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)       = checkAssertPasses c
        | passes (CHECK_ERROR c)        = checkErrorPasses  c
        | passes (CHECK_TYPE  (c, tau)) = checkTypePasses          (c, tau)
        | passes (CHECK_PTYPE (c, tau)) = checkPrincipalTypePasses (c, tau)
        | passes (CHECK_TYPE_ERROR c)   = checkTypeErrorPasses c

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
(* <boxed values 44>=                           *)
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
      (* <boxed values 71>=                           *)
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
(* <boxed values 70>=                           *)
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
(* <boxed values 70>=                           *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
  in  basis
  end



(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \UHASKELL\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* <implementations of \uhaskell\ primitives and definition of [[initialBasis]]>= *)
(* The list primitives are dead easy, except we have to *)
(* deal with non-strictness and forcing.        *)
(* <utility functions for building \uhaskell\ primitives>= *)
fun ns_unary  f = (fn [a]    => f a      | args => arityError 1 args)
fun ns_binary f = (fn [a, b] => f (a, b) | args => arityError 2 args)
(* <implementations of \uhaskell\ primitives>=  *)
fun fullyEval v =
  let fun full (CONVAL (c, args)) = app (full o force) args
        | full (NUM n)       = ()
        | full (SYM v)       = ()
        | full (CLOSURE   _) = ()
        | full (PRIMITIVE _) = ()
        | full (IO _       ) = ()
      val _ = full v
(* The full primitive                           *)
(*                                              *)
(* <boxed values 5>=                            *)
val _ = op fullyEval : value -> value
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

  in  v
  end
(* <implementations of \uhaskell\ primitives>=  *)
val unitval   = CONVAL ("unit", [])
val unitthunk = allocValue unitval
(* Monadic primitives                           *)
(*                                              *)
(* <boxed values 8>=                            *)
val _ = op apply : loc * loc list -> loc
val _ = op monadicBind : loc * loc -> value
val primitiveBasis =
  let fun addPrim ((name, prim, tau), (Gamma, rho)) = 
        ( bindtyscheme (name, generalize (tau, freetyvarsGamma Gamma), Gamma)
        , bind (name, allocValue (PRIMITIVE prim), rho)
        )
  in  foldl addPrim (emptyTypeEnv, emptyEnv) (
                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("full", unary_valop fullyEval,
                                                    funtype ([alpha], alpha)) ::

                          (* Here's a primitive to determined whether we show *)

                              (* forces:                                      *)

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ( "showforce"
                                              , unary_valop (fn x => (
                                             showingforce := project_bool x; x))
                                              , funtype ([booltype], unittype)
                                              ) ::

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("+", arithop op +, arithtype) :: 
                                              ("-", arithop op -, arithtype) :: 
                                              ("*", arithop op * , arithtype) ::
                                                                                
                                              ("/", arithop op div, arithtype)
                                                                              ::

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("<", intcompare op <, comptype
                                                                     inttype) ::
                                              (">", intcompare op >, comptype
                                                                     inttype) ::
                                              ("=", comparison (fn (NUM n1, NUM
                                                                  n2) => n1 = n2
                                                                 | (SYM v1, SYM
                                                                  v2) => v1 = v2
                                                                 |  _ => raise
                                 RuntimeError "equality used on non-base type"),
                                                    comptype alpha) ::
                                              (*
                                              ("number?",  predop (fn (NUM  _)
                                                       => true | _ => false)) ::
                                              ("symbol?",  predop (fn (SYM  _)
                                                       => true | _ => false)) ::
                                              *)

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("error", unary_valop (fn v =>
                                raise RuntimeError (valueString (fullyEval v))),
                                                          funtype ([alpha], beta
                                                                           )) ::
                                              ("show", unary_valop (SYM o
                                                                   valueString),
                                                        funtype ([alpha],
                                                                    symtype)) ::
                                              ("symwidth", unary_valop (fn (SYM
                                                              s) => NUM (size s)
                                                                         | _ =>
                            raise BugInTypeInference "symwidth got non-symbol"),
                                                        funtype ([symtype],
                                                                    inttype)) ::

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("cons", ns_binary (fn (a, b) =>
                                                        CONVAL("cons", [a, b])),
                                                               funtype ([alpha,
                                            listtype alpha], listtype alpha)) ::

                               (* ("tuple", fn es => CONVAL ("tuple", es)) :: *)

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("trace", let fun bleat s =
                                                 TextIO.output(TextIO.stdErr, s)
                                                        in  ns_binary (fn (msg,
                                                                           v) =>
                                                                          (app
                                                           bleat ["**TRACE**: ",

                                      valueString (fullyEval (force msg)), "\n"]

                                                                     ; force v))
                                                        end,
                                                               funtype ([alpha,
                                                                beta], beta)) ::

                              (* <primitives for \uhaskell\ [[::]]>=          *)
                                              ("print", unary_valop (fn v => IO
                            (fn () => ( print (valueString (fullyEval v) ^ "\n")

                                                                 ; unitthunk))),
                                                        funtype ([alpha], iotype
                                                                   unittype)) ::
                                              ("return", ns_unary (fn thunk =>
                                                           IO (fn () => thunk)),
                                                        funtype ([alpha], iotype
                                                                      alpha)) ::
                                              (">>=", ns_binary monadicBind,
                                                        funtype ([iotype alpha,
                          funtype ([alpha], iotype beta)], iotype beta)) :: nil)
  end

val predefs = 
               [ ";  <predefined uHaskell functions>=          "
               , "(define map (f xs)"
               , "  (case xs"
               , "     (() '())"
               , "     ((cons y ys) (cons (f y) (map f ys)))))"
               , ";  Let's look at this evaluation in detail: \\resume "
               , ";  enumerate                                    "
               , ";  Evaluating [[(cdr nats)]] gets to \\thunk[[(map "
               , ";  ((curry +) 1) nats)]]                        "
               , ""
               , ";  The rest of the initial basis                "
               , ";                                               "
               , ";  The initial basis in micro-Haskell is almost exactly "
               , ";  as in micro-Scheme. THIS IS NOW A LIE!!! The single "
               , ";  difference is that in an association list, we have to "
               , ";  use a list of pairs, not a list of 2-element lists. "
               , ";  This representation makes it possible for keys and "
               , ";  values to be of different types.             "
               , ";  <predefined uHaskell functions>=          "
               , "(vcon pair (forall ('a 'b) (function ('a 'b) (pair 'a 'b))))"
               , "(define fst (p)"
               , "   (case p ((pair x y) x)))"
               , "(define snd (p)"
               , "   (case p ((pair x y) y)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define list1 (x) (cons x '()))"
               , "(define bind (x y alist)"
               , "  (case alist"
               , "       (() (list1 (pair x y)))"
               , "       ((cons p ps)"
               , "          (if (= x (fst p))"
               , "              (cons (pair x y) ps)"
               , "              (cons p (bind x y ps))))))"
               , ";  We need a test to see if a variable is bound; we "
               , ";  can't return the empty list for unbound variables, "
               , ";  because the empty list is not always of the right "
               , ";  type. It must therefore be an error to look up an "
               , ";  unbound variable.                            "
               , ""
               , ";  <predefined uHaskell functions>=          "
               , "(define null? (xs)"
               , "   (case xs (() #t)"
               , "            ((cons y ys) #f)))"
               , "(define car (xs)"
               , "   (case xs (() (error 'car-of-empty-list))"
               , "            ((cons x _) x)))"
               , "(define cdr (xs)"
               , "   (case xs (() (error 'cdr-of-empty-list))"
               , "            ((cons _ xs) xs)))"
               , "(define bound? (x alist)"
               , "  (if (null? alist) "
               , "    #f"
               , "    (if (= x (fst (car alist)))"
               , "      #t"
               , "      (bound? x (cdr alist)))))"
               , "(define find (x alist)"
               , "  (if (null? alist) "
               , "    (error 'not-found)"
               , "    (if (= x (fst (car alist)))"
               , "      (snd (car alist))"
               , "      (find x (cdr alist)))))"
               , ";  <predefined uHaskell functions>=          "
               , "(vcon unit unit)"
               , "(define >> (m1 m2) (>>= m1 (lambda (_) m2)))"
               , "(define mapM_ (f xs)"
               , "  (case xs (() (return unit))"
               , "           ((cons y ys) (>> (f y) (mapM_ f ys)))))"
               , ";  We represent a break point using a simple data "
               , ";  structure. A micro-Haskell value of type [[break]] "
               , ";  contains an integer giving the total demerits "
               , ";  associated with breaking lines at that point, and if "
               , ";  it is not the very first point in the paragraph, "
               , ";  it also points to the words in the line preceding it, "
               , ";  plus the previous break.                     "
               , ";  <predefined uHaskell functions>=          "
               , "(vcon some (forall ('a) (function ('a) (option 'a))))"
               , "(vcon none (forall ('a) (option 'a)))"
               , ";  This may look like a lot of candidates, but they are "
               , ";  generated lazily, and we needn't use them all. "
               , ";  In particular, once we have a candidate whose line is "
               , ";  too wide to be useful, we're guaranteed not to use "
               , ";  any subsequent candidates---they are also too wide. "
               , ";  To implement this reduction in the candidate pool, we "
               , ";  define function [[overfull-cutoff]]. (``Overfull'' is "
               , ";  TeX's word for a line that is too wide.)     "
               , ""
               , ";  The function [[tails]] is defined as follows: "
               , ";  <predefined uHaskell functions>=          "
               , "(define tails (xs)"
               , "  (cons xs (if (null? xs) '() (tails (cdr xs)))))"
               , ";  The suffixes of [[xs]] always includes [[xs]] itself, "
               , ";  which is then followed by the suffixes of [[(cdr "
               , ";  xs)]]. The name [[tails]] is taken from the full "
               , ";  Haskell module [[Data.List]].                "
               , ""
               , ";  Initial basis                                "
               , ";                                               "
               , ";  These parts of the initial basis are identical to "
               , ";  what we find in micro-Scheme.                "
               , ";  <predefined uHaskell functions>=          "
               , "(define caar (xs) (car (car xs)))"
               , "(define cadr (xs) (car (cdr xs)))"
               , "(define cdar (xs) (cdr (car xs)))"
               , "(define cddr (xs) (cdr (cdr xs)))"
               , "(define cdddr (xs) (cdr (cddr xs)))"
               , "(define length (xs)"
               , "  (if (null? xs) 0"
               , "    (+ 1 (length (cdr xs)))))"
               , "(define and (b c) (if b  c  b))"
               , "(define or  (b c) (if b  b  c))"
               , "(define not (b)   (if b #f #t))"
               , ";  <predefined uHaskell functions>=          "
               , "(define append (xs ys)"
               , "  (if (null? xs)"
               , "     ys"
               , "     (cons (car xs) (append (cdr xs) ys))))"
               , "(define revapp (xs ys)"
               , "  (if (null? xs)"
               , "     ys"
               , "     (revapp (cdr xs) (cons (car xs) ys))))"
               , "(define reverse (xs) (revapp xs '()))"
               , ";  <predefined uHaskell functions>=          "
               , "(define o (f g) (lambda (x) (f (g x))))"
               , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
               , "(define uncurry (f) (lambda (x y) ((f x) y)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define filter (p? l)"
               , "  (if (null? l)"
               , "    '()"
               , "    (if (p? (car l))"
               , "      (cons (car l) (filter p? (cdr l)))"
               , "      (filter p? (cdr l)))))"
               , ";  <predefined uHaskell functions>=          "
               , "(define exists? (p? l)"
               , "  (if (null? l)"
               , "    #f"
               , "    (if (p? (car l)) "
               , "      #t"
               , "      (exists? p? (cdr l)))))"
               , "(define all? (p? l)"
               , "  (if (null? l)"
               , "    #t"
               , "    (if (p? (car l))"
               , "      (all? p? (cdr l))"
               , "      #f)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define foldr (op zero l)"
               , "  (if (null? l)"
               , "    zero"
               , "    (op (car l) (foldr op zero (cdr l)))))"
               , "(define foldl (op zero l)"
               , "  (if (null? l)"
               , "    zero"
               , "    (foldl op (op (car l) zero) (cdr l))))"
               , ";  <predefined uHaskell functions>=          "
               , "(define <= (x y) (not (> x y)))"
               , "(define >= (x y) (not (< x y)))"
               , "(define != (x y) (not (= x y)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define max (x y) (if (> x y) x y))"
               , "(define min (x y) (if (< x y) x y))"
               , "(define mod (m n) (- m (* n (/ m n))))"
               , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
               , "(define lcm (m n) (* m (/ n (gcd m n))))"
               , ";  <predefined uHaskell functions>=          "
               , "(define min* (l) (foldr min (car l) (cdr l)))"
               , "(define max* (l) (foldr max (car l) (cdr l)))"
               , "(define gcd* (l) (foldr gcd (car l) (cdr l)))"
               , "(define lcm* (l) (foldr lcm (car l) (cdr l)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define list1 (x)               (cons x '()))"
               , "(define list2 (x y)             (cons x (list1 y)))"
               , "(define list3 (x y z)           (cons x (list2 y z)))"
               , "(define list4 (x y z a)         (cons x (list3 y z a)))"
               , "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
               , "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
               , "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
               ,
               "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define takewhile (p? l)"
               , "  (if (null? l)"
               , "     '()"
               , "     (if (p? (car l))"
               , "         (cons (car l) (takewhile p? (cdr l)))"
               , "         '())))"
               , "(define dropwhile (p? l)"
               , "  (if (null? l)"
               , "     '()"
               , "     (if (p? (car l))"
               , "         (dropwhile p? (cdr l))"
               , "         l)))"
               , ";  <predefined uHaskell functions>=          "
               , "(define drop (n l)"
               , "  (if (null? l)"
               , "     '()"
               , "     (if (= n 0)"
               , "         l"
               , "         (drop (- n 1) (cdr l)))))"
               , ";  <predefined uHaskell functions>=          "
               , "(define nth (xs n)"
               , "   (car (drop n xs)))"
               , ";  <predefined uHaskell functions>=          "
               , ";  Applying [[full]] to [[nats]] won't terminate; if you "
               , ";  try it, the interpreter will keep evaluating "
               , ";  arguments to successive [[cons]] calls until it runs "
               , ";  out of memory. To make it possible to probe infinite "
               , ";  lists, micro-Haskell includes a [[take]] function "
               , ";  that returns a finite prefix, of length [[n]]: "
               , ";  <definition of [[take]], a uHaskell basis function>= "
               , "(define take (n xs)"
               , "  (if (or (= n 0) (null? xs))"
               , "      '()"
               , "      (cons (car xs) (take (- n 1) (cdr xs)))))"
               , ";  (micro-Haskell also includes [[drop]], which returns "
               , ";  what's left when [[n]] elements have been taken. "
               , ";  Together, [[take]] and [[drop]] obey the law (append "
               , ";  (take n xs) (drop n xs))=xs.)                "
               , ""
                ]

val initialBasis =
  let val xdefs = stringsxdefs ("predefined functions", predefs)
  in  readEvalPrintWith predefinedFunctionError (xdefs, primitiveBasis,
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
(* <boxed values 72>=                           *)
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
(* <boxed values 72>=                           *)
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
(* <boxed values 73>=                           *)
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
(* <boxed values 74>=                           *)
val _ = op usage : (unit -> unit) ref
(* \qbreak To represent actions that might be called for *)
(* by command-line options, I define type [[action]]. *)
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
(* <boxed values 75>=                           *)
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
(* <boxed values 76>=                           *)
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
(* <boxed values 77>=                           *)
val _ = op actions : (string * action) list
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* \qbreak Now that the available command-line options *)
(* are known, I can define a usage function. Function *)
(* [[CommandLine.name]] returns the name by which the *)
(* interpreter was invoked.                     *)
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
(* <boxed values 78>=                           *)
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
(* <boxed values 79>=                           *)
val _ = op strip_options : action -> string list -> action * string list
