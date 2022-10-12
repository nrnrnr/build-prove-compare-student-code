(* <usm.sml>=                                   *)


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
(* <boxed values 148>=                          *)
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
(* <boxed values 16>=                           *)
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
(* <boxed values 16>=                           *)
val _ = op find : name * 'a env -> 'a
(* \mlsflabelfind                               *)

(* Again using [[::]], function [[bind]] adds a new *)
(* binding to an existing environment. Unlike \cref *)
(* scheme.chap's [[bind]], it does not allocate a *)
(* mutable reference cell.                      *)
(* <boxed values 16>=                           *)
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
(* <boxed values 16>=                           *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* Finally, environments can be composed using the + *)
(*  operator. In my ML code, this operator is   *)
(* implemented by function [[<+>]], which I declare to *)
(* be [[infix]]. It uses the predefined infix function  *)
(* [[@]], which is ML's way of writing [[append]]. \ *)
(* mlsflabel<+>                                 *)
(* <boxed values 16>=                           *)
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
(* <boxed values 98>=                           *)
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
(* <boxed values 96>=                           *)
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
(* <boxed values 97>=                           *)
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
(* <boxed values 88>=                           *)
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
(* <boxed values 89>=                           *)
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
(* Sometimes, as when printing substitutions for *)
(* example, the empty list should be represented by *)
(* something besides the empty string. Like maybe the *)
(* string [["idsubst"]]. Such output can be produced by, *)
(* e.g., [[nullOrCommaSep "idsubst"]].          *)
(* <boxed values 90>=                           *)
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
(* <boxed values 91>=                           *)
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
(* <boxed values 92>=                           *)
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
(* <boxed values 100>=                          *)
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
(* <boxed values 101>=                          *)
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
(* <boxed values 102>=                          *)
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
(* <boxed values 133>=                          *)
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
(* <boxed values 93>=                           *)
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
(* <boxed values 99>=                           *)
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
(* <boxed values 94>=                           *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* <collections with mapping and combining functions>= *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* <boxed values 95>=                           *)
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
(* <boxed values 109>=                          *)
type 'a susp = 'a susp
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

(* <suspensions>=                               *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* <boxed values 110>=                          *)
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
(* <boxed values 111>=                          *)
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
(* <boxed values 111>=                          *)
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
(* <boxed values 112>=                          *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 112>=                          *)
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
(* <boxed values 113>=                          *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* Function [[streamOfEffects]] can be used to produce a *)
(* stream of lines from an input file:          *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 114>=                          *)
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
(* <boxed values 115>=                          *)
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
(* <boxed values 116>=                          *)
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
(* <boxed values 117>=                          *)
val _ = op naturals : int stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* The format is consulted by function [[synerrormsg]], *)
(* which produces the message that accompanies a syntax *)
(* error. The source location may be omitted only for *)
(* standard input; error messages about files loaded *)
(* with [[use]] are always accompanied by source-code *)
(* locations.                                   *)

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
(* <boxed values 118>=                          *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 118>=                          *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* <boxed values 119>=                          *)
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
(* <boxed values 120>=                          *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 121>=                          *)
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
(* <boxed values 122>=                          *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* Concatenation turns a stream of streams of tau's into *)
(* a single stream of tau's. I define it using a *)
(* [[streamOfUnfold]] with a two-part state: the first *)
(* element of the state holds an initial [[xs]], \qbreak *)
(* and the second part holds the stream of all remaining *)
(* streams, [[xss]]. To concatenate the stream of *)
(* streams [[xss]], I use an initial state of [[(EOS, *)
(* xss)]].                                      *)
(* <boxed values 122>=                          *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* In list and stream processing, [[concat]] is very *)
(* often composed with [[map f]]. The composition is *)
(* usually called [[concatMap]].                *)
(* <boxed values 123>=                          *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* <boxed values 124>=                          *)
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
(* <boxed values 125>=                          *)
val _ = op streamTake : int * 'a stream -> 'a list
(* <streams>=                                   *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* <boxed values 126>=                          *)
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
(* <boxed values 144>=                          *)
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
(* <boxed values 145>=                          *)
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
(* <boxed values 146>=                          *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 147>=                          *)
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
(* <boxed values 149>=                          *)
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
(* <boxed values 150>=                          *)
val _ = op pzero : ('a, 'b) xformer
(* This parser obeys the algebraic law          *)
(*                                              *)
(*  \monoboxt <|> pzero = \monoboxpzero <|> t = \ *)
(*  monoboxt\text.                              *)
(*                                              *)
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)
(* <boxed values 150>=                          *)
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
(* <boxed values 151>=                          *)
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
(* <boxed values 152>=                          *)
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
(* <boxed values 153>=                          *)
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
(* <boxed values 154>=                          *)
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
(* <boxed values 155>=                          *)
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
(* <boxed values 156>=                          *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 157>=                          *)
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
(* <boxed values 158>=                          *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* <boxed values 159>=                          *)
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
(* <boxed values 160>=                          *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* <stream transformers and their combinators>= *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* <boxed values 161>=                          *)
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
(* <boxed values 162>=                          *)
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
(* <boxed values 163>=                          *)
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
(* <boxed values 164>=                          *)
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
(* <boxed values 128>=                          *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 128>=                          *)
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
(* <boxed values 129>=                          *)
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
(* <boxed values 130>=                          *)
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
(* <boxed values 131>=                          *)
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
(* <boxed values 132>=                          *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 132>=                          *)
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
(* <boxed values 173>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* \qbreak To support a stream of marked lines—possibly *)
(* marked, located lines—I define transformers [[eol]], *)
(* [[inline]], and [[srcloc]]. The [[eol]] transformer *)
(* returns the number of the line just ended.   *)
(* <boxed values 173>=                          *)
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
(* <boxed values 165>=                          *)
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
(* <boxed values 166>=                          *)
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
(* <boxed values 167>=                          *)
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
(* <boxed values 168>=                          *)
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
(* <boxed values 169>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 170>=                          *)
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
(* <boxed values 171>=                          *)
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
(* <boxed values 172>=                          *)
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
(* <boxed values 174>=                          *)
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
(* <boxed values 175>=                          *)
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
(* <boxed values 176>=                          *)
val _ = op asAscii : ('t, string) polyparser -> ('t, string) polyparser
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
(* <boxed values 176>=                          *)
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
(* <boxed values 177>=                          *)
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
(* <boxed values 186>=                          *)
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
(* <boxed values 187>=                          *)
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
(* <boxed values 179>=                          *)
val _ = op left  : ('t plus_brackets, bracket_shape located) polyparser
val _ = op right : ('t plus_brackets, bracket_shape located) polyparser
val _ = op pretoken : ('t plus_brackets, 't) polyparser
(* <transformers for interchangeable brackets>= *)
fun badRight msg =
  (fn (loc, shape) => synerrorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* <boxed values 180>=                          *)
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
(* <boxed values 178>=                          *)
val _ = op errorAtEnd : ('t plus_brackets, 'a) polyparser * ('a -> string list)
                                            -> ('t plus_brackets, 'b) polyparser
(* <transformers for interchangeable brackets>= *)
datatype right_result
  = FOUND_RIGHT      of bracket_shape located
  | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
  | NO_RIGHT
(* <boxed values 181>=                          *)
val _ = op notCurly  : bracket_shape located -> bool
val _ = op leftCurly : ('t plus_brackets, bracket_shape located) polyparser
(* <boxed values 181>=                          *)
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
(* <boxed values 182>=                          *)
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
(* <boxed values 183>=                          *)
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
(* <boxed values 184>=                          *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* <boxed values 184>=                          *)
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
(* <boxed values 185>=                          *)
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
(* <boxed values 188>=                          *)
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
(* The type-compatibility judgment can fail in unusually *)
(* many ways. So my implementation has lots of code for *)
(* detecting bad outcomes and issuing error messages, *)
(* and it defines several auxiliary functions:  *)
(*                                              *)
(*   • Function [[appliesMu]] says if a type is an *)
(*  application of type constructor µ.         *)
(*   • Function [[validateTyvarArguments]] ensures that *)
(*  the arguments in a constructor application are *)
(*  distinct type variables; it is defined only on *)
(*  constructor applications.                   *)
(*   • Function [[validateLengths]] checks that the *)
(*  number of type variables in a \/ is the same as *)
(*  the number of type parameters specified by µ's *)
(*  kind.                                       *)
(*                                              *)
(* \umlflabelvalidate [*]                       *)
(* <boxed values 189>=                          *)
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
(* <boxed values 190>=                          *)
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
(* <boxed values 191>=                          *)
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
(* <boxed values 192>=                          *)
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
(* <boxed values 193>=                          *)
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
(* <boxed values 194>=                          *)
val _ = op parseWithErrors : ('t, 'a) polyparser ->                     't
                                    located eol_marked stream -> 'a error stream
(* <streams that issue two forms of prompts>=   *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* <boxed values 195>=                          *)
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
(* <boxed values 196>=                          *)
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
(*   ABSTRACT SYNTAX AND VALUES FOR \USMALLTALK                  *)
(*                                                               *)
(*****************************************************************)

(* <abstract syntax and values for \usmalltalk>= *)
(* \qbreak                                      *)
(*                                              *)
(* Frames and [[return]]                        *)
(*                                              *)
(* According to uSmalltalk's operational semantics, *)
(* every message send allocates a new frame F, by the *)
(* predicate F \notinF_n. In the interpreter, a frame is *)
(* represented by a value of type [[frame]], each of *)
(* which carries a unique integer. A new frame is *)
(* allocated by function [[newFrame]].          *)
(* <support for \usmalltalk\ stack frames>=     *)
datatype frame = FRAME_NUMBER of int
local
  val next_f = ref 0
in
  fun newFrame () = FRAME_NUMBER (!next_f) before next_f := !next_f + 1
end
(* Top-level expressions and expressions in unit tests *)
(* are evaluated outside the context of any message *)
(* send. In these circumstances, [[eval]] is told that *)
(* the current frame is [[noFrame]].            *)
(* <support for \usmalltalk\ stack frames>=     *)
val noFrame = newFrame () (* top level, unit tests, etc... *)
(* A uSmalltalk [[return]] is implemented by the ML *)
(* [[Return]] exception. The exception keeps track of *)
(* the frames that are unwound by the [[return]]; each *)
(* element of the list has type [[active_send]]. *)
(* <support for \usmalltalk\ stack frames>=     *)
type active_send = { method : name, class : name, loc : srcloc }
(* <support for \usmalltalk\ stack frames>=     *)
fun activeSendString { method, class, loc = (file, line) } =
  let val obj = if String.isPrefix "class " class then class
                else "an object of class " ^ class
  in  concat [file, ", line ", intString line, ": ", "sent `", method, "` to ",
                                                                            obj]
  end
(* The [[rep]] part of an object exposes one of the *)
(* biggest differences between uSmalltalk and   *)
(* Smalltalk-80. In Smalltalk-80, every object owns a *)
(* collection of mutable locations, called ``instance *)
(* variables,'' each of which can be filled either with *)
(* an ordinary object or with a sequence of bytes. But *)
(* because uSmalltalk is implemented in ML, raw *)
(* locations and sequences of bytes are not useful *)
(* representations. In uSmalltalk, every object owns a *)
(* single representation, which is defined by   *)
(* ML datatype [[rep]]. That representation may be a *)
(* collection of named, mutable locations representing *)
(* instance variables, or it may be any of half a dozen *)
(* other primitive representations.             *)
(* <definitions of [[exp]], [[rep]], and [[class]] for \usmalltalk>= *)
datatype rep
  = USER     of value ref env (* ordinary object *)
  | ARRAY    of value Array.array
  | NUM      of int
  | SYM      of name
  | CLOSURE  of name list * exp list * value ref env * class * frame
  | CLASSREP of class
  | METHODV  of method        (* compiled method *)
(* Internally, an object's class is represented by an ML *)
(* value of type [[class]]. The internal representation *)
(* includes a superclass, instance-variable names, and *)
(* methods. A superclass is found on every class except *)
(* the distinguished root class, [[Object]]. A class's *)
(* [[ivars]] and [[methods]] lists include only the *)
(* instance variables and methods defined in that class, *)
(* not those of its superclass. [*]             *)
(* <definitions of [[exp]], [[rep]], and [[class]] for \usmalltalk>= *)
and class
  = CLASS of { name    : name            (* name of the class *)
             , super   : class option    (* superclass, if any *)
             , ivars   : string list     (* instance variables *)
             , methods : method env ref  (* both exported and private *)
             , class   : metaclass ref   (* class of the class object *)
             }
and metaclass = PENDING | META of class
(* <definitions of [[exp]], [[rep]], and [[class]] for \usmalltalk>= *)
and exp = VAR       of name
        | SET       of name * exp
        | SEND      of srcloc * exp * name * exp list
        | BEGIN     of exp list
        | BLOCK     of name list * exp list
        | RETURN    of exp
        | PRIMITIVE of name * exp list
        | METHOD    of name list * name list * exp list
        | SUPER
        | LITERAL   of rep
        | VALUE     of class * rep
(* Operational \rlapheadersemantics             *)
(*                                              *)
(* [*]                                          *)
(*                                              *)
(* \qtrim1                                      *)
(*                                              *)
(* The operational semantics of uSmalltalk is in the *)
(* same family as the operational semantics of  *)
(* micro-Scheme: it's a big-step semantics in which each *)
(* variable name stands for a mutable location. And like *)
(* micro-Scheme's semantics, uSmalltalk's semantics uses *)
(* closures; a block in uSmalltalk works about the same *)
(* way as a [[lambda]] expression in micro-Scheme. But *)
(* in several other ways, uSmalltalk's semantics works *)
(* quite differently.                           *)
(*                                              *)
(*   • Unlike a block, a uSmalltalk method does not *)
(*  evaluate to a closure; a method is represented as *)
(*  code plus a static superclass, with no other *)
(*  environment. When a message is dispatched to a *)
(*  method, the method's body is evaluated in an *)
(*  environment built from global variables, instance *)
(*  variables, message arguments, and local     *)
(*  variables. Like arguments and local variables, *)
(*  the instance variables and global variables are *)
(*  not known until the message is sent. Because *)
(*  methods send messages that activate other   *)
(*  methods, global variables must be available *)
(*  separately, to help build the environment of the *)
(*  next method. The globals' locations are therefore *)
(*  stored in their own environment xi. All other *)
(*  variables' locations are stored in environment  *)
(*  rho.                                        *)
(*   • The semantics of [[return]], which terminates the *)
(*  method in which it appears, cannot be expressed *)
(*  using the same judgment as the evaluation of an *)
(*  expression. The [[return]] expression is a  *)
(*  ``control operator'' like those described in \ *)
(*  crefschemes.chap, and like those other control *)
(*  operators, it could be described using a    *)
(*  small-step semantics with an explicit stack. But *)
(*  this would be a bad idea: unless you already *)
(*  understand how a language works, that kind of *)
(*  semantics is hard to follow. Instead, the   *)
(*  [[return]] operator is described by a new   *)
(*  big-step judgment form; with some parts omitted, *)
(*  the judgment looks like \nomathbreak <e,...> \ *)
(*  returns<v, F; ...>, and it means that evaluating  *)
(*  e causes stack frame F to return value v.   *)
(*   • Every value is an object, and every object has *)
(*  both a class and a representation. When     *)
(*  necessary, a value v is written in the form v = \ *)
(*  usmvalclassrep.                             *)
(*   • The behaviors of literal integers, symbols, and *)
(*  arrays are defined by classes, and if class *)
(*  [[SmallInteger]], [[Symbol]], or [[Array]] is *)
(*  redefined, the behaviors of the associated  *)
(*  literals change. For example, if you complete *)
(*  Exercises [->], [->], and [->], you will change *)
(*  the behavior of integer literals to provide a *)
(*  seamless, transparent blend of small- and   *)
(*  large-integer arithmetic, all without touching *)
(*  the uSmalltalk interpreter. The power comes at a *)
(*  price: if you make a mistake redefining     *)
(*  [[SmallInteger]], for example, you could render *)
(*  your interpreter unusable. The dependence of *)
(*  behavior on the current definitions of classes *)
(*  like [[SmallInteger]] is reflected in the   *)
(*  semantics.                                  *)
(*                                              *)
(* uSmalltalk's semantics requires a more elaborate *)
(* abstract-machine state than is needed for    *)
(* micro-Scheme. micro-Scheme's state, <e,rho,sigma>, *)
(* has only three components: the syntax being  *)
(* evaluated, the locations of all the variables, and *)
(* the contents of all the locations. To specify message *)
(* send, sending to [[super]], and [[return]],  *)
(* uSmalltalk's abstract-machine state needs four more *)
(* components (\vrefcommasmall.tab.metavars):   *)
(*                                              *)
(*   • Environment xi holds the locations of the global *)
(*  variables. It's needed because unlike a     *)
(*  micro-Scheme function, a method is given access *)
(*  to the global variables defined at the time it *)
(*  receives a message, not at the time it is   *)
(*  defined.                                    *)
(*   • Class \superclass tracks the static superclass of *)
(*  the method definition within which an expression *)
(*  is evaluated. This class, which is not the same *)
(*  as the superclass of the object that received the *)
(*  message (Exercise [->]), is the class at which *)
(*  method search begins when a message is sent to *)
(*  [[super]]. A static superclass is associated with *)
(*  every method and every block, and for all the *)
(*  expressions and blocks of a single method,  *)
(*  it remains unchanged.                       *)
(*   • Stack frame \aframe tracks the method activation *)
(*  that [[return]] should cause to return. Like a *)
(*  static superclass, an active frame is associated *)
(*  with every method and every block, and for all *)
(*  the expressions and blocks of a single activation *)
(*  of a single method, it remains unchanged.   *)
(*   • Set \usedframes records all the stack frames that *)
(*  have ever been used. It is used to ensure that *)
(*  every time a method is activated, the activation *)
(*  is uniquely identified with a new stack frame \ *)
(*  aframe. That device ensures in turn that if a *)
(*  [[return]] escapes its original activation, any *)
(*  attempt to evaluate that [[return]] results in a *)
(*  checked run-time error. The frame set \usedframes *)
(*  is threaded throughout the entire computation, in *)
(*  much the same way as the store sigma.       *)
(*                                              *)
(* Each individual component is used in a       *)
(* straightforward way, but the sheer number can be *)
(* intimidating. No wonder most theorists prefer to work *)
(* with functional languages!                   *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(*  Metavariable           What it stands for   *)
(*  e          Expression  Expression being evaluated *)
(*  rho        Environment Instance variables,  *)
(*                         arguments, and local *)
(*                         variables            *)
(*  \          Class       Destination for messages *)
(*  superclass             to [[super]]         *)
(*  \aframe    Stack frame Method activation that is *)
(*                         terminated by [[return]] *)
(*  xi         Environment Global variables     *)
(*  sigma      Store       Current values of all *)
(*                         variables            *)
(*  \          Frame set   Every activation of every *)
(*  usedframes             method ever          *)
(*                                              *)
(* Components of an initial state [*]           *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* [*]                                          *)
(*                                              *)
(* The seven components listed in \cref         *)
(* small.tab.metavars form the initial state of an *)
(* abstract machine for evaluating uSmalltalk: \usmevale *)
(* . If expression e is evaluated successfully, the *)
(* machine transitions, expressing one of two behaviors: *)
(*                                              *)
(*   • If an expression terminates normally and produces *)
(*  a value v, this behavior is represented by a *)
(*  judgment of the form \jform[usmalltalk.eval.exp]\ *)
(*  usmevale ==>\usmevalr[']v. As usual for a   *)
(*  language with imperative features, evaluating e *)
(*  can change the values of variables, so evaluation *)
(*  results in a new store sigma'. And evaluation may *)
(*  send messages and allocate new stack frames, *)
(*  so evaluation also produces a new used-frame set  *)
(*  \usedframes'. But the main result of evaluation *)
(*  is the value v; sigma' and \usedframes' just *)
(*  capture effects. To make the judgment a little *)
(*  easier to read, v is separated from the effects *)
(*  using a semicolon, not a comma.             *)
(*                                              *)
(*   • If an expression evaluates a [[return]], *)
(*  it immediately terminates an activation of the *)
(*  method in which the [[return]] appears. I say it *)
(*  ``returns v to frame \aframe','' and the behavior *)
(*  is represented by a judgment of the form \jform *)
(*  [usmalltalk.return.exp]\usmevale \returns \ *)
(*  usmevalret[']v\aframe', with an arrow       *)
(*  pointing up. Again, the main results are    *)
(*  separated from the effects by a semicolon.  *)
(*                                              *)
(* If a syntactic form contains an expression, its *)
(* evaluation can end in [[return]] behavior. But while *)
(* we are learning the main part of the language, *)
(* [[return]] behaviors are distracting. For that *)
(* reason, the semantics of [[return]] are presented in *)
(* their own section.                           *)
(*                                              *)
(* Another complication of [[return]] is that its *)
(* evaluation can terminate the evaluation of a list of *)
(* expressions. To express this behavior precisely, the *)
(* semantics uses new judgment forms that describe the *)
(* possible outcomes of evaluating a list of    *)
(* expressions.                                 *)
(*                                              *)
(*   • \jform[usmalltalk.eval.exps]\usmeval[\ldotsne] = *)
(*  => \usmevalr['][\ldotsnv]                   *)
(*  Evaluating a list of expressions produces a list *)
(*  of values [\ldotsnv].                       *)
(*   • \jform[usmalltalk.return.exps]\usmeval[\ldotsne] *)
(*  \returns \usmevalret[']v\aframe'            *)
(*  Evaluating a list of expressions returns v to \ *)
(*  aframe'.                                    *)
(*                                              *)
(* The first judgment describes evaluation as it would *)
(* happen in a language like micro-Scheme, and its rules *)
(* are worth giving right away. Formally, a list of *)
(* expressions [\ldotsne] has one of two forms: it is \ *)
(* emptylist or it has the form \sconse \es. The *)
(* corresponding result of its evaluation also has two *)
(* forms: it is \emptylist or it has the form \sconsv \ *)
(* vs. Each form has its own rule for evaluation. *)
(*                                              *)
(* \ops EmptyList \usmeval\emptylist ==>\usmevalr\ *)
(* emptylist                                    *)
(*                                              *)
(* \ops NonemptyList \twoline \usmevale ==>\usmevalr[']v *)
(* \usmeval[']\es ==>\usmevalr['']\vs \usmeval\sconse \ *)
(* es ==>\usmevalr['']\sconsv \vs The rules for the *)
(* second list-evaluation judgment are given with the *)
(* rules for the other returns.                 *)
(*                                              *)
(* Finally, because the [[value]] primitive evaluates a *)
(* block, which can contain any uSmalltalk expression, *)
(* the evaluation of a primitive requires its own form. *)
(* A primitive gets access to global variables through  *)
(* xi; it may change values of variables in the store  *)
(* sigma; and it may allocate new stack frames, adding *)
(* to \usedframes. When a primitive p is passed values \ *)
(* ldotsnv, its behavior is therefore described using *)
(* the judgment form \jform[usmalltalk.eval.prim] \ *)
(* usmevalprimp[\ldotsnv] \primyields \usmevalr['] v. *)
(*                                              *)
(* Semantics of expressions without \texttt{\upshape *)
(* return}                                      *)
(*                                              *)
(* [*] The part of uSmalltalk's semantics that is most *)
(* compatible with micro-Scheme is the part that applies *)
(* to situations in which no [[return]] is evaluated. *)
(* Except for [[return]], each syntactic form in \ *)
(* crefpage(small.fig.syntax has a rule that describes *)
(* its non-returning evaluation. (The semantics of *)
(* [[return]] is deferred to \crefsmall.opsem.return.) *)
(*                                              *)
(* Variables and assignment                     *)
(*                                              *)
(* As in Impcore, environments rho and xi track local *)
(* and global variables, but as in micro-Scheme, they *)
(* bind each defined name x to a mutable location. Aside *)
(* from the extra bookkeeping imposed by messages to *)
(* [[super]] and by returns, which manifests as extra *)
(* components in the abstract-machine state, nothing *)
(* here is new.                                 *)
(*                                              *)
(* \ops Var \twoquadx in dom rho rho(x) = l \usmeval\var *)
(* (x) ==>\usmevalrsigma(l)                     *)
(*                                              *)
(* \ops GlobalVar x \notindom rho\andalsox in dom xi\ *)
(* andalsoxi(x) = l \usmeval\var(x) ==>\usmevalrsigma(l) *)
(*                                              *)
(* Assignment to x translates x into a location l, then *)
(* changes the value in l. As in micro-Scheme, the store *)
(* is threaded. The set of allocated stack frames is *)
(* threaded in the same way; evaluating expression e *)
(* transitions that set from \usedframes to \usedframes' *)
(* . \ops Assign x in dom rho\andalsorho(x) = l\andalso\ *)
(* usmevale ==>\usmevalr[']v \usmeval\xset(x, e) ==>\ *)
(* fvaluevsigma'{l|->v}\usedframes'             *)
(*                                              *)
(* \ops AssignGlobal x \notindom rho\andalsox in dom xi\ *)
(* andalsoxi(x) = l\andalso\usmevale ==>\usmevalr[']v \ *)
(* usmeval\xset(x, e) ==>\fvaluevsigma'{l|->v}\ *)
(* usedframes' Assignment to names [[self]], [[super]], *)
(* [[true]], [[false]], and [[nil]] is not permitted, *)
(* but this restriction is enforced in the parser, so it *)
(* need not be mentioned here.                  *)
(*                                              *)
(* Self and super                               *)
(*                                              *)
(* In uSmalltalk's syntax, [[self]] is treated as an *)
(* ordinary variable, but [[super]] is a distinct *)
(* syntactic form. In most contexts, [[super]] is *)
(* evaluated like [[self]]. \ops Super \usmeval\var *)
(* (self) ==>\usmevalrv \usmeval\xsuper ==>\usmevalrv *)
(* When [[super]] identifies the recipient of a message, *)
(* however, it behaves differently from [[self]]; *)
(* to account for the difference in behavior, message *)
(* send requires a special rule for messages sent to *)
(* [[super]] (\cpagerefsmall.ops.SendSuper).    *)
(*                                              *)
(* Values                                       *)
(*                                              *)
(* As in Impcore, a \xvalue form evaluates to itself *)
(* without changing the store.                  *)
(*                                              *)
(* \ops Value \usmeval\xvalue(v) ==>\usmevalrv  *)
(*                                              *)
(* Literals                                     *)
(*                                              *)
(* A literal evaluates to an instance of        *)
(* [[SmallInteger]], [[Symbol]], or [[Array]]. Only *)
(* integer and symbol literals are formalized here. \ops *)
(* LiteralNumber \usmeval\xliteral(\xnum(n)) ==> \ *)
(* usmevalr\usmvalsigma(xi([[SmallInteger]]))\xnum(n) *)
(*                                              *)
(* \ops LiteralSymbol \usmeval\xliteral(\xsym(s)) ==> \ *)
(* usmevalr\usmvalsigma(xi([[Symbol]]))\xsym(s) The *)
(* class of a literal number or a literal symbol is *)
(* taken from the current global environment xi, which *)
(* means that a literal's behavior can be changed by *)
(* changing class [[SmallInteger]] or [[Symbol]]. *)
(*                                              *)
(* Blocks                                       *)
(*                                              *)
(* A block is much like a [[lambda]] abstraction, except *)
(* that its body \es is a sequence of expressions, not a *)
(* single expression. Evaluating a block creates a *)
(* closure, which captures the current environment rho, *)
(* the static superclass \superclass, and the current *)
(* stack frame \aframe. If the block is sent somewhere *)
(* else and is evaluated inside another method, as in *)
(* the [[isEmpty]] method on class [[Collection]] (\ *)
(* chunkrefsmall.chunk.isEmpty), for example, its *)
(* [[return]] still terminates frame \aframe. \ops *)
(* MkClosure v = \usmvalsigma(xi([[Block]])) \xclo<x_1, *)
(* ..., x_n>\esrho \superclass\aframe \usmeval\xblock *)
(* (<x_1, ..., x_n>, \es) ==> \usmevalrv        *)
(*                                              *)
(* Sequential execution                         *)
(*                                              *)
(* \xbegin expressions are evaluated as in micro-Scheme: *)
(* evaluate the expressions in sequence and produce the *)
(* last value. \ops EmptyBegin \usmeval\xbegin() ==>\ *)
(* usmevalr\usmnil                              *)
(*                                              *)
(* \ops Begin \usmeval[\ldotsne] ==>\usmevalr['][\ldotsn *)
(* v] \usmeval\xbegin(e_1, e_2, ..., e_n) ==>\usmevalr *)
(* [']v_n                                       *)
(*                                              *)
(* Message send                                 *)
(*                                              *)
(* [*] A message send takes place in four stages: *)
(* evaluate the receiver and the arguments, find the *)
(* method to dispatch to, set up a new environment and *)
(* frame, and evaluate the method's body. The dispatch *)
(* algorithm is expressed using the judgment form \jform *)
(* [usmalltalk.eval.dispatch]\sendToDispatchesm c imp, *)
(* which should be pronounced ``sending m to c is *)
(* answered by imp.'' The judgment means that sending a *)
(* message m to an object of class c dispatches to the *)
(* implementation imp. Judgment \sendToDispatchesm c imp *)
(* is provable if and only if imp is the first method *)
(* named m defined either on class c or on one of c's *)
(* superclasses (\exrefsmall.ex.send-to-dispatches). *)
(*                                              *)
(* An ordinary message send tries \sendToDispatchesm c *)
(* imp on the class of the receiver: [*] [*] \ops. Send *)
(* \senduserlines \fiveline e !=\xsuper \usmevale ==>\ *)
(* usmevalr[_0]\usmvalc r \usmeval[_0][\ldotsne] ==>\ *)
(* usmevalr[_n][\ldotsnv] \sendToDispatchesm c \ *)
(* xusermethod(_, <\ldotsnx>, <\ldotsky>, e_m, s) \ *)
(* newframe\notin\usedframes_n l_1, ..., l_n \notindom *)
(* sigma_n \qquad\ldotskl' \notindom sigma_n l_1, ..., *)
(* l_n, \ldotskl' all distinct rho_i = instanceVars(\ *)
(* usmvalc r) rho_a = {x_1|->l_1, ..., x_n|->l_n } rho_l *)
(* = {y_1 |->l'_1, ..., y_k |->l'_k } \sigma = sigma_n *)
(* {l_1 |->v_1, ...l_n |->v_n, l'_1 |->\usmnil, ...l'_k *)
(* |->\usmnil} \se_mrho_i \envplusrho_a \envplusrho_l s\ *)
(* newframexi\sigma\usedframes_n \cup{\newframe} ==>\ *)
(* usmevalr[']v \usmeval\xsend(m, e, e_1, ..., e_n) ==>\ *)
(* usmevalr[']v The premise on the first line shows that *)
(* this is a rule for an ordinary send, not a send to \ *)
(* xsuper. The rest of the rule has much in common with *)
(* the closure rule for micro-Scheme:           *)
(*                                              *)
(*   • The premises on the next two lines show the *)
(*  evaluation of the receiver e and of the arguments *)
(*  \ldotsne. After these evaluations, we know we are *)
(*  sending message m to receiver r of class c with *)
(*  actual parameters \ldotsnv, and the store is  *)
(*  sigma_n.                                    *)
(*                                              *)
(*   • The premise \sendToDispatchesm c \xusermethod(_, *)
(*  <\ldotsnx>, <\ldotsky>, e_m, s) shows that this *)
(*  send executes a method with formal parameters \ *)
(*  ldotsnx, local variables \ldotsky, body e_m, and *)
(*  static superclass [[s]].                    *)
(*                                              *)
(*   • The next three lines show the allocation of a *)
(*  fresh stack frame and of fresh locations to hold *)
(*  the message arguments and the local variables of *)
(*  the method.                                 *)
(*                                              *)
(*   • The equations for rho_i, rho_a, and rho_l create *)
(*  environments for the receiver's instance    *)
(*  variables, the method's formal parameters, and *)
(*  the method's local variables, respectively. *)
(*                                              *)
(*   • The equation for \sigma initializes the formal *)
(*  parameters and the local variables.         *)
(*                                              *)
(*   • Finally, the last premise shows the evaluation of *)
(*  the body of the method e_m, in the new      *)
(*  environment created by combining environments for *)
(*  instance variables, actual parameters, and local *)
(*  variables. Any returns go to the new stack frame  *)
(*  \newframe.                                  *)
(*                                              *)
(* \stdbreak Function [[instanceVars]] is not specified *)
(* formally. Calling instanceVars(\usmvalc r),  *)
(* as defined in chunk [->], takes the representation r *)
(* of an object and returns an environment mapping the *)
(* names of that object's instance variables to the *)
(* locations containing those instance variables. The *)
(* environment also maps the name [[self]] to a location *)
(* containing the object itself.                *)
(*                                              *)
(* When a message is sent to \xsuper, the action is *)
(* almost the same, except the method search takes place *)
(* on \superclass, the static superclass of the current *)
(* method, not on the class of the receiver. The new *)
(* parts of the \xsuper rule are shown in black; the *)
(* parts that are shared with the ordinary \rulenameSend *)
(* rule appear in \colorgraygray. [*] \opstemplate *)
(* SendSuper \eightline \usmeval\newpart\xsuper ==>\ *)
(* usmevalr[_0]\usmvalc r \usmeval[_0][\ldotsne] ==>\ *)
(* usmevalr[_n][\ldotsnv] \sendToDispatchesm \newpart\ *)
(* superclass \xusermethod(_, <\ldotsnx>, <\ldotsky>, *)
(* e_m, s) \newframe\notin\usedframes \twoline l_1, ..., *)
(* l_n \notindom sigma_n \qquad\ldotskl' \notindom sigma *)
(* _n l_1, ..., l_n, \ldotskl' all distinct \threeline *)
(* rho_i = instanceVars(\usmvalc r) rho_a = {x_1|->l_1, *)
(* ..., x_n|->l_n } rho_l = {y_1 |->l'_1, ..., y_k |-> *)
(* l'_k } \sigma = sigma_n {l_1 |->v_1, ...l_n |->v_n, *)
(* l'_1 |->\usmnil, ...l'_k |->\usmnil} \se_mrho_i \ *)
(* envplusrho_a \envplusrho_l s\newframexi\sigma\ *)
(* usedframes\cup{\newframe} ==>\usmevalr[']v \usmeval\ *)
(* xsend(m, \newpart\xsuper, e_1, ..., e_n) ==>\usmevalr *)
(* [']v                                         *)
(*                                              *)
(* Primitives                                   *)
(*                                              *)
(* When a \xprimitive expression is evaluated,  *)
(* it evaluates its arguments, then passes them to the *)
(* primitive named by p. \ops Primitive \twoline \ *)
(* usmeval[\ldotsne] ==>\usmevalr['][\ldotsnv] \ *)
(* usmevalprim[']p[\ldotsnv] \primyields\usmevalr['']v \ *)
(* usmeval\xprimitive(p, e_1, ..., e_n) ==>\usmevalr[''] *)
(* v                                            *)
(*                                              *)
(* Each primitive p is described by its own rule. The *)
(* most interesting one is the [[value]] primitive, *)
(* which evaluates a block. Its rule resembles the rule *)
(* for sending a message, except unlike a method, *)
(* a block has no local variables or instance variables *)
(* of its own. And the body of a block is evaluated *)
(* using its stored [[return]] frame \aframe_c, not the *)
(* frame of the calling context. \ops ValuePrimitive \ *)
(* threeline l_1, ..., l_n \notindom sigma\qquad l_1, *)
(* ..., l_n all distinct \sigma = sigma{l_1 |->v_1, *)
(* ...l_n |->v_n } \s\xbegin(\es)rho_c \envplus{x_1|-> *)
(* l_1, ..., x_n|->l_n } s_c\aframe_cxi\sigma\usedframes *)
(* ==>\usmevalr[']v \twoline \usmevalprimvalue [\usmvalc *)
(* \xclo<x_1, ..., x_n>\ess_c rho_c \aframe_c, \ldotsnv] *)
(* \primyields\mskip20mu \usmevalr[']v          *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* {mathpar} \jform\usmeval[\ldotsne] \returns\ *)
(* usmevalret[']v\aframe'                       *)
(*                                              *)
(* \inferrule \usmevale \returns\usmevalret[']v\aframe' *)
(* \usmeval\sconse es \returns\usmevalret[']v\aframe' *)
(*                                              *)
(* \inferrule \usmevale ==>\usmevalr[']v        *)
(*                                              *)
(* \usmevales \returns\usmevalret['']v\aframe' \usmeval\ *)
(* sconse es \returns\usmevalret['']v\aframe'   *)
(* \jform\usmevale \returns\usmevalret[']v\aframe' *)
(*                                              *)
(* \inferrule \usmevale ==>\usmevalr[']v \usmeval\ *)
(* xreturn(e) \returns\usmevalret[']v\aframe    *)
(*                                              *)
(* \inferrule \usmevale \returns\usmevalret[']v\aframe' *)
(* \usmeval\xset(x, e) \returns\usmevalret[']v\aframe' *)
(*                                              *)
(* \inferrule \usmeval[\ldotsne] \returns\usmevalret[']v *)
(* \aframe' \usmeval\xbegin(\ldotsne) \returns\ *)
(* usmevalret[']v\aframe'                       *)
(*                                              *)
(* \inferrule \usmevale \returns\usmevalret[']v\aframe' *)
(* \usmeval\xsend(m, e, e_1, ..., e_n) \returns\ *)
(* usmevalret[']v\aframe'                       *)
(*                                              *)
(* \inferrule \usmevale ==>\usmevalr[_0]\usmvalc r *)
(*                                              *)
(* \usmeval[_0][\ldotsne] \returns\usmevalret[']v\aframe *)
(* ' \usmeval\xsend(m, e, e_1, ..., e_n) \returns\ *)
(* usmevalret[']v\aframe'                       *)
(*                                              *)
(* \colorblack                                  *)
(*                                              *)
(* {mathpar} Rules for propagation of returns (other \ *)
(* xreturn rules appear in the text) [*]        *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Semantics of returns                         *)
(*                                              *)
(* [*] Most of the rules for [[return]] describe *)
(* variations on one situation: during the evaluation of *)
(* an expression e, one of e's subexpressions returns, *)
(* and this behavior causes e also to return (\vrefcomma *)
(* small.fig.returns). But eventually the [[return]] *)
(* terminates an activation frame of the method in which *)
(* it appears. When a return to frame \newframe reaches *)
(* a method body executing in frame \newframe, the *)
(* result of the return becomes the result of the \xsend *)
(* that activated the method. As with messages to *)
(* [[super]], the \textcolorgraygray parts of the rule *)
(* are the same as in \rulenameSend, and the black parts *)
(* are different. \opstemplate ReturnTo \eightline \ *)
(* usmevale ==>\usmevalr[_0]\usmvalc r \usmeval[_0][\ *)
(* ldotsne] ==>\usmevalr[_n][\ldotsnv] \sendToDispatches *)
(* m c \xusermethod(_, <\ldotsnx>, <\ldotsky>, e_m, s) \ *)
(* newframe\notin\usedframes \twoline l_1, ..., l_n \ *)
(* notindom sigma_n \qquad\ldotskl' \notindom sigma_n *)
(* l_1, ..., l_n, \ldotskl' all distinct \threeline rho *)
(* _i = instanceVars(\usmvalc r) rho_a = {x_1|->l_1, *)
(* ..., x_n|->l_n } rho_l = {y_1 |->l'_1, ..., y_k |-> *)
(* l'_k } \sigma = sigma_n {l_1 |->v_1, ...l_n |->v_n, *)
(* l'_1 |->\usmnil, ...l'_k |->\usmnil} \se_mrho_i \ *)
(* envplusrho_a \envplusrho_l s\newframexi\sigma\ *)
(* usedframes\cup{\newframe} \newpart\returns \newpart\ *)
(* usmevalret[']v\newframe \usmeval\xsend(m, e, e_1, *)
(* ..., e_n) ==>\newpart\usmevalr[']v           *)
(*                                              *)
(* If method body e_m tries to return somewhere else, *)
(* to \aframe', the whole \xsend operation returns to \ *)
(* aframe'. \opstemplate ReturnPast \eightline \usmevale *)
(* ==>\usmevalr[_0]\usmvalc r \usmeval[_0][\ldotsne] ==> *)
(* \usmevalr[_n][\ldotsnv] \sendToDispatchesm c \ *)
(* xusermethod(_, <\ldotsnx>, <\ldotsky>, e_m, s) \ *)
(* newframe\notin\usedframes \twoline l_1, ..., l_n \ *)
(* notindom sigma_n \qquad\ldotskl' \notindom sigma_n *)
(* l_1, ..., l_n, \ldotskl' all distinct \threeline rho *)
(* _i = instanceVars(\usmvalc r) rho_a = {x_1|->l_1, *)
(* ..., x_n|->l_n } rho_l = {y_1 |->l'_1, ..., y_k |-> *)
(* l'_k } \sigma = sigma_n {l_1 |->v_1, ...l_n |->v_n, *)
(* l'_1 |->\usmnil, ...l'_k |->\usmnil} \se_mrho_i \ *)
(* envplusrho_a \envplusrho_l s\newframexi\sigma\ *)
(* usedframes\cup {\newframe} \newpart\returns \newpart\ *)
(* usmevalret[']v\aframe' \qquad \newpart\aframe' !=\ *)
(* newframe \usmeval\xsend(m, e, e_1, ..., e_n) \newpart *)
(* \returns \newpart\usmevalret[']v\aframe'     *)
(*                                              *)
(* \zvspace1                                    *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* height 0pt depth 0pt                         *)
(*                                              *)
(* Semantics of definitions                     *)
(*                                              *)
(* A definition d is evaluated in the context of the *)
(* top-level, persistent state of a uSmalltalk machine, *)
(* which has only three of the seven components listed *)
(* in \crefsmall.tab.metavars: a global environment xi, *)
(* a store sigma, and a set of used stack frames \ *)
(* usedframes. Evaluating d may change all three; the *)
(* judgment form is \topevald -->\topevalr'. \jlabel *)
(* usmalltalk.eval.def\usmed                    *)
(*                                              *)
(* Global variables                             *)
(*                                              *)
(* As in micro-Scheme, a \xval binding for an existing *)
(* variable x assigns the value of a right-hand side e *)
(* to x's location. Expression e is evaluated using the *)
(* judgment form \usmevale ==>\usmevalr[']v, so the \ *)
(* xval rule has to gin up an environment rho, a class \ *)
(* superclass that will receive messages sent to  *)
(* [[super]], and a new stack frame \aframe. Since e is *)
(* evaluated outside any method, there are no instance *)
(* variables and no formal parameters, and rho is empty. *)
(* To receive messages sent to [[super]], the \xval rule *)
(* uses the root class [[Object]]—the original *)
(* definition of [[Object]] taken from the initial *)
(* global environment xi_0, not whatever definition of *)
(* [[Object]] happens to be current. (In practice, the *)
(* identity of \superclass is irrelevant. If a message *)
(* is sent to [[super]], the abstract machine tries to *)
(* look up [[self]] in the empty environment, and it *)
(* gets stuck.) The new frame \aframe is \newframe, *)
(* which may be any frame not previously allocated, that *)
(* is, any frame not in \usedframes.            *)
(*                                              *)
(* \ops DefineOldGlobal \twoline xin dom xi\qquadxi(x) = *)
(* l\qquad\newframe\notin\usedframes \se\emptyenvxi_0 *)
(* (Object)\newframexisigma{\newframe} \cup\usedframes = *)
(* =>\usmevalr[']v \topeval\xval(x, e) --><xi, sigma'{l| *)
(* ->v}, \usedframes'>                          *)
(*                                              *)
(* \ops DefineNewGlobal \twoline x \notindom xi\qquadl\ *)
(* notindom sigma\qquad\newframe\notin\usedframes \se\ *)
(* emptyenvxi_0(Object)\newframexisigma{\newframe} \cup\ *)
(* usedframes ==>\usmevalr[']v \topeval\xval(x, e) -->< *)
(* xi{x |->l}, sigma'{l|->v}, \usedframes'>     *)
(*                                              *)
(* Top-level expressions                        *)
(*                                              *)
(* A top-level expression is syntactic sugar for a *)
(* binding to [[it]].                           *)
(*                                              *)
(* \ops EvalExp \topeval\xval(it, e) -->\usmdefresultxi' *)
(* sigma'\usedframes' \topeval\xexp(e) -->\usmdefresult *)
(* xi'sigma'\usedframes'                        *)
(*                                              *)
(* Block definition                             *)
(*                                              *)
(* \xdefine is syntactic sugar for creating a block. \ *)
(* ops DefineBlock \topeval\xval(f, \xblock(<x_1, ..., *)
(* x_n>, e)) -->\topevalr' \topeval\define(f, <x_1, ..., *)
(* x_n>, e) -->\topevalr'                       *)
(*                                              *)
(* Class definition                             *)
(*                                              *)
(* The evaluation of a class definition is rather *)
(* involved; the interpreter creates an object that *)
(* represents the class. The details are hidden in the *)
(* function [[newClassObject]], which I don't specify *)
(* formally. To see how it works, consult the code in *)
(* chunk [->].                                  *)
(*                                              *)
(* \ops DefineOldClass \twoline xin dom xi\qquadxi(x) = *)
(* l v = newClassObject(d, xi, sigma) \topeval\xclassd *)
(* (d) -->\usmdefresultxisigma{l|->v}\usedframes *)
(*                                              *)
(* \ops DefineNewClass \twoline x \notindom xi\qquadl\ *)
(* notindom sigma v = newClassObject(d, xi, sigma) \ *)
(* topeval\xclassd(d) -->\usmdefresultxi{x |->l}sigma{l| *)
(* ->v}\usedframes                              *)
(*                                              *)
(* The interpreter                              *)
(*                                              *)
(* The key parts of uSmalltalk's interpreter involve the *)
(* elements that make Smalltalk unique: objects and *)
(* classes. An object is represented in two parts: *)
(* a class and an internal representation (ML types *)
(* [[class]] and [[rep]]). The class determines the *)
(* object's response to messages, and the internal *)
(* representation holds the object's state. And although *)
(* the Smalltalk word is ``object,'' the ML type of its *)
(* representation is called [[value]], just like *)
(* whatever thing an expression evaluates to in every *)
(* other interpreter in this book.              *)
(* <definitions of [[value]] and [[method]] for \usmalltalk>= *)
withtype value = class * rep
(* Every class is also an object, and as an object, it *)
(* is an instance of another class—its metaclass, which *)
(* is stored in field [[class]]. This field initially *)
(* holds [[PENDING]], but when the metaclass becomes *)
(* available, [[class]] is updated to hold it.  *)
(*                                              *)
(* A method has a name, formal parameters, local *)
(* variables, and a body. A method also stores the *)
(* superclass of the class in which it is defined, which *)
(* it uses to interpret messages sent to [[super]]. *)
(* <definitions of [[value]] and [[method]] for \usmalltalk>= *)
and method = { name : name, formals : name list, locals : name list
             , body : exp, superclass : class
             }
(* <definition of [[def]] for \usmalltalk>=     *)
datatype def = VAL    of name * exp
             | EXP    of exp
             | DEFINE of name * name list * exp
             | CLASSD of { name    : string
                         , super   : string
                         , ivars   : string list
                         , methods : method_def list
                         }
and method_flavor = IMETHOD          (* instance method *)
                  | CMETHOD          (* class method    *)
  withtype method_def = { flavor : method_flavor, name : name
                        , formals : name list, locals : name list, body : exp 
                        }
(* Unit tests are those of micro-Scheme, plus the *)
(* [[check-print]] form, which is unique to uSmalltalk. *)
(* <definition of [[unit_test]] for \usmalltalk>= *)
(* Common syntactic \chaptocsplitforms          *)
(*                                              *)
(* [*] Syntactic forms for unit tests and for extended *)
(* definitions are often shared.                *)
(*                                              *)
(* The following forms of unit test are used by both of *)
(* the major untyped languages in this book:    *)
(* micro-Scheme (\crefmlscheme.chap) and uSmalltalk (\ *)
(* crefsmall.chap). [*]                         *)
(* <definition of [[unit_test]] for untyped languages (shared)>= *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ASSERT of exp
                   | CHECK_ERROR  of exp
             | CHECK_PRINT of exp * string
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)

fun className (CLASS {name, ...}) = name

(* <definition of [[valueString]] for \usmalltalk>= *)
fun valueString (c, NUM n) = intString n ^ valueString(c, USER [])
  | valueString (_, SYM v) = v
  | valueString (c, _) = "<" ^ className c ^ ">"
(* <definition of [[expString]] for \usmalltalk>= *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun symString x = x
      fun valueString (_, NUM n) = intString n
        | valueString (_, SYM x) = "'" ^ symString x
        | valueString (c, _) = "<" ^ className c ^ ">"
  in  case e
        of LITERAL (NUM n) => intString n
         | LITERAL (SYM n) => "'" ^ symString n
         | LITERAL _ => "<wildly unexpected literal>"
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | RETURN e   => bracketSpace ["return", expString e]
         | SEND (_, e, msg, es) => bracketSpace (expString e :: msg :: exps es)
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | PRIMITIVE (p, es) => bracketSpace ("primitive" :: p :: exps es)
         | BLOCK ([], es) => "[" ^ spaceSep (exps es) ^ "]"
         | BLOCK (xs, es) =>
             bracketSpace ["block", bracketSpace xs, spaceSep (exps es)]
         | METHOD (xs, [], es) =>
             bracketSpace ["compiled-method", bracketSpace xs,
                           spaceSep (exps es)]
         | METHOD (xs, ys, es) =>
             bracketSpace ["compiled-method", bracketSpace xs,
                           bracketSpace ("locals" :: ys), spaceSep (exps es)]
         | VALUE v => valueString v
         | SUPER => "super"
  end
(*OMIT*)

(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR LOGGING (FOR COVERAGE ANALYSIS)                 *)
(*                                                               *)
(*****************************************************************)

(* <support for logging (for coverage analysis)>= *)
val logging = hasOption "log"
fun q s = "\"" ^ s ^ "\""
val _ = if logging then println "val ops = ...\n" else ()

fun logSend srcloc msgname =
  app print [ "\nops.SEND { loc = ", q (srclocString srcloc)
            , ", selector = ", q msgname, " }\n" ]
fun logFind name candidate =
  app print ["\nops.findMethod { selector = ", q name
              , ", on = ", q (className candidate), "}\n"]

fun logClass name (ms : method list) =
  let fun subclassExp (SEND (_, _, "subclassResponsibility", _)) = true
        | subclassExp (BEGIN [e]) = subclassExp e
        | subclassExp _ = false
      val subclassM = subclassExp o #body
      val methodNames = commaSep o map (q o #name)
  in  app print [ "\nops.class { name = ", q name, ", methods = { " ,
                                                                  methodNames ms
                , " }, subclass_responsibilities = { "
                , methodNames (List.filter subclassM ms), " } }\n"
                ]
  end

fun logGetMethod class m =
  app print ["\nops.getMethod { class = ", q class, ", method = ", q m, " }\n"]

fun logSetMethod class m =
  app print ["\nops.setMethod { class = ", q class, ", method = ", q m, " }\n"]


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \USMALLTALK\ CLASSES, METHODS, AND VALUES *)
(*                                                               *)
(*****************************************************************)

(* <utility functions on \usmalltalk\ classes, methods, and values>= *)
fun instanceVars (_, USER rep) = rep
  | instanceVars self = bind ("self", ref self, emptyEnv)
(* A primitive representation is typically created by *)
(* evaluating some particular syntactic form in the *)
(* source code: an array literal, a numeric literal, *)
(* a literal symbol, a block, a class definition, or a *)
(* [[compiled-method]] form. Several representations *)
(* (arrays, numbers, classes) can also be created by *)
(* primitives.                                  *)
(*                                              *)
(* No matter what its internal representation, every *)
(* object provides instance variables to its methods. A  *)
(* [[USER]] object provides all the instance variables *)
(* dictated by its class's definition, a set that always *)
(* includes [[self]]. An object with any other  *)
(* representation provides only [[self]].       *)
(* <boxed values 1>=                            *)
val _ = op instanceVars : value -> value ref env
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Utilities for error messages                 *)
(*                                              *)
(* If a block is sent a message with the wrong number of *)
(* arguments, an error message is issued showing what *)
(* message was expected. The message name (in Smalltalk *)
(* lingo, ``selector'') is computed by function *)
(* [[valueSelector]].                           *)
(* <utility functions on \usmalltalk\ classes, methods, and values>= *)
fun valueSelector [] = "value"
  | valueSelector args = concat (map (fn _ => "value:") args)
(* <utility functions on \usmalltalk\ classes, methods, and values>= *)
fun className (CLASS {name,  ...}) = name
fun classId   (CLASS {class, ...}) = class
(* Utilities for manipulating classes           *)
(*                                              *)
(* Because a class can point to its superclass, the type *)
(* [[class]] has to be a recursive type implemented as *)
(* an ML [[datatype]]. So a value of type [[class]] is *)
(* the value constructor [[CLASS]] applied to an *)
(* ML record with a bunch of named fields. Pattern *)
(* matching on such things is unpleasant, and when all I *)
(* want is a class's name or its unique identifier, the *)
(* notation is heavier than I would like. So I define *)
(* two convenience functions. The ``[[...]]'' notation *)
(* in each pattern match tells the Standard ML compiler *)
(* that not all fields of the record in curly braces are *)
(* mentioned, and the ones not mentioned should be *)
(* ignored.                                     *)
(* <boxed values 83>=                           *)
val _ = op className : class -> name
val _ = op classId   : class -> metaclass ref
(* <utility functions on \usmalltalk\ classes, methods, and values>= *)
fun methodName ({ name, ... } : method) = name
fun methodsEnv ms = foldl (fn (m, rho) => bind (methodName m, m, rho)) emptyEnv
                                                                              ms
(* A method is also represented by a big record, and *)
(* similar considerations apply. I extract a method's *)
(* name using another convenience function,     *)
(* [[methodName]]. I also define [[methodsEnv]], which *)
(* builds an environment suitable for use in a class. *)
(* <boxed values 84>=                           *)
val _ = op methodName : method -> name
val _ = op methodsEnv : method list -> method env
(* <utility functions on \usmalltalk\ classes, methods, and values>= *)
fun mkClass name meta super ivars ms =
  (
(* <if any name in [[ivars]] repeats a name declared in a superclass, raise [[RuntimeError]]>= *)
    let fun checkDuplicateIvar (SOME (CLASS { name = c', ivars, super, ... })) x
                                                                               =
            if member x ivars then
              raise RuntimeError ("Instance variable " ^ x ^ " of class " ^ name
                                                                               ^
                                  " duplicates a variable of superclass " ^ c')
            else
              checkDuplicateIvar super x
        | checkDuplicateIvar NONE x = ()
    in  app (checkDuplicateIvar (SOME super)) ivars
    end
  ; if logging then logClass name ms else () (*OMIT*)
  ; CLASS { name = name, super = SOME super, ivars = ivars
          , methods = ref (methodsEnv ms), class = ref meta }
  )
(* Utilities for making new classes             *)
(*                                              *)
(* In general, a new class is made by [[mkClass]], which *)
(* checks to be sure that no instance variable is *)
(* repeated. Each class is identified by its [[class]] *)
(* field, which points to a unique mutable location. \ *)
(* nwnarrowboxes                                *)
(* <boxed values 86>=                           *)
val _ = op mkClass : name -> metaclass -> class -> name list -> method list ->
                                                                           class



(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR BOOTSTRAPPING CLASSES/VALUES USED DURING PARSING *)
(*                                                               *)
(*****************************************************************)

(* <support for bootstrapping classes/values used during parsing>= *)
local 
  val intClass    = ref NONE : class option ref
  val symbolClass = ref NONE : class option ref
  val arrayClass  = ref NONE : class option ref
  fun badlit what = 
    raise InternalError
      ("(bootstrapping) -- can't " ^ what ^ " in predefined classes")
in
  fun mkInteger n = (valOf (!intClass), NUM n)
    handle Option => badlit "evaluate integer literal or use array literal"
  
  fun mkSymbol s = (valOf (!symbolClass), SYM s)
    handle Option => badlit "evaluate symbol literal or use array literal"
  
  fun mkArray a = (valOf (!arrayClass), ARRAY (Array.fromList a))
    handle Option => badlit "use array literal"
(* Bootstrapping for literal and Boolean values *)
(*                                              *)
(* [*] [*] [*]                                  *)
(*                                              *)
(* In most languages, literal integers, Booleans, and *)
(* [[nil]] would be simple atomic values. But in *)
(* Smalltalk, they are objects, every object has a *)
(* class, and relations among objects and classes *)
(* include circular dependencies:               *)
(*                                              *)
(*  \tightlist*                                 *)
(*  1. When the evaluator sees an integer literal, it *)
(*  must create an integer value.               *)
(*  2. That value must be an instance of class  *)
(*  [[SmallInteger]]. [In full Smalltalk-80, an *)
(*  integer literal could also be an instance of a *)
(*  large-integer class.]                       *)
(*  3. Class [[SmallInteger]] is defined by uSmalltalk *)
(*  code.                                       *)
(*  4. That code must be interpreted by the evaluator. *)
(*                                              *)
(* Another circular dependency involves Boolean values *)
(* and their classes:                           *)
(*                                              *)
(*  \tightlist*                                 *)
(*  1. Value [[true]] must be an instance of class *)
(*  [[Boolean]].                                *)
(*  2. Class [[Boolean]] must be a subclass of class *)
(*  [[Object]].                                 *)
(*  3. Class [[Object]] has method [[notNil]].  *)
(*  4. Method [[notNil]] must return [[true]] on class *)
(*  [[Object]].                                 *)
(*                                              *)
(* Each of these things depends on all the others, *)
(* creating a cycle that must be broken. Value [[false]] *)
(* and method [[isNil]] participate in a similar cycle. *)
(*                                              *)
(* Each cycle is broken with a well-placed ref cell. *)
(* First, the ref cell is initialized with an unusable *)
(* value; then the interpreter is bootstrapped by *)
(* feeding it the definitions of the predefined classes; *)
(* and finally the ref cell is assigned its proper *)
(* value, closing the cycle. The ref cells and the *)
(* functions that update them are defined in the chunk *)
(* [[<<support for bootstrapping classes/values used *)
(* during parsing>>]]. The ref cells are used by *)
(* primitives, by built-in objects, and by the parser. *)
(*                                              *)
(* [*]                                          *)
(*                                              *)
(* Cycles of literals                           *)
(*                                              *)
(* When an integer literal is evaluated, the evaluator *)
(* must create an object of class [[SmallInteger]], *)
(* which means that the evaluator requires a    *)
(* representation of [[SmallInteger]]. But that *)
(* representation is created by evaluating a class *)
(* definition (\chunkrefsmall.integer-primops.use), *)
(* which requires the evaluator!                *)
(*                                              *)
(* \qtrim-2                                     *)
(*                                              *)
(* I break the cycle by reserving a mutable reference *)
(* cell, [[intClass]], to hold the representation of *)
(* class [[SmallInteger]]. The cell is initially empty, *)
(* but after the definition of [[SmallInteger]] is read, *)
(* it is updated. The cell is used every time an integer *)
(* literal is read: the evaluator calls function *)
(* [[mkInteger]], which fetches the [[SmallInteger]] *)
(* class out of [[intClass]]. Provided [[SmallInteger]] *)
(* has been read, [[intClass]] is not empty, and *)
(* [[mkInteger]] creates and returns a new instance of *)
(* the class. Otherwise, it crashes the interpreter by *)
(* raising the [[InternalError]] exception. Similar *)
(* functions and reference cells are used to create *)
(* objects from symbol literals and array literals. *)
(*                                              *)
(* Because reference cells are hard to reason about, *)
(* I define them inside ML's [[local]] form. This form *)
(* limits the scope of code that the reference cells can *)
(* affect; the reference cells are accessible only to *)
(* functions defined between [[local]] and [[end]]. *)
(* <boxed values 11>=                           *)
val _ = op mkInteger : int        -> value
val _ = op mkSymbol  : string     -> value
val _ = op mkArray   : value list -> value
(* [*]                                          *)

(* Function [[valOf]] and exception [[Option]] are part *)
(* of the initial basis of Standard ML.         *)
(*                                              *)
(* \penalty-800                                 *)
(*                                              *)
(* Once the predefined class definitions have been read, *)
(* the reference cells are updated by function  *)
(* [[saveLiteralClasses]], which takes one parameter, *)
(* the global environment [[xi]].               *)

(* <support for bootstrapping classes/values used during parsing>= *)
  fun saveLiteralClasses xi =
    ( intClass    := SOME (findClass ("SmallInteger", xi))
    ; symbolClass := SOME (findClass ("Symbol",       xi))
    ; arrayClass  := SOME (findClass ("Array",        xi))
    )
  and findClass (name, xi) =
        case !(find (name, xi))
          of (_, CLASSREP c) => c
           | _ => raise InternalError ("class " ^ name ^ " isn't defined")
end
(* <boxed values 12>=                           *)
val _ = op findClass : string * value ref env -> class
(* [*]                                          *)

(* <support for bootstrapping classes/values used during parsing>= *)
local
  val trueValue  = ref NONE : value option ref
  val falseValue = ref NONE : value option ref
in
  fun mkBoolean b = valOf (!(if b then trueValue else falseValue))
    handle Option => raise InternalError "uninitialized Booleans"
  fun saveTrueAndFalse xi =
    ( trueValue  := SOME (!(find ("true",  xi)))
    ; falseValue := SOME (!(find ("false", xi)))
    )
end
(* \zbreak                                      *)
(*                                              *)
(* Booleans                                     *)
(*                                              *)
(* Booleans also participate in a cycle, but it's the *)
(* Boolean objects, not the Boolean classes, that must *)
(* be bootstrapped. Each Boolean object is stored in its *)
(* own mutable reference cell.                  *)
(* <boxed values 14>=                           *)
val _ = op mkBoolean : bool -> value
(* <support for bootstrapping classes/values used during parsing>= *)
local
  val blockClass = ref NONE : class option ref
in
  fun mkBlock c = (valOf (!blockClass), CLOSURE c)
    handle Option => 
        raise InternalError 
            "Bad blockClass; evaluated block expression in predefined classes?"
  fun saveBlockClass xi =
    blockClass := SOME (findClass ("Block", xi))
end
(* \qbreak                                      *)
(*                                              *)
(* Bootstrapping blocks                         *)
(*                                              *)
(* The [[Block]] class doesn't actually have to be *)
(* bootstrapped, but by defining [[Block]] and  *)
(* [[Boolean]] together (and bootstrapping them both), *)
(* I clarify their relationship. That clarity is *)
(* especially important to the implementations of the *)
(* [[whileTrue:]] and [[whileFalse:]] methods. \ *)
(* nwnarrowboxes                                *)
(* <boxed values 65>=                           *)
val _ = op mkBlock : name list * exp list * value ref env * class * frame ->
                                                                           value
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <support for bootstrapping classes/values used during parsing>= *)
local
  val compiledMethodClass = ref NONE : class option ref
in
  fun mkCompiledMethod m = (valOf (!compiledMethodClass), METHODV m)
    handle Option => 
      raise InternalError "Bad compiledMethodClass"
  fun saveCompiledMethodClass xi =
    compiledMethodClass := SOME (findClass ("CompiledMethod", xi))
end



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \USMALLTALK, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* Lexical analysis \chaptocsplitand parsing    *)
(*                                              *)
(* Lexical analysis and parsing are organized as *)
(* follows:                                     *)
(* <lexical analysis and parsing for \usmalltalk, providing [[filexdefs]] and [[stringsxdefs]]>= *)
(* Lexical analysis                             *)
(*                                              *)
(* micro-Scheme's lexer can't be reused for uSmalltalk, *)
(* for two reasons: uSmalltalk treats curly braces as *)
(* syntactic sugar for parameterless blocks, and *)
(* uSmalltalk keeps track of source-code locations. *)
(* Aside from these details, the lexers are the same. *)
(*                                              *)
(* The representation of a token is almost the same as *)
(* in micro-Scheme. But uSmalltalk accepts integer *)
(* tokens that are too large to fit in a machine  *)
(* [[int]], and uSmalltalk doesn't have the [[SHARP] *)
(* token, because in uSmalltalk, a [[#]] character does *)
(* not introduce a Boolean.                     *)
(* <lexical analysis for \usmalltalk>=          *)
datatype pretoken = INTCHARS of char list
                  | NAME     of name
                  | QUOTE    of string option (* symbol or array *)
type token = pretoken plus_brackets
(* In error messages, a token may be converted back to a *)
(* string.                                      *)
(* <lexical analysis for \usmalltalk>=          *)
fun pretokenString (INTCHARS ds)    = implode ds
  | pretokenString (NAME    x)      = x
  | pretokenString (QUOTE NONE)     = "'"
  | pretokenString (QUOTE (SOME s)) = "'" ^ s
(* <lexical analysis for \usmalltalk>=          *)
local
  val nondelims = many1 (sat (not o isDelim) one)

  fun validate NONE = NONE (* end of line *)
    | validate (SOME (#";", cs)) = NONE (* comment *)
    | validate (SOME (c, cs)) = 
        let val msg = "invalid initial character in `" ^
                      implode (c::listOfStream cs) ^ "'"
        in  SOME (ERROR msg, EOS) : (pretoken error * char stream) option
        end
in
  val smalltalkToken =
    whitespace *> bracketLexer (
            (QUOTE o SOME o implode) <$> (eqx #"'" one *> nondelims)
        <|> QUOTE NONE               <$  eqx #"'" one
        <|> INTCHARS                 <$> intChars isDelim   
        <|> (NAME o implode)         <$> nondelims                          
        <|> (validate o streamGet)
        )
(* Common syntactic \chaptocsplitforms          *)
(*                                              *)
(* [*] Syntactic forms for unit tests and for extended *)
(* definitions are often shared.                *)
(*                                              *)
(* The following forms of unit test are used by both of *)
(* the major untyped languages in this book:    *)
(* micro-Scheme (\crefmlscheme.chap) and uSmalltalk (\ *)
(* crefsmall.chap). [*]                         *)
(* <boxed values 66>=                           *)
val _ = op smalltalkToken : token lexer
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

end
(* <parsers for single \usmalltalk\ tokens>=    *)
type 'a parser = (token, 'a) polyparser
val token : token parser = token (* make it monomorphic *)
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token
val namelike  = (fn (NAME s)         => SOME s  | _ => NONE) <$>? pretoken
val intchars  = (fn (INTCHARS ds)=> SOME ds | _ => NONE) <$>? pretoken
val sym   = (fn (QUOTE (SOME s)) => SOME s  | _ => NONE) <$>? pretoken
val quote = (fn (QUOTE NONE    ) => SOME () | _ => NONE) <$>? pretoken

val namelike = asAscii namelike

val int = intFromChars <$>! intchars
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
(* <boxed values 43>=                           *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
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
(* <boxed values 43>=                           *)
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* Record fields must also have mutually distinct names. *)
(* <boxed values 45>=                           *)
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
(* <boxed values 47>=                           *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* Arity and colon checking for message names   *)
(*                                              *)
(* Smalltalk has simple rules for computing the arity of *)
(* a message based on the message's name: if the name is *)
(* symbolic, the message is binary (one receiver, one *)
(* argument); if the name is alphanumeric, the number of *)
(* arguments is the number of colons. Unfortunately, in *)
(* uSmalltalk a name can mix alphanumerics and symbols. *)
(* Whether a message's name is considered symbolic or *)
(* alphanumeric is determined by the name's first *)
(* character.                                   *)
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
fun arity name =
      let val cs = explode name
          fun isColon c = (c = #":")
      in  if Char.isAlpha (hd cs) then
            length (List.filter isColon cs)
          else
            1
      end
(* Each message send is checked to see if the number of *)
(* arguments matches the arity of the message name. *)
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
fun arityOk name args = arity name = length args

fun arityErrorAt loc what msgname args =
  let fun argn n = if n = 1 then "1 argument" else intString n ^ " arguments"
  in  synerrorAt ("in " ^ what ^ ", message " ^ msgname ^ " expects " ^
                         argn (arity msgname) ^ ", but gets " ^
                         argn (length args)) loc
  end
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
fun colonsOK name =
  Char.isAlpha (String.sub (name, 0)) orelse not (Char.contains name #":")
  handle Subscript => false

val badColon = not o colonsOK

fun badColonErrorAt msgname loc =
  synerrorAt ("a symbolic method name like " ^ msgname ^
              " may not contain a colon")
             loc
(* Parsers for expressions                      *)
(*                                              *)
(* uSmalltalk reserves a whole bunch of words, but not *)
(* [[class]]. The word [[class]] can't be reserved *)
(* because it's also the name of the message that is *)
(* sent to any object to determine its class.   *)
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
val reserved = [ "set", "begin", "primitive", "return", "block", "quote"
               , "compiled-method" , "subclass-of", "ivars", "method"
               , "class-method", "locals", "val", "define", "use"
               , "check-error", "check-expect", "check-assert"
               ]
val name = rejectReserved reserved <$>! namelike
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
fun isImmutable x =
  List.exists (fn x' => x' = x) ["true", "false", "nil", "self", "super"] 
val immutable = sat isImmutable name

val mutable =
  let fun can'tMutate (loc, x) =
        ERROR (srclocString loc ^
               ": you cannot set or val-bind pseudovariable " ^ x)
  in  can'tMutate <$>! @@ immutable <|> OK <$>! name
  end
(* Parsing                                      *)
(*                                              *)
(* [*]                                          *)
(*                                              *)
(* Parsers for tokens                           *)
(*                                              *)
(* The parser begins with parsers for individual tokens. *)
(* <boxed values 67>=                           *)
val _ = op name : string parser
val _ = op int  : int    parser
(* \qbreak In Smalltalk, the predefined         *)
(* ``pseudovariables'' [[true]], [[false]], [[nil]], *)
(* [[self]], and [[super]] can't be mutated. Any attempt *)
(* to assign to one of these pseudovariables is detected *)
(* in the parser, so the fault can be identified without *)
(* having to run the code. I define parsers for both *)
(* [[mutable]] and [[immutable]] names.         *)
(* <boxed values 67>=                           *)
val _ = op mutable   : name parser
val _ = op immutable : name parser
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
val atomicExp
  =  LITERAL <$> NUM    <$> int
 <|> LITERAL <$> SYM    <$> (sym <|> (quote *> name)
                                 <|> (quote *> (intString <$> int)))
 <|> SUPER              <$  eqx "super" name
 <|> VAR                <$> name
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
fun quotelit tokens = (
         mkSymbol  <$> name
    <|>  mkInteger <$> int
    <|>  shaped ROUND  left <&> mkArray <$> bracket("(literal ...)", many
                                                                       quotelit)
    <|>  shaped SQUARE left <&> mkArray <$> bracket("(literal ...)", many
                                                                       quotelit)
    <|>  quote               <!> "' within ' is not legal" 
    <|>  shaped CURLY  left  <!> "{ within ' is not legal"
    <|>  shaped CURLY  right <!> "} within ' is not legal"
    ) tokens
and shaped shape delim = sat (fn (_, s) => s = shape) delim
(* A quoted literal is read by function [[quotelit]], *)
(* which in turn may call [[mkSymbol]], [[mkInteger]], *)
(* or [[mkArray]]. These functions must not be called *)
(* until after the initial basis is read in.    *)
(* <boxed values 68>=                           *)
val _ = op quotelit : value parser
(* \qvfilbreak2in                               *)
(*                                              *)
(* Parsers for bracketed keyword expressions are similar *)
(* to those in other bridge languages.          *)
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
(* Similarly, the body of a method can be a list of *)
(* expressions, but a message send is more common. If *)
(* the list of expressions includes a variable or *)
(* literal that is followed by something, it's almost *)
(* certain that round brackets have been forgotten, *)
(* since otherwise the variable or literal would be *)
(* evaluated only for its (nonexistent) side effect. *)
(* <definition of function [[bodyWarning]]>=    *)
fun bodyWarning (loc, es) =
  let fun nameOrLitFollowed (VAR _     :: _ :: _) = true
        | nameOrLitFollowed (LITERAL _ :: _ :: _) = true
        | nameOrLitFollowed (_ :: es) = nameOrLitFollowed es
        | nameOrLitFollowed [] = false
       val () = if nameOrLitFollowed es then
                  warnAt loc ["it looks like the body of the method ",
                              "should be wrapped in (...)"]
                else
                  ()
  in  es
  end
fun formalsIn context = formalsOf "(x1 x2 ...)" name context
fun sendClass (loc, e) = SEND (loc, e, "class", [])
val locals = usageParsers [("[locals y ...]", many name)] <|> pure []
fun method_body exp kind =
      (curry3 id <$> @@ (formalsIn kind) <*> locals <*> (bodyWarning <$> @@ (
                                                                     many exp)))
fun withoutArity f ((_, xs), ys, es) = f (xs, ys, es)

fun exptable exp = usageParsers
  [ ("(set x e)",             curry SET       <$> mutable <*> exp)
  , ("(begin e ...)",               BEGIN     <$> many exp)
  , ("(primitive p e ...)",   curry PRIMITIVE <$> name <*> many exp)
  , ("(return e)",                  RETURN    <$> exp)
  , ("(block (x ...) e ...)", curry BLOCK     <$> formalsIn "block" <*> many exp
                                                                               )
  , ("(compiled-method (x ...) [locals ...] e ...)",
                       withoutArity METHOD    <$> method_body exp
                                                              "compiled method")
  , ("(class e)",                   sendClass <$> @@ exp)
  , ("(locals x ...)",
     pure () <!> "found '(locals ...)' where an expression was expected")
  , ("(quote v)",                   VALUE <$> quotelit)
  ]
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
(* <definition of function [[curlyWarning]]>=   *)
fun curlyWarning (loc, es) =
  let fun nameFollowed (VAR _ :: _ :: _) = true
        | nameFollowed (_ :: es) = nameFollowed es
        | nameFollowed [] = false
      val () = if nameFollowed es then
                 warnAt loc ["inside {...} it looks like (...) was forgotten"]
               else
                 (* It's a little less definitive, but a block of the *)
                 (* form \monobox`{e name`} also suggests that round *)
                 (* brackets have been forgotten.                *)
                 (* <warn about blocks of the form [[{exp name}]]>= *)
                 case es
                   of [_, VAR method] =>
                      let val method = "`" ^ method ^ "`"
                      in  warnAt loc ["inside {...} it looks like ",
                                      method, " was meant to send a ",
                                      "message. If so, then wrap the message ",
                                      "send in round brackets (...).  ",
                                      "If the code is meant to answer ", method,
                                      ", insert a literal `0` before ", method,
                                                                            "."]
                      end
                    | _=> ()
  in  es
  end
fun exp tokens = (
      atomicExp
  <|> quote       *> (VALUE <$> quotelit)
                                      (* not while reading predefined classes *)
  <|> curlyBracket ("{exp ...}", curry BLOCK [] <$> curlyWarning <$> @@ (many
                                                                           exp))
  <|> exptable exp
  <|> liberalBracket ("(exp selector ...)",
                      messageSend <$> exp <*> @@ name <*>! many exp)
  <|> liberalBracket ("(exp selector ...)", noMsg <$>! @@ exp)
  <|> left *> right <!> "empty message send ()"
  ) 
  tokens
and noReceiver (loc, m) = 
      synerrorAt ("sent message " ^ m ^ " to no object") loc
and noMsg (loc, e) = 
      synerrorAt ("found receiver " ^ expString e ^ " with no message") loc
and messageSend receiver (loc, msgname) args = 
      if badColon msgname then
          badColonErrorAt msgname loc
      else if arityOk msgname args then
          OK (SEND (loc, receiver, msgname, args))
      else
          arityErrorAt loc "message send" msgname args
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
val printable = name <|> implode <$> intchars

val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  , ("(check-print e chars)", curry CHECK_PRINT <$> exp <*> printable)
  ]
(* <boxed values 69>=                           *)
val _ = op exp      : exp parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Function [[curlyWarning]] is defined below.  *)
(*                                              *)
(* \qbreak                                      *)
(*                                              *)
(* Parsers for definitions                      *)
(*                                              *)
(* \qtrim1                                      *)
(*                                              *)
(* Unit tests are recognized by [[testtable]].  *)
(* <boxed values 69>=                           *)
val _ = op testtable : unit_test parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and [[xdef]] streams for \usmalltalk>= *)
val method =
  let fun method kind (nameloc, name) impl =
            check (kname kind, nameloc, name, impl) >>=+
            (fn (formals, locals, body) =>
                { flavor = kind, name = name, formals = formals
                , locals = locals, body = body })
      and kname IMETHOD = "method"
        | kname CMETHOD = "class-method"
      and check (kind, nameloc, name, (formals, locals, body)) = 
            let val (formalsloc, xs) = formals
            in  if badColon name then
                    badColonErrorAt name nameloc
                else if arityOk name xs then
                  OK (xs, locals, BEGIN body)
                else
                  arityErrorAt formalsloc (kind ^ " definition") name xs
            end
      val mbody = method_body exp
  in  usageParsers
      [ ("(method f (args) body)",
                      method IMETHOD <$> @@ name <*>! mbody "method")
      , ("(class-method f (args) body)",
                      method CMETHOD <$> @@ name <*>! mbody "class method")
      ]
  end
(* Definitions of both class methods and instance *)
(* methods are recognized by [[method]].        *)
(* <boxed values 70>=                           *)
val _ = op method : method_def parser
val parseMethods = many method <* many eol <* eos
(* <parsers and [[xdef]] streams for \usmalltalk>= *)
fun classDef name super ivars methods =
      CLASSD { name = name, super = super, ivars = ivars, methods = methods }

val ivars = 
  nodups ("instance variable", "class definition") <$>! @@ (many name)

val subclass_of = usageParsers [("[subclass-of className]", name)]
val ivars = (fn xs => getOpt (xs, [])) <$> 
            optional (usageParsers [("[ivars name...]", ivars)])

val deftable = usageParsers
  [ ("(val x e)", curry  VAL    <$> mutable <*> exp)
  , ("(define f (args) body)",
                  curry3 DEFINE <$> name <*> formalsIn "define" <*> exp)
  , ("(class name [subclass-of ...] [ivars ...] methods)",
                  classDef <$> name <*> subclass_of <*> ivars <*> many method
              <|> (EXP o sendClass) <$> @@ exp)

  ]
(* <boxed values 71>=                           *)
val _ = op deftable : def parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and [[xdef]] streams for \usmalltalk>= *)
val xdeftable = 
  let fun bad what =
        "unexpected `(" ^ what ^ "...'; " ^
        "did a class definition end prematurely?"
  in  usageParsers
      [ ("(use filename)",      USE <$> name)
      , ("(method ...)",        pzero <!> bad "method")
      , ("(class-method ...)",  pzero <!> bad "class-method")
      ]
  end

val xdef =  DEF  <$> deftable
        <|> TEST <$> testtable
        <|> xdeftable
        <|> badRight "unexpected right bracket"
        <|> DEF <$> EXP <$> exp
        <?> "definition"
(* <boxed values 72>=                           *)
val _ = op xdeftable : xdef parser
val _ = op xdef      : xdef parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and [[xdef]] streams for \usmalltalk>= *)
val xdefstream = interactiveParsedStream (smalltalkToken, xdef)
(* <shared definitions of [[filexdefs]] and [[stringsxdefs]]>= *)
fun filexdefs (filename, fd, prompts) =
      xdefstream (filename, filelines fd, prompts)
fun stringsxdefs (name, strings) =
      xdefstream (name, streamOfList strings, noPrompts)
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
(* <boxed values 127>=                          *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \USMALLTALK *)
(*                                                               *)
(*****************************************************************)

(* Evaluation involves more parts than in interpreters *)
(* for other bridge languages—primarily because of the *)
(* way primitives are used in the evaluator.    *)
(* <evaluation, testing, and the read-eval-print loop for \usmalltalk>= *)
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

(* <definition of [[nullsrc]], for use in \ml~code that sends messages>= *)
val nullsrc : srcloc = ("internally generated SEND node", 1)
(* <support for primitives and built-in classes>= *)
(* <utility functions for building primitives in \usmalltalk>= *)
type primitive = value list * value ref env -> value
fun arityError n args =
  raise RuntimeError ("primitive expected " ^ intString n ^
                      " arguments; got " ^ intString (length args))
fun unaryPrim  f = (fn ([a],    _) => f  a     | (vs, _) => arityError 0 vs)
fun binaryPrim f = (fn ([a, b], _) => f (a, b) | (vs, _) => arityError 1 vs)
(* \qbreak                                      *)
(*                                              *)
(* Utilities for creating primitives            *)
(*                                              *)
(* Most primitives are created directly from    *)
(* ML functions. As in the interpreter for micro-Scheme *)
(* (\crefmlscheme.chap), I organize the code into *)
(* stages. The first stage turns unary and binary *)
(* functions into primitives.                   *)
(* <boxed values 57>=                           *)
val _ = op unaryPrim  : (value         -> value) -> primitive
val _ = op binaryPrim : (value * value -> value) -> primitive
(* <metaclass utilities>=                       *)
fun metaclass (CLASS { class = ref meta, ... }) =
  case meta of META c => c
             | PENDING => raise InternalError "pending class"

fun classObject c = (metaclass c, CLASSREP c)
(* Function [[classObject]] uses [[CLASSREP]] to make *)
(* the class a [[rep]], then pairs it with the class of *)
(* which it is an instance, that is, its metaclass. *)
(* Any attempt to refer to an uninitialized metaclass *)
(* results in a checked run-time error.         *)
(* <boxed values 8>=                            *)
val _ = op metaclass   : class -> class
val _ = op classObject : class -> value
(* [*]                                          *)

(* As described in \usmpage                     *)
(* small.classes-and-metaclasses, Smalltalk's classes *)
(* point to their metaclasses, and some metaclasses *)
(* point (indirectly) back to classes. My interpreter *)
(* deals with the circularity by initializing a class's *)
(* metaclass field to [[PENDING]], then updated later. *)
(* The update is performed by function [[setMeta]], *)
(* which insists that no metaclass field be updated more *)
(* than once.                                   *)
(* <metaclass utilities>=                       *)
fun setMeta (CLASS { class = m as ref PENDING, ... }, meta) = m := META meta
  | setMeta (CLASS { class = ref (META _), ... }, _) =
      raise InternalError "double patch"
(* <\ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives>= *)
fun eqRep ((cx, x), (cy, y)) = 
  classId cx = classId cy andalso
  case (x, y)
    of (ARRAY x,    ARRAY    y) => x = y
     | (NUM   x,    NUM      y) => x = y
     | (SYM   x,    SYM      y) => x = y
     | (USER  x,    USER     y) => x = y
     | (CLOSURE  x, CLOSURE  y) => false
     | (CLASSREP x, CLASSREP y) => classId x = classId y
     | _ => false
(* <boxed values 59>=                           *)
val _ = op eqRep : value * value -> bool
(* <\ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives>= *)
fun memberOf ((c, _), (_, CLASSREP c')) = mkBoolean (classId c = classId c')
  | memberOf _ = raise RuntimeError "argument of isMemberOf: must be a class"

fun kindOf ((c, _), (_, CLASSREP (CLASS {class=u', ...}))) =
      let fun subclassOfClassU' (CLASS {super, class=u, ... }) =
            u = u' orelse (case super of NONE => false
                                       | SOME c => subclassOfClassU' c)
      in  mkBoolean (subclassOfClassU' c)
      end
  | kindOf _ = raise RuntimeError "argument of isKindOf: must be a class"
(* Error messages                               *)
(*                                              *)
(* The [[error:]] primitive raises [[RuntimeError]]. *)
(* <\ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives>= *)
fun error (_, (_, SYM s)) = raise RuntimeError s
  | error (_, (c, _    )) =
      raise RuntimeError ("error: got class " ^ className c ^
                                                            "; expected Symbol")
(* <\ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives>= *)
fun errorPrim msg = fn _ => raise RuntimeError msg
(* <\ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives>= *)
fun findClassAndMeta (supername, xi) =
  case !(find (supername, xi))
    of (meta, CLASSREP c) => (c, meta)
     | v => raise RuntimeError ("object " ^ supername ^ " = " ^ valueString v ^
                                " is not a class")
(* When a new class object is defined, it inherits from *)
(* a superclass. The interpreter needs not only the *)
(* internal representation of the superclass, but also *)
(* the internal representation of the superclass's *)
(* metaclass. Both are found by function        *)
(* [[findClassAndMeta]]. If an object is found that is *)
(* not a class, then somebody has tried to inherit from *)
(* something that's not a class, which is a checked *)
(* run-time error.                              *)
(* <boxed values 85>=                           *)
val _ = op findClassAndMeta : name * value ref env -> class * class
(* <\ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives>= *)
fun methodDefns (superMeta, super) ms =
  let fun method { flavor, name, formals, locals, body } =
            { name = name, formals = formals, body = body, locals = locals
            , superclass = case flavor of IMETHOD => super
                                        | CMETHOD => superMeta
            }
      fun addMethodDefn (m as { flavor = CMETHOD, ... }, (c's, i's)) =
                                                    (method m :: c's, i's)
        | addMethodDefn (m as { flavor = IMETHOD, ... }, (c's, i's)) =
                                                    (c's, method m :: i's)
(* When a new class is created, its method definitions *)
(* are converted to methods by function [[methodDefns]]. *)
(* This function also separates class methods from *)
(* instance methods. Each method has to know what *)
(* [[super]] means; it means one thing in an instance *)
(* method and another thing in a class method. Function *)
(* [[methodDefns]] is organized like this:      *)
(*                                              *)
(*   • Information about superclasses is passed in: *)
(*  argument [[super]] is the superclass from which *)
(*  the new class inherits; [[superMeta]] is    *)
(*  [[super]]'s metaclass.                      *)
(*                                              *)
(*   • Internal function [[method]] builds the *)
(*  representation of a method from its syntax. *)
(*  It associates class [[super]] with each instance *)
(*  method and [[superMeta]] with each class method. *)
(*  These associations guarantee that every message *)
(*  sent to [[SUPER]] arrives at the proper     *)
(*  destination.                                *)
(*                                              *)
(*   • Internal function [[addMethodDefn]] processes *)
(*  each method definition, adding it either to the *)
(*  list of class methods or to the list of instance *)
(*  methods for the new class. To accumulate these *)
(*  lists and place them in [[imethods]] and    *)
(*  [[cmethods]], [[methodDefns]] applies [[foldr]] *)
(*  to [[addMethodDefn]], a pair of empty lists, and *)
(*  the list of method definitions [[ms]].      *)
(*                                              *)
(* \qbreak \nwnarrowboxes                       *)
(* <boxed values 87>=                           *)
val _ = op methodDefns : class * class -> method_def list -> method list *
                                                                     method list
val _ = op method : method_def -> method
  in  foldr addMethodDefn ([], []) ms
  end
(* <utility functions for parsing internal method definitions>= *)
fun internalParse parser ss =
  let val synopsis = case ss of [s] => s
                              | ["(begin ", s, ")"] => s
                              | s :: ss => s ^ "..." 
                              | [] => ""
      val name = "internal syntax"
      val input = interactiveParsedStream (smalltalkToken, parser)
                                          (name, streamOfList ss, noPrompts)
      exception BadUserMethodInInterpreter of string (* can't be caught *)
  in  case streamGet input
        of SOME (e, _) => e
         | NONE => (app eprintln ("Failure to parse:" :: ss)
                   ; raise BadUserMethodInInterpreter (concat ss))
  end
(* \qbreak                                      *)
(*                                              *)
(* Remaining \chaptocsplitimplementations of primitive *)
(* classes                                      *)
(*                                              *)
(* The remaining methods of the primitive classes are *)
(* written in uSmalltalk. Just like methods in  *)
(* predefined or user-defined classes, the code for *)
(* these methods needs to be parsed—but the uSmalltalk *)
(* code lives in string literals in the interpreter's *)
(* source, not in an external file. So this part of the *)
(* interpreter needs a parser for methods represented as *)
(* strings.                                     *)
(*                                              *)
(* \qbreak Parsing begins with [[internalParse]], an *)
(* auxiliary function that applies any parser to a list *)
(* of strings.                                  *)
(* <boxed values 63>=                           *)
val _ = op internalParse : 'a parser -> string list -> 'a
(* <utility functions for parsing internal method definitions>= *)
val bogusSuperclass =
  CLASS { name = "bogus superclass", super = NONE
        , ivars = [], methods = ref [ ], class = ref PENDING
        }
val internalMethodDefns = methodDefns (bogusSuperclass, bogusSuperclass)
fun internalMethods strings =
  case (internalMethodDefns o internalParse parseMethods) strings
    of ([], imethods) => imethods 
     | (_ :: _, _) => raise InternalError "primitive class has class methods"
(* The strings are converted to methods by function *)
(* [[internalMethods]].                         *)
(* <boxed values 64>=                           *)
val _ = op internalMethods : string list -> method list
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <built-in class [[Object]]>=                 *)
val objectMethods =
  internalMethods 
                   [ ";  The answer [[C]] is delivered by the following "
                   , ";  computation:                                 "
                   , ";                                               "
                   , ";   1. When [[m1]] is sent to [[x]], [[x]] is an "
                   , ";   instance of class [[C]], and class [[C]] does not "
                   , ";   define method [[m1]]. But [[C]]'s superclass, "
                   , ";   class [[B]], does define [[m1]]. So the message "
                   , ";   is dispatched to [[B]]'s [[m1]] method, which "
                   , ";   executes.                                   "
                   , ";                                               "
                   , ";   2. [B--]'s [[m1]] method sends message [[m2]] to "
                   , ";   [[self]]—which is to say, to [[x]]. The search "
                   , ";   for method [[m2]] begins in [[x]]'s class,  "
                   , ";   namely [[C]].                               "
                   , ";                                               "
                   , ";   3. Class [[C]] defines [[m2]], so the message send "
                   , ";   dispatches to that definition. And class [[C]]'s "
                   , ";   [[m2]] method answers the symbol [[C]].     "
                   , ";                                               "
                   , ";  This example illustrates a crucial fact about "
                   , ";  Smalltalk: the search for a method begins in the "
                   , ";  class of the receiver. Many more examples of method "
                   , ";  dispatch appear throughout the chapter.      "
                   , ";                                               "
                   , ";  Does method dispatch always begin in the class of the "
                   , ";  receiver? [*] Almost always. A message sent to "
                   , ";  [[super]] is dispatched in a special way. The message "
                   , ";  is sent to [[self]], but the method search begins in "
                   , ";  the superclass of the class in which the message to "
                   , ";  [[super]] appears. That is, unlike the starting place "
                   , ";  for a normal message send, the starting place for a "
                   , ";  message to [[super]] is statically determined, "
                   , ";  independent of the class of the receiver [[self]]. "
                   , ";  This behavior guarantees that a particular method "
                   , ";  from the superclass will be executed.        "
                   , ";                                               "
                   , ";  We typically send to [[super]] when we wish to add to "
                   , ";  the behavior of an inherited method, not simply "
                   , ";  replace it. The most common examples are class "
                   , ";  methods that initialize objects, like method [[new]] "
                   , ";  in class [[Shape]]. A [[new]] method is defined on "
                   , ";  every class, and a properly designed [[new]] method "
                   , ";  not only allocates a new object but also establishes "
                   , ";  the private invariants of its class. [*] Simply "
                   , ";  sending [[new]] to [[self]] executes only the [[new]] "
                   , ";  method defined on the class of the object being "
                   , ";  created. But if there are invariants associated with "
                   , ";  the superclass, those invariants need to be  "
                   , ";  established too. All the invariants can be   "
                   , ";  established by the following idiom: each subclass "
                   , ";  sends [[new]] to [[super]], establishing the "
                   , ";  superclass invariants, then executes whatever "
                   , ";  additional code is needed to establish subclass "
                   , ";  invariants. And when a subclass has no invariants of "
                   , ";  its own, it can take a shortcut and simply inherit "
                   , ";  [[new]].                                     "
                   , ";                                               "
                   , ";  Equality: Identity and equivalence           "
                   , ";                                               "
                   , ";  [*] Below, \\crefsmall.predefined-objects dives into "
                   , ";  Smalltalk's initial basis, starting with the messages "
                   , ";  that every object understands. But some of those "
                   , ";  messages implement equality tests, which deserve a "
                   , ";  section of their own.                        "
                   , ";                                               "
                   , ";  The issue of when two objects should be considered "
                   , ";  equal is one we have danced around since \\cref "
                   , ";  scheme.chap. Equality is so central, and yet so "
                   , ";  seldom addressed well, that when you encounter a new "
                   , ";  programming language, it is one of the first things "
                   , ";  you should look at. This section will tell you what "
                   , ";  to look for.                                 "
                   , ";                                               "
                   , ";  Almost all languages support constant-time equality "
                   , ";  tests on simple, scalar values like integers and "
                   , ";  Booleans. But equality tests on more structured data, "
                   , ";  like records, sums, and arrays, can be done in more "
                   ,
                   ";  than one way—and there is no single right way \\citep "
                   , ";  noble-black:left-hand-equals. Moreover, when a "
                   , ";  language supports data abstraction, it is all too "
                   , ";  easy to test equality in the wrong way, by violating "
                   , ";  abstraction. To expose some of the issues, let's "
                   , ";  review some designs you've already seen.     "
                   , ";                                               "
                   , ";    • C has only one form of equality, and it applies "
                   , ";   only to scalar data (integers, Booleans,    "
                   , ";   floating-point numbers, and similar) and to "
                   , ";   pointers. Two pointers are equal if and only if "
                   , ";   they point to the same memory; viewed at a higher "
                   , ";   level of abstraction, C's [[==]] operator tests "
                   , ";   for object identity. (A well-known beginner's "
                   , ";   mistake is to use [[==]] to compare strings for "
                   , ";   equality; that comparison demands [[strcmp]].) "
                   , ";   Structured data like [[struct]]s and [[union]]s "
                   , ";   cannot be compared for equality, and C famously "
                   , ";   does not have arrays—only pointers and memory. [ "
                   , ";   ``Does not have arrays'' stretches the truth. "
                   , ";   In~initialized data, array notation and pointer "
                   , ";   notation mean different things \\citep[chapter~4] "
                   , ";   {van-der-linden:deep-secrets}.]             "
                   , ";                                               "
                   , ";    • micro-Scheme has two forms of equality, written "
                   , ";   [[=]] and [[equal?]]. The [[=]] operator works "
                   , ";   only on scalar data; given two pairs, it always "
                   , ";   returns [[#f]]—according to micro-Scheme's [[=]], "
                   , ";   a cons cell is not even equal to itself. The "
                   , ";   [[equal?]] predicate, on the other hand, provides "
                   , ";   structural similarity; it returns [[#t]] whenever "
                   , ";   two values have the same structure, even if the "
                   , ";   structure is arbitrarily large.             "
                   , ";                                               "
                   , ";   Full Scheme, in which cons cells are mutable, has "
                   , ";   a more principled design. Function [[equal?]] "
                   , ";   acts as in micro-Scheme, providing structural "
                   , ";   similarity. Function [[eqv?]] provides object "
                   , ";   identity, and it compares not only scalar data "
                   , ";   but also cons cells, vectors, and procedures. And "
                   , ";   function [[eq?]] provides object identity on "
                   , ";   structured data, but on numeric data and    "
                   , ";   character data it is more efficient if less "
                   , ";   predictable than [[eqv?]].                  "
                   , ";                                               "
                   , ";    • Standard ML has ``polymorphic equality'' "
                   , ";   comparison on integers, Booleans, strings, and "
                   , ";   mutable reference cells, but not on functions or "
                   , ";   floating-point numbers. Polymorphic equality can "
                   , ";   compare values of any type that ``admits    "
                   , ";   equality''; such types include the ones listed "
                   , ";   above, plus any constructed data (tuple type, "
                   , ";   record type, or algebraic data type) whose  "
                   , ";   components all admit equality. There are even "
                   , ";   special type variables that admit equality and "
                   , ";   that instantiated only with types that admit "
                   , ";   equality. Polymorphic equality hacks together "
                   , ";   object identity (for arrays and references) with "
                   , ";   structural similarity (for constructed data) in a "
                   , ";   hybrid that gets common cases right.        "
                   , ";                                               "
                   , ";   \\ocaml, like Scheme, has two forms of equality; "
                   , ";   the [[=]] sign means structural similarity, and "
                   , ";   the [[==]] sign means object identity (and is "
                   , ";   fully defined for mutable types only).      "
                   , ";                                               "
                   , ";  All these designs get abstract data wrong.   "
                   , ";  For evidence, review the association lists in \\ "
                   , ";  crefpage(scheme.alist-equality. Two association lists "
                   , ";  should be considered equal if and only if each "
                   , ";  contains all the key-value pairs found in the other. "
                   , ";  This equivalence relation, which is the correct one, "
                   , ";  is coarser than equivalence of representation. If two "
                   , ";  association lists have the same representation, they "
                   , ";  definitely represent the same associations, and "
                   , ";  therefore if two association lists represent "
                   , ";  different associations, they definitely have "
                   , ";  different representations. But two association lists "
                   , ";  may have different representations and yet represent "
                   , ";  the same associations. Given such lists, Scheme's "
                   , ";  [[equal?]] and ML's polymorphic equality produce "
                   , ";  wrong answers. Smalltalk's built-in [[=]] and [[==]] "
                   , ";  messages, which correspond roughly to micro-Scheme's  "
                   , ";  [[equal?]] and [[=]], pose the same risk. To produce "
                   , ";  right answers, the built-in [[=]] method may have to "
                   , ";  be overridden. And overriding it correctly requires "
                   , ";  deep understanding of what it could mean to consider "
                   , ";  two objects equivalent.                      "
                   , ";                                               "
                   , ";  Notions of equivalence                       "
                   , ";                                               "
                   , ";  To get equality right, Smalltalk uses the same "
                   , ";  methodology as \\mcl. The methodology is based on a "
                   , ";  central idea of programming-language theory, called "
                   , ";  observational equivalence. A pointy-headed theorist "
                   , ";  would say that two things are observationally "
                   , ";  equivalent if there is no computational context that "
                   , ";  can distinguish them. For a programmer who doesn't "
                   , ";  normally talk about computational contexts, the idea "
                   , ";  makes more sense as a programming principle: "
                   , ";                                               "
                   , ";   Two values should be considered equal if no "
                   , ";   program can tell them apart.                "
                   , ";                                               "
                   , ";  The principle has immediate consequences:    "
                   , ";                                               "
                   ,
                    ";    • A mutable abstraction, like a dictionary, should "
                   , ";   compare equal only with itself. That's because "
                   , ";   two different mutable objects can be told apart "
                   , ";   by mutating one and seeing that the other doesn't "
                   , ";   change. Therefore, on mutable data, equality must "
                   , ";   be implemented as object identity.          "
                   , ";    • Structurally similar representations of "
                   , ";   immutable, non-atomic abstractions, like large "
                   , ";   integers, should be considered equal.       "
                   , ";                                               "
                   , ";  In Smalltalk, object identity is implemented by the "
                   , ";  [[==]] method, which is defined on class [[Object]]:  "
                   , ";  [*]                                          "
                   , ";  <methods of class [[Object]]>=               "
                   ,
                  "(method ==  (anObject) (primitive sameObject self anObject))"
                   , "(method !== (anObject) ((self == anObject) not))"
                   , ";  \\ztrim-1 The [[=]] method, by contrast, should "
                   , ";  implement observational equivalence. But ``any "
                   , ";  program'' shouldn't be allowed to observe a  "
                   , ";  representation; programs are just too powerful. Like "
                   , ";  object identity and ML's polymorphic equality, "
                   , ";  an unrestricted program can observe differences "
                   , ";  between objects even when the objects really should "
                   , ";  be considered the same. It's better to restrict "
                   , ";  programs by ruling out some observations.    "
                   , ";                                               "
                   ,
                   ";    • Abstractions should be considered equivalent when "
                   , ";   no client code can tell them apart. For example, "
                   , ";   if finite maps are represented as association "
                   , ";   lists, and if no combination of [[find]] and "
                   , ";   [[bind]] operations can tell two maps apart, then "
                   , ";   they should be considered equivalent even if they "
                   , ";   are represented differently (\\cpageref      "
                   , ";   scheme.alist-equality).                     "
                   , ";    • If an abstraction is mutable, you might want to "
                   , ";   rule out mutation as a way of observing     "
                   , ";   differences. For example, perhaps lists [[ns]] "
                   , ";    and [[ms]] have the same elements right now, "
                   , ";   so you'd like to consider them equivalent, even "
                   , ";   if adding number 80 to list [[ns]] (but not  "
                   , ";   [[ms]]) would enable you to tell them apart. "
                   , ";    • Finally, in a language like Smalltalk, even "
                   , ";   though reflection can breach the abstraction "
                   , ";   barrier it should not be used to distinguish "
                   , ";   objects that would otherwise be             "
                   , ";   indistinguishable.                          "
                   , ";                                               "
                   , ";  All these restrictions apply to uSmalltalk's notion "
                   , ";  of equivalence:                              "
                   , ";                                               "
                   , ";   Two objects are considered equivalent if, without "
                   , ";   mutating either object or using reflection, "
                   , ";   client code cannot tell them apart.         "
                   , ";                                               "
                   , ";  In other words, two objects are considered equivalent "
                   , ";  if at this moment they represent the same    "
                   , ";  abstraction. This is the equivalence that is used by "
                   , ";  uSmalltalk's [[check-expect]] and implemented by the "
                   , ";  [[=]] method.                                "
                   , ";                                               "
                   , ";  To implement [[=]] correctly on each class requires "
                   , ";  an understanding of the class's representation "
                   , ";  invariant and abstraction function (\\crefcommapage "
                   , ";  mcl.data-abstraction). But as a default, a   "
                   , ";  conservative approximation is defined on all objects: "
                   , ";  <methods of class [[Object]]>=               "
                   , "(method =  (anObject) (self == anObject))"
                   , "(method != (anObject) ((self = anObject) not))"
                   , ";  On class [[Object]], they are                "
                   , ";  <methods of class [[Object]]>=               "
                   , "(method isNil  () false)  ;; definitions on Object"
                   , "(method notNil () true)"
                   , ";  In \\crefsmall.chap, methods of classes [[Object]], "
                   , ";  [[Class]], [[UndefinedObject]], and [[Metaclass]] are "
                   , ";  defined by applying [[internalMethods]] to uSmalltalk "
                   , ";  code stored in strings. Some of those methods are "
                   ,
                    ";  defined in \\crefsmall.chap. The remaining methods are "
                   , ";  defined in the rest of this section.         "
                   , ";                                               "
                   , ";  Class [[Object]]                             "
                   , ";                                               "
                   , ";  The methods of class [[Object]] that are not defined "
                   , ";  in \\crefsmall.chap are defined here.         "
                   , ";  <methods of class [[Object]]>=               "
                   ,
"(method print   ()          ('< print) (((self class) name) print) ('> print) self)"
                   ,
                "(method println ()          (self print) (newline print) self)"
                   , "(method class   ()          (primitive class self))"
                   ,
                 "(method isKindOf:  (aClass) (primitive isKindOf self aClass))"
                   ,
               "(method isMemberOf:(aClass) (primitive isMemberOf self aClass))"
                   , "(method error:     (msg)    (primitive error self msg))"
                   ,
    "(method subclassResponsibility () (primitive subclassResponsibility self))"
                   ,
                    "(method leftAsExercise () (primitive leftAsExercise self))"
                    ]
val objectClass = 
  CLASS { name = "Object", super = NONE, ivars = ["self"]
        , class = ref PENDING, methods = ref (methodsEnv objectMethods)
        }
val () = if logging then app print ["\nops.class { name = ", q "Object",
                ", methods = { ", commaSep (map (q o methodName) objectMethods),
                     " }, subclass_responsibilities = { } }\n"] else () (*OMIT*)
(* The [[]] are defined                         *)
(* throughout this chapter and in \crefusma.chap, *)
(* starting in \chunkrefsmall.chunk.object-methods. *)
(*                                              *)
(* Class [[UndefinedObject]], whose sole instance is  *)
(* [[nil]], redefines [[isNil]], [[notNil]], and *)
(* [[print]], as shown in chunks [->] and [->]. \ *)
(* makenowebnotdef(from \chunkref               *)
(* small.chunk.nil-methods)                     *)
(* <built-in class [[UndefinedObject]] and value [[nilValue]]>= *)
val nilClass = 
  mkClass "UndefinedObject" PENDING objectClass []
    (internalMethods 
                      [
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                      , ";  {sfprotocol}Class protocol for [[Natural]]class "
                      ,
                       ";  [[Natural]] \\aswidthof\\mfromSmall: anIntegersdiv: "
                      , ";  aSmallInteger --- Answer a natural-number object "
                      , ";  whose value is equal to the value of the argument, "
                      , ";  which must be a nonnegative integer.         "
                      ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                      , ";  {sfprotocol}                                 "
                      , ";                                               "
                      ,
                    ";  \\subfigskip {sfprotocol}Instance protocol for natural "
                      ,
                    ";  numbers[[Natural]] \\m+ aNatural --- Answer the sum of "
                      , ";  the receiver and the argument.               "
                      ,
                      ";  \\m* aNatural --- Answer the product of the receiver "
                      , ";  and the argument.                            "
                      , ";  \\m- aNatural --- Answer the difference of the "
                      ,
                       ";  receiver and the argument, or if this difference is "
                      , ";  not a natural number, fail with a run-time error. "
                      ,
                     ";  \\msdiv: aSmallInteger --- Answer the largest natural "
                      , ";  number whose value is at most the quotient of the "
                      , ";  receiver and the argument.                   "
                      ,
                    ";  \\msmod: aSmallInteger --- Answer a small integer that "
                      ,
                      ";  is the remainder when the receiver is divided by the "
                      , ";  argument.                                    "
                      ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                      , ";  \\mdecimal --- Answer a [[List]] containing the "
                      , ";  decimal digits of the receiver, most significant "
                      , ";  digit first.                                 "
                      ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                      ,
                      ";  \\msdivmod:with: aSmallInteger aBlock --- \\break An "
                      , ";  object n receiving \\monobox(n sdivmod:with: d b) "
                      ,
                    ";  answers the result of sending \\monobox(b value:value: "
                      , ";  Q r), where Q is n \\bdivd and r is n modd.   "
                      , ";  \\msubtract:withDifference:ifNegative: aNatural "
                      ,
                      ";  diffBlock negBlock --- \\break Subtract [[aNatural]] "
                      , ";  from the receiver to obtain difference d. If the "
                      , ";  difference is a natural number, answer \\monobox "
                      ,
                      ";  (diffBlock value: d). If the difference is negative, "
                      , ";  answer \\monobox(negBlock value).             "
                      , ";  \\misZero --- If the receiver is zero, answer "
                      , ";  [[true]]; otherwise answer [[false]].        "
                      ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                      , ";  {sfprotocol}                                 "
                      , ";                                               "
                      , ";  Protocols for natural numbers [*]            "
                      ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                      , ";                                               "
                      , ";  Natural numbers                              "
                      , ";                                               "
                      , ";  A natural number is more than a magnitude but less "
                      , ";  than a full [[Number]]: uSmalltalk's class   "
                      ,
                      ";  [[Natural]] supports limited arithmetic and a couple "
                      , ";  of new observers. A natural number is a form of "
                      , ";  magnitude, and class [[Natural]] is a subclass of "
                      , ";  [[Magnitude]]. In addition to the protocol for "
                      , ";  [[Magnitude]], including comparisons, class  "
                      ,
                     ";  [[Natural]] and its instances respond to the protocol "
                      , ";  in \\vrefsmall.fig.Natural.                   "
                      , ";                                               "
                      ,
                       ";  Natural numbers may be added and multiplied without "
                      , ";  fear of overflow—the size of a natural number is "
                      , ";  limited only by the amount of memory available. "
                      ,
                     ";  Natural numbers may also be subtracted, provided that "
                      ,
                       ";  the argument is no greater than the receiver. And a "
                      , ";  natural number may be divided by a small, positive "
                      ,
                      ";  integer; the [[s]] in [[sdiv:]] and [[smod:]] stands "
                      , ";  for ``short.'' (As noted on \\cpageref        "
                      , ";  arith.short-division, long division is beyond the "
                      , ";  scope of this book.)                         "
                      , ";                                               "
                      , ";  \\ztrim-2.5                                   "
                      , ";                                               "
                      ,
                     ";  The protocol for instances of [[Natural]] includes an "
                      ,
                      ";  observer [[decimal]], which converts the receiver to "
                      , ";  a list of decimal digits. (For efficiency, the "
                      , ";  receiver is expected to use an internal      "
                      , ";  representation with a base much larger than 10.) "
                      , ";                                               "
                      ,
                       ";  \\zbreak Finally, the [[Natural]] protocol includes "
                      , ";  three methods that are intended to promote   "
                      , ";  efficiency:                                  "
                      , ";                                               "
                      ,
                     ";    • Method [[sdivmod:with:]] computes both quotient "
                      , ";   and remainder in a single operation. It is  "
                      , ";   necessary in order to implement division in "
                      , ";   linear time. If a division operation were to send "
                      , ";   both [[sdiv:]] and [[smod:]], division could take "
                      , ";   time exponential in the number of digits, which "
                      , ";   is not acceptable.                          "
                      ,
                      ";    • Method [[subtract:withDifference:ifNegative:]] "
                      , ";   combines comparison and subtraction into a single "
                      , ";   operation. Implemented independently, each of "
                      , ";   these operations could take linear time, and "
                      , ";   comparison comes ``for free'' with a subtraction. "
                      ,
                      ";    • Method [[isZero]] can sometimes be implemented "
                      , ";   more efficiently than [[=]].                "
                      , ";                                               "
                      , ";  The implementation of class [[Natural]] is left to "
                      ,
                   ";  you (\\crefsmall.ex.Natural). Ideas are presented on \\ "
                      , ";  cpagerefsmall.imp.Natural.                   "
                      , ";                                               "
                      , ";  \\basislabelfalse                             "
                      , ";                                               "
                      , ";  Object-oriented programming techniques       "
                      , ";                                               "
                      , ";  To get started programming in uSmalltalk, you can "
                      ,
                       ";  focus on the examples from \\crefsmall.intro and on "
                      , ";  the protocols and informal descriptions of the "
                      ,
                     ";  predefined classes in \\crefsmall.predefined-objects. "
                      ,
                       ";  But to internalize object-oriented ways of thinking "
                      ,
                      ";  and programming, you need to study more deeply. Four "
                      , ";  increasingly deep techniques are presented in the "
                      , ";  next four sections:                          "
                      , ";                                               "
                      ,
                   ";    • \\Crefsmall.dispatch-not-conditional shows how to "
                      , ";   make decisions by dispatching messages to the "
                      , ";   right methods, not by evaluating conditionals. "
                      , ";   The technique is illustrated with example methods "
                      , ";   defined on classes [[Object]],              "
                      , ";   [[UndefinedObject]], and [[Boolean]].       "
                      ,
                     ";    • \\Crefsmall.wide-over-narrow shows how to reuse "
                      , ";   code by building abstract classes that provide "
                      , ";   many useful methods on top of just a few subclass "
                      , ";   responsibilities. The technique is illustrated "
                      , ";   with example methods defined on collection  "
                      , ";   classes.                                    "
                      ,
                   ";    • \\Crefsmall.multiple-representations shows how to "
                      , ";   define methods that want to look at         "
                      , ";   representations of more than one object, even "
                      , ";   though a method defined on an object has access "
                      , ";   only to its own instance variables. The technique "
                      , ";   is illustrated with example methods defined on "
                      , ";   numeric classes.                            "
                      ,
                   ";    • \\Crefsmall.imp.List shows how to integrate ideas "
                      , ";   from \\cref                                  "
                      ,
                    ";   small.dispatch-not-conditional,small.wide-over-narrow "
                      , ";   with program-design ideas from \\crefmcl.chap, "
                      , ";   including an abstraction function and       "
                      , ";   representation invariant. The techniques are "
                      , ";   illustrated with a complete implementation of "
                      , ";   class [[List]].                             "
                      , ";                                               "
                      , ";  Technique I: Method dispatch replaces \\      "
                      , ";  chaptocsplitconditionals                     "
                      , ";                                               "
                      , ";  [*]                                          "
                      , ";                                               "
                      , ";  [*] An object-oriented program differs most from a "
                      ,
                      ";  functional or procedural program in the way it makes "
                      ,
                      ";  decisions. An object-oriented program doesn't ask an "
                      ,
                      ";  object, ``How were you formed?'' Instead, it sends a "
                      , ";  message that asks a different question or makes a "
                      ,
                       ";  request. The form of the receiver becomes known not "
                      ,
                       ";  by evaluating an expression but by dispatching to a "
                      ,
                     ";  method. For example, we can ask any object, ``Are you "
                      , ";  [[nil]]?'' by sending it message [[isNil]] or "
                      , ";  [[notNil]]. Methods [[isNil]] and [[notNil]] are "
                      ,
                     ";  defined in ordinary uSmalltalk, on classes [[Object]] "
                      , ";  and [[UndefinedObject]]. Before we study the "
                      ,
                   ";  definitions, here's how not to do it—two ways to test "
                      , ";  for [[nil]] by evaluating an expression:     "
                      , ";  {smallverbatimx} (method isNil () (self == nil)) ; "
                      , ";  embarrassing (method isNil () (self isMemberOf: "
                      , ";  UndefinedObject)) ; more embarrassing        "
                      , ";  {smallverbatimx} This code makes a real Smalltalk "
                      ,
                     ";  programmer cringe. In Smalltalk, case analysis should "
                      , ";  be implemented by method dispatch. For [[isNil]] "
                      , ";  there are only two possible cases: an object is "
                      , ";  [[nil]] or it isn't. I arrange that on the [[nil]] "
                      , ";  object, the [[isNil]] method answers [[true]], and "
                      , ";  that on all other objects, it answers [[false]]. "
                      , ";  I need only two method definitions: one on class "
                      , ";  [[UndefinedObject]], which is used only to answer "
                      , ";  messages sent to [[nil]], and one on class   "
                      , ";  [[Object]], which all other classes inherit. "
                      ,
                      ";  I implement [[notNil]] the same way. The definitions "
                      , ";  on class [[UndefinedObject]] are [*]         "
                      , ";  <methods of class [[UndefinedObject]]>=      "
                      ,
                   "(method isNil  () true)   ;; definitions on UndefinedObject"
                      , "(method notNil () false)"
                      , ";  Class [[UndefinedObject]]                    "
                      , ";                                               "
                      , ";  The [[nil]] object gets a special print method. "
                      , ";  The other methods that are defined on class  "
                      ,
                     ";  [[UndefinedObject]] are defined in \\crefsmall.chap.  "
                      , ";  [*]                                          "
                      , ";  <methods of class [[UndefinedObject]]>=      "
                      , "(method print () ('nil print) self)"
                       ])
(* Class [[UndefinedObject]] has a single instance, *)
(* internally called [[nilValue]]. To enable it to be *)
(* returned from some primitives, it is created here. *)
(* <built-in class [[UndefinedObject]] and value [[nilValue]]>= *)
val nilValue = 
  let val nilCell  = ref (nilClass, USER []) : value ref
      val nilValue = (nilClass, USER (bind ("self", nilCell, emptyEnv)))
      val _        = nilCell := nilValue
  in  nilValue
  end
(* <higher-order functions for creating \usmalltalk\ primitives>= *)
fun arrayPrimitive f ((c, ARRAY a) :: vs, _) = f (a, vs)
  | arrayPrimitive f _ = raise RuntimeError "Array primitive used on non-array"
(* Each array primitive expects [[self]] to be an array. *)
(* This expectation is checked by higher-order function *)
(* [[arrayPrimitive]].                          *)
(* <boxed values 61>=                           *)
val _ = op arrayPrimitive : (value array * value list -> value) -> primitive
(* <higher-order functions for creating \usmalltalk\ primitives>= *)
fun classPrim f = 
  unaryPrim (fn (meta, CLASSREP c) => f (meta, c)
              | _ => raise RuntimeError "class primitive sent to non-class")
(* A primitive that expects a class argument can be made *)
(* using ML function [[classPrim]], which builds a *)
(* primitive from an ML function that takes both the *)
(* metaclass and class object of the argument.  *)
(*                                              *)
(* <boxed values 62>=                           *)
val _ = op classPrim : (class * class -> value) -> primitive
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* Code inside a closure is evaluated by the [[value]] *)
(* primitive. The [[value]] primitive is the only *)
(* primitive that is mutually recursive with [[eval]]; *)
(* it uses the function stored in [[applyClosureRef]].  *)
(* [*]                                          *)
(* <\ml\ code for remaining classes' primitives>= *)
type closure = name list * exp list * value ref env * class * frame
val applyClosureRef : (closure * value list * value ref env -> value) ref
  = ref (fn _ => raise InternalError "applyClosureRef not set")

fun valuePrim ((_, CLOSURE clo) :: vs, xi) = !applyClosureRef (clo, vs, xi)
  | valuePrim _ = raise RuntimeError "primitive `value` needs a closure"
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(*  sameObject             className         addWithOverflow value *)
(*  class                  protocol          subWithOverflow printu *)
(*  isKindOf               localProtocol     mulWithOverflow printSymbol *)
(*  isMemberOf             getMethod         +               newSymbol *)
(*  error                  setMethod         -               arrayNew *)
(*  subclassResponsibility removeMethod      *               arraySize *)
(*  leftAsExercise         methodNames       div             arrayAt *)
(*  newUserObject          newSmallInteger   <               arrayUpdate *)
(*  superclass             printSmallInteger >  *)
(*                                              *)
(* uSmalltalk's primitives [*]                  *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Primitives                                   *)
(*                                              *)
(* uSmalltalk's primitives are listed in \vref  *)
(* small.fig.primitives. Each primitive's specification *)
(* should be suggested by its name, but if you are *)
(* uncertain about what a primitive does, you can study *)
(* how it is used in predefined classes.        *)
(*                                              *)
(* A uSmalltalk primitive is almost the same thing as a *)
(* micro-Scheme primitive function, and like a  *)
(* micro-Scheme primitive function, it is represented as *)
(* an ML function. For example, the [[value]] primitive *)
(* is defined as function [[valuePrim]] in \chunkref *)
(* small.chunk.valuePrim. Like micro-Scheme's primitive *)
(* functions (\crefmlscheme.chap, \cpageref     *)
(* mlscheme.primitives), most of uSmalltalk's primitives *)
(* are defined using higher-order ML functions in the *)
(* interpreter. The definitions are so similar to the *)
(* examples in \crefmlscheme.chap that only a couple are *)
(* worth showing here: [[class]], which returns an *)
(* object's class, and [[newUserObject]], which creates *)
(* a new object.                                *)
(*                                              *)
(* The [[class]] primitive takes an object as its single *)
(* argument. The object is represented by a pair that *)
(* includes the object's class, which is promoted to a *)
(* full object by calling function [[classObject]] (\ *)
(* chunkrefsmall.chunk.classObject). [*]        *)
(* <\ml\ code for remaining classes' primitives>= *)
val classPrimitive = unaryPrim (fn (c, rep) => classObject c)
(* <\ml\ code for remaining classes' primitives>= *)
local
  fun mkIvars (CLASS { ivars, super, ... }) =
    let val supervars = case super of NONE => emptyEnv | SOME c => mkIvars c
    in  foldl (fn (x, rho) => bind (x, ref nilValue, rho)) supervars ivars
    end
  fun newUserObject c =
        let val ivars = mkIvars c
            val self = (c, USER ivars)
        in  (find ("self", ivars) := self; self)
        end
in
  val newPrimitive = classPrim (fn (meta, c) => newUserObject c)
(* \zbreak The [[newUserObject]] primitive allocates *)
(* fresh instance variables, each containing    *)
(* [[nilValue]]. It then allocates the object, and *)
(* finally it assigns [[self]] to point to the object *)
(* itself. [*]                                  *)
(* <boxed values 15>=                           *)
val _ = op mkIvars       : class -> value ref env
val _ = op newUserObject : class -> value
end
(* <\ml\ code for remaining classes' primitives>= *)
fun withOverflow binop ([(_, NUM n), (_, NUM m), ovflw], xi) =
      (internalThunk [VALUE (mkInteger (binop (n, m)))]
       handle Overflow => ovflw)
  | withOverflow _ ([_, _, _], _) =
      raise RuntimeError "numeric primitive with overflow expects numbers"
  | withOverflow _ _ =
      raise RuntimeError "numeric primitive with overflow expects 3 arguments"
and internalThunk exps =
      mkBlock ([], exps, emptyEnv, objectClass, noFrame)
(* \qbreak                                      *)
(*                                              *)
(* Primitives for arithmetic with overflow      *)
(*                                              *)
(* To detect overflow in arithmetic, the arithmetic *)
(* primitives catch ML's predefined [[Overflow]] *)
(* exception. Each relevant primitive tries to build a *)
(* block containing the result of the arithmetic, but if *)
(* [[Overflow]] is raised, the primitive answers the *)
(* overflow block instead. The logic is implemented once *)
(* and for all by function [[withOverflow]]. Function *)
(* [[internalThunk]] builds a block from a list of *)
(* expressions.                                 *)
(* <boxed values 58>=                           *)
val _ = op internalThunk : exp list -> value
val _ = op withOverflow : (int * int -> int) -> primitive
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

(* <\ml\ code for remaining classes' primitives>= *)
fun printInt (self as (_, NUM n)) = ( xprint (intString n); self )
  | printInt _ = raise RuntimeError ("printInt primitive on non-SmallInteger")
(* Integers also support UTF-8 printing.        *)
(* <\ml\ code for remaining classes' primitives>= *)
fun printu (self as (_, NUM n)) = ( printUTF8 n; self )
  | printu _ = raise RuntimeError ("printu primitives on non-SmallInteger")
(* <\ml\ code for remaining classes' primitives>= *)
fun binaryInt mk operator ((_, NUM n), (_, NUM m)) = mk (operator (n, m))
  | binaryInt _ _         ((_, NUM n), (c, _)) =
      raise RuntimeError ("numeric primitive expected numeric argument, got <"
                          ^ className c ^ ">")
  | binaryInt _ _         ((c, _), _) =
      raise RuntimeError ("numeric primitive method defined on <" ^ className c
                                                                          ^ ">")
fun arithop    operator = binaryPrim (binaryInt mkInteger operator)
fun intcompare operator = binaryPrim (binaryInt mkBoolean operator)
(* <boxed values 60>=                           *)
val _ = op binaryInt  : ('a -> value) -> (int * int -> 'a)   -> value * value ->
                                                                           value
val _ = op arithop    :                  (int * int -> int)  -> primitive
val _ = op intcompare :                  (int * int -> bool) -> primitive
(* To implement class method [[new:]] on        *)
(* [[SmallInteger]] requires a primitive. This primitive *)
(* must receive the class, plus an argument that is *)
(* represented by the integer being created.    *)
(* <\ml\ code for remaining classes' primitives>= *)
fun newInteger ((_, CLASSREP c), (_, NUM n)) = (c, NUM n)
  | newInteger _ = raise RuntimeError (
                                   "made new integer with non-int or non-class")
(* The primitives above are used to define class *)
(* [[SmallInteger]] (chunk [->]).               *)
(*                                              *)
(* Symbol primitives                            *)
(*                                              *)
(* A symbol prints as its name, with no leading [[']]. *)
(* <\ml\ code for remaining classes' primitives>= *)
fun printSymbol (self as (_, SYM s)) = (xprint s; self)
  | printSymbol _ = raise RuntimeError
                                 "cannot print when object inherits from Symbol"
(* <\ml\ code for remaining classes' primitives>= *)
fun newSymbol ((_, CLASSREP c), (_, SYM s)) = (c, SYM s)
  | newSymbol _ = raise RuntimeError (
                                 "made new symbol with non-symbol or non-class")
(* \qbreak                                      *)
(*                                              *)
(* Array primitives                             *)
(*                                              *)
(* The primitive operations on arrays are creation, *)
(* subscript, update, and size.                 *)
(*                                              *)
(* In a new array, every element is initialized to *)
(* [[nil]]. [*] [*]                             *)
(* <\ml\ code for remaining classes' primitives>= *)
fun newArray ((_, CLASSREP c), (_, NUM n)) = (c, ARRAY (Array.array (n, nilValue
                                                                             )))
  | newArray _ = raise RuntimeError
                                "Array new sent to non-class or got non-integer"
(* Each array primitive is built from a function that *)
(* takes a \monoboxvalue array as its first argument. *)
(* Starting with [[arraySize]].                 *)
(* <\ml\ code for remaining classes' primitives>= *)
fun arraySize (a, []) = mkInteger (Array.length a)
  | arraySize (a, vs) = arityError 0 vs
(* The array primitives for [[at:]] and [[at:put:]] use *)
(* Standard ML's [[Array]] module.              *)
(* <\ml\ code for remaining classes' primitives>= *)
fun arrayAt (a, [(_, NUM n)]) = Array.sub (a, n)
  | arrayAt (_, [_])  = raise RuntimeError "Non-integer used as array subscript"
  | arrayAt (_, vs)   = arityError 1 vs

fun arrayUpdate (a, [(_, NUM n), x]) = (Array.update (a, n, x); nilValue)
  | arrayUpdate (_, [_, _]) = raise RuntimeError
                                           "Non-integer used as array subscript"
  | arrayUpdate (_, vs)     = arityError 2 vs
(* Showing protocols                            *)
(*                                              *)
(* The [[showProtocol]] function helps implement the *)
(* [[protocol]] and [[localProtocol]] primitives, which *)
(* are used to implement the methods of the same names *)
(* on class [[Class]]. The implementation of    *)
(* [[showProtocol]] is not very interesting. Function *)
(* [[insert]] helps implement an insertion sort, which *)
(* is used to present methods in alphabetical order. *)
(* <\ml\ code for remaining classes' primitives>= *)
local
  fun showProtocol doSuper kind c =
    let fun member x l = List.exists (fn x' : string => x' = x) l
        fun insert (x, []) = [x]
          | insert (x, (h::t)) =
              case compare x h
                of LESS    => x :: h :: t
                 | EQUAL   => x :: t (* replace *)
                 | GREATER => h :: insert (x, t)
        and compare (name, _) (name', _) = String.compare (name, name')
        fun methods (CLASS { super, methods = ref ms, name, ... }) =
              if     not doSuper 
              orelse (kind = "class-method" andalso name = "Class")
              then
                foldl insert [] ms
              else
                foldl insert (case super of NONE => [] | SOME c => methods c) ms
        fun show (name, { formals, ... } : method) =
              app xprint ["(", kind, " ", name,
                          " (", spaceSep formals, ") ...)\n"]
    in  app show (methods c)
    end
in
  fun protocols all (meta, c) =
    ( showProtocol all "class-method" meta
    ; showProtocol all "method" c
    ; (meta, CLASSREP c)
    )
end
(* \qbreak                                      *)
(*                                              *)
(* Reflection: Manipulating a class's methods   *)
(*                                              *)
(* The remaining functions implement the primitives that *)
(* query, add, and remove methods from a class object. *)
(* <\ml\ code for remaining classes' primitives>= *)
fun methodNames (_, CLASS { methods, ... }) = mkArray (map (mkSymbol o fst) (!
                                                                       methods))
(* <\ml\ code for remaining classes' primitives>= *)
fun getMethod ((_, CLASSREP (c as CLASS { methods, name, ... })), (_, SYM s)) =
      (mkCompiledMethod (find (s, !methods))
       handle NotFound _ =>
         raise RuntimeError ("class " ^ className c ^ " has no method " ^ s))
      before (if logging then logGetMethod name s else ())
  | getMethod ((_, CLASSREP _), _) =
      raise RuntimeError "getMethod primitive given non-name"    
  | getMethod _ =
      raise RuntimeError "getMethod primitive given non-class"    
(* \qbreak                                      *)
(* <\ml\ code for remaining classes' primitives>= *)
fun removeMethod ((_, CLASSREP (c as CLASS { methods, ... })), (_, SYM s)) =
      (methods := List.filter (fn (m, _) => m <> s) (!methods); nilValue)
  | removeMethod ((_, CLASSREP _), _) =
      raise RuntimeError "removeMethod primitive given non-name"    
  | removeMethod _ =
      raise RuntimeError "removeMethod primitive given non-class"    
(* In [[setMethod]], the ellipsis ([[...]]) in the *)
(* binding for [[formals]], [[locals]], and [[body]] is *)
(* a nifty way of binding just some of the fields in a *)
(* Standard ML record. Local function [[given]] handles *)
(* the error message if the [[setMethod]] primitive is *)
(* given the wrong number or classes of arguments. *)
(* <\ml\ code for remaining classes' primitives>= *)
local 
  fun given what = raise RuntimeError ("setMethod primitive given " ^ what)
in
  fun setMethod [(_, CLASSREP c), (_, SYM s), (_, METHODV m)] =
        let val CLASS { methods, super, name = cname, ... } = c
            val superclass = case super of SOME s => s | NONE => c (* bogus *)
            val { formals = xs, locals = ys, body = e, ... } = m
            val m' = { name = s, formals = xs, locals = ys, body = e
                     , superclass = superclass }
            val _ = if badColon s then
                        raise RuntimeError ("compiled method cannot have " ^
                                            "symbolic name " ^ s ^
                                            ", which has a colon")
                    else ()
            val _ = if arity s = length xs then ()
                    else raise RuntimeError ("compiled method with " ^
                                             countString xs "argument" ^
                                             " cannot have name `" ^ s ^ "`")
            val _ = if logging then logSetMethod cname s else ()
        in  (methods := bind (s, m', !methods); nilValue)
        end
    | setMethod [(_, CLASSREP _), (_, SYM s), m] =
                            given ("non-method " ^ valueString m)
    | setMethod [(_, CLASSREP _), s, _] =
                            given ("non-symbol " ^ valueString s)
    | setMethod [c, _, _] = given ("non-class " ^ valueString c)
    | setMethod _ =         given "wrong number of arguments"
end
(* For the [[superclass]] primitive, a class's  *)
(* superclass, if it has one, is taken right out of its *)
(* representation.                              *)
(* <\ml\ code for remaining classes' primitives>= *)
fun superclassObject (_, CLASS { super = NONE, ... })   = nilValue
  | superclassObject (_, CLASS { super = SOME c, ... }) = classObject c
(* \savenowebnotdef                             *)
(*                                              *)
(* Class [[Class]] is in the interpreter so that *)
(* metaclasses can inherit from it, and [[Metaclass]] is *)
(* here so that each metaclass can be an instance of it. *)
(* \makenowebnotdef(from \chunkref              *)
(* small.chunk.class-methods)                   *)
(* <built-in classes [[Class]] and [[Metaclass]]>= *)
val classClass =
  mkClass "Class" PENDING objectClass []
    (internalMethods 
                      [
                      ";  Most of the methods of class [[Class]] are relegated "
                      ,
                    ";  to \\crefusma.chap, but the default implementation of  "
                      , ";  [[new]] is shown here: [*]                   "
                      , ";  <methods of class [[Class]]>=                "
                      , "(method new () (primitive newUserObject self))"
                      , ";  Class [[Class]]                              "
                      , ";                                               "
                      ,
                       ";  The methods of class [[Class]] that are not defined "
                      , ";  in \\crefsmall.chap are defined here. The rest are "
                      , ";  defined here.                                "
                      , ";  <methods of class [[Class]]>=                "
                      ,
                    "(method superclass         () (primitive superclass self))"
                      ,
                     "(method name               () (primitive className self))"
                      ,
                      "(method printProtocol      () (primitive protocol self))"
                      ,
                 "(method printLocalProtocol () (primitive localProtocol self))"
                      ,
                   "(method methodNames        () (primitive methodNames self))"
                      ,
       "(method compiledMethodAt: (aSymbol) (primitive getMethod self aSymbol))"
                      , "(method addSelector:withMethod: (aSymbol aMethod) "
                      , "    (primitive setMethod self aSymbol aMethod)"
                      , "    self)"
                      , "(method removeSelector: (aSymbol) "
                      , "   (primitive removeMethod self aSymbol)"
                      , "   self)"
                       ])

val metaclassClass =
  mkClass "Metaclass" PENDING classClass []
    (internalMethods 
                      [ ";  For metaclasses, this default is overridden; "
                      , ";  a metaclass may not be used to instantiate new "
                      , ";  objects. [*]                                 "
                      , ";  <methods of class [[Metaclass]]>=  "
                      ,
         "(method new () (self error: 'a-metaclass-may-have-only-one-instance))"
                       ])
(* <metaclasses for built-in classes>=          *)
fun metaSuper (CLASS { super = NONE,       ... }) = classClass
  | metaSuper (CLASS { super = SOME c_sup, ... }) = metaclass c_sup
(* When [[mkMeta]] creates a metaclass for class C, it *)
(* gives C's metaclass a superclass, that is, it finds *)
(* the class that will be \monobox((C metaclass) *)
(* superclass). C's metaclass's superclass is actually *)
(* found by function [[metaSuper]], and as noted above, *)
(* it is normally \monobox((C superclass) metaclass). *)
(* But when C has no superclass, its metaclass's *)
(* superclass is instead class [[Class]]. Internal *)
(* representation [[classClass]] is defined below. *)
(* <boxed values 9>=                            *)
val _ = op metaSuper : class -> class
(* <metaclasses for built-in classes>=          *)
fun mkMeta c classmethods =
  mkClass ("class " ^ className c) (META metaclassClass) (metaSuper c)
          [] classmethods
(* <boxed values 10>=                           *)
val _ = op mkMeta : class -> method list -> class
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <metaclasses for built-in classes>=          *)
fun patchMeta c = setMeta (c, mkMeta c [])
val () = app patchMeta [objectClass, nilClass, classClass, metaclassClass]
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \zbreak The representation of class objects is *)
(* defined in code \chunkrefsmall.chunk.class, but the *)
(* large record type is awkward to manipulate directly. *)
(* Instead, functions that manipulate ML values of type *)
(* [[class]] do so using utility functions that are *)
(* defined in \crefusma.chap and are summarized in \vref *)
(* small.tab.classutils.                        *)
(*                                              *)
(* A typical class object and its corresponding *)
(* metaclass are both created by function       *)
(* [[newClassObject]], which is given the definition of *)
(* the class. The new class has a superclass, which is *)
(* found (along with its metaclass) by function *)
(* [[findClass]], which looks up the name of the *)
(* superclass in environment xi. The method definitions *)
(* are segrated into class methods and instance methods *)
(* by function [[methodDefns]], which also attaches the *)
(* correct static superclass to each method. The new *)
(* class is built by [[mkClass]], its metaclass is built *)
(* by [[mkMeta]], and the final class object is built by *)
(* [[classObject]]. [*]                         *)
(* <definition of [[newClassObject]] and supporting functions>= *)
fun newClassObject {name, super, ivars, methods} xi =
  let val (super, superMeta) = findClassAndMeta (super, xi)
        handle NotFound s =>
          raise RuntimeError ("Superclass " ^ s ^ " not found")
      val (cmethods, imethods) = methodDefns (superMeta, super) methods
      val class = mkClass name PENDING super ivars imethods
      val ()    = setMeta (class, mkMeta class cmethods)
  in  classObject class
  end
(* \qbreak                                      *)
(*                                              *)
(* Support for tracing                          *)
(*                                              *)
(* The uSmalltalk interpreter supports two forms of *)
(* diagnostics that are called tracing:         *)
(*                                              *)
(*   • When requested by a program, message tracing *)
(*  shows every message send and the reply to it. *)
(*   • When a checked run-time error occurs, stack *)
(*  tracing shows the state of the call stack,  *)
(*  that is, every active send.                 *)
(*                                              *)
(* Both forms of tracing share some mutable state, and *)
(* both are implemented in this section. To keep the *)
(* details hidden from the rest of the interpreter, the *)
(* shared mutable state and related functions are made *)
(* [[local]].                                   *)
(* <functions for managing and printing a \usmalltalk\ stack trace>= *)
local

(* <private state and functions for printing indented message traces ((usm))>= *)
  fun traceMe xi =
    let val count = find("&trace", xi)
    in  case !count
          of (c, NUM n) =>
              if n = 0 then false
              else ( count := (c, NUM (n - 1))
                   ; if n = 1 then (xprint "<trace ends>\n"; false) else true
                   )
           | _ => false
    end handle NotFound _ => false
  (* <boxed values 73>=                           *)
  val _ = op curlyWarning : exp list located -> exp list
  (* [[funty]] stand for \tau, [[actualtypes]]    *)
  (* stand for \ldotsntau, and [[rettype]] stand for alpha *)
  (* . The first premise is implemented by a call to *)
  (* [[typesof]] and the second by a call to      *)
  (* [[freshtyvar]]. The constraint is represented just as *)
  (* written in the rule.                         *)

  (* Message tracing                              *)
  (*                                              *)
  (* Interface to the programmer                  *)
  (*                                              *)
  (* When global variable [[ --- trace]] is set to a *)
  (* nonzero number, the uSmalltalk interpreter prints a *)
  (* trace of every message send and its reply. And at *)
  (* every send, [[ --- trace]] is decremented, so if a *)
  (* programmer wants to trace just the next N messages, *)
  (* they set [[ --- trace]] to N. This work is done by *)
  (* function [[traceMe]]. When the evaluator wants to *)
  (* know if it should trace a message send, it calls *)
  (* [[traceMe]]. When [[ --- trace] is a nonzero number, *)
  (* [[traceMe]] returns [[true]]; otherwise it returns *)
  (* [[false]]. And when [[ --- trace]] is nonzero, *)
  (* [[traceMe]] decrements [[ --- trace]].       *)
  (* <boxed values 73>=                           *)
  val _ = op traceMe : value ref env -> bool

(* <private state and functions for printing indented message traces ((usm))>= *)
  val tindent = ref 0
  fun indent 0 = ()
    | indent n = (xprint "  "; indent (n-1))
  (* \qbreak                                      *)
  (*                                              *)
  (* Conditional printing with indentation        *)
  (*                                              *)
  (* To depict how many sends are active at any given *)
  (* moment, each message trace is indented by a number of *)
  (* spaces proportional to the number of active sends. *)
  (* The current indentation is maintained in local *)
  (* variable [[tindent]]. Function [[indent]] uses *)
  (* [[tindent]] to print the indentation.        *)
  (* <boxed values 76>=                           *)
  val _ = op indent : int -> unit

(* <private state and functions for printing indented message traces ((usm))>= *)
  datatype indentation = INDENT_AFTER | OUTDENT_BEFORE

  fun tracePrint direction xi f =
      if traceMe xi then
        let val msg = f () (* could change tindent *)
        in  ( if direction = OUTDENT_BEFORE then tindent := !tindent - 1 else ()
            ; indent (!tindent)
            ; app xprint msg
            ; xprint "\n"
            ; if direction = INDENT_AFTER   then tindent := !tindent + 1 else ()
            )
        end
      else
          ()    
  (* \qtrim1.5                                    *)
  (*                                              *)
  (* Stack tracing                                *)
  (*                                              *)
  (* Each active send is identified by a message name, the *)
  (* sending location, and a string describing the *)
  (* receiver. The stack of active sends is stored in *)
  (* private variable [[locationStack]]. This stack is *)
  (* displayed when an error occurs.              *)

(* <private state and functions for maintaining a stack of source-code locations ((usm))>= *)
  val locationStack = ref [] : (string * srcloc * string) list ref
  fun push info = locationStack := info :: !locationStack
  fun pop () = case !locationStack
                 of []     => raise InternalError "tracing stack underflows"
                  | h :: t => locationStack := t
in
  (* <exposed message-tracing functions ((usm))>= *)
  fun resetTrace ()       = (locationStack := []; tindent := 0)
  fun traceIndent what xi = (push what; tracePrint INDENT_AFTER   xi)
  fun outdentTrace     xi = (pop ();    tracePrint OUTDENT_BEFORE xi)
  (* <boxed values 74>=                           *)
  val _ = op resetTrace   : unit -> unit
  val _ = op traceIndent : string * srcloc * string ->       value ref env -> (
                                                    unit -> string list) -> unit
  val _ = op outdentTrace   :               value ref env -> (unit -> string
                                                                   list) -> unit
  (* Primitive functions and the initial basis    *)
  (*                                              *)
  (* Defining the remaining primitives            *)
  (*                                              *)
  (* Some primitives are defined in \crefmlscheme.chap. *)
  (* The rest are here.                           *)

  (* <exposed stack-tracing functions ((usm))>=   *)
  fun removeRepeat 0 xs = (0, [], xs)
    | removeRepeat n xs =
        let val header = List.take (xs, n) 
            fun count k xs =
              if (header = List.take (xs, n)) handle Subscript => false then
                count (k + 1) (List.drop (xs, n))
              else
                (k, header, xs)
        in  count 0 xs
        end handle Subscript => (0, [], xs)
  (* Showing the stack trace itself is somewhat trickier *)
  (* than you might think. The issue is that if a *)
  (* Smalltalk program suffers from infinite recursion, *)
  (* the stack will not overflow until it has several *)
  (* thousand active sends. And stack overflow is quite *)
  (* common; because of Smalltalk's dynamic dispatch, it *)
  (* is all too easy to write recursions that don't *)
  (* terminate. Moreover, such recursions often involve *)
  (* two, three, or more methods. When printing such a *)
  (* stack, the tracing functions condense repeated *)
  (* sequences of active sends. This operation is *)
  (* implemented by a team of several functions.  *)
  (*                                              *)
  (* The fundamental operation used to condense a stack is *)
  (* to remove a repeated sequence from the beginning of a *)
  (* list of active sends. Function [[removeRepeat]] takes *)
  (* an argument n and a list xs, considers the first n *)
  (* elements of xs as a block, and removes as many copies *)
  (* of that block as it can. The number of copies removed *)
  (* is k, and [[removeRepeat]] satisfies the following *)
  (* algebraic law: {align*} removeRepeat n xs --- = (k, *)
  (* ys, zs)                                      *)
  (* where  --- xs = (ys)^kzs                     *)
  (* --- length ys = n {align*} An element of the list xs *)
  (* may by of any type that admits equality, which in *)
  (* Standard ML is written with a type variable [[''a]]. *)
  (* <boxed values 78>=                           *)
  val _ = op removeRepeat : int -> ''a list -> int * ''a list * ''a list
  (* <exposed stack-tracing functions ((usm))>=   *)
  fun findRepeat xs n =
    if n > 20 then
      (0, [], xs)
    else
      let val repeat as (k, _, _) = removeRepeat n xs
      in  if k >= 3 then
            repeat
          else
            findRepeat xs (n + 1)
      end
  (* Since [[removeRepeat]] says how many copies of *)
  (* length n it removed (that's k), it can be used to *)
  (* search for a repeat. Function [[findRepeat]] looks *)
  (* for an initial repeated sequence of length n or *)
  (* greater, and removes it. It stops after trying n=20. *)
  (* <boxed values 79>=                           *)
  val _ = op findRepeat : ''a list -> int -> int * ''a list * ''a list
  (* <exposed stack-tracing functions ((usm))>=   *)
  fun findRepeatAfter xs 10 = ([], (0, [], xs))
    | findRepeatAfter xs  m =
        let val (k, header, ys) = findRepeat (List.drop (xs, m)) 1
        in  if k > 0 then
              (List.take(xs, m), (k, header, ys))
            else
              findRepeatAfter xs (m + 1)
        end handle Subscript => ([], (0, [], xs))
  (* \qvfilbreak2in                               *)
  (*                                              *)
  (* Repeated sequences don't always appear at the extreme *)
  (* young end of the stack. Sometimes when the stack is *)
  (* exhausted, there are a few active sends that aren't *)
  (* involved in the infinite recursion. Function *)
  (* [[findRepeatAfter]] searches for a repeated sequence *)
  (* after the first m or more elements of xs (the *)
  (* ``header'') are dropped. It tries m up to 10. *)
  (* It returns the header of length m, paired with *)
  (* whatever repeat is found by [[findRepeat]]. \ *)
  (* nwnarrowboxes                                *)
  (* <boxed values 80>=                           *)
  val _ = op findRepeatAfter : ''a list -> int -> ''a list * (int * ''a list *
                                                                       ''a list)
  (* <exposed stack-tracing functions ((usm))>=   *)
  fun showStackTrace condense =
    if null (!locationStack) then
      ()
    else
      let fun showActiveSend (msg, (file, n), receiver) =
            app xprint ["  In ", file, ", line ", intString n,
                        ", sent `", msg, "` to ", receiver, "\n"]
          val headerAndRepeat =
            if condense then findRepeatAfter (!locationStack) 0
            else ([], (0, [], !locationStack))
          val _ = xprint "Method-stack traceback:\n"
      in  (* The display of a stack depends on whether    *)
          (* [[findRepeatAfter]] finds a header and a repeated *)
          (* sequence. If there's no header and no repeated *)
          (* sequence, all the active sends are shown. Otherwise *)
          (* the stack is shown in three parts: the header, the *)
          (* repeated sequence, and what's left.          *)
          (* <show the (possibly condensed) stack in [[headerAndRepeat]]>= *)
          case headerAndRepeat
            of ([], (0, _, locs)) => app showActiveSend locs 
             | (_,  (0, _, _)) => raise InternalError
                                          "nonempty header with 0-length repeat"
             | (header, (k, repeated, locs)) =>
                  ( app showActiveSend header
                  ; if null header then ()
                    else app xprint [ "    ... loop of size "
                                    , Int.toString (length repeated) ,
                                                                 " begins ...\n"
                                    ]
                  ; app showActiveSend repeated
                  ; app xprint [ "    ... loop of size ", Int.toString (length
                                                                       repeated)
                               , " repeated ", Int.toString k, " times ...\n"
                               ]
                  ; app showActiveSend locs
                  )
      end
  (* To condense any given stack, [[findRepeatAfter]] is *)
  (* called with m=0. The resulting information is stored *)
  (* in variable [[headerAndRepeat]]. If there's no reason *)
  (* to condense the stack, [[headerAndRepeat]] is *)
  (* initialized with an empty header and no repeats. *)
  (* <boxed values 81>=                           *)
  val _ = op showStackTrace : bool -> unit
  (* <exposed stack-tracing functions ((usm))>=   *)
  fun eprintlnTrace s = 
    ( eprintln s
    ; showStackTrace (String.isSubstring "recursion too deep" s
                      orelse String.isSubstring "CPU time exhausted" s)
    ; resetTrace ()
    )
  (* \qbreak A stack is condensed when some sort of *)
  (* resource is exhausted: either stack space    *)
  (* (``recursion too deep'') or the CPU cap.     *)
  (* <boxed values 82>=                           *)
  val _ = op eprintlnTrace  : string -> unit
  (* [[funty]] stand for \tau, [[actualtypes]]    *)
  (* stand for \ldotsntau, and [[rettype]] stand for alpha *)
  (* . The first premise is implemented by a call to *)
  (* [[typesof]] and the second by a call to      *)
  (* [[freshtyvar]]. The constraint is represented just as *)
  (* written in the rule.                         *)

end
(* [*] A primitive is found by looking up its name in *)
(* the association list [[primitives]].         *)
(* <definition of [[primitives]]>=              *)
val primitives = (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("addWithOverflow", withOverflow op + ) ::
                 ("subWithOverflow", withOverflow op - ) ::
                 ("mulWithOverflow", withOverflow op * ) ::
                 (* Hashing                                      *)
                 (*                                              *)
                 (* The [[hash]] primitive works only on symbols. It uses *)
                 (* the hash function of Fowler, Vo, and Noll, from \cref *)
                 (* mlinterps.chap (\cpagerefmlinterps.fnvHash). *)
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("hash", unaryPrim
                              (fn (_, SYM s) => mkInteger (fnvHash s)
                                | v => raise RuntimeError
                                          "hash primitive expects a symbol")) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("sameObject", binaryPrim (mkBoolean o eqRep)) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("isKindOf",   binaryPrim kindOf) ::
                 ("isMemberOf", binaryPrim memberOf) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("class", classPrimitive) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("error", binaryPrim error) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("printSmallInteger", unaryPrim printInt) ::     
                 ("printu",            unaryPrim printu)   ::     
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("+",   arithop op +  )  ::
                 ("-",   arithop op -  )  ::
                 ("*",   arithop op *  )  ::
                 ("div", arithop op div)  ::
                 ("<",   intcompare op <) ::
                 (">",   intcompare op >) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("newSmallInteger", binaryPrim newInteger) ::
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("printSymbol", unaryPrim  printSymbol) ::
                 ("newSymbol",   binaryPrim newSymbol  ) ::
                 (* The functions above are used to define the array *)
                 (* primitives. [*]                              *)
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("arrayNew",    binaryPrim     newArray)   ::
                 ("arraySize",   arrayPrimitive arraySize)  ::
                 ("arrayAt",     arrayPrimitive arrayAt)    ::
                 ("arrayUpdate", arrayPrimitive arrayUpdate) ::
                 (* Block primitives                             *)
                 (*                                              *)
                 (* The only primitive a block needs is [[value]], which *)
                 (* is defined in \usmchunksmall.chunk.valuePrim. *)
                 (*                                              *)
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("value", valuePrim) ::
                 (* Class primitives                             *)
                 (*                                              *)
                 (* The following primitives are used in class objects. *)
                 (* Their implementations appear below.          *)
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("protocol",      classPrim (protocols true)) ::
                 ("localProtocol", classPrim (protocols false)) ::
                 ("newUserObject", newPrimitive) ::
                 ("superclass",    classPrim superclassObject) ::
                 ("className",     classPrim (fn (_, c) => mkSymbol (className c
                                                                          ))) ::
                 ("getMethod",     binaryPrim getMethod) ::
                 ("setMethod",     setMethod o fst) ::
                 ("removeMethod",  binaryPrim removeMethod) ::
                 ("methodNames",   classPrim methodNames) ::
                 (* Methods [[subclassResponsibility]] and       *)
                 (* [[leftAsExercise]] are actually implemented by *)
                 (* primitives.                                  *)
                 (* <primitives for \usmalltalk\ [[::]]>=        *)
                 ("subclassResponsibility",
                     errorPrim
              "subclass failed to implement a method it was responsible for") ::
                 ("leftAsExercise", 
                     errorPrim
                     "method was meant to be implemented as an exercise") :: nil
val () =   if isSome (OS.Process.getEnv "USMPRIM") then      (*OMIT*)
           app (println o fst) primitives else ()   (*OMIT*)
(* <helper functions for evaluation>=           *)
fun findMethod (name, class) =
  let fun fm (subclass as CLASS { methods, super, ...}) =
        find (name, !methods)
        before (if logging then logFind name subclass else ()) (*OMIT*)
        handle NotFound m =>
          case super
            of SOME c => fm c
             | NONE   =>
                 raise RuntimeError (className class ^
                                     " does not understand message " ^ m)
(* <boxed values 4>=                            *)
val _ = op findMethod : name * class -> method
val _ = op fm         : class        -> method
(* [*]                                          *)

  in  fm class
  end
(* <helper functions for evaluation>=           *)
fun optimizedBind (x, v, xi) =
  let val loc = find (x, xi)
  in  (loc := v; xi)
  end handle NotFound _ => bind (x, ref v, xi)
(* The [[class]] and [[value]] representations inform *)
(* the representations of the elements of the   *)
(* abstract-machine state (\crefpage            *)
(* ,small.tab.metavars).                        *)
(*                                              *)
(*   • An expression e or definition d is represented in *)
(*  the usual way by a constructed value from   *)
(*  algebraic data type [[exp]] or [[def]], as  *)
(*  defined below.                              *)
(*   • An environment rho or xi is represented in the *)
(*  usual way by an ML environment of type \monobox *)
(*  value ref env.                              *)
(*   • A superclass \superclass is represented by an *)
(*  ML value of type [[class]].                 *)
(*   • A stack frame \aframe is represented by an *)
(*  ML value of type [[frame]]. The definition of *)
(*  [[frame]] isn't important; it's enough to know *)
(*  that a new frame can be allocated by calling *)
(*  [[newFrame]], and that a frame is equal only to *)
(*  itself.                                     *)
(*   • The store sigma and the set of used frames \ *)
(*  usedframes are both represented by mutable state *)
(*  of the ML program. Just as in the other     *)
(*  interpreters, sigma and sigma' never coexist; *)
(*  instead, the interpreter updates its state, in *)
(*  effect replacing sigma by sigma'. The set \ *)
(*  usedframes is updated to \usedframes' in the same *)
(*  way.                                        *)
(*                                              *)
(* The [[value]] and [[frame]] types are also used to *)
(* represent behaviors.                         *)
(*                                              *)
(*   • The behavior of producing a value, which is *)
(*  described by judgment form \nomathbreak<e, \dots> *)
(*  ==>\usmevalr['] v, is represented by an ML  *)
(*  computation that produces a value v of ML type *)
(*  [[value]], while writing sigma' and \usedframes' *)
(*  over the previous sigma and \usedframes as a side *)
(*  effect.                                     *)
(*                                              *)
(*   • The behavior of a uSmalltalk [[return]], which is *)
(*  described by judgment form \nomathbreak<e, \dots> *)
(*  \returns\usmevalret['] v \aframe', is represented *)
(*  by an ML computation that raises the ML     *)
(*  [[Return]] exception, again writing sigma' and \ *)
(*  usedframes' as a side effect. The [[Return]] *)
(*  exception is defined as follows:            *)
(* <definition of the [[Return]] exception>=    *)
exception
  Return of { value : value, to : frame, unwound : active_send list }
(* The values of Typed uScheme are the same as the *)
(* values of micro-Scheme; adding a type system doesn't *)
(* change the representation used at run time.  *)

(* <evaluation, [[basis]], and [[processDef]] for \usmalltalk>= *)
fun eval (e, rho, superclass, F, xi) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let (* <definition of function [[invokeMethod]]>=   *)
      fun invokeMethod ({ name, superclass, formals, locals, body },
                      receiver, vs, Fhat) =
            let val ivars  = instanceVars receiver
                val args   = mkEnv (formals, map ref vs)
                val locals = mkEnv (locals,  map (fn _ => ref nilValue) locals)
            in  eval (body, ivars <+> args <+> locals, superclass, Fhat, xi)
            end
            handle BindListLength => raise InternalError
                                             "bad arity in user method" (*OMIT*)
      (* The second part of the \rulenameSend rule is *)
      (* implemented by function [[invokeMethod]]. Function *)
      (* [[invokeMethod]] computes rho_i as [[ivars]], rho_a *)
      (*  as [[args]], and rho_l as [[locals]]. It also *)
      (* allocates and initializes locations \ldotsn l and \ *)
      (* ldotskl', then calls [[eval]].               *)
      (* <boxed values 3>=                            *)
      val _ = op invokeMethod   : method * value * value list * frame -> value
      (* [*]                                          *)

      (* Internal function [[ev]] handles all the syntactic *)
      (* forms, the most interesting of which are [[RETURN]] *)
      (* and [[SEND]].                                *)
      (*                                              *)
      (* Evaluating returns and sends                 *)
      (*                                              *)
      (* A [[RETURN]] evaluates the expression to be returned, *)
      (* then returns to frame [[F]] by raising the [[Return]] *)
      (* exception.                                   *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
      fun ev (RETURN e) = raise Return { value = ev e, to = F, unwound = [] }
      (* That [[Return]] exception is caught by the code that *)
      (* interprets message send. The [[SEND]] code carries a *)
      (* lot of freight: it implements most of rules \rulename *)
      (* Send, \rulenameSendSuper, \rulenameReturnTo, and \ *)
      (* rulenameReturnPast, and it also supports diagnostic *)
      (* tracing. The send and return rules all follow the *)
      (* same outline; for reference, that outline is *)
      (* formalized by the first, highlighted part of the \ *)
      (* rulenameSend rule: Each [[SEND]] computation begins *)
      (* in the same way: evaluate the receiver and the *)
      (* arguments using [[ev]], then use the syntax of the *)
      (* receiver to identify the class on which method search *)
      (* begins. Message send dispatches on the receiver, *)
      (* whose class is used to find the method that defines  *)
      (* [[message]], except when the message is sent to *)
      (* [[super]], in which case the [[superclass]] of the *)
      (* currently running method is used. At that point, *)
      (* because of tracing and returns, things start to get *)
      (* complicated, so let's look at the code, then focus on *)
      (* the anonymous function passed to [[trace]]: [*] *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (SEND (srcloc, receiver, msgname, args))  =
            let val obj as (class, rep) = ev receiver
                val vs = map ev args
                val _ = if logging then logSend srcloc msgname else () (*OMIT*)
                val startingClass =
                      case receiver of SUPER => superclass | _ => class
                val checkpoint = checkpointLimit ()  (*OMIT*)
                (* <definition of function [[trace]]>=          *)
                fun trace action =
                  let val (file, line) = srcloc
                (* Functions [[traceIndent]] and [[outdentTrace]] are *)
                (* Curried; each returns a partial application of *)
                (* [[tracePrint]]. This partial application, when *)
                (* applied to its final argument, prints strings *)
                (* produced by that argument when and only when tracing *)
                (* is enabled.                                  *)
                (*                                              *)
                (* \qvfilbreak2.8in                             *)
                (*                                              *)
                (* The [[trace]] function itself is given an [[action]] *)
                (* with which to perform the send; [[action]] is run by *)
                (* applying it to the empty tuple. If tracing is *)
                (* enabled, [[trace]] emits two tracing messages: one *)
                (* before and one after running the action. The printing *)
                (* is done conditionally by [[traceIndent]] and *)
                (* [[outdentTrace]], which also (unconditionally) *)
                (* maintain the stack of active sends.          *)
                (* <boxed values 75>=                           *)
                val _ = op trace : (unit -> value) -> value
                      val c  = className startingClass
                      val objString = if String.isPrefix "class " c then c
                                      else "an object of class " ^ c
                      val () = 
                        traceIndent (msgname, (file, line), objString) xi (fn ()
                                                                              =>
                          [file, ", line ", intString line, ": ",
                                                            "Sending message (",
                           spaceSep (msgname :: map valueString vs), ")", " to "
                                                                   , objString])
                      fun traceOut answer =
                        answer before
                        outdentTrace xi (fn () =>
                           [file, ", line ", intString line, ": ",
                            "(", spaceSep (valueString obj :: msgname :: map
                                                           valueString vs), ")",
                            " = ", valueString answer])

                      fun traceReturn r =
                        ( outdentTrace xi (fn () =>
                             [file, ", line ", intString line, ": ",
                              "(", spaceSep (valueString obj :: msgname :: map
                                                           valueString vs), ")",
                                " terminated by return"])
                        ; raise Return r
                        )

                  in  traceOut (action ()) handle Return r => traceReturn r
                  end
            in  trace
                (fn () =>
                   let val imp  = findMethod (msgname, startingClass)
                       val Fhat = newFrame ()
                   in  invokeMethod (imp, obj, vs, Fhat)
                       handle Return { value = v, to = F', unwound = unwound }
                                                                              =>
                         if F' = Fhat then
                           v
                           before restoreLimit checkpoint (*OMIT*)
                         else

                        (* Unwound frames are added to the list when [[eval]] *)

                          (* catches the [[Return]] exception and unwinds the *)
                           (* frame [[this]].                              *)

(* <reraise [[Return]], adding [[msgname]], [[class]], and [[loc]] to [[unwound]]>= *)
                           let val this = { method = msgname, class = className
                                                           class, loc = srcloc }
                           in  raise Return { value = v, to = F', unwound = this
                                                                    :: unwound }
                           end
                   end)
            end
      (* Allocating and evaluating blocks             *)
      (*                                              *)
      (* Evaluating a [[BLOCK]] form captures the current *)
      (* environment, superclass, and stack frame in a *)
      (* closure.                                     *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (BLOCK (formals, body)) = mkBlock (formals, body, rho, superclass,
                                                                              F)
      (* Evaluating literal and value forms           *)
      (*                                              *)
      (* A [[LITERAL]] form represents a literal integer or *)
      (* symbol, and it is evaluated by calling [[mkInteger]] *)
      (* or [[mkSymbol]]. These functions cannot be called *)
      (* safely until after the initial basis has been read *)
      (* and the interpreter has been bootstrapped (\crefpage *)
      (* ,small.bootstrapping-literals); for that reason, *)
      (* integer and symbol literals in the initial basis may *)
      (* appear only inside method definitions.       *)
      (*                                              *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (LITERAL c) = 
            (case c of NUM n => mkInteger n
                     | SYM s => mkSymbol s
                     | _ => raise InternalError "unexpected literal")
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (VALUE v) = v
      (* Reading and writing variables                *)
      (*                                              *)
      (* The [[VAR]] and [[SET]] forms are evaluated as we *)
      (* would expect; they use the local and global  *)
      (* environments in the same way as Impcore.     *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (VAR x) = !(find (x, rho) handle NotFound _ => find (x, xi))
        | ev (SET (x, e)) =
            let val v = ev e
                val cell = find (x, rho) handle NotFound _ => find (x, xi)
            in  cell := v; v
            end 
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (SUPER) = ev (VAR "self")
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, nilValue)
            end
      (* Evaluating primitives                        *)
      (*                                              *)
      (* Each uSmalltalk primitive is implemented by a *)
      (* function that expects a list of values and a global *)
      (* environment xi. (The global environment is used only *)
      (* by the [[value]] primitive, which calls      *)
      (* [[applyClosure]].) \zbreak The primitives are stored *)
      (* on the association list [[primitives]] list (\cref *)
      (* usma.chap, \cpagerefusma.primitives), and a  *)
      (* [[PRIMITIVE]] form is evaluated by looking up the *)
      (* associated function on the list, then applying that *)
      (* function to the values of the arguments.     *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (PRIMITIVE (p, args)) =
            let val f = find (p, primitives)
                        handle NotFound n =>
                          raise RuntimeError ("There is no primitive named " ^ n
                                                                               )
            in  f (map ev args, xi)
            end
      (* Compiled methods                             *)
      (*                                              *)
      (* A [[compiled-method]] is evaluated without actual *)
      (* compilation; its formal parameters, local variables, *)
      (* body, and current superclass go into an object. *)
      (* <function [[ev]], the evaluator proper ((usm))>= *)
        | ev (METHOD (xs, ys, es)) =
            mkCompiledMethod { name = "", formals = xs, locals = ys
                             , body = BEGIN es, superclass = objectClass }
(* <boxed values 2>=                            *)
val _ = op eval: exp * value ref env * class * frame * value ref env -> value
val _ = op ev  : exp -> value
  in  ev e
  end
(* <evaluation, [[basis]], and [[processDef]] for \usmalltalk>= *)
fun applyClosure ((formals, body, rho_c, superclass, frame), vs, xi) =
  eval (BEGIN body, rho_c <+> mkEnv (formals, map ref vs), superclass,
        frame, xi)
  handle BindListLength => 
    raise RuntimeError ("wrong number of arguments to block; expected " ^
                        "(<block> " ^ valueSelector formals ^ " " ^
                        spaceSep formals ^ ")")
(* Once [[eval]] is defined, [[applyClosureRef]] can be *)
(* initialized properly, to a function that implements *)
(* this rule: [*]                               *)
(* <boxed values 5>=                            *)
val _ = op applyClosure : closure * value list * value ref env -> value
val () = applyClosureRef := applyClosure
(* <evaluation, [[basis]], and [[processDef]] for \usmalltalk>= *)
fun evaldef (d, xi) =
  let fun ev e = eval (e, emptyEnv, objectClass, noFrame, xi)
                 (* <handle unexpected [[Return]] in [[evaldef]]>= *)
                 handle Return { value = v, unwound = unwoundFrames, ... } =>
                   if null unwoundFrames then
                     raise RuntimeError
                       ("tried to (return " ^ valueString v ^
                                           ") from an activation that has died")
                   else
                     raise RuntimeError (
                       "tried to return from an activation that has died:\n  " ^
                                         String.concatWith "\n  " (map
                                                activeSendString unwoundFrames))
      val (x, v) =
        case d
          of VAL (name, e)             => (name, ev e)
           | EXP e                     => ("it", ev e)
           | DEFINE (name, args, body) => (name, ev (BLOCK (args, [body])))
           | CLASSD (d as {name, ...}) => (name, newClassObject d xi)
      val xi' = optimizedBind (x, v, xi)
      val _ = saveLiteralClasses xi' handle NotFound _ => ()  (*OMIT*)
(* Evaluating definitions                       *)
(*                                              *)
(* Most definitions are evaluated more or less as in *)
(* other interpreters, but class definitions require a *)
(* lot of special-purpose code for creating classes. *)
(*                                              *)
(* Function evaldef, for evaluating definitions *)
(*                                              *)
(* Evaluating a definition computes a new global *)
(* environment, and it also has a side effect on the *)
(* state of the interpreter. \usmflabelevaldef  *)
(* <boxed values 6>=                            *)
val _ = op evaldef : def * value ref env -> value ref env * value
val _ = op ev      : exp -> value
  in  (xi', v)
  end
(* Processing definitions and building the \    *)
(* chaptocsplitinitial basis                    *)
(*                                              *)
(* In uSmalltalk, as in micro-Scheme, every name stands *)
(* for a mutable location that holds a value, so basis *)
(* is simply a global environment.              *)
(* <evaluation, [[basis]], and [[processDef]] for \usmalltalk>= *)
type basis = value ref env
(* Extended definitions are evaluated using the reusable *)
(* code presented in \crefmlscheme.chap. Like   *)
(* micro-Scheme, uSmalltalk works with a single *)
(* top-level environment, which maps each name to a *)
(* mutable location holding a value. ``Processing'' a *)
(* definition means evaluating it, then showing the *)
(* result by sending [[println]] to the defined value. *)
(* The default [[println]] method calls the object's *)
(* [[print]] method, which you can redefine.    *)
(* <evaluation, [[basis]], and [[processDef]] for \usmalltalk>= *)
fun processDef (d, xi, interactivity) =
  let val (xi', v) = evaldef (d, xi)
      val _ = if echoes interactivity then 
                ignore (eval (SEND (nullsrc, VALUE v, "println", []),
                              emptyEnv, objectClass, noFrame, xi'))
              else
                ()
  in  xi'
  end
fun dump_names basis = app (println o fst) basis  (*OMIT*)
(* <shared unit-testing utilities>=             *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* <boxed values 107>=                          *)
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
(* <definition of [[testIsGood]] for \usmalltalk>= *)
fun testIsGood (test, xi) =
  let fun ev e = eval (e, emptyEnv, objectClass, noFrame, xi)
      fun outcome e = withHandlers (OK o ev) e (ERROR o stripAtLoc)
                      before resetTrace ()
      fun testEquals (v1, v2) =
        let val areSimilar = ev (SEND (nullsrc, VALUE v1, "=", [VALUE v2]))
        in  eqRep (areSimilar, mkBoolean true)
        end
      fun printsAs v =
        let val (bprint, contents) = bprinter ()
            val _ = withXprinter bprint ev (SEND (nullsrc, VALUE v, "print", [])
                                                                               )
        in  contents ()
        end
      fun valueString _ =
        raise RuntimeError "internal error: called the wrong ValueString"
      (* In case of a test failure, function [[printsAs]] is *)
      (* used to show what was expected.              *)

(* <definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]]>= *)
      fun whatWasExpected (LITERAL (NUM n), _) = printsAs (mkInteger n)
        | whatWasExpected (LITERAL (SYM x), _) = printsAs (mkSymbol x)
        | whatWasExpected (e, OK v) =
            concat [printsAs v, " (from evaluating ", expString e, ")"]
        | whatWasExpected (e, ERROR _) =
            concat ["the result of evaluating ", expString e]
      (* \qvfilbreak1.8in                             *)
      (*                                              *)
      (* Once printing is sorted out, the implementations of *)
      (* the unit-test forms are similar to the       *)
      (* implementations of the same forms in other   *)
      (* interpreters.                                *)

(* <definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]]>= *)
      val cxfailed = "check-expect failed: "
      fun checkExpectPasses (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               (case withHandlers (OK o testEquals)
                                  (check, expect)
                                  (ERROR o stripAtLoc)
                  of OK true => true
                   | OK false =>
                       failtest [cxfailed, "expected ", expString checkx, 
                                 " to be similar to ",
                                 whatWasExpected (expectx, OK expect),
                                 ", but it's ", printsAs check]
                   | ERROR msg =>
                       failtest [cxfailed, "testing equality of ",
                                 expString checkx, " to ",
                                 expString expectx, " caused error ", msg])
           | (ERROR msg, tried) =>
               failtest [cxfailed, "evaluating ", expString checkx,
                         " caused error ", msg]
           | (_, ERROR msg) =>
               failtest  [cxfailed, "evaluating ", expString expectx,
                          " caused error ", msg]

(* <definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]]>= *)
      val cafailed = "check-assert failed: "
      fun checkAssertPasses checkx =
        case outcome checkx
          of OK check =>
               eqRep (check, mkBoolean true) orelse
               failtest [cafailed, "expected assertion ", expString checkx,
                         " to hold, but it doesn't"]
           | ERROR msg =>
               failtest [cafailed, "evaluating ", expString checkx,
                         " caused error ", msg]

(* <definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]]>= *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, "expected evaluating ", expString checkx,
                             " to cause an error, but evaluation produced ",
                             printsAs check]
      (* <definition of [[checkPrintPasses]]>=        *)
      val cpfailed = "check-print failed: "
      fun checkPrintPasses (checkx, s) =
        case outcome checkx 
          of OK check =>
               (case withHandlers (OK o printsAs) check (ERROR o stripAtLoc)
                  of OK s' =>
                       s = s' orelse
                       failtest [cpfailed, "expected \"", s,
                                 "\" but got \"", s', "\""]
                   | ERROR msg =>
                       failtest [cpfailed, "calling print method on ",
                                 expString checkx, " caused error ", msg])
           | ERROR msg =>
               failtest [cpfailed, "evaluating ", expString checkx,
                         " caused error ", msg]
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ASSERT c)      = checkAssertPasses c
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
        | passes (CHECK_PRINT (c, s))  = checkPrintPasses  (c, s)
  in  passes test
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
(* <boxed values 108>=                          *)
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
      (* <boxed values 135>=                          *)
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
(* <boxed values 134>=                          *)
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
(* <boxed values 134>=                          *)
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
(*   IMPLEMENTATIONS OF \USMALLTALK\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* The first entries in the initial basis are the *)
(* primitive classes.                           *)
(* <implementations of \usmalltalk\ primitives and definition of [[initialBasis]]>= *)
val initialXi = emptyEnv

fun addClass (c, xi) = bind (className c, ref (classObject c), xi)
val initialXi =
  foldl addClass initialXi [ objectClass, nilClass, classClass, metaclassClass ]
(* \makenowebnotdef (from chunk \upshape[->]) The next *)
(* entries are the predefined classes. To help with *)
(* debugging, I define function [[errmsg]] to identify *)
(* an error as originating in a predefined class and to *)
(* use [[eprintlnTrace]] instead of [[eprintln]], so *)
(* that if an error occurs, a stack trace is printed.  *)
(* [*]                                          *)
(* <implementations of \usmalltalk\ primitives and definition of [[initialBasis]]>= *)
val predefs = 
               [ ";  The definitions contain no conditionals; decisions "
               , ";  are made entirely by method dispatch. For example, "
               , ";  when [[isNil]] is sent to [[nil]], [[nil]] is an "
               , ";  instance of class [[UndefinedObject]], so the message "
               , ";  dispatches to [[UndefinedObject]]'s [[isNil]] method, "
               , ";  which answers [[true]]. But when [[isNil]] is sent to "
               , ";  any other object, method search starts in the class "
               , ";  of that object and eventually reaches class  "
               , ";  [[Object]], where it dispatches to [[Object]]'s "
               , ";  [[isNil]] method, which answers [[false]]. A  "
               , ";  [[notNil]] message works the same way.       "
               , ";                                               "
               , ";  If you take object-oriented programming seriously, "
               , ";  you will never use an explicit conditional if you can "
               , ";  achieve the same effect using method dispatch. Method "
               , ";  dispatch is preferred because it is extensible: "
               , ";  to add new cases, you just add new classes—think, "
               , ";  for example, about adding new kinds of shapes to the "
               , ";  pictures in \\crefsmall.intro. To add new cases to a "
               , ";  conditional, you would have to edit the code—at every "
               , ";  location where a similar conditional decision is "
               , ";  made. Method dispatch makes it easier to evolve the "
               , ";  code. And in many implementations, it is also more "
               , ";  efficient.                                   "
               , ";                                               "
               , ";  [*] Some conditionals can't be avoided. For example, "
               , ";  to know if a number [[n]] is at least 10, we must "
               , ";  send a conditional message like [[ifTrue:]] to the "
               , ";  Boolean object produced by \\monobox(n >= 10). But "
               , ";  [[ifTrue:]] is itself implemented using method "
               , ";  dispatch! Smalltalk's conditionals and loops aren't "
               , ";  written using syntactic forms like micro-Scheme's "
               , ";  [[if]] and [[while]], because Smalltalk has no such "
               , ";  forms—it has only message passing and [[return]]. "
               , ";  Conditionals are implemented by sending continuations "
               , ";  to Boolean objects, and loops are implemented by "
               , ";  sending continuations to block objects. [This "
               , ";  implementation of conditionals is the same one used "
               , ";  in the \\emph{Church encoding} of Booleans in the \\ "
               , ";  emph{lambda calculus}---a~tool used in the   "
               , ";  theoretical study of programming languages.] "
               , ";  <predefined uSmalltalk classes and values ((elided))>= "
               , ";  <definition of class [[Boolean]]>=           "
               , "(class Boolean "
               , "    [subclass-of Object]"
               , "    (method ifTrue:ifFalse: (trueBlock falseBlock)"
               ,
            "                                    (self subclassResponsibility))"
               , "    (method ifFalse:ifTrue: (falseBlock trueBlock) "
               ,
            "                                    (self subclassResponsibility))"
               ,
            "    (method ifTrue:  (trueBlock)    (self subclassResponsibility))"
               ,
            "    (method ifFalse: (falseBlock)   (self subclassResponsibility))"
               , ""
               ,
            "    (method not ()                  (self subclassResponsibility))"
               ,
            "    (method eqv: (aBoolean)         (self subclassResponsibility))"
               ,
            "    (method xor: (aBoolean)         (self subclassResponsibility))"
               ,
            "    (method & (aBoolean)            (self subclassResponsibility))"
               ,
            "    (method | (aBoolean)            (self subclassResponsibility))"
               , ""
               ,
            "    (method and: (alternativeBlock) (self subclassResponsibility))"
               ,
            "    (method or:  (alternativeBlock) (self subclassResponsibility))"
               , ")"
               , ";  <predefined uSmalltalk classes and values>= "
               , "(class True "
               , "  [subclass-of Boolean]"
               ,
           "  (method ifTrue:         (trueBlock)            (trueBlock value))"
               , "  (method ifFalse:        (falseBlock)           nil)"
               ,
           "  (method ifTrue:ifFalse: (trueBlock falseBlock) (trueBlock value))"
               ,
           "  (method ifFalse:ifTrue: (falseBlock trueBlock) (trueBlock value))"
               , ""
               , "  (method not ()                  false)"
               , "  (method & (aBoolean)            aBoolean)"
               , "  (method | (aBoolean)            self)"
               , "  (method eqv: (aBoolean)         aBoolean)"
               , "  (method xor: (aBoolean)         (aBoolean not))"
               , ""
               , "  (method and: (alternativeBlock) (alternativeBlock value))"
               , "  (method or:  (alternativeBlock) self)"
               , ")"
               , ";  The implementation of class [[False]], which is "
               , ";  similar, is left as \\crefsmall.ex.False.     "
               , ";  The ingenious division of class [[Boolean]] into "
               , ";  subclasses [[True]] and [[False]] is owed to Dan "
               , ";  Ingalls \\citeyearparingalls:evolution. [*]   "
               , ";  <predefined uSmalltalk classes and values ((elided))>= "
               , "(class False"
               , "  [subclass-of Boolean]"
               ,
          "  (method ifTrue:ifFalse: (trueBlock falseBlock) (falseBlock value))"
               ,
          "  (method ifFalse:ifTrue: (falseBlock trueBlock) (falseBlock value))"
               , "  (method ifTrue:  (trueBlock)    nil)"
               , "  (method ifFalse: (falseBlock)   (falseBlock value))"
               , "  (method not ()                  true)"
               , "  (method eqv: (aBoolean)         (aBoolean not))"
               , "  (method xor: (aBoolean)         aBoolean)"
               , "  (method & (aBoolean)            self)"
               , "  (method | (aBoolean)            aBoolean)"
               , "  (method and: (alternativeBlock) self)"
               , "  (method or:  (alternativeBlock) (alternativeBlock value))"
               , ")"
               , ";  The predefined classes are sufficiently complicated "
               , ";  that they need a little organization of their own. "
               , ";  They include both numeric and collection classes. "
               , ";  <predefined uSmalltalk classes and values>= "
               , ";  Technique II: \\chaptocsplitAbstract classes  "
               , ";                                               "
               , ";  [*] Object-oriented code can achieve a form of "
               , ";  polymorphism not readily available in languages like "
               , ";  Scheme and ML: By relying on subclass        "
               , ";  responsibilities, methods like [[min:]] and [[max:]] "
               , ";  (for magnitudes) or [[detect:]] and [[select:]] "
               , ";  (for collections) can be implemented once and reused "
               , ";  by many different subclasses. Such reuse is  "
               , ";  illustrated below with examples from magnitudes and "
               , ";  collections. The collection example is then developed "
               , ";  further by showing how subclasses for keyed and "
               , ";  sequenceable collections refine and extend the "
               , ";  [[Collection]] protocol. This is a great technique to "
               , ";  emulate in your own designs.                 "
               , ";                                               "
               , ";  Implementing wide protocols: \\               "
               , ";  chaptocbacksplitMagnitudes and \\rlapheadercollections "
               , ";                                               "
               , ";  [*] The [[Magnitude]] protocol suits any abstraction "
               , ";  that is totally ordered, even those that do not "
               , ";  support arithmetic: numbers, dates, times, and so on. "
               , ";  A subclass of [[Magnitude]] has only two     "
               , ";  responsibilities: comparisons [[=]] and [[<]]. "
               , ";  [According to the rules of \\smalltalk, a~magnitude "
               , ";  may not inherit the default implementation of~[[=]] "
               , ";  from class [[Object]], which is object identity. "
               , ";  That's why method [[=]] is redefined as a subclass "
               , ";  responsibility.] \\basislabelMagnitude        "
               , ";  <numeric classes>=                           "
               , "(class Magnitude"
               , "    [subclass-of Object] ; abstract class"
               ,
            "    (method = (x) (self subclassResponsibility)) ; may not inherit"
               , "    (method < (x) (self subclassResponsibility))"
               , ";      The other comparisons, as well as [[min:]] and "
               , ";      [[max:]], are implemented using [[<]], and they can "
               , ";      be reused in every subclass.                 "
               , ";      <other methods of class [[Magnitude]]>=      "
               , "    (method >  (y) (y < self))"
               , "    (method <= (x) ((self > x) not))"
               , "    (method >= (x) ((self < x) not))"
               , "    (method min: (aMagnitude)"
               ,
             "       ((self < aMagnitude) ifTrue:ifFalse: {self} {aMagnitude}))"
               , "    (method max: (aMagnitude)"
               ,
             "       ((self > aMagnitude) ifTrue:ifFalse: {self} {aMagnitude}))"
               , ")"
               , ";  <numeric classes ((elided))>=                "
               , ";  Instance methods [[select:]] and [[collect:]] and "
               , ";  class method [[withAll:]] are left as \\cref  "
               , ";  small.ex.array-select,small.ex.Array.withAll:, and "
               , ";  the rest of [[Array]] is relegated to \\crefusma.chap. "
               , ";                                               "
               , ";  {\\normalsize Technique III: Multiple representations "
               , ";  the object-oriented way}                     "
               , ";                                               "
               , ";  [*] [*]                                      "
               , ";                                               "
               , ";  Smalltalk's collection abstractions are relatively "
               , ";  easy to implement, in part because a typical "
               , ";  operation involves only one collection: the receiver. "
               , ";  And even an operation that takes a collection as "
               , ";  argument doesn't have to look at the argument's "
               , ";  representation; for example, methods [[addAll:]] and "
               , ";  [[removeAll:]] simply use [[do:]] to iterate over the "
               , ";  argument's elements. But in some other abstractions, "
               , ";  like the leftist heap from \\crefmcl.chap (\\cpageref "
               , ";  mcl.leftist), an operation like heap merge does have "
               , ";  to look at the representation of an argument. Such an "
               , ";  operation is called complex \\citep           "
               , ";  cook:data-abstraction-revisited. And in a pure "
               , ";  object-oriented language like Smalltalk, complex "
               , ";  operations are not so easy to implement.     "
               , ";                                               "
               , ";  We'll study complex operations on numbers (\\cpageref "
               , ";  small.proto.Number), which have to look at the "
               , ";  representations of two numbers. For example, "
               , ";                                               "
               , ";    • Operation [[<]] on fractions needs to look at the "
               , ";   numerators and denominators of both fractions. "
               , ";    • Operation [[*]] on floating-point numbers needs "
               , ";   to look at the mantissas and exponents of both "
               , ";   numbers.                                    "
               , ";                                               "
               , ";  In a language that uses abstract data types, like \\ "
               , ";  mcl, complex operations like these are easy to "
               , ";  implement: if an operation can see the definition of "
               , ";  a type, it can see the representation of every "
               , ";  argument of that type. But in a pure object-oriented "
               , ";  language, like Smalltalk, a complex method isn't so "
               , ";  easy to implement: it can see only the representation "
               , ";  of the receiver, and all it can do with an argument "
               , ";  is send messages to it. To figure out what messages "
               , ";  to send, we have several options:            "
               , ";                                               "
               , ";    • We can extend the argument's protocol with new "
               , ";   messages that provide access to its         "
               , ";   representation. This technique is illustrated "
               , ";   using classes [[Number]], [[Integer]], and  "
               , ";   especially [[Fraction]] (\\cref              "
               , ";   small.imp.Number,small.imp.Fraction).       "
               , ";    • If we don't know the argument's representation, "
               , ";   we might not know what new messages it can  "
               , ";   respond to. In such a case, we can have the "
               , ";   receiver send a message to the argument telling "
               , ";   the argument what new messages the receiver can "
               , ";   respond to. This technique, called double   "
               , ";   dispatch, is illustrated using the integer  "
               , ";   classes (\\crefsmall.double-dispatch).       "
               , ";    • \\ztrim-3 We can coerce one object to have the "
               , ";   same representation as another. For example, to "
               , ";   add [[aNumber]] to a fraction, we can coerce "
               , ";   [[aNumber]] to a fraction. This technique is "
               , ";   illustrated using the [[Fraction]] and      "
               , ";   [[Integer]] classes (\\crefsmall.coercion).  "
               , ";                                               "
               , ";  All three techniques can work with any       "
               , ";  representation. To help you integrate them into your "
               , ";  own programming, I recommend a case study for which "
               , ";  there is more than one reasonable representation: "
               , ";  arithmetic on natural numbers. A natural number "
               , ";  should be represented as a sequence of digits, and "
               , ";  every representation of that sequence suggests its "
               , ";  own set of new messages that are analogous to the new "
               , ";  messages defined on class [[Fraction]] (\\cref "
               , ";  small.imp.Natural).                          "
               , ";                                               "
               , ";  [                                            "
               , ";                                               "
               , ";  Context: Abstract classes [[Number]] and [[Integer]]] "
               , ";  A context for complex operations: Abstract classes "
               , ";  [[Number]] and [[Integer]]                   "
               , ";                                               "
               , ";  [*]                                          "
               , ";                                               "
               , ";  Before studying complex operations on fractions, "
               , ";  we look at the context in which fractions are "
               , ";  defined. A fraction is a number, and abstract class "
               , ";  [[Number]] (\\figrefpagesmall.Number) defines two "
               , ";  groups of subclass responsibilities: arithmetic "
               , ";  methods ([[+]], [[*]], [[negated]], and      "
               , ";  [[reciprocal]]) and coercion methods ([[asInteger]], "
               , ";  [[asFraction]], [[asFloat]], and [[coerce:]]). \\ "
               , ";  onerealbasislabelNumber                      "
               , ";  <definition of class [[Number]]>=            "
               , "(class Number"
               , "    [subclass-of Magnitude]  ; abstract class"
               , "    ;;;;;;; arithmetic"
               , "    (method +   (aNumber)     (self subclassResponsibility))"
               , "    (method *   (aNumber)     (self subclassResponsibility))"
               , "    (method negated    ()     (self subclassResponsibility))"
               , "    (method reciprocal ()     (self subclassResponsibility))"
               , "    "
               , "    ;;;;;;; coercion"
               , "    (method asInteger  ()     (self subclassResponsibility))"
               , "    (method asFraction ()     (self subclassResponsibility))"
               , "    (method asFloat    ()     (self subclassResponsibility))"
               , "    (method coerce: (aNumber) (self subclassResponsibility))"
               , ";      Subclasses of [[Number]] must also implement subclass "
               , ";      responsibilities for magnitudes: methods [[=]] and  "
               , ";      [[<]]. Methods [[=]], [[<]], [[+]], [[*]], and "
               , ";      [[coerce:]] take another [[Number]] as argument, and "
               , ";      all except [[coerce:]] turn out to be complex. "
               , ";                                                   "
               , ";      To get a feel for the class, let's see how subclass "
               , ";      responsibilities are used to implement other parts of "
               , ";      the [[Number]] protocol (\\cpageref           "
               , ";      small.proto.Number). Arithmetic methods are  "
               , ";      implemented on top of subclass arithmetic methods, "
               , ";      and sign tests are implemented on top of comparison "
               , ";      and coercion:                                "
               , ";      <other methods of class [[Number]]>=         "
               , "    (method -  (y) (self + (y  negated)))"
               ,
"    (method abs () ((self isNegative) ifTrue:ifFalse: {(self negated)} {self}))"
               , "    (method /  (y) (self * (y reciprocal)))"
               , ""
               , "    (method isZero             () (self  = (self coerce: 0)))"
               , "    (method isNegative         () (self  < (self coerce: 0)))"
               , "    (method isNonnegative      () (self >= (self coerce: 0)))"
               , "    (method isStrictlyPositive () (self  > (self coerce: 0)))"
               , ";      Remaining methods of Number: powers and roots "
               , ";                                                   "
               , ";      Numbers can be squared or raised to other integer "
               , ";      powers. Method [[squared]] is easy. Method   "
               , ";      [[raisedToInteger:]] computes x^n using a standard "
               , ";      algorithm that requires O(logn) multiplications. The "
               , ";      algorithm has two base cases, for x^0 and x^1. "
               , ";      <other methods of class [[Number]]>=         "
               , "    (method squared () (self * self))"
               , "    (method raisedToInteger: (anInteger)"
               , "        ((anInteger = 0) ifTrue:ifFalse:"
               , "            {(self coerce: 1)}"
               , "            {((anInteger = 1) ifTrue:ifFalse: {self}"
               ,
      "                {(((self raisedToInteger: (anInteger div: 2)) squared) *"
               ,
          "                    (self raisedToInteger: (anInteger mod: 2)))})}))"
               , ";      Numbers can also have their square roots taken. [*] "
               , ";      My implementation uses Newton-Raphson iteration. "
               , ";      Given input n, this algorithm uses an initial "
               , ";      approximation x_0 = 1 and improves it stepwise. "
               , ";      At step i, the improved approximation is x_i = (x_i-1 "
               , ";      + n/x_i-1) /2. To know when to stop improving, the "
               , ";      algorithm needs a convergence condition, which "
               , ";      examines x_i and x_i-1 and says when they are close "
               , ";      enough to accept x_i as the answer. [The idea is that "
               ,
                ";      if $x_{i} \\approx x_{i-1}$, \\( x_{i} = (x_{i-1} + n/ "
               , ";      x_{i-1}) / 2 \\approx (x_{i} + n/x_{i}) / 2 \\), and "
               , ";      solving yields $x_{i} \\approx \\sqrt{n}$.]    "
               , ";      My convergence condition is |x_i-x_i-1| < epsilon. "
               , ";      The default epsilon used in [[sqrt]] is 1/100. Using "
               , ";      [[coerce:]] ensures that the same [[sqrt]] method can "
               , ";      be used for both fractions and floats.       "
               , ";      <other methods of class [[Number]]>=         "
               ,
              "    (method sqrt () (self sqrtWithin: (self coerce: (1 / 100))))"
               , "    (method sqrtWithin: (epsilon) [locals two x<i-1> x<i>]"
               , "        ; find square root of receiver within epsilon"
               , "        (set two    (self coerce: 2))"
               , "        (set x<i-1> (self coerce: 1))"
               , "        (set x<i>   ((x<i-1> + (self / x<i-1>)) / two))"
               , "        ({(((x<i-1> - x<i>) abs) > epsilon)} whileTrue:"
               , "               {(set x<i-1> x<i>)"
               ,
               "                (set x<i> ((x<i-1> + (self / x<i-1>)) / two))})"
               , "        x<i>)"
               , ")"
               , ";  <numeric classes>=                           "
               , "(class Integer"
               , "    [subclass-of Number] ; abstract class"
               , "    (method div: (n) (self subclassResponsibility))"
               , "    (method mod: (n) (self - (n * (self div: n))))"
               , "    (method gcd: (n) ((n = (self coerce: 0))"
               ,
       "                      ifTrue:ifFalse: {self} {(n gcd: (self mod: n))}))"
               , "    (method lcm: (n) (self * (n div: (self gcd: n))))"
               , ";      Class [[Integer]] typically has three concrete "
               , ";      subclasses: [[SmallInteger]], for integers that fit "
               , ";      in a machine word (\\crefusma.chap);          "
               , ";      [[LargePositiveInteger]], for arbitrarily large "
               , ";      positive integers; and [[LargeNegativeInteger]], for "
               , ";      arbitrarily large negative integers (\\cref   "
               , ";      small.imp.LargeInteger). In uSmalltalk, only "
               , ";      [[SmallInteger]] is defined; the other two are meant "
               , ";      to be added by you (\\crefsmall.ex.LargeInteger). "
               , ";                                                   "
               , ";      Class [[Integer]] implements the subclass    "
               , ";      responsibility [[reciprocal]], and it also overrides "
               , ";      the default [[/]] method. Both methods answer "
               , ";      fractions, not integers.                     "
               , ";      <other methods of class [[Integer]]>=        "
               , "    (method reciprocal () (Fraction num:den: 1 self)) "
               , "    (method / (aNumber) ((self asFraction) / aNumber))"
               , ";      The coercion methods on classes [[Float]] and "
               , ";      [[Integer]] follow the same structure. Class "
               , ";      [[Float]] is relegated to \\crefusma.chap, but the "
               , ";      [[Integer]] methods are shown here. Just as a "
               , ";      fraction must know what integer or floating-point "
               , ";      operations to use to divide its numerator by its "
               , ";      denominator, an integer must know what fractional or "
               , ";      floating-point operations to use to represent an "
               , ";      integer. In this case, it's [[self]] divided by 1 and "
               ,
                ";      [[self]] times a base to the power 0, respectively. \\ "
               , ";      qbreak                                       "
               , ";      <other methods of class [[Integer]]>=        "
               , "    (method asFraction () (Fraction num:den:  self 1))"
               , "    (method asFloat    () (Float    mant:exp: self 0))"
               , ";      Just as in class [[Fraction]], the other two methods "
               , ";      simply exploit the knowledge that the receiver is an "
               , ";      integer:                                     "
               , ";      <other methods of class [[Integer]]>=        "
               , "    (method asInteger () self)"
               , "    (method coerce: (aNumber) (aNumber asInteger))"
               , ";      \\qbreak                                      "
               , ";                                                   "
               , ";      Implementation of integers                   "
               , ";                                                   "
               , ";      In addition to the [[Integer]] methods defined in \\ "
               , ";      usmpagesmall.class-Integer, integers also support a "
               , ";      [[timesRepeat:]] method, which executes a loop a "
               , ";      finite number of times.                      "
               , ";      <other methods of class [[Integer]]>=        "
               , "    (method timesRepeat: (aBlock) [locals count]"
               ,
    "        ((self isNegative) ifTrue: {(self error: 'negative-repeat-count)})"
               , "        (set count self)"
               , "        ({(count != 0)} whileTrue:"
               , "             {(aBlock value)"
               , "              (set count (count - 1))}))"
               , ")"
               , ";  Implementing complex operations: Class \\headerrlap{\\ "
               , ";  texttt{\\upshape Fraction}}                   "
               , ";                                               "
               , ";  [*]                                          "
               , ";                                               "
               , ";  A method is complex when it needs to inspect the "
               , ";  representation of an argument. I introduce complex "
               , ";  methods using class [[Fraction]], whose      "
               , ";  representation includes a numerator [[num]] and "
               , ";  denominator [[den]], both of which are integer "
               , ";  instance variables. \\basislabelFraction      "
               , ";  <numeric classes>=                           "
               , "(class Fraction"
               , "    [subclass-of Number]"
               , "    [ivars num den]"
               , "    (method print () (num print) ('/ print) (den print) self)"
               , ";      This representation stands for the ratio fracnumden. "
               , ";      Such ratios can be compared, added, and multiplied "
               , ";      only if each method has access to the numerator and "
               , ";      denominator of its argument, not just its    "
               , ";      receiver—that is, if the methods are complex. "
               , ";      The access is provided by private methods [[num]] and "
               , ";      [[den]], each of which answers the value of the "
               , ";      instance variable with which it shares a name. "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method num () num)  ; private"
               , "    (method den () den)  ; private"
               , ";      Methods [[num]] and [[den]] are used to implement the "
               , ";      four complex operations [[=]], [[<]], [[*]], and  "
               , ";      [[+]]. Each operation relies on and guarantees these "
               , ";      representation invariants:                   "
               , ";                                                   "
               , ";       \\tightlist                                  "
               , ";       1. The denominator is positive.             "
               , ";       2. If the numerator is zero, the denominator is 1. "
               , ";                                                   "
               , ";       \\zvfilbreak1.0in                            "
               , ";                                                   "
               , ";       3. The numerator and denominator are reduced to "
               , ";       lowest terms, that is, their only common divisor "
               , ";       is 1.                                       "
               , ";                                                   "
               , ";      These invariants imply that two [[Fraction]] objects "
               , ";      represent the same fraction if and only if they have "
               , ";      the same numerator and denominator, and they enable "
               , ";      the following implementations of the comparison "
               , ";      methods from class [[Magnitude]]:            "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method = (f) ((num = (f num)) and: {(den = (f den))}))"
               , "    (method < (f) ((num * (f den)) < ((f num) * den)))"
               , ";      The [[<]] method uses the law that fracnd < fracn'd' "
               ,
               ";      if and only if \\nomathbreakn\\atimes d' < n'\\atimesd, "
               , ";      which holds only because d and d' are positive. And "
               , ";      argument [[f]] doesn't have to be an instance of "
               , ";      class [[Fraction]]; it's enough if [[f]] is a number "
               , ";      and if it responds sensibly to messages [[num]] and "
               , ";      [[den]].                                     "
               , ";                                                   "
               , ";      Methods [[=]] and [[<]] rely on the representation "
               , ";      invariants. The invariants for any given fraction are "
               , ";      established by two private methods: method   "
               , ";      [[signReduce]] establishes invariant 1, and method "
               , ";      [[divReduce]] establishes invariants 2 and 3. "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method signReduce () ; private"
               , "        ((den isZero) ifTrue: {(self error: 'ZeroDivide)})"
               , "        ((den isNegative) ifTrue:"
               ,
                "            {(set num (num negated)) (set den (den negated))})"
               , "        self)"
               , "    (method divReduce () [locals temp] ; private"
               , "        ((num = 0) ifTrue:ifFalse:"
               , "            {(set den 1)}"
               , "            {(set temp ((num abs) gcd: den))"
               , "             (set num  (num div: temp))"
               , "             (set den  (den div: temp))})"
               , "        self)"
               , ";      When a new [[Fraction]] is created by public class "
               , ";      method [[num:den:]], all three invariants are "
               , ";      established by private method [[initNum:den:]]. "
               , ";      <other methods of class [[Fraction]]>=       "
               ,
               "    (class-method num:den: (a b) ((self new) initNum:den: a b))"
               ,
         "    (method setNum:den: (a b) (set num a) (set den b) self) ; private"
               , "    (method initNum:den: (a b) ; private"
               , "        (self setNum:den: a b)"
               , "        (self signReduce)"
               , "        (self divReduce))"
               , ";      Private method [[setNum:den:]] sets the numerator and "
               , ";      denominator of a fraction, but it does not establish "
               , ";      the invariants. It is used in multiplication and "
               , ";      addition.                                    "
               , ";                                                   "
               , ";      Multiplication is specified by the law fracnd\\atimes "
               , ";      fracn'd' = fracn\\atimesn'd\\atimesd', but the "
               , ";      right-hand side could violate invariant 2 or 3. "
               ,
                ";      (Numerator n\\atimesn' and denominator d\\atimesd' can "
               , ";      have common factors, but d\\atimesd' cannot be "
               , ";      negative.) The multiplication method therefore sends "
               , ";      [[divReduce]] to the \\naive result.          "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method * (f)"
               ,
"      (((Fraction new) setNum:den: (num * (f num)) (den * (f den))) divReduce))"
               , ";      Addition is specified by the law \\nomathbreak fracnd "
               ,
              ";      + fracn'd' = fracn\\atimesd'+n'\\atimesdd\\atimesd'. The "
               , ";      resulting numerator and denominator may have common "
               , ";      factors, violating invariant 3, but such factors are "
               , ";      eliminated by [[divReduce]], as in the example \\ "
               , ";      nomathbreakfrac12+frac12 = frac44 = frac1 1. Method "
               , ";      [[divReduce]] also restores invariant 2. In addition, "
               , ";      the computation of denominator d\\atimesd' might "
               , ";      overflow. To make overflow less likely, my code "
               , ";      defines temp = lcm(d, d'), puts [[temp]] in the "
               , ";      denominator, and uses fractempd and fractempd' as "
               , ";      needed. Without this tweak, the square-root  "
               , ";      computations in Section [->] would overflow. "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method + (f) [locals temp]"
               , "        (set temp (den lcm: (f den)))"
               , "        (((Fraction new) setNum:den:"
               , "                         ((num     * (temp div: den)) +"
               , "                          ((f num) * (temp div: (f den))))"
               , "                         temp)"
               , "            divReduce))"
               , ";      Method [[+]] is the last of the complex methods. But "
               , ";      it's not the last of the methods that rely on or "
               , ";      guarantee invariants. For example, [[reciprocal]] "
               , ";      must not leave a negative fraction with a negative "
               , ";      denominator. The denominator is given the correct "
               , ";      sign by sending [[signReduce]] to the inverted "
               , ";      fraction. (The reciprocal of zero cannot be put into "
               , ";      reduced form. Nothing can be done about it.) "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method reciprocal ()"
               , "       (((Fraction new) setNum:den: den num) signReduce))"
               , ";      Negation negates the numerator; the invariants are "
               , ";      guaranteed to be maintained.                 "
               , ";      <other methods of class [[Fraction]]>=       "
               ,
        "    (method negated () ((Fraction new) setNum:den: (num negated) den))"
               , ";      The invariants enable the sign tests to inspect only "
               , ";      the receiver's numerator. These tests are much more "
               , ";      efficient than the versions inherited from   "
               , ";      [[Number]].                                  "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method isZero             () (num isZero))"
               , "    (method isNegative         () (num isNegative))"
               , "    (method isNonnegative      () (num isNonnegative))"
               , "    (method isStrictlyPositive () (num isStrictlyPositive))"
               , ";      Abstract class [[LargeInteger]] [*]          "
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                                   "
               , ";      A starter kit for class [[LargeInteger]] is shown in "
               , ";      \\vrefbrsmall.fig.LargeInteger. The [[LargeInteger]] "
               , ";      class is meant to be abstract; do not instantiate it. "
               , ";      Instead, define subclasses [[LargePositiveInteger]] "
               , ";      and [[LargeNegativeInteger]], which you can then "
               , ";      instantiate (\\crefsmall.ex.LargeInteger).    "
               , ";                                                   "
               , ";      Coercion between abstractions in [[Fraction]] and "
               , ";      [[Integer]]                                  "
               , ";                                                   "
               , ";      [*]                                          "
               , ";                                                   "
               , ";      A binary message like [[<]] or [[+]] should be sent "
               , ";      only when the receiver and the argument are  "
               , ";      compatible. If numbers aren't compatible, they can be "
               , ";      made so using coercion. In Smalltalk, coercion is "
               , ";      part of the [[Number]] protocol; every number must be "
               , ";      able to coerce itself to an integer, a floating-point "
               , ";      number, or a fraction. A coercion method typically "
               , ";      uses the public protocol of the classes it is "
               , ";      coercing its receiver to, like these methods defined "
               , ";      on class [[Fraction]]:                       "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method asInteger  () (num div: den))"
               , "    (method asFloat    () ((num asFloat) / (den asFloat)))"
               , "    (method asFraction () self)"
               , ";      To coerce itself to an integer or a floating-point "
               , ";      number, a fraction divides [[num]] by [[den]]. "
               , ";      Division may be implemented by the integer-division "
               , ";      message [[div:]] or (after coercing [[num]] and "
               , ";      [[den]] to floating point) by the floating-point "
               , ";      division message [[/]]. To coerce itself to a "
               , ";      fraction, a fraction needn't divide at all.  "
               , ";                                                   "
               , ";      When their classes aren't known, numbers can still be "
               , ";      made compatible by sending the [[coerce:]] message, "
               , ";      which tells the receiver to coerce its argument to be "
               , ";      like itself. For example, a fraction coerces its "
               , ";      argument to a fraction.                      "
               , ";      <other methods of class [[Fraction]]>=       "
               , "    (method coerce: (aNumber) (aNumber asFraction))"
               , ")"
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  \\basislabelNatural                           "
               , ";                                               "
               , ";  <numeric classes>=                           "
               , "(class Natural"
               , "   [subclass-of Magnitude]"
               , "   ; instance variables left as an exercise"
               , ""
               ,
                "   (class-method fromSmall: (anInteger) (self leftAsExercise))"
               , ""
               , "   (method = (aNatural) (self leftAsExercise))"
               , "   (method < (aNatural) (self leftAsExercise))"
               , ""
               , "   (method + (aNatural) (self leftAsExercise))"
               , "   (method * (aNatural) (self leftAsExercise))"
               , "   (method - (aNatural)"
               , "      (self subtract:withDifference:ifNegative:"
               , "            aNatural"
               , "            [block (x) x]"
               ,
              "            {(self error: 'Natural-subtraction-went-negative)}))"
               ,
  "   (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)"
               , "      (self leftAsExercise))"
               , ""
               , "   (method sdiv: (n) (self sdivmod:with: n [block (q r) q]))"
               , "   (method smod: (n) (self sdivmod:with: n [block (q r) r]))"
               , "   (method sdivmod:with: (n aBlock) (self leftAsExercise))"
               , ""
               , "   (method decimal () (self leftAsExercise))"
               , "   (method isZero  () (self leftAsExercise))"
               , ""
               ,
             "   (method print   () ((self decimal) do: [block (x) (x print)]))"
               , ")"
               , ";  The only concrete integer class built into uSmalltalk "
               , ";  is [[SmallInteger]]. Almost all its methods are "
               , ";  primitive. They are defined in chunks [->]– [->]. [*] "
               , ";  \\basislabelSmallInteger                      "
               , ";  <numeric classes>=                           "
               , "(class SmallInteger"
               , "    [subclass-of Integer] ; primitive representation"
               ,
                "    (class-method new: (n) (primitive newSmallInteger self n))"
               , "    (class-method new  ()  (self new: 0))"
               , "    (method negated    ()  (0 - self))"
               ,
                "    (method print      ()  (primitive printSmallInteger self))"
               , "    (method +          (n) (primitive + self n))"
               , "    (method -          (n) (primitive - self n))"
               , "    (method *          (n) (primitive * self n))"
               , "    (method div:       (n) (primitive div self n))"
               , "    (method =          (n) (primitive sameObject self n))"
               , "    (method <          (n) (primitive < self n))"
               , "    (method >          (n) (primitive > self n))"
               , ")"
               , ";  An object of class [[Float]] is an abstraction of a "
               , ";  rational number. The representation is an integer m "
               , ";  (the mantissa) combined with an integer e (the  "
               , ";  exponent), stored in instance variables [[mant]] and "
               , ";  [[exp]]. \\stdbreak The abstraction function maps this "
               , ";  representation to the number \\nomathbreakm\\atimes10^e "
               , ";  . Both m and e can be negative. The representation "
               , ";  invariant guarantees that the absolute value of the "
               , ";  mantissa is at most 2^15-1. The invariant ensures "
               , ";  that two mantissas can be multiplied without "
               , ";  overflow, even on an implementation that provides "
               , ";  only 31-bit small integers. [Some implementations "
               , ";  of ML reserve one bit as a dynamic-type tag or as a "
               , ";  tag for the garbage collector.] The invariant is "
               , ";  maintained with the help of a private [[normalize]] "
               , ";  method: when a mantissa's magnitude exceeds 2^15-1, "
               , ";  the [[normalize]] method divides the mantissa by 10 "
               , ";  and increments the exponent until the mantissa is "
               , ";  small enough. \\stdbreak This operation loses "
               , ";  precision; it is the source of so-called     "
               , ";  ``floating-point rounding error.'' \\stdbreak "
               , ";  The possibility of rounding error implies that the "
               , ";  answers obtained \\stdbreak from floating-point "
               , ";  arithmetic are approximate. This possibility is part "
               , ";  of the specification of class [[Float]], but "
               , ";  specifying exactly what ``approximate'' means is "
               , ";  beyond the scope of this book.               "
               , ";  <numeric classes>=                           "
               , "(class Float"
               , "    [subclass-of Number]"
               , "    [ivars mant exp]"
               ,
             "    (class-method mant:exp: (m e) ((self new) initMant:exp: m e))"
               , "    (method initMant:exp: (m e) ; private"
               , "        (set mant m) (set exp e) (self normalize))"
               , "    (method normalize ()    ; private"
               , "        ({((mant abs) > 32767)} whileTrue:"
               , "               {(set mant (mant div: 10))"
               , "                (set exp (exp + 1))})"
               , "        self)"
               , ";      Like the other numeric classes, [[Float]] must "
               , ";      provide methods that give a binary operation access "
               , ";      to the representation of its argument.       "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method mant () mant)  ; private"
               , "    (method exp  () exp)   ; private"
               , ";      Comparing two floats with different exponents is "
               , ";      awkward, so instead I compute their difference and "
               , ";      compare it with zero.                        "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method < (x) ((self - x) isNegative))"
               , "    (method = (x) ((self - x) isZero))"
               , ";      Negation is easy: answer a new float with a negated "
               , ";      mantissa.                                    "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method negated () (Float mant:exp: (mant negated) exp))"
               , ";      The [[+]] method adds x' = m' \\atimes10^e' to "
               , ";      [[self]], which is m \\atimes10^e. Its implementation "
               , ";      is based on the algebraic law m \\atimes10^e = (m \\ "
               , ";      atimes10^e-e') \\atimes10^e'. This law implies "
               , ";                                                   "
               , ";       m \\atimes10^e + m' \\atimes10^e' = (m \\atimes10^ "
               , ";       e-e' + m') \\atimes10^e'.                    "
               , ";                                                   "
               ,
                ";      I provide a naï ve implementation which enforces e-e' "
               , ";      >=0. This implementation risks overflow, but at least "
               , ";      overflow can be detected. A naï ve implementation "
               , ";      using e-e'<=0 might well lose valuable bits of "
               , ";      precision from m. A better implementation can be "
               , ";      constructed using the ideas in Exercise [->]. "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method + (x-prime) "
               , "        ((exp >= (x-prime exp)) ifTrue:ifFalse:"
               ,
"            {(Float mant:exp: ((mant * (10 raisedToInteger: (exp - (x-prime exp)))) +"
               , "                                 (x-prime mant))"
               , "                              (x-prime exp))}"
               , "            {(x-prime + self)}))"
               , ";      Multiplication is much simpler: (m \\atimes10^e) \\ "
               , ";      atimes(m' \\atimes10^e') = (m \\atimesm') \\atimes10^ "
               ,
                ";      e+e'. The product's mantissa m\\atimesm' may be large, "
               , ";      but the class method [[mant:exp:]] normalizes it. "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method * (x-prime) "
               ,
      "        (Float mant:exp: (mant * (x-prime mant)) (exp + (x-prime exp))))"
               , ";      <other methods of class [[Float]]>=          "
               , "    (method reciprocal ()"
               , "        (Float mant:exp: (1000000000 div: mant) (-9 - exp)))"
               , ";      Coercing converts to [[Float]], and converting "
               , ";      [[Float]] to [[Float]] is the identity.      "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method coerce: (aNumber) (aNumber asFloat))"
               , "    (method asFloat () self)"
               , ";      When converting a float to another class of number, a "
               , ";      negative exponent means divide, and a nonnegative "
               , ";      exponent means multiply.                     "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method asInteger ()"
               , "        ((exp isNegative) ifTrue:ifFalse:"
               , "            {(mant div: (10 raisedToInteger: (exp negated)))}"
               , "            {(mant    * (10 raisedToInteger: exp))}))"
               , ";      <other methods of class [[Float]]>=          "
               , "    (method asFraction ()"
               , "        ((exp < 0) ifTrue:ifFalse:"
               ,
    "            {(Fraction num:den: mant (10 raisedToInteger: (exp negated)))}"
               ,
      "            {(Fraction num:den: (mant * (10 raisedToInteger: exp)) 1)}))"
               , ";      Unlike the sign tests in [[Fraction]], the sign tests "
               , ";      in [[Float]] aren't just an optimization: the [[<]] "
               , ";      method sends [[negative]] to a floating-point number, "
               , ";      so the superclass implementation of [[negative]], "
               , ";      which sends [[<]] to self, would lead to infinite "
               , ";      recursion. Fortunately, the sign of a floating-point "
               , ";      number is the sign of its mantissa, so all four "
               , ";      methods can be delegated to [[Integer]].     "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method isZero             () (mant isZero))"
               , "    (method isNegative         () (mant isNegative))"
               , "    (method isNonnegative      () (mant isNonnegative))"
               , "    (method isStrictlyPositive () (mant isStrictlyPositive))"
               , ";      \\qbreak A floating-point number is printed as m[[x10 "
               , ";      ^]]e. But I want to avoid printing a number like 77 "
               , ";      as [[770x10^-1]]. So if my [[print]] method sees a "
               , ";      number with a negative exponent and a mantissa that "
               , ";      is a multiple of 10, it divides the mantissa by 10 "
               , ";      and increases the exponent, continuing until the "
               , ";      exponent reaches zero or the mantissa is no longer a "
               , ";      multiple of 10. As a result, a whole number always "
               , ";      prints as a whole number times 10^0, no matter what "
               , ";      its internal representation is.              "
               , ";      <other methods of class [[Float]]>=          "
               , "    (method print () "
               , "        (self print-normalize) "
               , "        (mant print) ('x10^ print) (exp print)"
               , "        (self normalize))"
               , ""
               , "    (method print-normalize ()"
               , "        ({((exp < 0) and: {((mant mod: 10) = 0)})} whileTrue:"
               , "            {(set exp (exp + 1)) (set mant (mant div: 10))}))"
               , ")"
               , ";  The [[ --- trace]] referred to is a global variable, "
               , ";  normally 0.                                  "
               ,
     ";  <predefined uSmalltalk classes and values that use numeric literals>= "
               , "(val &trace 0)"
               , ";  Implementation of Char: Unicode characters   "
               , ";                                               "
               , ";  As in the other bridge languages, a Unicode character "
               , ";  prints using the UTF-8 encoding. The [[Char]] class "
               , ";  defines a representation, initialization methods, and "
               , ";  a [[print]] method. It must also redefine [[=]], "
               , ";  because two objects that represent the same Unicode "
               , ";  character should be considered equal, even if they "
               , ";  are not the same object. The representation invariant "
               , ";  is that [[code-point]] is an integer between 0 and "
               , ";  hexadecimal 1fffff.                          "
               ,
     ";  <predefined uSmalltalk classes and values that use numeric literals>= "
               , "(class Char"
               , "   [subclass-of Object]"
               , "   [ivars code-point]"
               , "   (class-method new: (n) ((self new) init: n))"
               , "   (method init:      (n) (set code-point n) self) ;; private"
               , "   (method print      ()  (primitive printu code-point))"
               , "   (method =          (c) (code-point = (c code-point)))"
               , "   (method code-point ()  code-point) ;; private"
               , ")"
               , ";  The predefined characters are defined using their "
               , ";  code points, which coincide with 7-bit ASCII codes. "
               ,
     ";  <predefined uSmalltalk classes and values that use numeric literals>= "
               ,
        "(val newline      (Char new: 10))   (val left-round   (Char new:  40))"
               ,
        "(val space        (Char new: 32))   (val right-round  (Char new:  41))"
               ,
        "(val semicolon    (Char new: 59))   (val left-curly   (Char new: 123))"
               ,
        "(val quotemark    (Char new: 39))   (val right-curly  (Char new: 125))"
               ,
        "                                    (val left-square  (Char new:  91))"
               ,
        "                                    (val right-square (Char new:  93))"
               , ";  [*]                                          "
               , ";                                               "
               , ";  \\ztrim1.5                                    "
               , ";                                               "
               , ";  Another big protocol that relies on just a few "
               , ";  subclass responsibilities is the [[Collection]] "
               , ";  protocol. This protocol is a joy to work with; "
               , ";  it includes not only object-oriented analogs to "
               , ";  functions like [[exists?]], [[map]], [[filter]], and "
               , ";  [[foldl]], but also many other methods, which do "
               , ";  things like add, remove, find, and count elements (\\ "
               , ";  crefpage,small.proto.Collection). And unlike their "
               , ";  Scheme analogs, these operations support not only "
               , ";  lists but also several other forms of collection. "
               , ";  All this functionality is provided using just four "
               , ";  subclass responsibilities: a collection class must "
               , ";  define methods [[do:]], [[add:]],            "
               , ";  [[remove:ifAbsent:]], and [[=]].             "
               , ";  <collection classes>=                        "
               , "(class Collection"
               , "  [subclass-of Object] ; abstract"
               ,
               "  (method do:     (aBlock)       (self subclassResponsibility))"
               ,
               "  (method add:    (newObject)    (self subclassResponsibility))"
               , "  (method remove:ifAbsent: (oldObject exnBlock)"
               ,
               "                                 (self subclassResponsibility))"
               ,
               "  (method =       (aCollection)  (self subclassResponsibility))"
               , ";    <other methods of class [[Collection]]>=     "
               , "  (class-method with: (anObject)"
               , "      ((self new) add: anObject))"
               , "  (class-method withAll: (aCollection)"
               , "      ((self new) addAll: aCollection))"
               , ";    When [[addAll:]] is sent to an object of a subclass, "
               , ";    the message dispatches to the method shown here, "
               , ";    which is defined on class [[Collection]]. It is "
               , ";    implemented using [[do:]] and [[add:]].      "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method addAll: (aCollection) "
               , "      (aCollection do: [block (x) (self add: x)])"
               , "      self)"
               , ";    When method [[addAll:]] sends [[do:]] and [[add:]], "
               , ";    they dispatch to the methods defined on the subclass. "
               , ";                                                 "
               , ";    Removal works in the same way, building on [[do:]] "
               , ";    and [[remove:ifAbsent:]].                    "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method remove: (oldObject) "
               ,
   "      (self remove:ifAbsent: oldObject {(self error: 'remove-was-absent)}))"
               , "  (method removeAll: (aCollection) "
               , "      (aCollection do: [block (x) (self remove: x)])"
               , "      self)"
               , ";    In addition to these mutators, the [[Collection]] "
               , ";    protocol defines a host of observers, including "
               , ";    [[isEmpty]] and [[size]], among others (\\cpageref "
               , ";    small.proto.Collection). The default implementations "
               , ";    given here iterate through the elements of the "
               , ";    collection using [[do:]].                    "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method size () [locals n]"
               , "      (set n 0)"
               , "      (self do: [block (_) (set n (n + 1))])"
               , "      n)"
               , "  (method occurrencesOf: (anObject) [locals n]"
               , "      (set n 0)"
               ,
       "      (self do: [block (x) ((x = anObject) ifTrue: {(set n (n + 1))})])"
               , "      n)"
               , ";    Using a linear search to compute [[size]], for "
               , ";    example, may seem inefficient, but if a subclass "
               , ";    knows a more efficient way to compute the number of "
               , ";    elements, it redefines the [[size]] method. And for "
               , ";    some collections, like [[List]], there is no more "
               , ";    efficient way to compute size or count occurrences. "
               , ";                                                 "
               , ";    An iteration that uses [[do:]] can be cut short by a "
               , ";    [[return]] expression, as in methods [[isEmpty]], "
               , ";    [[includes:]], and [[detect:ifNone:]] below. And "
               , ";    again, if the collection is a linked list, no more "
               , ";    efficient implementation is possible. [*]    "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method isEmpty () "
               , "      (self do: [block (_) (return false)])"
               , "      true)"
               , "  (method includes: (anObject)"
               ,
         "      (self do: [block (x) ((x = anObject) ifTrue: {(return true)})])"
               , "      false)"
               , "  (method detect:ifNone: (aBlock exnBlock)"
               ,
         "      (self do: [block (x) ((aBlock value: x) ifTrue: {(return x)})])"
               , "      (exnBlock value))"
               , "  (method detect: (aBlock)"
               ,
       "      (self detect:ifNone: aBlock {(self error: 'no-object-detected)}))"
               , ";    [*] Variadic functions. Extend micro-Scheme to "
               , ";    support functions with a variable number of  "
               , ";    arguments. Do so by giving the name ... (three dots) "
               , ";    special significance when it appears as the last "
               , ";    formal parameter in a lambda. For example:   "
               , ";    {smallverbatimx} -> (val f (lambda (x y ...)) (+ x (+ "
               , ";    y (foldl + 0 ...))) -> (f 1 2 3 4 5) ; in f, rho = x "
               , ";    |-> 1, y |-> 2, ... |-> '(3 4 5) 15 {smallverbatimx} "
               , ";    In this example, if f gets fewer than two arguments, "
               , ";    it is a checked run-time error. If f gets at least "
               , ";    two arguments, any additional arguments are placed "
               , ";    into an ordinary list, and the list is used to "
               , ";    initialize the location of the formal parameteter "
               , ";    associated with ....                         "
               , ";                                                 "
               , ";     1. Implement this new feature. Begin by changing the "
               , ";     definition of lambda on \\cpageref           "
               , ";     mlscheme.type.exp to {smallverbatimx} and lambda "
               , ";     = name list * varargs : bool * exp          "
               , ";     {smallverbatimx} Now recompile; type-error  "
               , ";     messages will tell you what other code you have "
               , ";     to change.                                  "
               , ";                                                 "
               , ";     For the parser, you may find the following  "
               , ";     function useful: {smallverbatimx} fun newLambda "
               , ";     (formals, body) = case reverse formals of \"...\" "
               , ";     :: fs' => LAMBDA (reverse fs', varargs=true, "
               , ";     body) | _ => LAMBDA (formals, varargs=false, "
               , ";     body) {smallverbatimx} This function has type "
               , ";                                                 "
               , ";         \\monoboxname list * exp -> name list * [[]] "
               , ";         varargs : bool [[]] * exp,              "
               , ";                                                 "
               , ";     and it is designed for you to adapt old syntax to "
               , ";     new syntax; just drop it into the parser wherever "
               , ";     LAMBDA is used.                             "
               , ";     2. As a complement to the varargs lambda, write a "
               , ";     new apply primitive such that               "
               , ";                                                 "
               , ";         \\monobox(apply f '(1 2 3))              "
               , ";                                                 "
               , ";     is equivalent to                            "
               , ";                                                 "
               , ";         \\monobox(f 1 2 3)                       "
               , ";                                                 "
               , ";     Sadly, you can't use PRIMITIVE for this; you'll "
               , ";     have to invent a new kind of thing that has "
               , ";     access to the internal eval.                "
               , ";     3. Demonstrate these utilities by writing a "
               , ";     higher-order micro-Scheme function cons-logger "
               , ";     that counts cons calls in a private variable. It "
               , ";     should operate as follows:                  "
               , ";                                                 "
               , ";     {smallverbatimx} -> (val cl (cons-logger)) -> "
               , ";     (val log-cons (car cl)) -> (val conses-logged "
               , ";     (cdr cl)) -> (conses-logged) 0 -> (log-cons f e1 "
               , ";     e2 ... en) ; returns (f e1 e2 ... en), ;    "
               , ";     incrementing private counter ; whenever cons is "
               , ";     called -> (conses-logged) 99 ; or whatever else "
               , ";     is the number of times cons is called ; during "
               , ";     the call to log-cons {smallverbatimx}       "
               , ";     4. Rewrite the \\rulenameApplyClosure rule (\\cpageref "
               , ";     mlscheme.rule.ApplyClosure) to account for the "
               , ";     new abstract syntax and behavior.           "
               , ";                                                 "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method inject:into: (aValue binaryBlock)"
               ,
  "     (self do: [block (x) (set aValue (binaryBlock value:value: x aValue))])"
               , "     aValue)"
               ,
";    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";    {protocol}[[Collection]] \\pmspecies --- Answer a "
               , ";    class that should be used to create new instances of "
               , ";    collections like the receiver, to help with the "
               , ";    implementation of [[select:]], [[collect:]], and "
               , ";    similar methods.                             "
               , ";    \\mprintName --- Print the name of the object's class, "
               , ";    to help with the implementation of [[print]]. (Almost "
               , ";    all [[Collection]] objects print as the name of the "
               , ";    class, followed by the list of elements in   "
               , ";    parentheses. [[Array]] objects omit the name of the "
               , ";    class.)                                      "
               ,
";    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";    {protocol}                                   "
               , ";                                                 "
               , ";    Private methods internal to [[Collection]] classes.  "
               , ";    [*]                                          "
               ,
";    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                                 "
               , ";    The methods [[select:]], [[reject:]], and    "
               , ";    [[collect:]] resemble micro-Scheme's [[filter]] and "
               , ";    [[map]] functions. Like [[inject:into:]], they work "
               , ";    on all collections, not just on lists.       "
               , ";    The implementations use [[species]], which is a "
               , ";    private message used to create ``a new collection "
               , ";    like the receiver'' (\\crefsmall.Collection.private). "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method select: (aBlock) [locals temp]"
               , "     (set temp ((self species) new))"
               ,
       "     (self do: [block (x) ((aBlock value: x) ifTrue: {(temp add: x)})])"
               , "     temp)"
               , "  (method reject: (aBlock)"
               , "     (self select: [block (x) ((aBlock value: x) not)]))"
               , "  (method collect: (aBlock) [locals temp]"
               , "     (set temp ((self species) new))"
               , "     (self do: [block (x) (temp add: (aBlock value: x))])"
               , "     temp)"
               , ";    A [[species]] defaults to the class of the receiver. "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method species () (self class))"
               , ";    Finally, [[Collection]] defines its own [[print]] "
               , ";    method. By default, a collection prints as the name "
               , ";    of its class, followed by a parenthesized list of its "
               , ";    elements.                                    "
               , ";    <other methods of class [[Collection]]>=     "
               , "  (method print ()"
               , "      (self printName)"
               , "      (left-round print)"
               , "      (self do: [block (x) (space print) (x print)])"
               , "      (space print)"
               , "      (right-round print)"
               , "      self)"
               , "  (method printName () (((self class) name) print))"
               , ")"
               , ";  Both methods may be overridden by subclasses. "
               , ";                                               "
               , ";  Widening a protocol: Keyed and sequenceable  "
               , ";  collections                                  "
               , ";                                               "
               , ";  [*] [[Collection]] isn't just an abstract class with "
               , ";  multiple implementations. It's an abstraction that is "
               , ";  refined into more abstractions:              "
               , ";                                               "
               , ";   \\tightlist                                  "
               , ";    • Keyed collections, which collect key-value pairs "
               , ";   and can be indexed by key                   "
               , ";    • Sequenceable collections, which are keyed by "
               , ";   consecutive integers                        "
               , ";                                               "
               , ";  Each of these collections refines the protocol "
               , ";  defined by its superclass. To study such refinement, "
               , ";  we ask the same questions about each subclass: "
               , ";                                               "
               , ";   \\tightlist*                                 "
               , ";    • Which of the subclass responsibilities inherited "
               , ";   from the superclass does it implement?      "
               , ";    • What subclass responsibilities inherited from the "
               , ";   superclass does it pass on to its own subclasses? "
               , ";    • What new subclass responsibilities does it add? "
               , ";    • What methods inherited from the superclass does "
               , ";   it override? On what grounds?               "
               , ";                                               "
               , ";  For keyed and sequenceable collections, the answers "
               , ";  are shown in \\crefsmall.table.widening.      "
               , ";                                               "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  {trivlist}                                   "
               ,
";         Implements          Passes on               Adds                    Overrides "
               ,
       ";  \\smash\\                                               [[at:put]], "
               ,
";  rotatebox [[do:]], [[=]]      [[add]],                [[associationsDo:]],    — "
               ,
";  [origin=                      [[remove:ifAbsent]]     [[removeKey:ifAbsent:]] "
               , ";  r]90Keyed                                    "
               , ";  \\smash\\                       [[add]], [[at:put]], "
               ,
";  rotatebox [[associationsDo:]] [[remove:ifAbsent]],    [[firstKey]],           [[at:IfAbsent:]] "
               ,
         ";  [origin=                      [[removeKey:ifAbsent]], [[lastKey]] "
               , ";  r]90Seq.                      \\break [[species]] "
               , ";                                               "
               , ";  {trivlist}                                   "
               , ";                                               "
               , ";  How protocols are refined for keyed and sequenceable "
               , ";  collections [*]                              "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                               "
               , ";  Implementation of [[KeyedCollection]]        "
               , ";                                               "
               , ";  A keyed collection provides access to key-value "
               , ";  pairs. It must define method [[associationsDo:]], "
               , ";  which replaces [[do:]], method               "
               , ";  [[removeKey:ifAbsent:]], which replaces      "
               , ";  [[remove:ifAbsent:]], and method [[at:put:]], which "
               , ";  sometimes replaces [[add:]]. The key-value pairs "
               , ";  answer the [[Association]] protocol.         "
               , ";  <collection classes>=                        "
               , "(class KeyedCollection"
               , "    [subclass-of Collection]  ; abstract class"
               ,
 "    (method associationsDo: (aBlock)           (self subclassResponsibility))"
               ,
 "    (method removeKey:ifAbsent: (key exnBlock) (self subclassResponsibility))"
               ,
 "    (method at:put: (key value)                (self subclassResponsibility))"
               , ";      The [[associationsDo:]] method is used to implement "
               , ";      the [[do:]] method required by the superclass: "
               , ";      <other methods of class [[KeyedCollection]]>= "
               , "    (method do: (aBlock) "
               ,
"        (self associationsDo: [block (anAssoc) (aBlock value: (anAssoc value))]))"
               , ";      <other methods of class [[KeyedCollection]]>= "
               , "    (method at: (key)    "
               ,
               "        (self at:ifAbsent: key {(self error: 'key-not-found)}))"
               , "    (method at:ifAbsent: (key exnBlock) "
               ,
"        ((self associationAt:ifAbsent: key {(return (exnBlock value))}) value))"
               , "    (method includesKey: (key) "
               , "        ((self associationAt:ifAbsent: key {}) notNil))"
               , "    (method associationAt: (key) "
               ,
    "        (self associationAt:ifAbsent: key {(self error: 'key-not-found)}))"
               , "    (method associationAt:ifAbsent: (key exnBlock)"
               ,
"        (self associationsDo: [block (x) (((x key) = key) ifTrue: {(return x)})])"
               , "        (exnBlock value))"
               , ";      When a key is found, method                  "
               , ";      [[associationAt:ifAbsent:]] terminates the search "
               , ";      immediately by evaluating a [[return]] expression. "
               , ";      This efficiency benefits all the other methods. "
               , ";      And if a subclass implements                 "
               , ";      [[associationAt:ifAbsent:]] in a more efficient way, "
               , ";      the other methods benefit from that, too.    "
               , ";                                                   "
               , ";      \\qbreak The key associated with a value is found in "
               , ";      the same way as the value associated with a key. "
               , ";      <other methods of class [[KeyedCollection]]>= "
               , "    (method keyAtValue: (value) "
               ,
   "        (self keyAtValue:ifAbsent: value {(self error: 'value-not-found)}))"
               , "    (method keyAtValue:ifAbsent: (value exnBlock)"
               , "        (self associationsDo: [block (x) "
               ,
                "            (((x value) = value) ifTrue: {(return (x key))})])"
               , "        (exnBlock value))"
               , ";      A key is removed by [[removeAt:ifAbsent]].   "
               , ";      <other methods of class [[KeyedCollection]]>= "
               , "    (method removeKey: (key)    "
               ,
        "        (self removeKey:ifAbsent: key {(self error: 'key-not-found)}))"
               , ";      The [[=]] method can be implemented once on class "
               , ";      [[KeyedCollection]], instead of separately for "
               , ";      dictionaries, lists, and arrays. Two keyed   "
               , ";      collections are equivalent if they have equivalent "
               , ";      associations. For efficiency, the code looks first "
               , ";      for an association that's in the receiver but not in "
               , ";      the argument. If it finds one, the collections are "
               , ";      not equivalent, and it returns [[false]] immediately. "
               , ";      Otherwise, it just has to confirm that both receiver "
               , ";      and argument have the same number of         "
               ,
                ";      associations—then and only then are they equivalent. "
               , ";      <other methods of class [[KeyedCollection]]>= "
               , "    (method = (collection)"
               ,
     "        (self associationsDo:    ; look for `anAssoc` not in `collection`"
               , "            [block (anAssoc)"
               , "               (((anAssoc value) !="
               ,
"                      (collection at:ifAbsent: (anAssoc key) {(return false)}))"
               , "                ifTrue:"
               , "                {(return false)})])"
               , "        ((self size) = (collection size)))"
               , ")"
               , ";  The classic keyed collection is [[Dictionary]]. "
               , ";  My implementation, which appears in \\crefusma.chap (\\ "
               , ";  cpagerefusma.Dictionary), is a simple list of "
               , ";  key-value pairs, just like the [[env]] type in \\cref "
               , ";  mlscheme.chap. Implementations using hash tables or "
               , ";  search trees can be explored in \\cref        "
               , ";  small.ex.hash,small.ex.bst.                  "
               , ";                                               "
               , ";  Implementation of [[SequenceableCollection]] "
               , ";                                               "
               , ";  The abstract class [[SequenceableCollection]] defines "
               , ";  methods used by keyed collections whose keys are "
               , ";  consecutive integers. Its concrete, predefined "
               , ";  subclasses are [[List]] and [[Array]].       "
               , ";  <collection classes>=                        "
               , "(class SequenceableCollection"
               , "    [subclass-of KeyedCollection] ; abstract class"
               , "    (method firstKey () (self subclassResponsibility))"
               , "    (method lastKey  () (self subclassResponsibility))"
               , "    (method last     () (self at: (self  lastKey)))"
               , "    (method first    () (self at: (self firstKey)))"
               , "    (method at:ifAbsent: (index exnBlock) [locals current]"
               , "        (set current (self firstKey))"
               , "        (self do: [block (v)"
               , "            ((current = index) ifTrue: {(return v)})"
               , "            (set current (current + 1))])"
               , "        (exnBlock value))"
               , ";      Because keys are consecutive integers, method "
               , ";      [[at:ifAbsent:]] can track the value of the key "
               , ";      inside a [[do:]] loop, without ever allocating an "
               , ";      [[Association]]. This implementation is more "
               , ";      efficient than the generic implementation inherited "
               , ";      from class [[KeyedCollection]].              "
               , ";                                                   "
               , ";      Method [[associationsDo:]] also loops over   "
               , ";      consecutive keys.                            "
               , ";      <other methods of class [[SequenceableCollection]]>= "
               , "    (method associationsDo: (bodyBlock) [locals i last]"
               , "        (set i    (self firstKey))"
               , "        (set last (self lastKey))"
               , "        ({(i <= last)} whileTrue:"
               ,
   "            {(bodyBlock value: (Association withKey:value: i (self at: i)))"
               , "             (set i (i + 1))}))"
               , ")"
               , ";  Template for a definition of class [[Natural]] [*] "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                               "
               , ";  Choice of representation: Natural numbers    "
               , ";                                               "
               , ";  [*] Using representations that I have defined, the "
               , ";  examples above demonstrate techniques used to "
               , ";  implement complex methods like [[+]] and [[<]]. The "
               , ";  same techniques can be applied to a representation "
               , ";  that you can define: a representation of natural "
               , ";  numbers (\\crefsmall.ex.Natural). To get started, "
               , ";  follow the guidance below, which presents hints, "
               , ";  ideas, and private protocols for two possible "
               , ";  representations.                             "
               , ";                                               "
               , ";  For efficiency, a natural number should be   "
               , ";  represented as a sequence of digits in some base b (\\ "
               , ";  crefarith.bignums). The sequence may reasonably be "
               , ";  represented as an array or as a list. uSmalltalk's "
               , ";  [[Array]] class works fine here, but the predefined "
               , ";  [[List]] class does not; you are better off defining "
               , ";  empty and nonempty lists of digits as subclasses of "
               , ";  class [[Natural]]. For this reason, I refer to the "
               , ";  two representations as the ``array representation'' "
               , ";  and the ``subclass representation.''         "
               , ";  Each representation calls for its own private "
               , ";  protocol to be used to implement the complex methods. "
               , ";  And both can start with the template in \\vrefbracket "
               , ";  small.fig.Natural-template.                  "
               , ";                                               "
               , ";  Natural numbers: The array representation    "
               , ";                                               "
               , ";  If a natural number is represented using an array of "
               , ";  digits, I recommend giving it two instance variables: "
               , ";  [[degree]] and [[digits]]. \\stdbreak[3000] The "
               , ";  representation invariant should be as follows: "
               , ";  [[digits]] should be an array containing at least "
               , ";  degree+1 integers, each of which lies in the range 0 "
               , ";  <=x_i < b, where b is the base. \\stdbreak[3000] If  "
               , ";  [[digits]] contains coefficients x_i, where 0 <=i <= "
               , ";  degree, then the abstraction function says that the "
               , ";  object represents natural number X, where    "
               , ";                                               "
               , ";   X = \\sum_i = 0^degree x_i \\atimesb^i.       "
               , ";                                               "
               , ";  \\ptrim1                                      "
               , ";                                               "
               , ";  With this array representation, I recommend the "
               , ";  private protocol shown in \\vref              "
               , ";  small.fig.Natural.array.                     "
               , ";                                               "
               , ";    • The [[base]] method on the class provides a "
               , ";   single point of truth about b, which you choose. "
               , ";                                               "
               , ";    • The digit-related methods are used to read, "
               , ";   write, and iterate over digits.             "
               , ";                                               "
               , ";    • Methods [[trim]] and [[degree]] are used to keep "
               , ";   the arrays as small as possible, so leading "
               , ";   zeroes don't accumulate.                    "
               , ";                                               "
               ,
";   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";   {sfprotocol}Private class method for class  "
               , ";   [[Natural]]class [[Natural]] \\aswidthof\\pm  "
               , ";   basedigit:put: anIndex aDigit --- Answers b. "
               ,
";   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";   {sfprotocol}                                "
               , ";                                               "
               , ";   \\subfigskip {sfprotocol}Private instance methods "
               , ";   for class [[Natural]][[Natural]] \\pmdigit:  "
               , ";   anIndex --- Upon receiving \\monoboxdigit: i, "
               , ";   answer x_i. Should work for any nonnegative i, no "
               , ";   matter how large.                           "
               , ";   \\pmdigit:put: anIndex aDigit --- On receiving \\ "
               , ";   monoboxdigit:put: i y, mutate the receiver, "
               , ";   making x_i = y. Although [[Natural]] is not a "
               , ";   mutable type (and therefore this method should "
               , ";   never be called by clients), it can be quite "
               , ";   useful to mutate individual digits while you are "
               , ";   constructing a new instance.                "
               , ";   \\pmdigits: aSequence --- Take a sequence of x_i "
               , ";   and use it to initialize [[digits]] and     "
               , ";   [[degree]].                                 "
               , ";   \\pmdoDigitIndices: aBlock --- For i from zero to "
               , ";   [[degree]], send [[value]] i to [[aBlock]]. "
               ,
";   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";   \\pmtrim --- Set [[degree]] on the receiver as "
               , ";   small as possible, and answer the receiver. "
               , ";   \\pmdegree --- Answer the [[degree]] of the  "
               , ";   receiver.                                   "
               ,
";   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";   \\pmmakeEmpty: aDegree --- Set [[digits]] to an "
               , ";   array suitable for representing natural numbers "
               , ";   of the specified degree. (Also change the   "
               , ";   [[degree]] of the receiver to [[aDegree]].) "
               ,
";   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";   {sfprotocol} Suggested private methods for class "
               , ";   [[Natural]], array representation [*]       "
               ,
";   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                               "
               , ";    • Method [[makeEmpty:]] is used to initialize newly "
               , ";   allocated instances.                        "
               , ";                                               "
               , ";  The array representation offers these trade-offs: "
               , ";  Because it provides easy access to any digit you "
               , ";  like, it enables you to treat Smalltalk as if it were "
               , ";  a procedural language, like C. In particular, you can "
               , ";  get away without thinking too hard about dynamic "
               , ";  dispatch, because a lot of decisions can be made by "
               , ";  looking at digits and at [[degree]]. But the "
               , ";  individual methods are a little complicated, and you "
               , ";  may not learn a whole lot—my array-based code uses "
               , ";  objects only to hide information from client code, "
               , ";  and it doesn't exploit dynamic dispatch or   "
               , ";  inheritance.                                 "
               , ";                                               "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  {sfprotocol}Private class methods for class  "
               , ";  [[Natural]]class [[Natural]] \\aswidthof\\pm   "
               , ";  baseminus:borrow: aNatural c --- Answers b, the base "
               , ";  of [[Natural]] numbers.                      "
               , ";  \\longmethod*first:rest: anInteger aNatural Answers a "
               , ";  [[Natural]] number representing anInteger + aNatural  "
               , ";  \\atimesb.                                    "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  {sfprotocol}                                 "
               , ";                                               "
               , ";  \\subfigskip {sfprotocol}Private instance methods for "
               , ";  class [[Natural]][[Natural]] \\pmmodBase --- Answers a "
               , ";  small integer whose value is the receiver modulo b. "
               , ";  \\pmdivBase --- Answers a [[Natural]] whose value is "
               , ";  the receiver divided by b.                   "
               , ";  \\pmtimesBase --- Answers a [[Natural]] whose value is "
               , ";  the receiver multiplied by b.                "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  \\pmcompare:withLt:withEq:withGt: aNatural ltBlock "
               , ";  eqBlock gtBlock --- \\break Compares [[self]] with "
               , ";  [[aNatural]]. If [[self]] is smaller than    "
               , ";  [[aNatural]], evaluate [[ltBlock]]. If they are "
               , ";  equal, evaluate [[eqBlock]]. If [[self]] is greater, "
               , ";  evaluate [[gtBlock]].                        "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  \\pmplus:carry: aNatural c --- Answer the sum self + "
               , ";  aNatural + c, where c is a carry bit (either 0 or 1). "
               , ";  \\pmminus:borrow: aNatural c --- Compute the  "
               , ";  difference self - (aNatural + c), where c is a borrow "
               , ";  bit (either 0 or 1). If the difference is    "
               , ";  nonnegative, answer the difference; otherwise, halt "
               , ";  the program with a checked run-time error.   "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";  {sfprotocol} Suggested private methods for class "
               , ";  [[Natural]], subclass representation [*]     "
               ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                               "
               , ";  Natural numbers: The subclass representation "
               , ";                                               "
               , ";  If a natural number is represented as a list of "
               , ";  digits, I recommend defining two additional classes "
               , ";  that inherit from [[Natural]]: [[NatZero]] and "
               , ";  [[NatNonzero]]. An instance of class [[NatZero]] "
               , ";  represents zero, and it doesn't need any instance "
               , ";  variables. An instance of class [[NatNonzero]] "
               , ";  represents the natural number x_0 + X' \\atimesb, "
               , ";  where x_0 is a digit (a small integer), X' is a "
               , ";  natural number, and b is the base. Class     "
               , ";  [[NatNonzero]] needs instance variables for x_0 and  "
               , ";  X'; these might be called [[x-0]] and        "
               , ";  [[other-digits]]. The representation invariants are "
               , ";  that x_0 and X' are not both zero, and 0 <=x_0 < b. "
               , ";                                               "
               , ";  With the subclass representation, I recommend the "
               , ";  private protocol in \\vrefsmall.fig.Natural.list. "
               , ";                                               "
               , ";    • As with arrays, class method [[base]] provides a "
               , ";   single point of truth about b.              "
               , ";                                               "
               , ";    • Class method [[first:rest:]] creates a new "
               , ";   instance of one of the two subclasses. If both "
               , ";   arguments are zero, it answers an instance of "
               , ";   class [[NatZero]]. Otherwise, it answers an "
               , ";   instance of class [[NatNonzero]].           "
               , ";                                               "
               , ";    • Private methods [[modBase]], [[divBase]], and "
               , ";   [[timesBase]], together with public method  "
               , ";   [[isZero]] (\\crefsmall.fig.Natural), are the "
               , ";   protocol that allows a method to inspect its "
               , ";   argument. If a natural number X is x_0 + X' \\ "
               , ";   atimesb, then [[modBase]] answers x_0 and   "
               , ";   [[divBase]] answers X'. (If a natural number is "
               , ";   zero, it answers all of these messages with "
               , ";   zero.)                                      "
               , ";                                               "
               , ";   \\ztrim1                                     "
               , ";                                               "
               , ";    • The comparison method simplifies the   "
               , ";   implementations of methods [[<]] and [[=]], which "
               , ";   are subclass responsibilities of class      "
               , ";   [[Natural]] (from the [[Magnitude]] protocol). "
               , ";                                               "
               , ";    • Methods [[plus:carry:]] and [[minus:borrow:]] "
               , ";   implement functions \\adc and \\sbb, which are "
               , ";   explained in \\crefarith.bignums.            "
               , ";                                               "
               , ";  \\zbreak[8000]                                "
               , ";                                               "
               , ";  The subclass representation offers these trade-offs: "
               , ";  Because it provides easy access only to the least "
               , ";  significant digit of a natural number (using "
               , ";  [[modBase]]), it forces you to treat the other digits "
               , ";  abstractly. The abstraction implies that many "
               , ";  decisions about what to do next and when algorithms "
               , ";  should terminate are made implicitly by dynamic "
               , ";  dispatch: in each method, the action is determined by "
               , ";  the class on which the method is defined. And each "
               , ";  individual method is therefore simpler than  "
               , ";  corresponding methods that use the array     "
               , ";  representation; for example, the [[+]] method defined "
               , ";  on class [[NatZero]] simply answers its argument, and "
               , ";  the [[*]] method simply answers zero.        "
               , ";  No conditionals, no scrutiny, end of story. But "
               , ";  although the individual methods are simple, the "
               , ";  overall algorithm makes sense only once you  "
               , ";  understand dynamic dispatch.                 "
               , ";                                               "
               , ";  {\\quasilarge Technique IV: Invariants in     "
               , ";  object-oriented programming}                 "
               , ";                                               "
               , ";  [*] Object-oriented programmers use the same "
               , ";  program-design techniques that are described in \\cref "
               , ";  mcl.chap in the context of abstract data     "
               , ";  types—especially abstraction functions and "
               , ";  invariants. In this section, an abstraction function "
               , ";  and representation invariant are used to implement a "
               , ";  mutable linked list with an appealing cost model: "
               , ";  linear-time traversal and constant-time access to "
               , ";  first and last elements.                     "
               , ";                                               "
               , ";  As in micro-Scheme, the representation uses cons "
               , ";  cells. But unlike micro-Scheme code, uSmalltalk code "
               , ";  never asks a list if it is empty or nonempty. "
               , ";  Instead, empty and nonempty lists are represented by "
               , ";  objects of different classes, and decisions are made "
               , ";  by dispatching to the right method.          "
               , ";                                               "
               , ";  To support efficient insertion and deletion at either "
               , ";  end of a list, I represent it using a circular list "
               , ";  of cons cells. This representation relies on a "
               , ";  sophisticated invariant: both the beginning and end "
               , ";  of the list are marked by a special cons cell, which "
               , ";  is called a sentinel. A sentinel is a standard "
               , ";  technique that often simplifies the implementation of "
               , ";  a data structure [cite sedgewick:algorithms]. And in "
               , ";  Smalltalk, the sentinel can handle all the special "
               , ";  cases normally associated with the end of a list, "
               , ";  just by defining appropriate methods. As a result, "
               , ";  the list code does not contain even one conditional "
               , ";  that checks if a list is empty.              "
               , ";                                               "
               , ";  Just as in micro-Scheme, a cons cell holds two "
               , ";  values: a car and a cdr. Unlike in micro-Scheme, the "
               , ";  cdr always points to another cons cell. This "
               , ";  invariant holds because every list is circularly "
               , ";  linked—the other cons cell might be a sentinel. For "
               , ";  example, a list containing the elements 1, 2, and 3 "
               , ";  (plus a sentinel) is structured as follows:  "
               , ";                                               "
               , ";   \\zvspace-1.2 {tikzpicture}[x=1cm,y=1cm,semithick] "
               , ";   \\draw(-0.5,1) node (list) (-0.5,0) node [draw= "
               , ";   black,rectangle] (sentinel) sentinel (1,0) node "
               , ";   [draw=black,rectangle] (node1) 1 (2,0) node [draw "
               , ";   =black,rectangle] (node2) 2 (3,0) node [draw= "
               , ";   black,rectangle] (node3) 3 ; \\draw[->] (list) – "
               , ";   (sentinel) ; \\draw[->] (sentinel) – (node1) ; \\ "
               , ";   draw[->] (node1) – (node2) ; \\draw[->] (node2) – "
               , ";   (node3); \\draw[->,dashed] ((sentinel.north)+ "
               , ";   (5pt,0)) to [out=90] [in=20] ((node3.east)+ "
               , ";   (0,2pt)) ; \\draw[<-] ((sentinel.south)) to [out= "
               , ";   -90] [in=-20] ((node3.east)+(0,-2pt)) ;     "
               , ";   {tikzpicture}                               "
               , ";                                               "
               , ";  The sentinel contains two pointers: the [[cdr]] "
               , ";  (solid line), which it inherits from class [[Cons]], "
               , ";  points to the elements of the list, if any; the "
               , ";  [[pred]] (dashed line), which is found only on "
               , ";  objects of class [[ListSentinel]], points to the "
               , ";  sentinel's predecessor.                      "
               , ";                                               "
               , ";  \\ztrim1.0                                    "
               , ";                                               "
               , ";  A sentinel's predecessor is normally the last element "
               , ";  of its list, but when a list is empty, both fields of "
               , ";  its sentinel point to the sentinel itself:   "
               , ";                                               "
               , ";   \\pssetunit=1cm {pspicture}(-2, -0.9)(5, 1.1) \\ "
               , ";   pnode(-0.5, 1)elist \\rput(-0.5,0)\\rnodeesentinel\\ "
               , ";   psframeboxsentinel \\pssetnodesep=0pt \\ncline-> "
               , ";   elistesentinel \\nccurve[linestyle=dashed,angleA= "
               , ";   0,angleB=90,ncurv=4,offsetB=5pt,offsetA=2pt] "
               , ";   <-esentinelesentinel \\nccurve[angleA=0,angleB= "
               , ";   -90,ncurv=3,offsetA=-2pt]->esentinelesentinel "
               , ";   {pspicture} \\zvspace-0.4                    "
               , ";                                               "
               , ";  When the [[cdr]] points to the sentinel itself, the "
               , ";  abstraction function maps the representation to the "
               , ";  empty sequence. When the [[cdr]] points to another "
               , ";  object, that object is a cons cell, and the  "
               , ";  abstraction function maps the representation to the "
               , ";  sequence of objects stored in the cons cells pointed "
               , ";  to by the [[cdr]] of the sentinel.           "
               , ";                                               "
               , ";  Each cons cell, including the sentinel, responds to "
               , ";  the protocol shown in \\vrefbracket           "
               , ";  small.fig.Cons-protocol.                     "
               , ";                                               "
               , ";  A [[List]] object has only one instance variable: a "
               , ";  pointer to the sentinel.                     "
               , ";  <collection classes>=                        "
               , ";  If [[n]] is out of range, the method can produce "
               , ";  wrong answers—which can be made right (\\cref "
               , ";  small.ex.List-at:put:).                      "
               , ";                                               "
               , ";  The low-level work of manipulating pointers is done "
               , ";  by the methods in the cons-cell protocol (\\cref "
               , ";  small.fig.Cons-protocol).                    "
               , ";  <classes that define cons cells and sentinels>= "
               , "(class Cons"
               , "    [subclass-of Object]"
               , "    [ivars car cdr]"
               , ";      \\zvfilbreak1.0in                             "
               , ";                                                   "
               , ";      The first four methods of class [[Cons]] expose the "
               , ";      representation as a pair of car and cdr. And the "
               , ";      [[pred:]] method makes it possible to tell any cons "
               ,
               ";      cell what its predecessor is—information that is used "
               , ";      only by the sentinel. (A sentinel is an instance of a "
               , ";      subclass of [[Cons]].)                       "
               , ";      <methods of class [[Cons]]>=                 "
               , "    (method car ()           car)"
               , "    (method cdr ()           cdr)"
               , "    (method car: (anObject)  (set car anObject) self)"
               , "    (method cdr: (anObject)  (set cdr anObject) self)"
               , "    (method pred: (aCons)    nil)"
               , ";      Methods [[deleteAfter]] and [[insertAfter:]] "
               , ";      implement the standard pointer manipulations for a "
               , ";      circularly linked list. Circularity comes into play "
               , ";      when a node is deleted or inserted; sending [[pred:]] "
               , ";      notifies the node's successor of its new predecessor. "
               , ";      <methods of class [[Cons]]>=                 "
               , "    (method deleteAfter () [locals answer]"
               , "        (set answer (cdr car))"
               , "        (set cdr    (cdr cdr))"
               , "        (cdr pred: self)"
               , "        answer)"
               , "    (method insertAfter: (anObject)"
               , "        (set cdr (((Cons new) cdr: cdr) car: anObject))"
               , "        ((cdr cdr) pred: cdr)"
               , "        anObject)"
               , ";      The iteration and removal methods take full advantage "
               , ";      of object-oriented programming. By defining [[do:]] "
               , ";      differently on classes and [[ListSentinel]], I create "
               , ";      code that iterates over a list without ever using an "
               , ";      explicit [[if]] or [[while]]—all it does is method "
               , ";      dispatch. To make the computation a little clearer, "
               , ";      I present some of the methods of class [[Cons]] right "
               , ";      next to the corresponding methods of class   "
               , ";      [[ListSentinel]].                            "
               , ";                                                   "
               , ";      The [[do:]] method iterates over a list of cons cells "
               , ";      by first doing the [[car]], then continuing with a "
               , ";      tail call to the [[do:]] method of the [[cdr]]. The "
               , ";      iteration terminates in the sentinel, whose [[do:]] "
               , ";      method does nothing.                         "
               , ";      <methods of class [[Cons]]>=                 "
               ,
             "    (method do: (aBlock)       ; defined on an ordinary cons cell"
               , "        (aBlock value: car)"
               , "        (cdr do: aBlock))"
               , ";      Similarly, method [[rejectOne:ifAbsent:withPred:]] "
               , ";      checks the current cons cell to see if it should be "
               , ";      removed, and if so, sends [[deleteAfter]] to its "
               , ";      predecessor, which is passed as parameter [[pred]]. "
               , ";      Otherwise, the method tries the next cons cell. "
               , ";      If the method reaches the sentinel, it hasn't found "
               , ";      what it's looking for, \\qbreak and it terminates the "
               , ";      loop by sending the [[value]] message to the "
               , ";      exception block.                             "
               , ";      <methods of class [[Cons]]>=                 "
               ,
               "    (method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)"
               , "        ((aBlock value: self) ifTrue:ifFalse:"
               , "            {(pred deleteAfter)}"
               ,
       "            {(cdr rejectOne:ifAbsent:withPred: aBlock exnBlock self)}))"
               , ")"
               , ";  The final instance methods of class [[ListSentinel]] "
               , ";  expose the [[pred]] pointer. And class method [[new]] "
               , ";  allocates a new sentinel, whose [[pred]] and [[cdr]] "
               , ";  both point to itself. Such a sentinel represents an "
               , ";  empty list.                                  "
               , ";  <classes that define cons cells and sentinels>= "
               , "(class ListSentinel"
               , "    [subclass-of Cons]"
               , "    [ivars pred]"
               , "    (method pred: (aCons)   (set pred aCons))"
               , "    (method pred  ()        pred)"
               , "    (class-method new ()    "
               , "        [locals tmp]"
               , "        (set tmp (super new))"
               , "        (tmp pred: tmp)"
               , "        (tmp  cdr: tmp)"
               , "        tmp)"
               , ";      <iterating methods of class [[ListSentinel]]>= "
               , "    (method do: (aBlock) nil)  ; defined on a sentinel"
               , ";      <iterating methods of class [[ListSentinel]]>= "
               ,
               "    (method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)"
               , "        (exnBlock value)))"
               , "(class List"
               , "    [subclass-of SequenceableCollection]"
               , "    [ivars sentinel]"
               ,
   "    (class-method new ()        ((super new) sentinel: (ListSentinel new)))"
               ,
              "    (method sentinel: (s)       (set sentinel s) self) ; private"
               , "    (method isEmpty   ()        (sentinel == (sentinel cdr)))"
               , "    (method last      ()        ((sentinel pred) car))"
               , "    (method do:       (aBlock)  ((sentinel cdr) do: aBlock))"
               , ";      The method [[addLast:]] mutates a list by adding an "
               , ";      element to the end. This means inserting an element "
               , ";      just after the predecessor of the sentinel.  "
               , ";      Similarly, [[addFirst:]] inserts an element just "
               , ";      after the sentinel. Having a sentinel means there is "
               , ";      no special-case code for an empty list.      "
               , ";      <other methods of class [[List]]>=           "
               ,
      "    (method addLast:  (item)   ((sentinel pred) insertAfter: item) self)"
               ,
      "    (method addFirst: (item)   (sentinel insertAfter: item)        self)"
               , "    (method add:      (item)   (self addLast: item))"
               , ";      Method [[removeFirst]] removes the element after the "
               , ";      sentinel; [[removeLast]] is left as Exercise [->]. "
               , ";      <other methods of class [[List]]>=           "
               , "    (method removeFirst ()     (sentinel deleteAfter))"
               , "    (method removeLast  ()     (self leftAsExercise))"
               , ";      Method [[remove:ifAbsent:]], which removes an element "
               , ";      holding a given object, uses the private cons-cell "
               , ";      protocol described in \\crefsmall.fig.Cons-protocol; "
               , ";      the private method [[rejectOne:ifAbsent:withPred:]] "
               , ";      is modeled on the more general [[reject:]] method "
               , ";      defined on all collections.                  "
               , ";      <other methods of class [[List]]>=           "
               , "    (method remove:ifAbsent: (oldObject exnBlock)"
               , "        ((sentinel cdr)"
               , "            rejectOne:ifAbsent:withPred:"
               , "            [block (x) (oldObject = (x car))]"
               , "            exnBlock"
               , "            sentinel))"
               , ";      Method [[ removeKey:ifAbsent:]] is left as an "
               , ";      exercise.                                    "
               , ";      <other methods of class [[List]]>=           "
               ,
           "    (method removeKey:ifAbsent: (n exnBlock) (self leftAsExercise))"
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";      {sfprotocol}Instance protocol for all cons cells "
               , ";      [[Cons]] \\mcar --- Answer the car of the receiver. "
               , ";      \\mcdr --- Answer the cdr of the receiver.    "
               ,
                ";      \\mcar: anObject --- Set the receiver's car and answer "
               , ";      the receiver.                                "
               ,
                ";      \\mcdr: anObject --- Set the receiver's cdr and answer "
               , ";      the receiver.                                "
               , ";      \\mpred: aCons --- Notify the receiver that its "
               , ";      predecessor is the cons cell [[aCons]].      "
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";      \\mdeleteAfter --- Delete the cons cell that the "
               , ";      receiver's cdr points to. Answer the car of that cons "
               , ";      cell.                                        "
               , ";      \\minsertAfter: anObject --- Insert a new cons cell "
               , ";      after the receiver, letting the new cons cell's car "
               , ";      point to [[anObject]]. Answer [[anObject]].  "
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";      \\mdo: aBlock --- For each cons cell [[c]] in the "
               , ";      receiver, excluding the sentinel, use a [[value:]] "
               , ";      message to send \\monobox(c car) to [[aBlock]]. "
               , ";      \\mrejectOne:ifAbsent:withPred: aBlock exnBlock aCons "
               , ";      --- \\break Starting at the receiver, search the list "
               , ";      for a cons cell [[c]] such that \\monobox(aBlock "
               , ";      value: c) is true. If such a cell is found, remove "
               , ";      it. Otherwise, answer \\monobox(exnBlock value). As a "
               , ";      precondition, the argument [[aCons]] must be the "
               , ";      predecessor of the receiver.                 "
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";      {sfprotocol}                                 "
               , ";                                                   "
               , ";      \\subfigskip {sfprotocol}Instance protocol for "
               , ";      sentinels only[[ListSentinel]] \\mpred --- Answer the "
               , ";      predecessor of the receiver.                 "
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";      {sfprotocol}                                 "
               , ";                                                   "
               , ";      Protocols for cons cells [*]                 "
               ,
";      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
               , ";                                                   "
               , ";      [[List]] is a subclass of [[SequenceableCollection]], "
               , ";      so it must answer messages involving integer keys. "
               , ";      The first key in a [[List]] is always 0.     "
               , ";      <other methods of class [[List]]>=           "
               , "    (method firstKey () 0)"
               , "    (method lastKey  () ((self size) - 1))"
               , ";      List element [[n]] is updated by skipping [[n]] cons "
               , ";      cells and then sending the next cons call the "
               , ";      [[car:]] message.                            "
               , ";      <other methods of class [[List]]>=           "
               , "    (method at:put: (n value) [locals tmp]"
               , "        (set tmp (sentinel cdr))"
               , "        ({(n isZero)} whileFalse:"
               , "           {(set n (n - 1))"
               , "            (set tmp (tmp cdr))})"
               , "        (tmp car: value)"
               , "        self)"
               , ")"
               , ";  Implementation of [[Set]]                    "
               , ";                                               "
               , ";  [*] [[Set]] is a concrete class: it has instances. "
               , ";  And an instance of [[Set]] is an abstraction, so all "
               , ";  the technology from \\crefmcl.chap comes into play: "
               , ";  to implement [[Set]], I need to know what the "
               , ";  abstraction is, what the representation is, what the "
               , ";  abstraction function is, what the representation "
               , ";  invariant is, and what operations need to be "
               , ";  implemented.                                 "
               , ";                                               "
               , ";  The abstraction is a set of objects. Like most other "
               , ";  Smalltalk collections, a [[Set]] is mutable; for "
               , ";  example, sending [[add:]] to a set changes the set. "
               , ";  The representation is a list containing the members "
               , ";  of the set; that list is stored in a single instance "
               , ";  variable, [[members]]. The list is represented by a "
               , ";  [[List]] object; this structure makes [[Set]] a "
               , ";  client of [[List]], not a subclass or superclass. "
               , ";  The abstraction function takes the list of members "
               , ";  and returns the set containing exactly those members. "
               , ";  The representation invariant is that [[members]] "
               , ";  contains no repeated elements.               "
               , ";                                               "
               , ";  The abstraction, representation, abstraction "
               , ";  function, and invariant are as they would be in a "
               , ";  language with abstract data types. But the operations "
               , ";  that need to be implemented are different. It is true "
               , ";  that a [[Set]] object needs to implement everything "
               , ";  in its interface, which means the entire     "
               , ";  [[Collection]] protocol. But it doesn't do all the "
               , ";  work itself: almost all of the protocol is \\qbreak "
               , ";  implemented in class [[Collection]], and [[Set]] "
               , ";  inherits those implementations. The only methods that "
               , ";  must be implemented in [[Set]] are the ``subclass "
               , ";  responsibility'' methods [[do:]], [[add:]],  "
               , ";  [[remove:ifAbsent:]], [[=]], \\basislabelSet and "
               , ";  [[species]], plus the private method [[printName]]. "
               , ";  <collection classes>=                        "
               , "(class Set"
               , "    [subclass-of Collection]"
               ,
               "    [ivars members]  ; list of elements [invariant: no repeats]"
               , "    (class-method new () ((super new) initSet))"
               ,
             "    (method initSet   () (set members (List new)) self) ; private"
               , "    (method do: (aBlock) (members do: aBlock))"
               , "    (method add: (item)"
               ,
             "        ((members includes: item) ifFalse: {(members add: item)})"
               , "        self)"
               , "    (method remove:ifAbsent: (item exnBlock) "
               , "        (members remove:ifAbsent: item exnBlock)"
               , "        self)"
               , "    (method = (s)"
               , "      (((self size) = (s size)) ifFalse:ifTrue:"
               , "         { (return false) }"
               ,
 "         { (self do: [block (x) ((s includes: x) ifFalse: {(return false)})])"
               , "           (return true)"
               , "         }))"
               , ")"
               , ";  \\qbreak                                      "
               , ";                                               "
               , ";  Implementation of [[Association]]            "
               , ";                                               "
               , ";  Method [[associationsDo:]] visits all the key-value "
               , ";  pairs in a keyed collection. A key-value pair is "
               , ";  represented by an object of class [[Association]]. "
               , ";  <collection classes>=                        "
               , "(class Association"
               , "   [subclass-of Object]"
               , "   [ivars key value]"
               ,
         "   (class-method withKey:value: (x y) ((self new) setKey:value: x y))"
               ,
      "   (method setKey:value: (x y) (set key x) (set value y) self) ; private"
               , "   (method key       ()  key)"
               , "   (method value     ()  value)"
               , "   (method setKey:   (x) (set key   x))"
               , "   (method setValue: (y) (set value y))"
               ,
             "   (method =         (a) ((key = (a key)) & (value = (a value))))"
               , ")"
               , ";  Implementation of [[Dictionary]]             "
               , ";                                               "
               , ";  [*] A [[Dictionary]] is the simplest and least "
               , ";  specialized of the keyed collections. If all "
               , ";  uSmalltalk objects could be hashed, I would want to "
               , ";  represent a [[Dictionary]] as a hash table. \\ "
               , ";  basislabelDictionary Because not every uSmalltalk "
               , ";  object can be hashed, I use a list of [[Association]] "
               , ";  s instead. The abstraction is a finite map, which is "
               , ";  to say, a function with a finite domain.     "
               , ";  The representation is a list of [[Association]]s "
               , ";  stored in instance variable [[table]].       "
               , ";  The representation invariant is that in [[table]], no "
               , ";  single [[key]] appears in more than one      "
               , ";  [[Association]]. The abstraction function takes the "
               , ";  representation to the function that is undefined on "
               , ";  all [[key]]s not in [[table]] and that maps each "
               , ";  [[key]] in [[table]] to the corresponding [[value]]. "
               , ";  <collection classes>=                        "
               , "(class Dictionary"
               , "    [subclass-of KeyedCollection]"
               , "    [ivars table] ; list of Associations"
               , "    (class-method new ()      ((super new) initDictionary))"
               ,
          "    (method initDictionary () (set table (List new)) self) ; private"
               , ";      The operations that [[Dictionary]] must implement are "
               , ";      [[associationsDo:]], [[at:put]], and         "
               , ";      [[removeKey:ifAbsent]]. Iteration over associations "
               , ";      can be delegated to the list of associations. Method "
               , ";      [[at:put:]] searches for the association containing "
               , ";      the given key. If it finds such an association, "
               , ";      it mutate the association's value. If it finds no "
               , ";      such association, it adds one.               "
               , ";                                                   "
               , ";      <other methods of class [[Dictionary]]>=     "
               , "    (method associationsDo: (aBlock) (table do: aBlock))"
               , "    (method at:put: (key value) [locals tempassoc]"
               , "        (set tempassoc (self associationAt:ifAbsent: key {}))"
               , "        ((tempassoc isNil) ifTrue:ifFalse:"
               ,
            "             {(table add: (Association withKey:value: key value))}"
               , "             {(tempassoc setValue: value)})"
               , "        self)"
               , ";      <other methods of class [[Dictionary]]>=     "
               , "    (method removeKey:ifAbsent: (key exnBlock)"
               , "       [locals value-removed] ; value found if not absent"
               ,
"       (set value-removed (self at:ifAbsent: key {(return (exnBlock value))}))"
               ,
"       (set table (table reject: [block (assn) (key = (assn key))])) ; remove assoc"
               , "       value-removed)"
               , ";      Because more than one association might have the same "
               , ";      value, it makes no sense to implement        "
               , ";      [[remove:ifAbsent:]].                        "
               , ";      <other methods of class [[Dictionary]]>=     "
               , "    (method remove:ifAbsent: (value exnBlock)"
               ,
                "       (self error: 'Dictionary-uses-remove:key:-not-remove:))"
               , ";      And because a dictionary requires not just a value "
               , ";      but also a key, the only sensible thing to add is an "
               , ";      [[Association]].                             "
               , ";      <other methods of class [[Dictionary]]>=     "
               , "    (method add: (anAssociation)"
               ,
               "      (self at:put: (anAssociation key) (anAssociation value)))"
               , ";      <other methods of class [[Dictionary]]>=     "
               , "    (method print () [locals print-comma]"
               , "        (set print-comma false)"
               , "        (self printName)"
               , "        (left-round print)"
               , "        (self associationsDo:"
               , "            [block (x) (space print)"
               ,
       "                       (print-comma ifTrue: {(', print) (space print)})"
               , "                       (set print-comma true)"
               , "                       ((x key) print)   (space print)"
               , "                       ('|--> print)     (space print)"
               , "                       ((x value) print)])"
               , "        (space print)"
               , "        (right-round print)"
               , "        self)"
               , ")"
               , ";  <collection classes>=                        "
               , "(class Array"
               ,
        "    [subclass-of SequenceableCollection] ; representation is primitive"
               , "    (class-method new: (size) (primitive arrayNew self size))"
               ,
 "    (class-method new  ()     (self error: 'size-of-Array-must-be-specified))"
               , "    (method size       ()     (primitive arraySize self))"
               ,
              "    (method at:        (key)       (primitive arrayAt self key))"
               ,
"    (method at:put:    (key value) (primitive arrayUpdate self key value) self)"
               ,
               "    (method printName  () nil) ; names of arrays aren't printed"
               , ";      The implementation of concrete class [[List]] is "
               , ";      described in detail in \\crefsmall.imp.List.  "
               , ";                                                   "
               , ";      Compromising on protocol: Class [[Array]]    "
               , ";                                                   "
               , ";      [*] Classes [[KeyedCollection]] and          "
               , ";      [[SequenceableCollection]] refine the [[Collection]] "
               , ";      protocol, adding new operations. Sometimes, however, "
               , ";      a class may want to remove operations from a "
               , ";      protocol; it wants to reuse methods defined in a "
               , ";      superclass while implementing only some of its "
               , ";      subclass responsibilities. A classic example is a "
               , ";      fixed-size array: it is a sequenceable collection, "
               , ";      and [[at:]] and [[at:put:]] take only constant time, "
               , ";      but after it is allocated, a fixed-size array cannot "
               , ";      grow or shrink. As a result, it does not implement "
               , ";      subclass responsibilities [[add:]],          "
               , ";      [[remove:ifAbsent]], or [[removeKey:ifAbsent]]; "
               , ";      sending any of those messages results in a checked "
               , ";      run-time error.                              "
               , ";      <other methods of class [[Array]]>=          "
               , "    (method add:                (x)   (self fixedSizeError))"
               , "    (method remove:ifAbsent:    (x b) (self fixedSizeError))"
               , "    (method removeKey:ifAbsent: (x b) (self fixedSizeError))"
               ,
  "    (method fixedSizeError      ()    (self error: 'arrays-have-fixed-size))"
               , ";      \\zvfilbreak0.8in                             "
               , ";                                                   "
               , ";      Because class [[Array]] exists to promote efficiency, "
               , ";      it overrides many inherited methods; in particular, "
               , ";      it uses primitives to implement methods [[size]], "
               , ";      [[at:]], and [[at:put:]]. These methods are then used "
               , ";      to implement [[firstKey]], [[lastKey]], and [[do:]]. "
               , ";      <other methods of class [[Array]]>=          "
               , "    (method firstKey () 0)"
               , "    (method lastKey  () ((self size) - 1))"
               , "    (method do: (aBlock) [locals index]"
               , "        (set index (self firstKey))"
               , "        ((self size) timesRepeat:"
               , "           {(aBlock value: (self at: index))"
               , "            (set index (index + 1))}))"
               , ";      <other methods of class [[Array]] ((prototype))>= "
               , "    (method select:  (_) (self leftAsExercise))"
               , "    (method collect: (_) (self leftAsExercise))"
               , ")"
               , ";  \\qvfilbreak2.2in                             "
               , ";                                               "
               , ";  Implementations of predefined classes        "
               , ";                                               "
               , ";  While many of the predefined classes use primitives, "
               , ";  none of their methods are implemented        "
               , ";  in ML—uSmalltalk code is sufficient.       "
               , ";                                               "
               , ";  Implementation of blocks                     "
               , ";                                               "
               , ";  A block is an abstraction of a function, and its "
               , ";  representation is primitive. The [[value]] method is "
               , ";  also primitive,[*] but the [[while]], [[whileTrue:]], "
               , ";  and [[whileFalse:]] methods are easily defined in "
               , ";  ordinary uSmalltalk.                         "
               , ";  <predefined uSmalltalk classes and values>= "
               , "(class Block"
               , "    [subclass-of Object] ; internal representation"
               , "    (class-method new () {})"
               ,
             "    (method value              ()         (primitive value self))"
               ,
          "    (method value:             (a1)       (primitive value self a1))"
               ,
       "    (method value:value:       (a1 a2)    (primitive value self a1 a2))"
               ,
    "    (method value:value:value: (a1 a2 a3) (primitive value self a1 a2 a3))"
               , "    (method value:value:value:value: (a1 a2 a3 a4) "
               , "        (primitive value self a1 a2 a3 a4))"
               , "    (method whileTrue: (body)"
               , "        ((self value) ifTrue:ifFalse:"
               , "            {(body value)"
               , "             (self whileTrue: body)}"
               , "            {nil}))"
               , "    (method whileFalse: (body) "
               , "         ((self value) ifTrue:ifFalse:"
               , "             {nil}"
               , "             {(body value) "
               , "              (self whileFalse: body)}))"
               , ";      <tracing methods on class [[Block]]>=        "
               , "    (method messageTraceFor: (n) [locals answer]"
               , "        (set &trace n)"
               , "        (set answer (self value))"
               , "        (set &trace 0)"
               , "        answer)"
               , "    (method messageTrace () (self messageTraceFor: -1))"
               , ")"
               , ";  <predefined uSmalltalk classes and values>= "
               , "(class Symbol"
               , "    [subclass-of Object] ; internal representation"
               ,
            "    (class-method new  () (self error: 'can't-send-new-to-Symbol))"
               ,
          "    (class-method new: (aSymbol) (primitive newSymbol self aSymbol))"
               , "    (method       print  () (primitive printSymbol self))"
               , "    (method       hash   () (primitive hash self))"
               , ")"
               , ";  Implementation of compiled methods           "
               , ";                                               "
               , ";  A compiled method is just a box in which uSmalltalk "
               , ";  code can be stored, for use as an argument to "
               , ";  [[setMethod]]. It has no instance variables and "
               , ";  answers no special messages.                 "
               , ";  <predefined uSmalltalk classes and values>= "
               , "(class CompiledMethod"
               , "  [subclass-of Object]"
               , ")"
                ]
val initialXi =
  let val xdefs = stringsxdefs ("predefined classes", predefs)
      fun errmsg s = eprintlnTrace ("error in predefined class: " ^ s)
  in  readEvalPrintWith errmsg (xdefs, initialXi, noninteractive)
      before (if logging then print "\nops.predefined_ends ()\n" else ())
  end
(* <implementations of \usmalltalk\ primitives and definition of [[initialBasis]]>= *)
fun addVal x e xi = processDef (VAL (x, e), xi, noninteractive)

local 
  fun newInstance classname = SEND (nullsrc, VAR classname, "new", [])
in
  val initialXi = addVal "true"  (newInstance "True" ) initialXi
  val initialXi = addVal "false" (newInstance "False") initialXi
end
(* Now that the definitions of the predefined classes *)
(* have been processed, it's almost time to close the *)
(* cycles involving literals, blocks, and compiled *)
(* methods. But first the basis has to be extended with *)
(* [[VAL]] bindings for [[true]] and [[false]]. Because *)
(* the parser prevents user code from binding [[true]] *)
(* and [[false]], these bindings can't be created using *)
(* uSmalltalk code; instead, the bindings are added to *)
(* [[initialXi]] using ML code.                 *)
(* <boxed values 56>=                           *)
val _ = op addVal : name -> exp -> basis -> basis
(* \qbreak Now the cycles can be closed. All the *)
(* necessary classes should be defined, so if any cycle *)
(* fails to close, the interpreter halts with a fatal *)
(* error. [*]                                   *)
(* <implementations of \usmalltalk\ primitives and definition of [[initialBasis]]>= *)
val _ =
  ( saveLiteralClasses      initialXi
  ; saveTrueAndFalse        initialXi
  ; saveBlockClass          initialXi
  ; saveCompiledMethodClass initialXi
  ) handle NotFound n =>
      ( app eprint ["Fatal error: ", n, " is not predefined\n"]
      ; raise InternalError "this can't happen"
      )
  | e => ( eprintln "Error binding predefined classes into interpreter"; raise e
                                                                               )
(* <implementations of \usmalltalk\ primitives and definition of [[initialBasis]]>= *)
val initialXi = addVal "nil" (VALUE nilValue) initialXi
val initialBasis = initialXi
val primitiveBasis = primitives


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNSTREAM]] FOR \USMALLTALK, WHICH PRINTS STACK TRACES *)
(*                                                               *)
(*****************************************************************)

(* <function [[runStream]] for \usmalltalk, which prints stack traces>= *)
fun runStream inputName input interactivity basis = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs (inputName, input, prompts)
  in  readEvalPrintWith eprintlnTrace (xdefs, basis, interactivity)
  end 
(* More precisely, the stack of active sends is *)
(* displayed when an error message is printed. Function *)
(* [[eprintlnTrace]], defined below, prints an error *)
(* message and shows the stack of active sends. For it *)
(* to be called, it needs to be passed to       *)
(* [[readEvalPrintWith]]. That means that the uSmalltalk *)
(* interpreter needs its own implementation of  *)
(* [[runStream]]. (The other interpreters all share the *)
(* version defined in \crefmlinterps.chap, on \cpageref *)
(* mlinterps.runStream.) [*] \nwnarrowboxes     *)
(* <boxed values 77>=                           *)
val _ = op runStream : string -> TextIO.instream -> interactivity -> basis ->
                                                                           basis
fun dump_global_names () = app (println o fst) initialBasis  (*OMIT*)


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
(* Function [[distinctTyvars]] is used in multiple *)
(* interpreters.                                *)
(* <boxed values 137>=                          *)
val _ = op runPathWith : interactivity -> (string * basis -> basis)
(* <look at command-line arguments, then run>=  *)
val usage = ref (fn () => ())
(* <boxed values 138>=                          *)
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
(* <boxed values 139>=                          *)
val _ = op perform: action * string list -> unit
(* <look at command-line arguments, then run>=  *)
fun merge (_, a as FAIL _) = a
  | merge (a as FAIL _, _) = a
  | merge (DEFAULT, a) = a
  | merge (_, DEFAULT) = raise InternalError "DEFAULT on the right in MERGE"
  | merge (RUN_WITH _, right as RUN_WITH _) = right
  | merge (DUMP f, DUMP g) = DUMP (g o f)
  | merge (_, r) = FAIL "Interpret or dump, but don't try to do both"
(* Applying [[streamOfUnfold]] (\crefmlinterps.streams) *)
(* to an \monobox('a, 'b) xformer \mdbuse       *)
(* mlinterpsstreamOfUnfold produces a function that maps *)
(* a stream of A's to a stream of B's-with-error. *)
(* <boxed values 140>=                          *)
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
(* <boxed values 141>=                          *)
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
(* <boxed values 142>=                          *)
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
(* <boxed values 143>=                          *)
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

(*OMIT*)

(*****************************************************************)
(*                                                               *)
(*   TYPE ASSERTIONS FOR \USMALLTALK                             *)
(*                                                               *)
(*****************************************************************)

(* Utilities for manipulating classes (from the *)
(* Supplement) [*]                              *)
(* <type assertions for \usmalltalk ((elided))>= *)
(* Class objects and metaclasses                *)
(*                                              *)
(* [*] In both concrete and abstract syntax, a class *)
(* definition can define methods in two flavors: *)
(* instance method or class method. But in the  *)
(* operational semantics and at run time, there is only *)
(* one flavor: ``method.'' What's up with that? There *)
(* truly is just one mechanism for dynamic dispatch, *)
(* regardless of whether a message is sent to a class or *)
(* to an instance. The distinction between instance *)
(* method and class method is implemented by creating an *)
(* extra, hidden class for each class in the system: a  *)
(* metaclass. Metaclasses aren't needed for writing *)
(* typical Smalltalk code, but if you want to know how *)
(* the system implements the two flavors of methods or *)
(* how it creates new objects, metaclasses are  *)
(* essential.                                   *)
(*                                              *)
(* Metaclasses are governed by these invariants: *)
(*                                              *)
(*  \tightlist                                  *)
(*   • Every object is an instance of some class. *)
(*   • Every class is also an object—and is therefore an *)
(*  instance of some class.                     *)
(*   • A class whose instances are classes is called a *)
(*  metaclass.                                  *)
(*   • Classes and metaclasses are one to one: every *)
(*  class has a unique metaclass, and every metaclass *)
(*  has a unique instance.                      *)
(*   • The instance methods of a class are stored in the *)
(*  class object.                               *)
(*   • The class methods of a class are stored in the *)
(*  metaclass object.                           *)
(*   • Every metaclass is an instance of class *)
(*  [[Metaclass]].                              *)
(*   • If a class C has a superclass, the metaclass of *)
(*  its superclass is the superclass of its     *)
(*  metaclass. This invariant can be expressed as the *)
(*  algebraic law \monobox((C superclass) metaclass) *)
(*  = \monobox((C metaclass) superclass).       *)
(*   • Class [[Object]] has no superclass, and the *)
(*  superclass of its metaclass is class [[Class]]. *)
(*                                              *)
(* The invariants dictate that classes and metaclasses *)
(* be linked in memory in circular ways: because every *)
(* class points both to its superclass and to its *)
(* metaclass, the graph of class and metaclass objects *)
(* has a cycle. By itself, the ``subclass-of'' relation *)
(* has no cycles, but the ``instance-of'' relation has a *)
(* cycle, and the combined relation has an additional *)
(* cycle. The cycles are implemented by using mutable *)
(* state: every class object is first created with a *)
(* metaclass pointer that is [[PENDING]]; then its *)
(* metaclass object is created; and finally the original *)
(* class is mutated to point to the new metaclass. *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* <boxed values 7>=                            *)
val _ = mkClass     : name -> metaclass -> class -> name list -> method list ->
                                                                           class
val _ = methodDefns : class * class -> method_def list -> method list * method
                                                                            list
val _ = setMeta     : class * class -> unit
val _ = className   : class -> name
val _ = classId     : class -> metaclass ref
val _ = methodName  : method -> name
val _ = methodsEnv  : method list -> method env
val _ = findClassAndMeta : name * value ref env -> class * class
(* <type assertions for \usmalltalk ((elided))>= *)
(* \qtrim-2                                     *)
(*                                              *)
(* Cycles of blocks                             *)
(*                                              *)
(* The same drill that applies to literal expressions *)
(* also applies to blocks: Evaluating a [[block]] *)
(* expression creates an object of class [[Block]], *)
(* which also is not defined until its definition is *)
(* read. Blocks are supported by these functions defined *)
(* in \crefusma.chap:                           *)
(* <boxed values 13>=                           *)
val _ =   mkBlock : name list * exp list * value ref env * class * frame ->
                                                                           value
val _ =   saveBlockClass : value ref env -> unit
