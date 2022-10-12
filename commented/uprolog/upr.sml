(* <upr.sml>=                                   *)


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
(* <boxed values 113>=                          *)
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

(* Interlude: micro-Scheme in ML                *)
(*                                              *)
(* [*] \invisiblelocaltableofcontents[*]        *)
(*                                              *)
(* {epigraph}Conversation with David R. Hanson, coauthor *)
(* of A Retargetable C Compiler: Design and     *)
(* Implementation\break\citep                   *)
(* hanson-fraser:retargetable:book.             *)
(*                                              *)
(*  \myblock=\wd0 =0.8=0pt                      *)
(*                                              *)
(*  to \myblock\upshapeHanson: C is a lousy language *)
(*  to write a compiler in.                     *)
(*                                              *)
(* {epigraph} The interpreters in \crefrange    *)
(* impcore.chapgc.chap are written in C, which has much *)
(* to recommend it: C is relatively small and simple; *)
(* it is widely known and widely supported; its *)
(* perspicuous cost model makes it is easy to discover *)
(* what is happening at the machine level; and  *)
(* it provides pointer arithmetic, which makes it a fine *)
(* language in which to write a garbage collector. *)
(* But for implementing more complicated or ambitious *)
(* languages, C is less than ideal. In this and *)
(* succeeding chapters, I therefore present interpreters *)
(* written in the functional language Standard ML. *)
(*                                              *)
(* Standard ML is particularly well suited to symbolic *)
(* computing, especially functions that operate on *)
(* abstract-syntax trees; some advantages are detailed *)
(* in the sidebar \vpagerefmlscheme.good-ml. And an *)
(* ML program can illustrate connections between *)
(* language design, formal semantics, and       *)
(* implementations more clearly than a C program can. *)
(* Infrastructure suitable for writing interpreters *)
(* in ML is presented in this chapter and in \cref *)
(* mlinterps.chap,lazyparse.chap. That infrastructure is *)
(* introduced by using it to implement a language that *)
(* is now familiar: micro-Scheme.               *)
(*                                              *)
(* {sidebar}[t]Helpful properties of the ML family of *)
(* languages [*]                                *)
(*                                              *)
(*  \advance\parsepby -5.5pt \advance\itemsepby *)
(*  -5.5pt \advanceby -0.5pt                    *)
(*   • ML is safe: there are no unchecked run-time *)
(*  errors, which means there are no faults that are *)
(*  entirely up to the programmer to avoid.     *)
(*   • Like Scheme, ML is naturally polymorphic. *)
(*  Polymorphism simplifies everything. For example, *)
(*  unlike the C code in \crefrange             *)
(*  impcore.chapgcs.chap, the ML code in \crefrange *)
(*  mlscheme.chapsmall.chap uses just one       *)
(*  representation of lists and one length function. *)
(*  As another example, where the C code in \cref *)
(*  cinterps.chap defines three different types of *)
(*  streams, each with its own [[get]] function, the *)
(*  ML code in \crefmlinterps.chap defines one type *)
(*  of stream and one [[streamGet]] function. And *)
(*  when a job is done by just one polymorphic  *)
(*  function, not a group of similar functions, you *)
(*  know that the one function always does the same *)
(*  thing.                                      *)
(*   • Unlike Scheme, ML uses a static type system, and *)
(*  this system guarantees that data structures are *)
(*  internally consistent. For example, if one  *)
(*  element of a list is a function, every element of *)
(*  that list is a function. This happens without *)
(*  requiring variable declarations or type     *)
(*  annotations to be written in the code.      *)
(*                                              *)
(*  If talk of polymorphism mystifies you, don't *)
(*  worry; polymorphism in programming languages is *)
(*  an important topic in its own right. Polymorphism *)
(*  is formally introduced and defined in \cref *)
(*  typesys.chap, and the algorithms that ML uses to *)
(*  provide polymorphism without type annotations are *)
(*  described in \crefml.chap.                  *)
(*   • Like Scheme, ML provides first-class, nested *)
(*  functions, and its initial basis contains useful *)
(*  higher-order functions. These functions help *)
(*  simplify and clarify code. For example, they can *)
(*  eliminate the special-purpose functions that the *)
(*  C code uses to run a list of unit tests from back *)
(*  to front; the ML code just uses [[foldr]].  *)
(*   • To detect and signal errors, ML provides *)
(*  exception handlers and exceptions, which are more *)
(*  flexible and easier to use then C's [[setjmp]] *)
(*  and [[longjmp]].                            *)
(*   • Finally, least familiar but most important, *)
(*  ML provides native support for algebraic data *)
(*  types, which I use to represent both abstract *)
(*  syntax and values. These types provide value *)
(*  constructors like the [[IFX]] or [[APPLY]] used *)
(*  in previous chapters, but instead of [[switch]] *)
(*  statements, ML provides pattern matching. Pattern *)
(*  matching enables ML programmers to write function *)
(*  definitions that look like algebraic laws; such *)
(*  definitions are easier to follow than C code. *)
(*  The technique is demonstrated in the definition *)
(*  of function [[valueString]] on \cpageref    *)
(*  mlscheme.code.valueString. For a deeper dive into *)
(*  algebraic data types, jump ahead to \crefadt.chap *)
(*  and read through \crefadt.howto.            *)
(*                                              *)
(* {sidebar}                                    *)
(*                                              *)
(* The micro-Scheme interpreter in this chapter is *)
(* structured in the same way as the interpreter in *)
(* Chapter [->]. Like that interpreter, it has  *)
(* environments, abstract syntax, primitives, an *)
(* evaluator for expressions, and an evaluator for *)
(* definitions. Many details are as similar as I can *)
(* make them, but many are not: I want the interpreters *)
(* to look similar, but even more, I want my ML code to *)
(* look like ML and my C code to look like C.   *)
(*                                              *)
(* The ML code will be easier to read if you know my *)
(* programming conventions.                     *)
(*                                              *)
(*   • My naming conventions are the ones recommended by *)
(*  the SML'97 Standard Basis Library [cite     *)
(*  gansner:basis]. Names of types are written in *)
(*  lowercase letters with words separated by   *)
(*  underscores, like [[exp]], [[def]], or      *)
(*  [[unit_test]]. Names of functions and variables *)
(*  begin with lowercase letters, like [[eval]] or *)
(*  [[evaldef]], but long names may be written in *)
(*  ``camel case'' with a mix of uppercase and  *)
(*  lowercase letters, like [[processTests]] instead *)
(*  of the C-style [[process_tests]]. (Rarely, I may *)
(*  use an underscore in the name of a local    *)
(*  variable.)                                  *)
(*                                              *)
(*  Names of exceptions are capitalized, like   *)
(*  [[NotFound]] or [[RuntimeError]], and they use *)
(*  camel case. Names of value constructors, which *)
(*  identify alternatives in algebraic data types, *)
(*  are written in all capitals, possibly with  *)
(*  underscores, like [[IFX]], [[APPLY]], or    *)
(*  [[CHECK_EXPECT]]—just like enumeration literals *)
(*  in C.                                       *)
(*   • \qtrim0.5 If you happen to be a seasoned *)
(*  ML programmer, you'll notice something missing: *)
(*  the interpreter is not decomposed into modules. *)
(*  Modules get a book chapter of their own (\cref *)
(*  mcl.chap), but compared to what's in \cref  *)
(*  mcl.chap, Standard ML's module system is    *)
(*  complicated and hard to understand. To avoid *)
(*  explaining it, I define no modules—although I do *)
(*  use ``dot notation'' to select functions that are *)
(*  defined in Standard ML's predefined modules. *)
(*  By avoiding module definitions, I enable you to *)
(*  digest this chapter even if your only previous *)
(*  functional-programming experience is with   *)
(*  micro-Scheme.                               *)
(*                                              *)
(*  Because I don't use ML modules, I cannot easily *)
(*  write interfaces or distinguish them from   *)
(*  implementations. Instead, I use a           *)
(*  literate-programming trick: I put the types of *)
(*  functions and values, which is mostly what  *)
(*  ML interfaces describe, in boxes preceding the *)
(*  implementations. These types are checked by the *)
(*  ML compiler, and the trick makes it possible to *)
(*  present a function's interface just before its *)
(*  implementation.                             *)
(*                                              *)
(* My code is also affected by two limitations of ML: *)
(* ML is persnickety about the order in which   *)
(* definitions appear, and it has miserable support for *)
(* mutually recursive data definitions. These   *)
(* limitations arise because unlike C, which has *)
(* syntactic forms for both declarations and    *)
(* definitions, ML has only definition forms.   *)
(*                                              *)
(* In C, as long as declarations precede definitions, *)
(* you can be careless about the order in which both *)
(* appear. Declare all your structures (probably in *)
(* [[typedef]]s) in any order you like, and you can *)
(* define them in just about any order you like. Then *)
(* declare all your functions in any order you like, and *)
(* you can define them in any order you like—even if *)
(* your data structures and functions are mutually *)
(* recursive. Of course there are drawbacks: not all *)
(* variables are guaranteed to be initialized, and *)
(* global variables can be initialized only in limited *)
(* ways. And you can easily define mutually recursive *)
(* data structures that allow you to chase pointers *)
(* forever.                                     *)
(*                                              *)
(* In ML, there are no declarations, and you may write a *)
(* definition only after the definitions of the things *)
(* it refers to. Of course there are benefits: every *)
(* definition initializes its name, and initialization *)
(* may use any valid expression, including [[let]] *)
(* expressions, which in ML can contain nested  *)
(* definitions. And unless your code assigns to mutable *)
(* reference cells, you cannot define circular data *)
(* structures that allow you to chase pointers forever. *)
(* As a consequence, unless a structurally recursive *)
(* function fetches the contents of mutable reference *)
(* cells, it is guaranteed to terminate. ML's designers *)
(* thought this guarantee was more important than the *)
(* convenience of writing data definitions in many *)
(* orders. (And to be fair, using ML modules makes it *)
(* relatively convenient to get things in the right *)
(* order.)                                      *)
(*                                              *)
(* What about mutually recursive data? Suppose for *)
(* example, that type [[exp]] refers to [[value]] and *)
(* type [[value]] refers to [[exp]]? Mutually recursive *)
(* definitions like [[exp]] and [[value]] must be *)
(* written together, adjacent in the source code, *)
(* connected with the keyword [[and]]. (You won't see *)
(* [[and]] often, but when you do, please remember this: *)
(* it means mutual recursion, never a Boolean   *)
(* operation.)                                  *)
(*                                              *)
(* Mutually recursive function definitions provide more *)
(* options: they can be joined with [[and]], but it is *)
(* usually more convenient and more idiomatic to nest *)
(* one inside the other using a [[let]] binding. You *)
(* would use [[and]] only when both mutually recursive *)
(* functions need to be called by some third, client *)
(* function. When I use mutual recursion, I identify the *)
(* technique I use. Now, on to the code!        *)
(*                                              *)
(* Names and \chaptocsplitenvironments\cull, with \ *)
(* chaptocsplitintroduction to ML               *)
(*                                              *)
(* [*] In my C code, [[Name]] is an abstract type, and *)
(* by design, two values of type [[Name]] can be *)
(* compared using C's built-in [[==]] operator. In my ML *)
(* code, because ML strings are immutable and can be *)
(* meaningfully compared using ML's built-in [[=]] *)
(* operator, names are represented as strings. \mlslabel *)
(* name                                         *)
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
(* <boxed values 1>=                            *)
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
(* <boxed values 1>=                            *)
val _ = op find : name * 'a env -> 'a
(* \mlsflabelfind                               *)

(* Again using [[::]], function [[bind]] adds a new *)
(* binding to an existing environment. Unlike \cref *)
(* scheme.chap's [[bind]], it does not allocate a *)
(* mutable reference cell.                      *)
(* <boxed values 1>=                            *)
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
(* <boxed values 1>=                            *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* <boxed values 1>=                            *)
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
(* <boxed values 25>=                           *)
val _ = op duplicatename : name list -> name option
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
(* <boxed values 23>=                           *)
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
(* <boxed values 24>=                           *)
val _ = op optionList : 'a option list -> 'a list option
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <utility functions for string manipulation and printing>= *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* <boxed values 15>=                           *)
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
(* <boxed values 16>=                           *)
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
(* <boxed values 17>=                           *)
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
(* <boxed values 18>=                           *)
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
(* <boxed values 19>=                           *)
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
(* <utility functions for string manipulation and printing>= *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* The representations are the same as in C, with these *)
(* exceptions:                                  *)
(*                                              *)
(*   • In a [[LETX]] expression, the bindings are *)
(*  represented by a list of pairs, not a pair of *)
(*  lists—just like environments.             *)
(*   • In the representation of a primitive function, *)
(*  there's no need for an integer tag. As shown in \ *)
(*  crefmlscheme.primitives below, ML's higher-order *)
(*  functions makes it easy to create groups of *)
(*  primitives that share code. Tags would be useful *)
(*  only if we wanted to distinguish one primitive *)
(*  from another when printing.                 *)
(*   • None of the fields of [[exp]], [[value]], or *)
(*  [[lambda]] is named. Instead of being referred to *)
(*  by name, these fields are referred to by pattern *)
(*  matching.                                   *)
(*                                              *)
(* A primitive function that goes wrong raises the *)
(* [[RuntimeError]] exception, which is the ML  *)
(* equivalent of calling [[runerror]].          *)
(*                                              *)
(* True definitions are as in the C code, except again, *)
(* fields are not named. [*]                    *)

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
(* <boxed values 27>=                           *)
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
(* <boxed values 28>=                           *)
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
(* <boxed values 29>=                           *)
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
(* The two of information together form a value of type *)
(* [[interactivity]]. Such a value can be queried by *)
(* predicates [[prompts]] and [[print]].        *)
(* <boxed values 60>=                           *)
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
(* <boxed values 20>=                           *)
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
(* <boxed values 26>=                           *)
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
(* <boxed values 21>=                           *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* <collections with mapping and combining functions>= *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* <boxed values 22>=                           *)
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
(* <boxed values 36>=                           *)
type 'a susp = 'a susp
(* <suspensions>=                               *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* \qbreak Functions [[delay]] and [[demand]] convert to *)
(* and from suspensions.                        *)
(* <boxed values 37>=                           *)
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
(* <boxed values 38>=                           *)
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
(* <boxed values 38>=                           *)
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
(* <boxed values 39>=                           *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 39>=                           *)
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
(* <boxed values 40>=                           *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* Function [[streamOfEffects]] can be used to produce a *)
(* stream of lines from an input file:          *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 41>=                           *)
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
(* <boxed values 42>=                           *)
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
(* <boxed values 43>=                           *)
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
(* <boxed values 44>=                           *)
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
(* <boxed values 45>=                           *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 45>=                           *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* <boxed values 46>=                           *)
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
(* <boxed values 47>=                           *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 48>=                           *)
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
(* <boxed values 49>=                           *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* Concatenation turns a stream of streams of tau's into *)
(* a single stream of tau's. I define it using a *)
(* [[streamOfUnfold]] with a two-part state: the first *)
(* element of the state holds an initial [[xs]], \qbreak *)
(* and the second part holds the stream of all remaining *)
(* streams, [[xss]]. To concatenate the stream of *)
(* streams [[xss]], I use an initial state of [[(EOS, *)
(* xss)]].                                      *)
(* <boxed values 49>=                           *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* In list and stream processing, [[concat]] is very *)
(* often composed with [[map f]]. The composition is *)
(* usually called [[concatMap]].                *)
(* <boxed values 50>=                           *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* <boxed values 51>=                           *)
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
(* <boxed values 52>=                           *)
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
(* <boxed values 53>=                           *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* <streams>=                                   *)
fun every xs () k = streamConcatMap k xs
val run = ()
(* <streams>=                                   *)
fun cartesian xs ys =
  every xs run (fn x => 
    every ys run (fn y =>
      streamOfList [(x, y)]))
(* [*] Implementing reflective predicates. Using the *)
(* interpreter from \crefprolog.ex.reflection,  *)
(*                                              *)
(*  1. Define primitive predicates [[assert]] and *)
(*  [[retract]] (\cpagerefprolog.assert-retract). *)
(*                                              *)
(*  2. \partlabelprolog.ex.assert-map Test your work by *)
(*  using [[assert]] to convert a map-coloring  *)
(*  adjacency list (\crefpage,prolog.ex.color-adj) *)
(*  into map-coloring rules. Color, yet again, the *)
(*  map of the British Isles.                   *)
(*                                              *)
(*  3. Test your work by using [[assert]] and   *)
(*  [[retract]] to implement the general case of peg *)
(*  solitaire for a triangle of any size (\crefpage *)
(*  ,prolog.ex.general-pegs).                   *)
(*                                              *)
(* To represent a fact, use a term. To represent a *)
(* clause, wrap it in parentheses. As an example, *)
(* uProlog parses the term {nwverbatim} (sick(Patient) *)
(* :- psychiatrist(Doctor), analyzes(Doctor, Patient)) *)
(* {nwverbatim} as an application of functor [[:-]] to *)
(* the two arguments [[sick(Patient)]] and      *)
(* [[psychiatrist(Doctor)]]. It then \monoboxanalyzes *)
(* (Doctor, Patient). The first argument represents the *)
(* conclusion of the clause, and the remaining arguments *)
(* represent the premises. This information should be *)
(* enough to enable you to implement [[assert]] and *)
(* [[retract]].                                 *)
(*                                              *)
(* [*] A definitional interpreter for uProlog. Using *)
(* your operational semantics from \crefpage    *)
(* (prolog.ex.opsem-procedural, rewrite the core of the *)
(* interpreter for uProlog. Here are some suggestions: *)
(*                                              *)
(*   • The main part of your rewrite should be a new *)
(*  function [[solutions]], which takes a database *)
(*  and query and produces a [[stream]] of      *)
(*  substitutions (\crefpage,mlinterps.streams). *)
(*                                              *)
(*   • Function [[solutions]] should be specified by *)
(*  your operational semantics, which may include *)
(*  list comprehensions. To implement list      *)
(*  comprehensions, I recommend a variation on  *)
(*  [[streamConcatMap]]. I sometimes define \   *)
(*  nwnarrowboxes                               *)
(* <boxed values 107>=                          *)
val _ = op every : 'a stream -> unit -> ('a -> 'b stream) -> 'b stream
(* Using [[every]] and [[run]], the example list *)
(* comprehension for the Cartesian product, \mathbox[(x, *)
(* y) \suchthatx \getsa\listv*x, y \getsa\listv*y], is *)
(* written as                                   *)
(* <boxed values 107>=                          *)
val _ = op cartesian : 'a stream -> 'b stream -> ('a * 'b) stream
(* This style lends itself to implementing list *)
(* comprehensions.                              *)
(*                                              *)
(* Your [[solutions]] function should generate solutions *)
(* for uProlog's primitive predicates, but the  *)
(* implementations of those predicates need not change. *)
(* Those implementations expect success and failure *)
(* continuations, which can be manipulated to produce a *)
(* stream of substitutions. Use \monoboxstreamOfCPS (p *)
(* args), where [[p]] represents the primitive  *)
(* predicate, [[args]] represents its arguments, and *)
(* [[streamOfCPS]] is defined as follows:       *)
(* <streams>=                                   *)
fun streamOfCPS cpsSource =
  cpsSource (fn theta => fn resume => theta ::: resume ()) (fn () => EOS)
val _ = streamOfCPS : (('a -> (unit->'a stream) -> 'a stream) -> (unit->'a
                                     stream) -> 'a stream) -> 'a stream (*OMIT*)
val _ = streamOfCPS (fn succ => fn fail => succ [("a", 3)] (fn () => EOS)) : (
                                             string * int) list stream  (*OMIT*)
(* <streams>=                                   *)
fun cpsStream answers succ fail =
  case streamGet answers
    of NONE => fail ()
     | SOME (theta, answers) =>
         succ theta (fn () => cpsStream answers succ fail)
(* When [[solutions]] is complete, write a replacement *)
(* [[query]] function that calls [[cpsStream]] on the *)
(* result of [[solutions]], where [[cpsStream]] is *)
(* defined as follows:                          *)
(* <boxed values 108>=                          *)
val _ = op cpsStream : 'subst stream -> ('subst -> (unit->'a) -> 'a) -> (unit->
                                                                       'a) -> 'a
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
(* <boxed values 109>=                          *)
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
(* <boxed values 110>=                          *)
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
(* <boxed values 111>=                          *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 112>=                          *)
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
(* <boxed values 114>=                          *)
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
(* <boxed values 115>=                          *)
val _ = op pzero : ('a, 'b) xformer
(* This parser obeys the algebraic law          *)
(*                                              *)
(*  \monoboxt <|> pzero = \monoboxpzero <|> t = \ *)
(*  monoboxt\text.                              *)
(*                                              *)
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)
(* <boxed values 115>=                          *)
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
(* <boxed values 116>=                          *)
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
(* <boxed values 117>=                          *)
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
(* <boxed values 118>=                          *)
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
(* <boxed values 119>=                          *)
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
(* <boxed values 120>=                          *)
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
(* <boxed values 121>=                          *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 122>=                          *)
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
(* <boxed values 123>=                          *)
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
(* <boxed values 124>=                          *)
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
(* <boxed values 125>=                          *)
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
(* <boxed values 126>=                          *)
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
(* <boxed values 127>=                          *)
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
(* <boxed values 128>=                          *)
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
(* <boxed values 129>=                          *)
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
(* <boxed values 55>=                           *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 55>=                           *)
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
(* The [[Located]] exception is raised by function *)
(* [[atLoc]]. Calling \monoboxatLoc f x applies [[f]] *)
(* to [[x]] within the scope of handlers that convert *)
(* recognized exceptions to the [[Located]] exception: *)
(* <boxed values 56>=                           *)
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
(* <boxed values 57>=                           *)
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
(* <boxed values 58>=                           *)
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
(* <boxed values 59>=                           *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 59>=                           *)
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
(* <boxed values 138>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* <boxed values 138>=                          *)
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
(* <boxed values 130>=                          *)
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
(* <boxed values 131>=                          *)
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
(* <boxed values 132>=                          *)
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
(* <boxed values 133>=                          *)
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
(* <boxed values 134>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 135>=                          *)
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
(* <boxed values 136>=                          *)
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
(* <boxed values 137>=                          *)
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
(* <boxed values 139>=                          *)
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
(* <boxed values 140>=                          *)
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
(* Evaluation                                   *)
(*                                              *)
(* [*] The machinery above is enough to write an *)
(* evaluator, which takes an expression and an  *)
(* environment and produces a value. Because the *)
(* environment rarely changes, my evaluator is  *)
(* structured as a nested pair of mutually recursive *)
(* functions. The outer function, [[eval]], takes both *)
(* expression [[e]] and environment [[rho]] as  *)
(* arguments. The inner function, [[ev]], takes only an *)
(* expression as argument; it uses the [[rho]] from the *)
(* outer function.                              *)
(*                                              *)
(* Function [[ev]] begins with a clause that evaluates a *)
(* [[LITERAL]] form, which evaluates to the carried *)
(* value [[v]]. \mlsflabeleval                  *)
(* <boxed values 141>=                          *)
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
(* <boxed values 141>=                          *)
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
(* <boxed values 142>=                          *)
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
(* <boxed values 151>=                          *)
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
(* <boxed values 152>=                          *)
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
(* <boxed values 144>=                          *)
val _ = op left  : ('t plus_brackets, bracket_shape located) polyparser
val _ = op right : ('t plus_brackets, bracket_shape located) polyparser
val _ = op pretoken : ('t plus_brackets, 't) polyparser
(* <transformers for interchangeable brackets>= *)
fun badRight msg =
  (fn (loc, shape) => synerrorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* <boxed values 145>=                          *)
val _ = op badRight : string -> ('t plus_brackets, 'a) polyparser
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <transformers for interchangeable brackets>= *)
fun notCurly (_, CURLY) = false
  | notCurly _          = true
fun leftCurly tokens = sat (not o notCurly) left tokens
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
(* <boxed values 143>=                          *)
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
(* <boxed values 146>=                          *)
val _ = op notCurly  : bracket_shape located -> bool
val _ = op leftCurly : ('t plus_brackets, bracket_shape located) polyparser
(* <boxed values 146>=                          *)
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
(* <boxed values 147>=                          *)
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
(* <boxed values 148>=                          *)
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
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

(* <transformers for interchangeable brackets>= *)

fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
(* <boxed values 149>=                          *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* <boxed values 149>=                          *)
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
(* <boxed values 150>=                          *)
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
(* <boxed values 153>=                          *)
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
(* <boxed values 154>=                          *)
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
(* <boxed values 155>=                          *)
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
(* <boxed values 156>=                          *)
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
(* <boxed values 157>=                          *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* <streams that issue two forms of prompts>=   *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* <boxed values 158>=                          *)
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
(* <boxed values 159>=                          *)
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
(* <boxed values 160>=                          *)
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
(* <boxed values 161>=                          *)
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
(* Supporting code \chapheadsplitfor uProlog    *)
(*                                              *)
(* [*][*]                                       *)
(*                                              *)
(* \invisiblelocaltableofcontents[*]            *)
(*                                              *)
(* Organizing code chunks \chaptocbacksplitinto an *)
(* interpreter                                  *)
(*                                              *)
(* Unlike the other bridge languages, \upr does not *)
(* evaluate syntax to produce values. Instead it *)
(* substitutes terms for logical variables. Its overall *)
(* structure is therefore different: \makenowebnotdef *)
(* (left as an exercise)                        *)

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
(*   ABSTRACT SYNTAX FOR \UPROLOG                                *)
(*                                                               *)
(*****************************************************************)

(* Finally, uProlog shares extended definitions with the *)
(* other bridge languages.                      *)
(* <abstract syntax for \uprolog>=              *)
(* Abstract syntax (and no values)              *)
(*                                              *)
(* Of all the languages in this book, Prolog has the *)
(* simplest structure. Unusually, Prolog does not *)
(* distinguish ``values'' from ``abstract syntax''; both *)
(* are represented as terms. A term is a logical *)
(* variable, a literal number, or an application of a *)
(* functor to a list of terms. (An atom is represented *)
(* as the application of a functor to an empty list *)
(* of terms.)                                   *)
(* <definitions of [[term]], [[goal]], and [[clause]] for \uprolog>= *)
datatype term = VAR     of name
              | LITERAL of int
              | APPLY   of name * term list
(* A term can be a functor applied to a list of terms; a *)
(* goal is a predicate applied to a list of terms. Goals *)
(* and applications have identical structure.   *)
(* <definitions of [[term]], [[goal]], and [[clause]] for \uprolog>= *)
type goal = name * term list
(* A clause is a conclusion and a list of premises, all *)
(* of which are goals. If the list of premises is empty, *)
(* the clause is a ``fact''; otherwise it is a ``rule,'' *)
(* but these distinctions are useful only for thinking *)
(* about and organizing programs---the underlying *)
(* meanings are the same. Writing our implementation in *)
(* ML enables us to use the identifier [[:-]] as a value *)
(* constructor for clauses.                     *)
(* <definitions of [[term]], [[goal]], and [[clause]] for \uprolog>= *)
datatype clause = :- of goal * goal list
infix 3 :-
(* At the read-eval-print loop, where a normal language *)
(* can present a true definition, a uProlog program can *)
(* either ask a query or add a clause to the database. *)
(* (The switch between query mode and rule mode is *)
(* hidden from the code in this chapter; the details are *)
(* buried in Section [->].) I group these actions into a *)
(* syntactic category called [[cq]], which is short for *)
(* clause-or-query. It is the Prolog analog of a true *)
(* definition [[def]]. [*]                      *)
(* <definitions of [[def]] and [[unit_test]] for \uprolog>= *)
datatype cq
  = ADD_CLAUSE of clause
  | QUERY      of goal list
type def = cq
(* uProlog includes three unit-test forms.      *)
(* <definitions of [[def]] and [[unit_test]] for \uprolog>= *)
datatype unit_test 
  = CHECK_SATISFIABLE   of goal list
  | CHECK_UNSATISFIABLE of goal list
  | CHECK_SATISFIED     of goal list * (name * term) list
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
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* String conversions                           *)
(*                                              *)
(* This code converts terms, goals, and clauses to *)
(* strings.                                     *)
(* <definitions of [[termString]], [[goalString]], and [[clauseString]]>= *)
fun termString (APPLY ("cons", [car, cdr])) = 
      let fun tail (APPLY ("cons", [car, cdr])) = ", " ^ termString car ^ tail
                                                                             cdr
            | tail (APPLY ("nil",  []))         = "]"
            | tail x                           = "|" ^ termString x ^ "]"
      in  "[" ^ termString car ^ tail cdr
      end
  | termString (APPLY ("nil", [])) = "[]"
  | termString (APPLY (f, []))     = f
  | termString (APPLY (f, [x, y])) =
      if Char.isAlpha (hd (explode f)) then appString f x [y]
      else String.concat ["(", termString x, " ", f, " ", termString y, ")"]
  | termString (APPLY (f, h::t)) = appString f h t
  | termString (VAR v) = v
  | termString (LITERAL n) = intString n
and appString f h t =
      String.concat (f :: "(" :: termString h ::
                     foldr (fn (t, tail) => ", " :: termString t :: tail) [")"]
                                                                              t)
(* <definitions of [[termString]], [[goalString]], and [[clauseString]]>= *)
fun goalString g = termString (APPLY g)
fun clauseString (g :- []) = goalString g
  | clauseString (g :- (h :: t)) =
      String.concat (goalString g :: " :- " :: goalString h ::
                     (foldr (fn (g, tail) => ", " :: goalString g :: tail)) [] t
                                                                               )
(* <definitions of [[termString]], [[goalString]], and [[clauseString]]>= *)
fun substString pairs =
      nullOrCommaSep "no substitution"
      (map (fn (x, t) => x ^ " = " ^ termString t) pairs)


(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR TRACING \UPROLOG\ COMPUTATION                   *)
(*                                                               *)
(*****************************************************************)

(* <support for tracing \uprolog\ computation>= *)
val tracer = ref (app print)
val _ = tracer := (fn _ => ())
fun trace l = !tracer l
(* If you attempt any of the exercises, your code can *)
(* call [[trace]], which will print things if and only *)
(* if the -trace option is given to the interpreter. *)
(* <boxed values 198>=                          *)
val _ = op trace : string list -> unit
(* Complete implementation of uProlog           *)
(*                                              *)
(* Substitution                                 *)
(*                                              *)



(*****************************************************************)
(*                                                               *)
(*   SUBSTITUTION AND UNIFICATION                                *)
(*                                                               *)
(*****************************************************************)

(* <substitution and unification ((upr))>=      *)
datatype con = ~  of term * term
             | /\ of con  * con
             | TRIVIAL
infix 4 ~
infix 3 /\

(* <free variables (terms/goals/clauses)>=      *)
fun termFreevars t =
  let fun f (LITERAL _, xs) = xs
        | f (VAR x,     xs) = insert (x, xs)
        | f (APPLY(_, args), xs) = foldl f xs args
  in  reverse (f (t, []))
  end  
fun goalFreevars goal = termFreevars (APPLY goal)
fun union' (s1, s2) = s1 @ diff (s2, s1)   (* preserves order *)
fun clauseFreevars (c :- ps) =
  foldl (fn (p, f) => union' (goalFreevars p, f)) (goalFreevars c) ps
(* Free variables                               *)
(*                                              *)
(* The function [[termFreevars]] computes the free *)
(* variables of a term. For readability, those free *)
(* variables are ordered by their first appearance in *)
(* the term, when reading from left to right. Similar *)
(* functions compute the free variables of goals and *)
(* clauses.                                     *)
(* <boxed values 99>=                           *)
val _ = op termFreevars   : term   -> name set
val _ = op goalFreevars   : goal   -> name set
val _ = op clauseFreevars : clause -> name set
(* <substitutions for \uprolog>=                *)
type subst = term env
val idsubst = emptyEnv
(* <boxed values 165>=                          *)
type subst = subst
val _ = op idsubst : subst
(* <substitutions for \uprolog>=                *)
fun varsubst theta = 
  (fn x => find (x, theta) handle NotFound _ => VAR x)
(* A substitution is applied to a variable by   *)
(* [[varsubst]].                                *)
(* <boxed values 166>=                          *)
val _ = op varsubst : subst -> (name -> term)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \uprolog>=                *)
fun termsubst theta =
  let fun subst (VAR x)         = varsubst theta x
        | subst (LITERAL n)     = LITERAL n
        | subst (APPLY (f, ts)) = APPLY (f, map subst ts)
(* A substitution is applied to a term by [[termsubst]]. *)
(* <boxed values 167>=                          *)
val _ = op termsubst : subst -> (term -> term)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

  in  subst
  end
(* <substitutions for \uprolog>=                *)
fun goalsubst   theta (f, ts)   = (f, map (termsubst theta) ts)
fun clausesubst theta (c :- ps) = (goalsubst theta c :- map (goalsubst theta) ps
                                                                               )
(* A substitution is applied to a goal or a clause by *)
(* [[goalsubst]] or [[clausesubst]].            *)
(* <boxed values 168>=                          *)
val _ = op goalsubst   : subst -> (goal   -> goal)
val _ = op clausesubst : subst -> (clause -> clause)
(* <substitutions for \uprolog>=                *)
fun consubst theta =
  let fun subst (t1 ~  t2) = termsubst theta t1 ~ termsubst theta t2
        | subst (c1 /\ c2) = subst c1 /\ subst c2
        | subst TRIVIAL    = TRIVIAL
  in  subst
  end
(* <boxed values 169>=                          *)
val _ = op consubst : subst -> (con -> con)
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \uprolog>=                *)
infix 7 |-->
fun x |--> (VAR x') = if x = x' then idsubst else bind (x, VAR x', emptyEnv)
  | x |--> t        = if member x (termFreevars t) then
                        raise InternalError "non-idempotent substitution"
                      else
                        bind (x, t, emptyEnv)
(* Substitutions are created using the same infix *)
(* operator as in \crefml.chap.                 *)
(* <boxed values 170>=                          *)
val _ = op |--> : name * term -> subst
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <substitutions for \uprolog>=                *)
fun dom theta =
  map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = termsubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end

(* Substitution, free variables, and unification *)
(*                                              *)
(* As part of type inference, \crefml.chap develops a *)
(* representation of substitutions, as well as utility *)
(* functions that apply substitutions to types. Prolog *)
(* uses the same representation, but instead of *)
(* substituting types for type variables, Prolog *)
(* substitutes terms for logical variables. The code, *)
(* which closely resembles the code in \crefml.chap, is *)
(* in \crefupra.substitution. Substitutions are *)
(* discovered by solving equality constraints, which are *)
(* defined here:                                *)
(* <boxed values 98>=                           *)
type subst = subst
val _ = op idsubst : subst
val _ = op |-->    : name * term -> subst
val _ = op varsubst    : subst -> (name   -> term)
val _ = op termsubst   : subst -> (term   -> term)
val _ = op goalsubst   : subst -> (goal   -> goal)
val _ = op clausesubst : subst -> (clause -> clause)
type con = con
val _ = op consubst    : subst -> (con -> con)
(* <substitution and unification ((upr))>=      *)
exception Unsatisfiable
(* <constraint solving ((prototype))>=          *)
fun solve c = raise LeftAsExercise "solve"
(* As in \crefml.chap, you implement the solver. Prolog *)
(* uses the same kind of equality constraints as ML type *)
(* inference, and it uses the same algorithm for the *)
(* solver. If a constraint cannot be solved, [[solve]] *)
(* must raise the [[Unsatisfiable]] exception. [*] *)
(* <boxed values 103>=                          *)
val _ = op solve : con -> subst
fun unify ((f, ts), (f', ts')) =
  solve (APPLY (f, ts) ~ APPLY (f', ts'))
(* Unification by solving equality constraints  *)
(*                                              *)
(* \makenwnotdef(left as exercise)              *)
(*                                              *)
(* To unify a goal with the head of a clause, we solve *)
(* an equality constraint.                      *)
(* <boxed values 102>=                          *)
val _ = op unify : goal * goal -> subst


(*****************************************************************)
(*                                                               *)
(*   RENAMING \UPROLOG\ VARIABLES                                *)
(*                                                               *)
(*****************************************************************)

(* <renaming \uprolog\ variables>=              *)
local
  val n = ref 1
in
  fun freshVar s = VAR ("_" ^ s ^ intString (!n) before n := !n + 1)
(* Renaming variables in clauses: ``Freshening'' *)
(*                                              *)
(* Every time a clause is used, its variables are *)
(* renamed. To rename a variable, I put an underscore in *)
(* front of its name and a unique integer after it. *)
(* Because the parser in Section [->] does not accept *)
(* variables whose names begin with an underscore, these *)
(* names cannot possibly conflict with the names of *)
(* variables that appear in source code.        *)
(* <boxed values 100>=                          *)
val _ = op freshVar : string -> term
end
(* <renaming \uprolog\ variables>=              *)
fun freshen c =
  let val renamings = map (fn x => x |--> freshVar x) (clauseFreevars c)
      val renaming  = foldl compose idsubst renamings
  in  clausesubst renaming c
  end
(* Function [[freshen]] replaces free variables with *)
(* fresh variables. Value [[renaming]] represents a *)
(* renaming \renaming, as in Section [->].      *)
(* <boxed values 101>=                          *)
val _ = op freshen : clause -> clause
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UPROLOG, PROVIDING [[XDEFSINMODE]] *)
(*                                                               *)
(*****************************************************************)

(* <lexical analysis and parsing for \uprolog, providing [[xdefsInMode]]>= *)
(* <lexical analysis for \uprolog>=             *)
datatype token 
  = UPPER     of string
  | LOWER     of string
  | SYMBOLIC  of string
  | INT_TOKEN of int
  | RESERVED  of string
  | EOF
(* And substitutions compose just as in \crefml.chap. *)
(* <boxed values 171>=                          *)
val _ = op compose : subst * subst -> subst
(* Tokens                                       *)
(*                                              *)
(* uProlog has a more complex lexical structure than *)
(* other languages, so it has more forms of tokens. *)
(* It has uppercase, lowercase, and symbolic tokens, as *)
(* well as integers. And to simplify the parser, *)
(* I distinguish reserved words and symbols using *)
(* [[RESERVED]]. Because uProlog doesn't use the same *)
(* bracketed-keyword syntax as the other bridge *)
(* languages, I don't bother with the bracket machinery *)
(* of \creflazyparse.chap—a bracket is just another *)
(* [[RESERVED]] token.                          *)
(*                                              *)
(* Finally, because a C-style uProlog comment can span *)
(* multiple lines, the lexical analyzer may consume more *)
(* than one line—in which process it may encounter the *)
(* end of a file. Reading the end of file needs to be *)
(* distinguishable from failing to read a token, so *)
(* I represent end of file by its own special token  *)
(* [[EOF]].                                     *)
(* <boxed values 171>=                          *)
type token = token
(* <lexical analysis for \uprolog>=             *)
fun tokenString (UPPER s)     = s
  | tokenString (LOWER s)     = s
  | tokenString (INT_TOKEN n) = intString n
  | tokenString (SYMBOLIC  s) = s
  | tokenString (RESERVED  s) = s
  | tokenString EOF           = "<end-of-file>"
(* <lexical analysis for \uprolog>=             *)
fun symbolic ":-" = RESERVED ":-"
  | symbolic "."  = RESERVED "."
  | symbolic "|"  = RESERVED "|"
  | symbolic "!"  = LOWER "!"
  | symbolic s    = SYMBOLIC s
fun lower "is" = RESERVED "is"
  | lower "check_satisfiable"   = RESERVED "check_satisfiable"
  | lower "check_unsatisfiable" = RESERVED "check_unsatisfiable"
  | lower "check_satisfied"     = RESERVED "check_satisfied"
  | lower s    = LOWER s
(* Reserved words and anonymous variables       *)
(*                                              *)
(* Tokens are formed from symbols or from lower-case *)
(* letters by applying functions [[symbolic]] and *)
(* [[lowers]]. Function [[symbolic]] usually returns a *)
(* [[SYMBOLIC]] tokens, and [[lower]] usually returns a *)
(* [[LOWER]] token, but there are exceptions. One *)
(* exception is for reserved words. The other is for the *)
(* cut: because the cut is nullary, not binary, it is *)
(* treated as [[LOWER]], just like any other nullary *)
(* predicate.                                   *)
(* <boxed values 172>=                          *)
val _ = op symbolic : string -> token
val _ = op lower    : string -> token
(* A variable consisting of a single underscore gets *)
(* converted to a unique ``anonymous'' variable. *)
(* <lexical analysis for \uprolog>=             *)
fun anonymousVar () =
  case freshVar ""
    of VAR v => UPPER v
     | _ => raise InternalError "\"fresh variable\" is not a VAR"
local
  (* Classification of characters                 *)
  (*                                              *)
  (* Just as uML distinguishes the names of value *)
  (* constructors from the names of value variables, *)
  (* uProlog distinguishes symbolic names (like [[+]]) *)
  (* from alphanumeric names (like [[add1]]).     *)
  (* The distinction is established by distinguishing *)
  (* characters: every character is either a symbol, an *)
  (* alphanumeric, a space, or a delimiter.       *)
  (* <character-classification functions for \uprolog>= *)
  val symbols = explode "!%^&*-+:=|~<>/?`$\\"
  fun isSymbol c = List.exists (fn c' => c' = c) symbols
  fun isIdent  c = Char.isAlphaNum c orelse c = #"_"
  fun isSpace  c = Char.isSpace c
  fun isDelim  c = not (isIdent c orelse isSymbol c)
  (* <lexical utility functions for \uprolog>=    *)
  fun underscore _ [] = OK (anonymousVar ())
    | underscore c cs = ERROR ("name may not begin with underscore at " ^
                                   implode (c::cs))

  fun int cs [] = intFromChars cs >>=+ INT_TOKEN
    | int cs ids = 
        ERROR ("integer literal " ^ implode cs ^
               " may not be followed by '" ^ implode ids ^ "'")
  (* Converting characters to tokens              *)
  (*                                              *)
  (* Utility functions [[underscore]] and [[int]] make *)
  (* sure that an underscore or a sequence of digits, *)
  (* respectively, is never followed by any character that *)
  (* might be part of an alphanumeric identifier. When *)
  (* either of these functions succeeds, it returns an *)
  (* appropriate token. Each function takes two inputs: *)
  (* an argument and the sequence of characters that *)
  (* follow the argument in the input stream.     *)
  (* <boxed values 173>=                          *)
  val _ = op underscore : char      -> char list -> token error
  val _ = op int        : char list -> char list -> token error
  (* <lexical utility functions for \uprolog>=    *)
  fun unrecognized (ERROR _) = raise InternalError "this can't happen"
    | unrecognized (OK cs) =
        case cs
          of []        => NONE
           | #";" :: _ => raise InternalError "this can't happen"
           | _ =>
               SOME (ERROR ("invalid initial character in `" ^ implode cs ^ "'")
                                                                          , EOS)
  (* When the lexical analyzer cannot recognize a sequence *)
  (* of characters, it calls utility function     *)
  (* [[unrecognized]]. If the sequence is empty, it means *)
  (* there's no token. If anything else happens, an error *)
  (* has occurred. \nwnarrowboxes                 *)
  (* <boxed values 174>=                          *)
  val _ = op unrecognized : char list error -> ('a error * 'a error stream)
                                                                          option
  (* <lexical utility functions for \uprolog>=    *)
  fun nextline (file, line) = (file, line+1)
  (* When a lexical analyzer runs out of characters on a *)
  (* line, it calls [[nextline]] to compute the location *)
  (* of the next line.                            *)
  (* <boxed values 175>=                          *)
  val _ = op nextline : srcloc -> srcloc
in
  (* <lexical analyzers for for \uprolog>=        *)
  type 'a prolog_lexer =
    (char eol_marked, 'a) xformer
  fun char chars =
    case streamGet chars
      of SOME (INLINE c, chars) => SOME (OK c, chars) 
       | _ => NONE
  fun eol chars =
    case streamGet chars
      of SOME (EOL _, chars) => SOME (OK (), chars)
       | _ => NONE
  (* <lexical analyzers for for \uprolog>=        *)
  fun manySat p =
    many (sat p char)

  val whitespace =
    manySat isSpace
  val intChars = 
    (curry op :: <$> eqx #"-" char <|> pure id) <*> many1 (sat Char.isDigit char
                                                                               )
  (* <boxed values 176>=                          *)
  type 'a prolog_lexer = 'a prolog_lexer
  val _ = op char : char prolog_lexer
  val _ = op eol  : unit prolog_lexer
  (* uProlog must be aware of the end of an input line. *)
  (* Lexical analyzers [[char]] and [[eol]] recognize a *)
  (* character and the end-of-line marker, respectively. *)

  (* Function [[manySat]] provides a general tool for *)
  (* sequences of characters. Lexers [[whitespace]] and *)
  (* [[intChars]] handle two common cases.        *)
  (* <boxed values 176>=                          *)
  val _ = op manySat    : (char -> bool) -> char list prolog_lexer
  val _ = op whitespace : char list prolog_lexer
  val _ = op intChars   : char list prolog_lexer
  (* <lexical analyzers for for \uprolog>=        *)
  val ordinaryToken =
        underscore            <$> eqx #"_" char <*>! manySat isIdent
    <|> int                   <$> intChars      <*>! manySat isIdent
    <|> (RESERVED o str)      <$> sat isDelim char                    
    <|> (symbolic o implode)  <$> many1 (sat isSymbol char)
    <|> curry (lower o implode o op ::) <$> sat Char.isLower char <*> manySat
                                                                         isIdent
    <|> curry (UPPER o implode o op ::) <$> sat Char.isUpper char <*> manySat
                                                                         isIdent
    <|> unrecognized o fst o valOf o many char
  (* <lexical analyzers for for \uprolog>=        *)
  local
    fun the c = eqx c char
  in
    fun tokenAt loc cs =  (* eta-expanded to avoid infinite regress *)
      (whitespace *> (   the #"/" *> the #"*" *> skipComment loc loc
                     <|> the #";" *> many char *> eol *> tokenAt (nextline loc)
                     <|>                          eol *> tokenAt (nextline loc)
                     <|> (loc, EOF) <$ eos
                     <|> pair loc <$> ordinaryToken
                     )) cs
    and skipComment start loc cs =
      (   the #"*" *> the #"/" *> tokenAt loc
      <|> char *> skipComment start loc
      <|> eol  *> skipComment start (nextline loc)
      <|> id <$>! pure (ERROR ("end of file looking for */ to close comment in "
                                                                               ^
                               srclocString start))
      ) cs
  (* An ordinary token is an underscore, delimiter, *)
  (* integer literal, symbolic name, or alphanumeric name. *)
  (* Uppercase and lowercase names produce different *)
  (* tokens.                                      *)
  (* <boxed values 177>=                          *)
  val _ = op ordinaryToken : token prolog_lexer
  (* [[funty]] stand for \tau, [[actualtypes]]    *)
  (* stand for \ldotsntau, and [[rettype]] stand for alpha *)
  (* . The first premise is implemented by a call to *)
  (* [[typesof]] and the second by a call to      *)
  (* [[freshtyvar]]. The constraint is represented just as *)
  (* written in the rule.                         *)

  (* I define two main lexical analyzers that keep track *)
  (* of source locations: [[tokenAt]] produces tokens, and *)
  (* [[skipComment]] skips comments. They are mutually *)
  (* recursive, and in order to delay the recursive calls *)
  (* until a stream is supplied, each definition has an *)
  (* explicit [[cs]] argument, which contains a stream of *)
  (* inline characters.                           *)
  (* <boxed values 177>=                          *)
  val _ = op tokenAt     : srcloc -> token located prolog_lexer
  val _ = op skipComment : srcloc -> srcloc -> token located prolog_lexer
  end
end
(* <lexical analysis and parsing for \uprolog, providing [[xdefsInMode]]>= *)
(* <parsers and streams for \uprolog>=          *)
type 'a parser = (token, 'a) polyparser
val symbol = asAscii ((fn SYMBOLIC  s => SOME s | _ => NONE) <$>? token)
val upper  = asAscii ((fn UPPER     s => SOME s | _ => NONE) <$>? token)
val lower  = asAscii ((fn LOWER     s => SOME s | _ => NONE) <$>? token)
val int    =          (fn INT_TOKEN n => SOME n | _ => NONE) <$>? token
fun reserved s = eqx s ((fn RESERVED s => SOME s | _ => NONE) <$>? token)
(* The one thing that's interesting about uProlog's *)
(* parser is that a uProlog interpreter has two modes: *)
(* rule mode and query mode. Tracking modes adds *)
(* complexity to the parser.                    *)
(*                                              *)
(* Utilities for parsing uProlog                *)
(*                                              *)
(* As always, the first combinators to be defined are *)
(* those that parse single tokens.              *)
(* <boxed values 178>=                          *)
val _ = op symbol : string parser
val _ = op upper  : string parser
val _ = op lower  : string parser
val _ = op int    : int    parser
(* <parsers and streams for \uprolog>=          *)
val notSymbol =
  symbol <!> "arithmetic expressions must be parenthesized" <|>
  pure ()
(* \qbreak Unlike more sophisticated languages, uProlog *)
(* does not have a system of operator precedence and *)
(* associativity. That means an input like \monobox3 + X *)
(* + Y is not a well-formed term. To ensure that an *)
(* input like \monobox3 + X is not followed by another *)
(* symbol, the parser uses [[notSymbol]].       *)
(* <boxed values 179>=                          *)
val _ = op notSymbol : unit parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and streams for \uprolog>=          *)
val nilt = APPLY ("nil", [])
fun cons (x, xs) = APPLY ("cons", [x, xs])
(* Like ML, uProlog provides syntactic sugar for lists. *)
(* To turn this sugar into list terms, the parser uses *)
(* term [[nilt]] and function [[cons]].         *)
(* <boxed values 180>=                          *)
val _ = op nilt : term
val _ = op cons : term * term -> term
(* <parsers and streams for \uprolog>=          *)
val variable        = upper
val binaryPredicate = symbol
val functr          = lower
fun commas p = 
  curry op :: <$> p <*> many (reserved "," *> p)
(* <boxed values 181>=                          *)
val _ = op variable        : string parser
val _ = op binaryPredicate : string parser
val _ = op functr          : string parser
val _ = op commas : 'a parser -> 'a list parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and streams for \uprolog>=          *)
fun closing bracket = reserved bracket <?> bracket
fun wrap left right p = reserved left *> p <* closing right
(* I spell ``functor'' without the ``o'' because in *)
(* Standard ML, [[functor]] is a reserved word. *)
(*                                              *)
(* Parser \monoboxwrap L R p is used to parse whatever p *)
(* parses, but wrapped in tokens on the left and right. *)
(* Tokens L and R will be either round brackets or *)
(* square brackets.                             *)
(* <boxed values 182>=                          *)
val _ = op wrap : string -> string -> 'a parser -> 'a parser
(* <parsers and streams for \uprolog>=          *)
local
  fun consElems terms tail = foldr cons tail terms
  fun applyIs a t = APPLY ("is", [a, t])
  fun applyBinary x operator y = APPLY (operator, [x, y])
  fun maybeClause t NONE = t
    | maybeClause t (SOME ts) = APPLY (":-", t :: ts)
in
  fun term tokens = 
    (   applyIs <$> atom <* reserved "is" <*> (term <?> "term")
    <|> applyBinary <$> atom <*> binaryPredicate <*> (atom <?> "atom") <*
                                                                       notSymbol
    <|> atom 
    ) 
    tokens
  and atom tokens = 
    (   curry APPLY <$> functr <*> (wrap "(" ")" (commas (term <?> "term"))
                                   <|> pure []
                                   )
    <|> VAR     <$> variable
    <|> LITERAL <$> int
    <|> wrap "(" ")" (maybeClause <$> term <*> optional (reserved ":-" *> commas
                                                                          term))
    <|> wrap "[" "]" 
            (consElems <$> commas term <*> ( reserved "|" *> (term <?>
                                                                 "list element")
                                          <|> pure nilt
                                           )
           <|> pure nilt
            )
    )
    tokens
(* \qvfilbreak4in                               *)
(*                                              *)
(* Parsing terms, atoms, and goals              *)
(*                                              *)
(* The elements above can now be combined into a parser *)
(* for uProlog. The grammar is based on the grammar from *)
(* \figrefpageprolog.syntax, except that atoms are *)
(* parsed using named functions, and I use some *)
(* specialized tricks to organize the grammar. Concrete *)
(* syntax is not for the faint of heart.        *)
(* <boxed values 183>=                          *)
val _ = op term   : term parser
val _ = op atom   : term parser
val _ = op commas : 'a parser -> 'a list parser
end
(* <parsers and streams for \uprolog>=          *)
fun asGoal _   (APPLY g) = OK g
  | asGoal loc (VAR v)   = 
      synerrorAt ("Variable " ^ v ^ " cannot be a predicate") loc
  | asGoal loc (LITERAL n) =
      synerrorAt ("Integer " ^ intString n ^ " cannot be a predicate") loc

val goal = asGoal <$> srcloc <*>! term 
(* <boxed values 184>=                          *)
val _ = op asGoal : srcloc -> term -> goal error
val _ = op goal   : goal parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and streams for \uprolog>=          *)
datatype concrete
  = BRACKET of string 
  | CLAUSE  of goal * goal list option
  | GOALS   of goal list
  | CTEST   of unit_test
(* Recognizing concrete syntax using modes      *)
(*                                              *)
(* I put together the uProlog parser in three layers. *)
(* The bottom layer is the concrete syntax itself. For a *)
(* moment let's ignore the meaning of uProlog's syntax *)
(* and look only at what can appear. At top level, we *)
(* might see                                    *)
(*                                              *)
(*  \tightlist                                  *)
(*   • A string in brackets                   *)
(*   • A clause containing a [[:-]] symbol    *)
(*   • A list of one or more goals separated by commas *)
(*   • A unit test                            *)
(*                                              *)
(* The meanings of some of these things can be depend on *)
(* which mode the interpreter is in. So each one is *)
(* parsed initially into a value of type [[concrete]], *)
(* which represents the concrete syntax while taking no *)
(* position on what the syntax means. This      *)
(* representation can be interpreted as abstract syntax *)
(* later, once the mode is known.               *)
(* <boxed values 185>=                          *)
type concrete = concrete
(* <parsers and streams for \uprolog>=          *)
fun checkSatisfied goals =
  let fun split (gs', []) = OK (CHECK_SATISFIED (reverse gs', []))
        | split (gs', rest as ("=", _) :: _) =
             validate ([], rest) >>=+
             (fn subst => CHECK_SATISFIED (reverse gs', subst))
        | split (gs', g :: gs) = split (g :: gs', gs)
      and validate (theta', ("=", [VAR x, t]) :: gs) =
            validate ((x, t) :: theta', gs)
        | validate (theta', ("=", [t1, t2]) :: gs) =
            ERROR ("in check_satisfied, " ^ termString t1 ^ " is set to " ^
                   termString t2 ^ ", but " ^ termString t1 ^
                                                           " is not a variable")
        | validate (theta', g :: gs) =
            ERROR ("in check_satisfied, expected a substitution but got " ^
                   goalString g)
        | validate (theta', []) = OK (reverse theta')
  in  split ([] , goals)
  end
(* One form of [[concrete]] is a [[unit_test]]. All unit *)
(* tests are parsed similarly, but parsing      *)
(* [[check-satisfied]] is a bit tricky: the concrete *)
(* syntax provides a list of goals, which must be split *)
(* into ``real'' goals [[gs']] and ``substitution'' *)
(* goals [[rest]]. A ``substitution'' goal is an *)
(* application of the [[=]] functor.            *)
(* <boxed values 186>=                          *)
val _ = op checkSatisfied : goal list -> unit_test error
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and streams for \uprolog>=          *)
val unit_test =
     reserved "check_satisfiable" *>
        (wrap "(" ")" (CHECK_SATISFIABLE <$> commas goal)
        <?> "check_satisfiable(goal, ...)")
 <|> reserved "check_unsatisfiable" *>
        (wrap "(" ")" (CHECK_UNSATISFIABLE <$> commas goal)
        <?> "check_unsatisfiable(goal, ...)")
 <|> reserved "check_satisfied" *>
        (wrap "(" ")" (checkSatisfied <$>! commas goal)
         <?> "check_satisfied(goal, ... [, X1 = t1, ...])")
(* \qtrim1 The three unit tests are recognized and *)
(* treated specially.                           *)
(* <boxed values 187>=                          *)
val _ = op unit_test : unit_test parser
(* <parsers and streams for \uprolog>=          *)
val notClosing =
  sat (fn RESERVED "]" => false | _ => true) token
val concrete = 
     (BRACKET o concat o map tokenString) <$> wrap "[" "]" (many notClosing)
 <|> CTEST <$> unit_test
 <|> curry CLAUSE <$> goal <*> reserved ":-" *> (SOME <$> commas goal)
 <|> GOALS <$> commas goal
(* Given the [[unit_test]] parser, the remaining forms *)
(* of [[concrete]] are easy to recognize.       *)
(* <boxed values 188>=                          *)
val _ = op concrete : concrete parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and streams for \uprolog>=          *)
datatype mode = QMODE | RMODE
fun mprompt RMODE = "-> "
  | mprompt QMODE = "?- "
(* [*] In most contexts, the parser knows what a *)
(* [[concrete]] value is supposed to mean, but there's *)
(* one case in which it doesn't: a phrase like ``[[color *)
(* (yellow).]]'' could be either a clause or a query. *)
(* To know which is meant, the parser has to know the *)
(* mode. In other words, the mode distinguishes [[CLAUSE *)
(* (g, NONE)]] from [[GOALS [g]]]. A parser may be in *)
(* either query mode or rule (clause) mode. Each mode *)
(* has its own prompt.                          *)
(* <boxed values 189>=                          *)
type mode = mode
val _ = op mprompt : mode -> string
(* <parsers and streams for \uprolog>=          *)
datatype xdef_or_mode
  = XDEF of xdef
  | NEW_MODE of mode
(* <boxed values 190>=                          *)
type xdef_or_mode = xdef_or_mode
(* <parsers and streams for \uprolog>=          *)
fun interpretConcrete mode =
  let val (newMode, cq, xdef) = (OK o NEW_MODE, OK o XDEF o DEF, OK o XDEF)
  in  fn c =>
        case (mode, c)
          of (_, BRACKET "rule")     => newMode RMODE
           | (_, BRACKET "fact")     => newMode RMODE
           | (_, BRACKET "user")     => newMode RMODE
           | (_, BRACKET "clause")   => newMode RMODE
           | (_, BRACKET "query")    => newMode QMODE
           | (_, BRACKET s)          => xdef (USE s)
           | (_, CTEST t)            => xdef (TEST t)
           | (RMODE, CLAUSE (g, ps)) => cq (ADD_CLAUSE (g :- getOpt (ps, [])))
           | (RMODE, GOALS [g])      => cq (ADD_CLAUSE (g :- []))
           | (RMODE, GOALS _ ) =>
                 ERROR ("You cannot enter a query in clause mode; " ^
                        "to change modes, type `[query].'")
           | (QMODE, GOALS gs)           => cq (QUERY gs)
           | (QMODE, CLAUSE (g, NONE))   => cq (QUERY [g])
           | (QMODE, CLAUSE (_, SOME _)) => 
                 ERROR ("You cannot enter a new clause in query mode; " ^
                        "to change modes, type `[rule].'")
  end                 
(* <boxed values 191>=                          *)
val _ = op interpretConcrete : mode -> concrete -> xdef_or_mode error
(* <parsers and streams for \uprolog>=          *)
val skippable = 
  (fn SYMBOLIC "." => NONE | EOF => NONE | t => SOME t) <$>? token

fun badConcrete (loc, skipped) last =
  ERROR (srclocString loc ^ ": expected clause or query; skipping" ^
         concat (map (fn t => " " ^ tokenString t) (skipped @ last)))

fun xdef_or_mode mode = interpretConcrete mode <$>!
  (   concrete <* reserved "."
  <|> badConcrete <$> @@ (many  skippable) <*>! ([RESERVED "."] <$ reserved ".")
  <|> badConcrete <$> @@ (many1 skippable) <*>! pure []  (* skip to EOF *)
  )
(* Parser \monobox[[xdef_or_mode]] m parses a   *)
(* [[concrete]] according to mode m. If it sees *)
(* something it doesn't recognize, it emits an error *)
(* message and skips ahead until it sees a dot or the *)
(* end of the input. Importantly, this parser never *)
(* fails: it always returns either a [[xdef_or_mode]] *)
(* value or an error message.                   *)
(* <boxed values 192>=                          *)
val _ = op xdef_or_mode : mode -> xdef_or_mode parser
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <parsers and streams for \uprolog>=          *)
fun xdefsInMode initialMode (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref (if ps1 = "" then "" else mprompt initialMode)
      val setPrompt = if ps1 = "" then (fn _ => ()) else (fn s => thePrompt := s
                                                                               )

      type read_state = string * mode * token located eol_marked stream
      (* <utility functions for [[xdefsInMode]]>=     *)
      fun startsWithEOF tokens =
        case streamGet tokens
          of SOME (INLINE (_, EOF), _) => true
           | _ => false
      (* Using [[INLINE]] may look strange, but many of the *)
      (* utility functions from \crefapp:lazyparse expect a *)
      (* stream of tokens tagged with [[INLINE]]. I want to *)
      (* use those functions, and even though the [[INLINE]] *)
      (* tag isn't useful for parsing uProlog, it is easy *)
      (* enough to ignore [[INLINE]]. Much easier than *)
      (* rewriting big chunks of \crefapp:lazyparse.  *)
      (*                                              *)
      (* Function [[getXdef]], which is defined below, uses *)
      (* [[startsWithEOF]] to check if the input stream has no *)
      (* more tokens.                                 *)
      (* <boxed values 194>=                          *)
      val _ = op startsWithEOF : token located eol_marked stream -> bool
      (* <utility functions for [[xdefsInMode]]>=     *)
      fun skipPastDot tokens =
        case streamGet tokens
          of SOME (INLINE (_, RESERVED "."), tokens) => tokens
           | SOME (INLINE (_, EOF), tokens) => tokens
           | SOME (_, tokens) => skipPastDot tokens
           | NONE => tokens
      (* If [[getXdef]] detects an error, it skips tokens in *)
      (* the input up to and including the next dot.  *)
      (* <boxed values 195>=                          *)
      val _ = op skipPastDot : token located eol_marked stream -> token located
                                                               eol_marked stream
      (* <utility functions for [[xdefsInMode]]>=     *)
      fun getXdef (ps1, mode, tokens) =
        ( setPrompt ps1
        ; if startsWithEOF tokens then
            NONE
          else
            case xdef_or_mode mode tokens
              of SOME (OK (XDEF d),        tokens) => SOME (d, (ps1, mode,
                                                                        tokens))
               | SOME (OK (NEW_MODE mode), tokens) => getXdef (mprompt mode,
                                                                   mode, tokens)
               | SOME (ERROR msg,          tokens) => 
                                               ( eprintln ("syntax error: " ^
                                                                            msg)
                                               ; getXdef (ps1, mode, skipPastDot
                                                                         tokens)
                                               )
               | NONE =>
                       (* <fail epically with a diagnostic about [[tokens]]>= *)
                         let val tokensStrings =
                               map (fn t => " " ^ tokenString t) o valOf o peek
                                                                    (many token)
                             val _ = app print (tokensStrings tokens)
                         in  raise InternalError "cq parser failed"
                         end
        )                 
      (* And now the definition of [[getXdef]]. It tracks the *)
      (* prompt, the mode, and the remaining unread tokens, *)
      (* which together form the [[read_state]]. It also, when *)
      (* called, sets the prompt.                     *)
      (* <boxed values 196>=                          *)
      val _ = op getXdef : read_state -> (xdef * read_state) option
      (* Parser [[xdef_or_mode]] is always supposed to return *)
      (* something. If it doesn't, the interpreter dumps all *)
      (* the tokens and fails with an internal error. *)


      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      val chars = 
        streamConcatMap
        (fn (loc, s) => streamOfList (map INLINE (explode s) @ [EOL (snd loc)]))
        (locatedStream (name, lines))

      fun getLocatedToken (loc, chars) =
        (case tokenAt loc chars
           of SOME (OK (loc, t), chars) => SOME (OK (loc, t), (loc, chars))
            | SOME (ERROR msg,   chars) => SOME (ERROR msg,   (loc, chars))
            | NONE => NONE
        ) before setPrompt ps2

      val tokens =
        stripAndReportErrors (streamOfUnfold getLocatedToken ((name, 1), chars))

(* Reading clauses and queries while tracking locations *)
(* and modes                                    *)
(*                                              *)
(* To produce a stream of definitions, every other *)
(* interpreter in this book uses the function   *)
(* [[interactiveParsedStream]] from page [->]. uProlog *)
(* can't: function [[interactiveParsedStream]] doesn't *)
(* keep track of modes. As a replacement, I define a *)
(* somewhat more complex function, [[xdefsInMode]], *)
(* below. At the core of [[xdefsInMode]] is function *)
(* [[getXdef]].                                 *)
(* <boxed values 193>=                          *)
val _ = op xdefsInMode : mode -> string * line stream * prompts -> xdef stream
type read_state  = read_state  fun zz__checktyperead_state (x : read_state ) = (
                           x :  string * mode * token located eol_marked stream)
val _ = op getXdef : read_state -> (xdef * read_state) option
  in  streamOfUnfold getXdef (!thePrompt, initialMode, streamMap INLINE tokens)
  end 
val xdefstream = xdefsInMode RMODE
(* <shared definitions of [[filexdefs]] and [[stringsxdefs]]>= *)
fun filexdefs (filename, fd, prompts) =
      xdefstream (filename, filelines fd, prompts)
fun stringsxdefs (name, strings) =
      xdefstream (name, streamOfList strings, noPrompts)
(* <boxed values 54>=                           *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream


(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UPROLOG *)
(*                                                               *)
(*****************************************************************)

(* The evaluation parts are organized as follows: *)
(* <evaluation, testing, and the read-eval-print loop for \uprolog>= *)
(* <\uprolog's database of clauses>=            *)
type database = clause list
val emptyDatabase = []
fun addClause (r, rs) = rs @ [r] (* must maintain order *)
fun potentialMatches (_, rs) = rs
(* My representation is a list. As a result, I treat *)
(* every clause as a potential match.           *)
(* <boxed values 97>=                           *)
type database = database
val _ = op emptyDatabase    : database
val _ = op addClause        : clause * database -> database
val _ = op potentialMatches : goal * database -> clause list
(* <functions [[eval]], [[is]], and [[compare]], used in primitive predicates>= *)
fun eval (LITERAL n) = n
  | eval (APPLY ("+", [x, y])) = eval x  +  eval y
  | eval (APPLY ("*", [x, y])) = eval x  *  eval y
  | eval (APPLY ("-", [x, y])) = eval x  -  eval y
  | eval (APPLY ("/", [x, y])) = eval x div eval y
  | eval (APPLY ("-", [x]))    = 0 - eval x
  | eval (APPLY (f, _))        = 
      raise RuntimeError (f ^ " is not an arithmetic predicate " ^
                          "or is used with wrong arity")
  | eval (VAR v) = raise RuntimeError ("Used uninstantiated variable " ^ v ^
                                       " in arithmetic expression")
(* Primitive predicate [[is]] requires a very small *)
(* evaluator. Because it works only with integers, never *)
(* with variables, the evaluator doesn't need an *)
(* environment.                                 *)
(* <boxed values 162>=                          *)
val _ = op eval : term -> int
(* Predicate x[[ is ]]e evaluates term e as an integer *)
(* expression and constrains it to equal x.     *)
(* <functions [[eval]], [[is]], and [[compare]], used in primitive predicates>= *)
fun is [x, e] succ fail = (succ (solve (x ~ LITERAL (eval e))) fail
                           handle Unsatisfiable => fail())
  | is _      _    fail = fail ()
(* A comparison predicate is applied to exactly two *)
(* arguments. If these arguments aren't integers, it's a *)
(* run-time error. If they are, ML function [[cmp]] *)
(* determines the success or failure of the predicate. *)
(* <functions [[eval]], [[is]], and [[compare]], used in primitive predicates>= *)
fun compare name cmp [LITERAL n, LITERAL m] succ fail =
      if cmp (n, m) then succ idsubst fail else fail ()
  | compare name _ [_, _] _ _ =
      raise RuntimeError ("Used comparison " ^ name ^ " on non-integer term")
  | compare name _ _ _ _ =
      raise InternalError ("this can't happen---non-binary comparison?!")
(* There are four comparison predicates.        *)

(* [*] Tracing flow through Byrd boxes. Create a tracing *)
(* version of the interpreter that logs every entry to *)
(* and exit from a Byrd box. Use the following  *)
(* functions:                                   *)
(* <tracing functions>=                         *)
fun logSucc goal succ theta resume =
  ( app print ["SUCC: ", goalString goal, " becomes ",
               goalString (goalsubst theta goal), "\n"]
  ; succ theta resume
  )
fun logFail goal fail () = 
  ( app print ["FAIL: ", goalString goal, "\n"]
  ; fail ()
  )
fun logResume goal resume () = 
  ( app print ["REDO: ", goalString goal, "\n"]
  ; resume ()
  )
fun logSolve solve goal succ fail = 
  ( app print ["START: ", goalString goal, "\n"]
  ; solve goal succ fail
  )
(* <search ((prototype))>=                      *)
fun 'a query database =
  let val primitives = foldl (fn ((n, p), rho) => bind (n, p, rho))
                       emptyEnv (
                              (* Primitives                                   *)

                              (*                                              *)

                        (* The uProlog interpreter doesn't store a persistent *)

                       (* initial basis in an ML variable. Instead, uProlog's *)

                        (* primitive predicates sit in an environment that is *)

                      (* used in the [[query]] function. These predicates are *)

                          (* defined in this section, starting with [[true]]. *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("true", fn args => fn succ => fn fail =>
                                            if null args then succ idsubst fail
                                                                else fail ()) ::

                     (* Predicate [[atom]] tests to see if its argument is an *)

                              (* atom.                                        *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("atom", fn args => fn succ => fn fail =>
                                             case args of [APPLY(f, [])] => succ
                                                                    idsubst fail
                                                        | _ => fail ()) ::

                      (* Printing a term always succeeds, and it produces the *)

                              (* identity substitution.                       *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("print", fn args => fn succ => fn fail =>
                                             ( app (fn x => (print (termString x
                                                             ); print " ")) args
                                             ; print "\n"
                                             ; succ idsubst fail
                                             )) ::

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("is", is) ::

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("<",  compare "<"  op < ) ::
                                 (">",  compare ">"  op > ) ::
                                 ("=<", compare "=<" op <= ) ::
                                 (">=", compare ">=" op >= ) ::

                          (* Each predicate above takes as argument a list of *)

                              (* terms, a success continuation, and a failure *)

                     (* continuation. Two more predicates, [[!]] and [[not]], *)

                     (* cannot be implemented using this technique; they have *)

                        (* to be added directly to the interpreter (Exercises *)

                      (* [->] and [->]). This code ensures that they can't be *)

                              (* used by mistake.                             *)

                              (* <\uprolog's primitive predicates [[::]]>=    *)
                                 ("!",   fn _ => raise LeftAsExercise
                                                               "the cut (!)") ::
                                 ("not", fn _ => raise LeftAsExercise
                                                       "predicate `not`") :: [])
      fun solveOne (goal as (predicate, args)) succ fail =
            find (predicate, primitives) args succ fail
            handle NotFound _ =>
              let fun search [] = fail ()
                    | search (clause :: clauses) =  
                        let fun resume () = search clauses
                            val G :- Hs = freshen clause
                            val theta = unify (goal, G)
                        in  solveMany (map (goalsubst theta) Hs) theta succ
                                                                          resume
                        end
                        handle Unsatisfiable => search clauses
(* <boxed values 104>=                          *)
val _ = op query : database -> goal list  -> (subst -> (unit->'a) -> 'a) -> (
                                                                 unit->'a) -> 'a
val _ = op solveOne  : goal               -> (subst -> (unit->'a) -> 'a) -> (
                                                                 unit->'a) -> 'a
val _ = op solveMany : goal list -> subst -> (subst -> (unit->'a) -> 'a) -> (
                                                                 unit->'a) -> 'a
val _ = op search    : clause list -> 'a
              in  search (potentialMatches (goal, database))
              end
      and solveMany []            theta succ fail = succ theta fail
        | solveMany (goal::goals) theta succ fail =
            solveOne goal
            (fn theta' => fn resume => solveMany (map (goalsubst theta') goals)
                                                 (compose (theta', theta))
                                                 succ
                                                 resume)
            fail
  in  fn gs => solveMany gs idsubst
  end
(* The environment [[primitives]] holds the primitive *)
(* predicates. These predicates are implemented by *)
(* polymorphic ML functions, and as a result, ML's *)
(* ``value restriction'' prevents me from defining *)
(* [[primitives]] at top level. To work around the *)
(* restriction, function [[query]] rebuilds     *)
(* [[primitives]] once per query. Luckily the cost is *)
(* small compared with the cost of the search.  *)

(* <interaction>=                               *)
fun showAndContinue interactivity theta gs =
  let fun varResult x = x ^ " = " ^ termString (varsubst theta x)
      val vars = foldr union' emptyset (map goalFreevars gs)
      val results = String.concatWith "\n" (map varResult vars)
  in  if null vars then
        false (* no more solutions possible; don't continue *)
      else
        ( print results
        ; if prompts interactivity then
            case Option.map explode (TextIO.inputLine TextIO.stdIn)
              of SOME (#";" :: _) => (print "\n"; true)
               | _ => false
          else
            (print "\n"; false)
        )
  end
(* <boxed values 106>=                          *)
val _ = op showAndContinue : interactivity -> subst -> goal list -> bool
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

(* <definitions of [[basis]] and [[processDef]] for \uprolog>= *)
type basis = database
fun processDef (cq, database, interactivity) =
  let fun process (ADD_CLAUSE c) = addClause (c, database)
        | process (QUERY gs) = (
                              (* <query goals [[gs]] against [[database]]>=   *)
                                query database gs
                                  (fn theta => fn resume =>
                                     if showAndContinue interactivity theta gs
                                                                  then resume ()
                                      else print "yes\n")
                                  (fn () => print "no\n"); database)
      fun caught msg = (eprintln (stripAtLoc msg); database)
  in  withHandlers process cq caught
  end
fun dump_names db = println "cannot dump uProlog names"  (*OMIT*)
(* <boxed values 105>=                          *)
type basis = basis
val _ = op processDef : cq * database * interactivity -> database
(* <shared unit-testing utilities>=             *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* <boxed values 34>=                           *)
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
(* <definition of [[testIsGood]] for \uprolog>= *)
fun testIsGood (test, database) =
  let
(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      type query = goal list
      val qstring = 
        nullOrCommaSep "?" o map goalString
      (* If a query fails a test, it is printed using function *)
      (* [[qstring]].                                 *)
      (* <boxed values 164>=                          *)
      type query = query
      val _ = op qstring : query -> string
      (* All three unit tests work by passing appropriate *)
      (* success and failure continuations to [[query]]. *)
      (* To pass the [[check-unsatisfiable]] test, the query *)
      (* must be unsatisfiable. If the test fails, the *)
      (* satisfying substitution is shown without logical *)
      (* variables that are introduced by renaming clauses. *)
      (* Such variables begin with underscores, and they are *)
      (* removed by function [[stripSubst]].          *)

(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      fun stripSubst theta =
        List.filter (fn (x, _) => String.sub (x, 0) <> #"_") theta
      fun checkUnsatisfiablePasses (gs) =
        let fun succ theta' _ =
              failtest ["check_unsatisfiable failed: ", qstring gs,
                          " is satisfiable with ", substString theta']
            fun fail () = true
        in  query database gs (succ o stripSubst) fail
        end
      (* To pass the [[check-satisfiable]] test, the query *)
      (* must be satisfiable.                         *)

(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      fun checkSatisfiablePasses (gs) =
        let fun succ _ _ = true
            fun fail () = failtest ["check_unsatisfiable failed: ", qstring gs,
                                    " is not satisfiable"]
        in  query database gs succ fail
        end

(* <definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]]>= *)
      fun checkSatisfiedPasses (gs, theta) =
        let val thetaVars =
              foldl (fn ((_, t), fv) => union (termFreevars t, fv)) emptyset
                                                                           theta
            val ground = null thetaVars
            val gs' = map (goalsubst theta) gs
            fun succ theta' _ =
              if ground andalso not (null theta') then
                failtest ["check_satisfied failed: ", qstring gs,
                          " required additional substitution ", substString
                                                                         theta']
              else
                true
            fun fail () =
              failtest ["check_satisfied failed: could not prove ", qstring gs']
        in  query database gs' (succ o stripSubst) fail
        end
      fun passes (CHECK_UNSATISFIABLE gs)      = checkUnsatisfiablePasses gs
        | passes (CHECK_SATISFIABLE   gs)      = checkSatisfiablePasses gs
        | passes (CHECK_SATISFIED (gs, theta)) = checkSatisfiedPasses (gs, theta
                                                                               )
(* <boxed values 163>=                          *)
val _ = op testIsGood : unit_test * basis -> bool
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

  in  passes test
  end
(* <shared definition of [[processTests]]>=     *)
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
and numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
(* <boxed values 35>=                           *)
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
            (* Altered \chaptocsplitinfrastructure          *)
            (*                                              *)
            (* A couple of the standard functions used in other *)
            (* interpreters have been tweaked to work better with \ *)
            (* upr and with implementations of real Prolog. *)
            (*                                              *)
            (* Altered [[useFile]]                          *)
            (*                                              *)
            (* To make uProlog more compatible with other   *)
            (* implementations of Prolog, I patch the [[useFile]] *)
            (* function defined in \crefmlscheme.chap. If the *)
            (* original [[useFile]] (here called [[try]]) fails with *)
            (* an I/O error, the patched version tries adding  *)
            (* ``[[.P]]'' to the name; this is the convention used *)
            (* by XSB Prolog. If adding [[.P]] fails, it also tries  *)
            (* ``[[.pl]]''; this is the convention used by  *)
            (* GNU Prolog and SWI Prolog.                   *)
            (* <definition of [[useFile]], to read from a file>= *)
            val try = useFile
            fun useFile filename = 
              try filename          handle IO.Io _ => 
              try (filename ^ ".P") handle IO.Io _ => 
              try (filename ^ ".pl")
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
      (* <boxed values 62>=                           *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* <boxed values 61>=                           *)
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
(* <boxed values 61>=                           *)
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
(*   FUNCTION [[RUNAS]] FOR \UPROLOG                             *)
(*                                                               *)
(*****************************************************************)

(* <function [[runAs]] for \uprolog>=           *)
fun runAs interactivity = 
  let val _ = setup_error_format interactivity
      val (prompts, prologMode) =
        if prompts interactivity then (stdPrompts, QMODE) else (noPrompts, RMODE
                                                                               )
      val xdefs =
        xdefsInMode prologMode ("standard input", filelines TextIO.stdIn,
                                                                        prompts)
  in  ignore (readEvalPrintWith eprintln (xdefs, emptyDatabase, interactivity))
  end 
(* \qbreak                                      *)
(*                                              *)
(* Altered command-line processing              *)
(*                                              *)
(* uProlog's command-line processor differs from our *)
(* other interpreters, because it has to deal with *)
(* modes. When prompting, it starts in query mode; when *)
(* not prompting, it starts in rule mode. And because *)
(* I'm pressed for time, it doesn't have all the nice *)
(* option processing of the other command-line  *)
(* processors.                                  *)
(* <boxed values 197>=                          *)
val _ = op runAs : interactivity -> unit
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)



(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT \UPROLOG'S COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] *)
(*                                                               *)
(*****************************************************************)

(* The [[-q]] option is as in other interpreters, and *)
(* the [[-trace]] option turns on tracing.      *)
(* <code that looks at \uprolog's command-line arguments and calls [[runAs]]>= *)
fun runmain ["-q"]          = runAs (NOT_PROMPTING, ECHOING)
  | runmain []              = runAs (PROMPTING,     ECHOING)
  | runmain ("-trace" :: t) = (tracer := app eprint; runmain t)
  | runmain _  =
      TextIO.output (TextIO.stdErr,
                     "Usage: " ^ CommandLine.name() ^ " [trace] [-q]\n")
val _ = runmain (CommandLine.arguments())
