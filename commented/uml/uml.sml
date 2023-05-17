(* <uml.sml>=                                   *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE INFERENCE            *)
(*                                                               *)
(*****************************************************************)

(* To infer the type of a [[LETSTAR]] form, we desugar *)
(* it into nested [[LET]]s.                     *)
(* <exceptions used in languages with type inference>= *)
exception TypeError of string
exception BugInTypeInference of string


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
(* <boxed values 197>=                          *)
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

(* <boxed values 1>=                            *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* Finally, environments can be composed using the + *)
(*  operator. In my ML code, this operator is   *)
(* implemented by function [[<+>]], which I declare to *)
(* be [[infix]]. It uses the predefined infix function  *)
(* [[@]], which is ML's way of writing [[append]]. \ *)
(* mlsflabel<+>                                 *)
(* <boxed values 1>=                            *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* <support for names and environments>=        *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* <boxed values 96>=                           *)
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
(* <boxed values 165>=                          *)
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
(* <boxed values 94>=                           *)
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
(* <boxed values 95>=                           *)
val _ = op optionList : 'a option list -> 'a list option
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* <utility functions for string manipulation and printing>= *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* <boxed values 86>=                           *)
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
(* <boxed values 87>=                           *)
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
(* <boxed values 88>=                           *)
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
(* <boxed values 89>=                           *)
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
(* <boxed values 90>=                           *)
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
(* The internal function [[kind]] computes the kind of  *)
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
(* <boxed values 98>=                           *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* A very common special case occurs when the   *)
(* continuation always succeeds; that is, the   *)
(* continuation [[k']] has type \monobox'a -> 'b instead *)
(* of \monobox'a -> 'b error. In this case, the *)
(* execution plan is that when [[(f x)]] succeeds, *)
(* continue by applying [[k']] to the result; otherwise *)
(* propagate the error. I know of no standard way to *)
(* write this operator, [Haskell uses [[flip fmap]].] , *)
(* so I use [[>>=+]], which you might also choose to *)
(* pronounce ``and then.''                      *)

(* <support for representing errors as \ml\ values>= *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* <boxed values 99>=                           *)
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
(* <boxed values 100>=                          *)
val _ = op errorList : 'a error list -> 'a list error
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

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
(* <boxed values 131>=                          *)
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
(* <boxed values 91>=                           *)
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
(* <boxed values 97>=                           *)
val _ = op hasOption : string -> bool
end
(* <collections with mapping and combining functions>= *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* <boxed values 92>=                           *)
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
(* <boxed values 93>=                           *)
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
(* <boxed values 107>=                          *)
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
(* <boxed values 108>=                          *)
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
(* <boxed values 109>=                          *)
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
(* <boxed values 109>=                          *)
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
(* <boxed values 110>=                          *)
val _ = op listOfStream : 'a stream -> 'a list
(* The more interesting streams are those that result *)
(* from actions. To help create such streams, I define *)
(* [[delayedStream]] as a convenience abbreviation for *)
(* creating a stream from one action.           *)
(* <boxed values 110>=                          *)
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
(* <boxed values 111>=                          *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* Function [[streamOfEffects]] can be used to produce a *)
(* stream of lines from an input file:          *)

(* <streams>=                                   *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* <boxed values 112>=                          *)
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
(* <boxed values 113>=                          *)
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
(* <boxed values 114>=                          *)
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
(* <boxed values 115>=                          *)
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
(* <boxed values 116>=                          *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* <boxed values 116>=                          *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* <streams>=                                   *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* <boxed values 117>=                          *)
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
(* <boxed values 118>=                          *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* <streams>=                                   *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* The only sensible order in which to fold the elements *)
(* of a stream is the order in which the actions are *)
(* taken and the results are produced: from left to *)
(* right. [*]                                   *)
(* <boxed values 119>=                          *)
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
(* <boxed values 120>=                          *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* <boxed values 120>=                          *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* <streams>=                                   *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* In list and stream processing, [[concat]] is very *)
(* often composed with [[map f]]. The composition is *)
(* usually called [[concatMap]].                *)
(* <boxed values 121>=                          *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* <streams>=                                   *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* The code used to append two streams is much like the *)
(* code used to concatenate arbitrarily many streams. *)
(* To avoid duplicating the tricky manipulation of *)
(* states, I implement append using concatenation. *)
(* <boxed values 122>=                          *)
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
(* <boxed values 123>=                          *)
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
(* <boxed values 124>=                          *)
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
(* <boxed values 193>=                          *)
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
(* <boxed values 194>=                          *)
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
(* <boxed values 195>=                          *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* <stream transformers and their combinators>= *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* The common case of creating [[tx_f]] using [[pure]] *)
(* is normally written using the special operator [[< *)
(* >]], which is also pronounced ``applied to.'' *)
(* It combines a B-to-C function with an \atob  *)
(* transformer to produce an \atoc transformer. *)
(* <boxed values 196>=                          *)
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
(* <boxed values 198>=                          *)
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
(* <boxed values 199>=                          *)
val _ = op pzero : ('a, 'b) xformer
(* This parser obeys the algebraic law          *)
(*                                              *)
(*  \monoboxt <|> pzero = \monoboxpzero <|> t = \ *)
(*  monoboxt\text.                              *)
(*                                              *)
(* Because building choices from lists is common, *)
(* I implement this special case as [[anyParser]]. *)
(* <boxed values 199>=                          *)
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
(* <boxed values 200>=                          *)
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
(* <boxed values 201>=                          *)
val _ = op one : ('a, 'a) xformer
(* <stream transformers and their combinators>= *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* <boxed values 202>=                          *)
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
(* <boxed values 203>=                          *)
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
(* <boxed values 204>=                          *)
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
(* <boxed values 205>=                          *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* <stream transformers and their combinators>= *)
fun eqx y = 
  sat (fn y' => y = y') 
(* Transformer [[eqx b]] is [[sat]] specialized to an *)
(* equality predicate. It is typically used to recognize *)
(* special characters like keywords and minus signs. *)
(* <boxed values 206>=                          *)
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
(* <boxed values 207>=                          *)
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
(* <boxed values 208>=                          *)
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
(* <boxed values 209>=                          *)
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
(* <boxed values 210>=                          *)
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
(* <boxed values 211>=                          *)
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
(* <boxed values 212>=                          *)
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
(* <boxed values 213>=                          *)
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
(* <boxed values 126>=                          *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* To keep track of the source location of a line, *)
(* token, expression, or other datum, I put the location *)
(* and the datum together in a pair. To make it easier *)
(* to read the types, I define a type abbreviation which *)
(* says that a value paired with a location is  *)
(* ``located.''                                 *)
(* <boxed values 126>=                          *)
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
           (* <more handlers for [[atLoc]] ((type-inference))>= *)
           | e as TypeError _          => raise Located (loc, e)
           | e as BugInTypeInference _ => raise Located (loc, e)
(* The [[Located]] exception is raised by function *)
(* [[atLoc]]. Calling \monoboxatLoc f x applies [[f]] *)
(* to [[x]] within the scope of handlers that convert *)
(* recognized exceptions to the [[Located]] exception: *)
(* <boxed values 127>=                          *)
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
(* <boxed values 128>=                          *)
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
(* <boxed values 129>=                          *)
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
(* <boxed values 130>=                          *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* All locations originate in a located stream of lines. *)
(* The locations share a filename, and the line numbers *)
(* are 1, 2, 3, ... and so on. [*]              *)
(* <boxed values 130>=                          *)
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
(* <boxed values 222>=                          *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* \qbreak To support a stream of marked lines—possibly *)
(* marked, located lines—I define transformers [[eol]], *)
(* [[inline]], and [[srcloc]]. The [[eol]] transformer *)
(* returns the number of the line just ended.   *)
(* <boxed values 222>=                          *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* <support for lexical analysis>=              *)
type 'a lexer = (char, 'a) xformer
(* <boxed values 214>=                          *)
type 'a lexer = 'a lexer
(* The type [['a lexer]] should be pronounced ``lexer *)
(* returning [['a]].''                          *)

(* <support for lexical analysis>=              *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* <boxed values 215>=                          *)
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
(* <boxed values 216>=                          *)
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
(* <boxed values 217>=                          *)
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
(* <boxed values 218>=                          *)
val _ = op intFromChars : char list -> int error
(* <support for lexical analysis>=              *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* In this book, every language except uProlog can use *)
(* [[intToken]].                                *)
(* <boxed values 219>=                          *)
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
(* <boxed values 220>=                          *)
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
(* <boxed values 221>=                          *)
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
(* <boxed values 223>=                          *)
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
(* <boxed values 224>=                          *)
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
(* <boxed values 225>=                          *)
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
(* <boxed values 225>=                          *)
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
(* <boxed values 226>=                          *)
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
(* <boxed values 235>=                          *)
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
(* <boxed values 236>=                          *)
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
(* <boxed values 228>=                          *)
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
(* <boxed values 229>=                          *)
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
(* Function [[<!>]] is adequate for simple cases, but to *)
(* produce a really good error message, I might wish to *)
(* use the result from [[p]] to build a message. *)
(* My interpreters produce such messages only for text *)
(* appearing in brackets, so [[errorAtEnd]] triggers *)
(* only when [[p]] parses tokens that are followed by a *)
(* right bracket. \nwcrazynarrowboxes           *)
(* <boxed values 227>=                          *)
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
(* <boxed values 230>=                          *)
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
(* <boxed values 230>=                          *)
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
(* <boxed values 231>=                          *)
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
(* <boxed values 232>=                          *)
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
(* <boxed values 233>=                          *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* <boxed values 233>=                          *)
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
(* <boxed values 234>=                          *)
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
(* <boxed values 237>=                          *)
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
(* <boxed values 238>=                          *)
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
(* <boxed values 239>=                          *)
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
(* <boxed values 240>=                          *)
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
(* <boxed values 241>=                          *)
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
(* <boxed values 242>=                          *)
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
(* <boxed values 243>=                          *)
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
(* <boxed values 244>=                          *)
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
(* <boxed values 245>=                          *)
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
(*   HINDLEY-MILNER TYPES WITH GENERATED TYPE CONSTRUCTORS       *)
(*                                                               *)
(*****************************************************************)

(* Most of uML's type components are shared with \nml or *)
(* with micro-Haskell (which didn't make the cut for the *)
(* book).                                       *)
(* <Hindley-Milner types with generated type constructors>= *)
(* The syntactic sugar completes what you need to know *)
(* in order to program effectively using algebraic data *)
(* types. To understand more deeply how they work, *)
(* consult the theory and code in the next sections: *)
(* when and how a user-defined type is distinct from *)
(* similar types (\crefadt.generativity), how the *)
(* relevant syntax and values are represented (\cref *)
(* adt.ast), how type definitions are typed and *)
(* evaluated (\crefadt.user-def-theory), and how case *)
(* expressions are typechecked and evaluated (\cref *)
(* adt.case).                                   *)
(*                                              *)
(* Type generativity and \maintocsplittype equivalence *)
(*                                              *)
(* [*] In any language that has user-defined types, *)
(* a programmer has to know if and when the types they *)
(* define are equivalent to anything else.      *)
(* That knowledge is determined by the ways the language *)
(* uses three concepts: structural equivalence, *)
(* generativity, and type abbreviation.         *)
(*                                              *)
(*   • Structural equivalence says two types are *)
(*  equivalent when they are applications of    *)
(*  equivalent type constructors to equivalent type *)
(*  arguments, as in Typed uScheme's \rulename  *)
(*  EquivApplications rule (\cpageref           *)
(*  tuscheme.EquivApplications): \typesystemtuscheme *)
(*  \usety.EquivApplications In uML, as in Typed *)
(*  uScheme and \nml, structural equivalence is used *)
(*  for list types, function types, pair types, and *)
(*  so on. In C, structural equivalence is used for *)
(*  pointer types and array types; for example, *)
(*  pointers to equivalent types are equivalent. And *)
(*  in \modula3, for example, structural equivalence *)
(*  is used for record types: record types with the *)
(*  same fields are equivalent, provided that   *)
(*  corresponding fields have equivalent types. *)
(*                                              *)
(*   • Generativity is a property that a language *)
(*  designer can associate with any syntactic form *)
(*  involving types. If a syntactic form is     *)
(*  generative, then a type or type constructor *)
(*  introduced by that form is distinct from—that is, *)
(*  not equivalent to—any other type or type  *)
(*  constructor. In Typed uScheme* and \nml, no forms *)
(*  are generative, because Typed uScheme and \nml *)
(*  have no type-definition forms. In uML, the  *)
(*  [[data]] and [[implicit-data]] forms are    *)
(*  generative, as are the corresponding forms in *)
(*  Standard ML, \ocaml, and Haskell. uML's     *)
(*  [[record]] form is syntactic sugar for [[data]], *)
(*  so it is generative, but the corresponding form *)
(*  in Standard ML is not. In C, the [[struct]], *)
(*  [[union]], and [[enum]] type definition forms—the *)
(*  ones that include fields in curly braces—are *)
(*  generative, but the corresponding type reference *)
(*  forms, like just plain \monoboxstruct Exp,  *)
(*  are not (\crefadt.ex.generative-struct).    *)
(*  In Haskell, both [[data]] and [[newtype]]   *)
(*  definition forms are generative; [[newtype]] *)
(*  introduces a new, distinct type that has the same *)
(*  run-time representation as an existing type. *)
(*                                              *)
(*   • A type abbreviation introduces a new name for an *)
(*  existing type. A type-abbreviation form is  *)
(*  purposefully not generative; the new name is *)
(*  equivalent to the original type. In \mcl (\cref *)
(*  mcl.chap), as well as Standard ML, type     *)
(*  abbreviations are written using the [[type]] *)
(*  keyword; the same form appears in Haskell, where *)
(*  it is called a type synonym, and in C, where it *)
(*  is called a ``[[typedef]] declaration.''    *)
(*                                              *)
(* By design, only structural equivalence and   *)
(* generativity are used in uML; type abbreviations are *)
(* added in \crefadt.ex.type-abbreviations.     *)
(*                                              *)
(* Generativity is a powerful idea, but in older work, *)
(* especially work oriented toward compilers, you may *)
(* see the term ``name equivalence'' or ``occurrence *)
(* equivalence.'' These terms refer to special  *)
(* applications of generativity—for example, ``name *)
(* equivalence'' may describe a language that has a *)
(* syntactic form which resembles a type abbreviation *)
(* but is generative. The terms ``name equivalence'' and *)
(* ``occurrence equivalence'' usually describe language *)
(* designs that were popular in the 1970s, but these *)
(* terms are outmoded and should no longer be used. *)
(* The concept of generativity is more flexible and can *)
(* be applied to more designs.                  *)
(*                                              *)
(* Of what use is generativity? Generativity helps *)
(* ensure that two types are equivalent only when you *)
(* mean them to be equivalent. This issue matters most *)
(* when programs are split into multiple modules (\cref *)
(* mcl.chap). For example, if two record types both have *)
(* numeric fields [[heading]] and [[distance]], but one *)
(* is degrees and miles and the other is radians and *)
(* kilometers, you want them not to be equivalent. *)
(*                                              *)
(* Generativity should inform our thinking about design, *)
(* theory, and implementation of programming languages. *)
(*                                              *)
(*   • The effect of typing a generative construct can *)
(*  be expressed by the idea of a ``fresh'' or  *)
(*  ``distinct'' type constructor. To define    *)
(*  freshness, uML's type theory remembers a set *)
(*  containing every type constructor ever created; *)
(*  a fresh constructor is one not in that set. *)
(*  In the implementation, there's less bookkeeping; *)
(*  each new type constructor is assigned an identity *)
(*  that is guaranteed to be unique. The techniques *)
(*  used are the same techniques used to allocate *)
(*  fresh locations in the operational semantics and *)
(*  implementation of micro-Scheme.             *)
(*                                              *)
(*   • When definitions can be generative and type names *)
(*  can be redefined, a single name can stand for *)
(*  different types in different parts of a program. *)
(*  In this way, a type name is like a variable name, *)
(*  which can stand for different values in different *)
(*  parts of a program.                         *)
(*                                              *)
(*  In the interpreter, types are represented in two *)
(*  ways. Type syntax, represented by ML type   *)
(*  [[tyex]] (\chunkrefadt.chunk.tyex) and shown *)
(*  mathematically as t, appears in programs, and *)
(*  type syntax is built up using type names (ML type *)
(*  [[name]]). Types themselves, represented by ML *)
(*  types [[ty]] and [[type_scheme]] (\chunkref *)
(*  ml.chunk.ty) and shown mathematically as tau and  *)
(*  sigma, are used by the type checker, and types *)
(*  are built up using type constructors (ML type *)
(*  [[tycon]]).                                 *)
(*                                              *)
(* The internal representation of type constructors, the *)
(* generation of fresh type constructors, and a *)
(* type-equivalence function are presented below. *)
(*                                              *)
(* Representing and \chaptocsplitgenerating type \ *)
(* chaptocsplitconstructors                     *)
(*                                              *)
(* [*]                                          *)
(*                                              *)
(* The representation of a type constructor must solve *)
(* two problems: it should be easy to create a type *)
(* constructor that is distinct from all others, and *)
(* it should be easy to tell if two type constructors *)
(* are the same. I address both problems by assigning *)
(* each type constructor an identity, which I represent *)
(* by an integer.                               *)
(* <foundational definitions for generated type constructors>= *)
type tycon_identity = int
(* Integers are great for algorithms but not so good for *)
(* talking to programmers. To make it possible to print *)
(* an informative representation of any type,   *)
(* I represent a type constructor as a record containing *)
(* not only its identity but also a name used to *)
(* print it. \umllabeltycon                     *)
(* <foundational definitions for generated type constructors>= *)
type tycon = { printName : name, identity : tycon_identity }
(* <foundational definitions for generated type constructors>= *)
fun eqTycon ( { identity = id,  printName = _ }
            , { identity = id', printName = _ }) = 
  id = id'
(* Every type constructor is created by function *)
(* [[freshTycon]], which is defined in \crefadta.chap. *)
(* This function takes a type name as its argument and *)
(* returns a [[tycon]] with a distinct [[printName]] and *)
(* a unique [[identity]]. Type constructors are equal if *)
(* and only if they have the same identity.     *)
(* <boxed values 142>=                          *)
val _ = op eqTycon : tycon * tycon -> bool
(* <utility functions for generated type constructors>= *)
fun tyconString { identity = _, printName = T } = T
(* \qbreak                                      *)
(*                                              *)
(* Types and type inference                     *)
(*                                              *)
(* Support for type equivalence and generativity *)
(*                                              *)
(* As explained in \crefadt.chap, a type constructor's *)
(* identity is distinct from its [[printName]]. Function *)
(* [[eqTycon]] uses field [[identity]], but messages *)
(* from the interpreter use function [[tyconString]], *)
(* which returns a type constructor's [[printName]]. *)
(*                                              *)
(* <boxed values 160>=                          *)
val _ = op tyconString : tycon -> string
(* <utility functions for generated type constructors>= *)
local
  val timesDefined : int env ref = ref emptyEnv
                             (* how many times each tycon is defined *)
in
  fun freshPrintName t =
    let val n = find (t, !timesDefined) handle NotFound _ => 0
        val _ = timesDefined := bind (t, n + 1, !timesDefined)
    in  if n = 0 then t  (* first definition *)
        else t ^ "@{" ^ Int.toString (n+1) ^ "}"
    end
end
(* <boxed values 161>=                          *)
val _ = op freshPrintName : string -> string
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <utility functions for generated type constructors>= *)
local
  val nextIdentity = ref 0
  fun freshIdentity () =
    !nextIdentity before nextIdentity := !nextIdentity + 2
in
  fun freshTycon t =
    { identity = freshIdentity(), printName = freshPrintName t }
end
(* <boxed values 162>=                          *)
val _ = op freshTycon : name -> tycon
(* Only one thing is different: the representation of *)
(* closures.                                    *)
(*                                              *)
(* A closure includes an environment, and if the closure *)
(* represents a recursive function, the closure's *)
(* environment includes a reference to the closure *)
(* itself. Such self-reference is implemented using *)
(* mutation. In micro-Scheme, the environment maps every *)
(* name to a mutable reference cell. Recursion is *)
(* implemented by first putting an unspecified value in *)
(* the function's cell, then updating the cell once the *)
(* closure is created. In \nml, an environment maps each *)
(* name to a value—the mutable cell is part of the *)
(* closure, not the environment. Recursion is   *)
(* implemented by first putting an unspecified  *)
(* environment in the closure's cell, then building a *)
(* new environment, and finally updating the cell to *)
(* hold that environment. Therefore the closure stores a *)
(* \monoboxvalue env ref, not a \monoboxvalue ref env as *)
(* in micro-Scheme.                             *)
(*                                              *)
(* Operational semantics                        *)
(*                                              *)
(* Because \nml doesn't have mutation and because the *)
(* effects of its imperative primitives aren't specified *)
(* formally, its operational semantics is simple. *)
(* Its abstract machine has no locations and no store; *)
(* evaluating an expression just produces a value. The *)
(* judgment is \nmlevale ==>\nmlevalrv. \jlabel *)
(* nml.eval.exp\nmlevale ==>\nmlevalrv The environment *)
(* rho maps a name to a value, not to a mutable location *)
(* as in micro-Scheme. And evaluating a definition *)
(* produces a new environment; the form of that judgment *)
(* is \nmlevald -->rho'. \jlabelnml.eval.def\nmlevald *)
(* -->rho'                                      *)
(*                                              *)
(* Rules for expressions                        *)
(*                                              *)
(* Most of the rules should be self-explanatory. \ops *)
(* Literal \nmleval\xliteral(v) ==>\nmlevalrv   *)
(*                                              *)
(* \ops Var x in dom rho \nmleval\var(x) ==>\nmlevalrrho *)
(* (x)                                          *)
(*                                              *)
(* \ops IfTrue \nmlevale_1 ==>\nmlevalrv_1 \andalso v_1 *)
(* \neq\vfalse\andalso\nmlevale_2 ==>\nmlevalrv_2 \ *)
(* nmleval\xif(e_1, e_2, e_3) ==>\nmlevalrv_2   *)
(*                                              *)
(* \ops IfFalse \nmlevale_1 ==>\nmlevalrv_1 \andalsov_1 *)
(* = \vfalse\andalso\nmlevale_3 ==>\nmlevalrv_3 \nmleval *)
(* \xif(e_1, e_2, e_3) ==>\nmlevalrv_3          *)
(*                                              *)
(* The rules for \xbegin are a cheat; the purpose of \ *)
(* xbegin is to force order of evaluation, but these *)
(* rules are so simplified that they don't enforce an *)
(* order of evaluation. \ops EmptyBegin \nmleval\xbegin *)
(* () ==>\nmlevalr\vnil \ops Begin \fourquad \nmlevale_1 *)
(* ==>\nmlevalrv_1 \nmlevale_2 ==>\nmlevalrv_2 ... \ *)
(* nmlevale_n ==>\nmlevalrv_n \nmleval\xbegin(e_1, e_2, *)
(* ..., e_n) ==>\nmlevalrv_n                    *)
(*                                              *)
(* Just as in micro-Scheme, \xlambda captures an *)
(* environment in a closure, and \xapply uses the *)
(* captured environment. Because \nml does not store *)
(* actual parameters in mutable locations, its rules are *)
(* simpler than micro-Scheme's rules. \ops MkClosure \ *)
(* nmleval\xlambda(<x_1, ..., x_n>, e) ==> \nmlevalr\ *)
(* xclo\xlambda(<x_1, ..., x_n>, e)rho \ops ApplyClosure *)
(* \threeline \nmlevale ==>\xclo\xlambda(<x_1, ..., *)
(* x_n>, e_c)rho_c \repeati\nmlevale_i ==>\nmlevalrv_i *)
(* <e_c, rho_c{x_1|->v_1, ..., x_n|->v_n}> ==>\nmlevalrv *)
(* \nmleval\apply(e, e_1, ..., e_n) ==>\nmlevalrv *)
(*                                              *)
(* The semantic rule for applying a \nml primitive is to *)
(* apply the function attached to that primitive. The *)
(* implementation is equally simple. \ops ApplyPrimitive *)
(* \threeline \nmlevale ==>\lprimitive(f) \repeati\ *)
(* nmlevale_i ==>\nmlevalrv_i f(v_1, ..., v_n) = v \ *)
(* nmleval\apply(e, e_1, ..., e_n) ==>v         *)
(*                                              *)
(* Because a \xlet-bound name stands for a value, not a *)
(* location, rules for \xlet forms are also simplified. *)
(* \ops Let \twoline \repeati\nmlevale_i ==>\nmlevalrv_i *)
(* \nmleval[{x_1|->v_1, ..., x_n|->v_n}] e ==>\nmlevalrv *)
(* \nmleval\xlet(<x_1,e_1,...,x_n,e_n>, e) ==>\nmlevalrv *)
(*                                              *)
(* As in micro-Scheme, a \xletstar expression requires a *)
(* sequence of environments. \ops Letstar \fourline \ *)
(* nmleval[_0]e_1 ==>v_1 \andalsorho_1 = rho_0{x_1|-> *)
(* v_1} to 0pt... \nmleval[_n-1] e ==>v_n \andalsorho_n *)
(* = rho_n-1{x_n|->v_n} to 0pt\nmleval[_n] e ==>\ *)
(* nmlevalrv \nmleval[_0]\xletstar              *)
(* (<x_1,e_1,...,x_n,e_n>, e) ==>\nmlevalrv     *)
(*                                              *)
(* \xletrec is the tricky one. The expressions are *)
(* evaluated in an environment rho' in which their names *)
(* are already bound to the resulting values. In other *)
(* words, to evaluate each e_i, we have to have rho', *)
(* but to build rho', we have to know all the v_i's. *)
(* It seems like it should be impossible to make *)
(* progress, but because the expressions are all *)
(* [[lambda]] abstractions, we can pull it off. \ops *)
(* Letrec \threeline rho' = rho{x_1|->v_1, ..., x_n|-> *)
(* v_n} \nmleval[']e_1 ==>v_1 \qquad...\qquad \nmleval *)
(* [']e_n ==>v_n \nmleval[']e ==>v \nmleval\xletrec *)
(* (<x_1,e_1,...,x_n,e_n>, e) ==>\nmlevalrv Because each *)
(* e_i is a \xlambda, evaluating it is going to produce *)
(* a closure that captures rho' and the body of the \ *)
(* xlambda. And in [[eval]], that makes it possible to *)
(* build rho' without calling [[eval]] recursively (\ *)
(* chunkrefml.chunk.eval-letrec). The resulting rho' *)
(* satisfies the equations in the premises, and the *)
(* implementation closes the loop by stuffing rho' into *)
(* the mutable cell contained in each closure.  *)
(*                                              *)
(* Rules for evaluating definitions             *)
(*                                              *)
(* In micro-Scheme or Typed uScheme, evaluating a *)
(* definition produces a new environment and a new *)
(* store. In \nml, because there is no mutation, *)
(* evaluating a definition produces only a new  *)
(* environment. (It may also print.) The judgment has *)
(* the form \nomathbreak\mltopld -->rho'.       *)
(*                                              *)
(* \Nml's definitions differ from micro-Scheme's in *)
(* several significant ways:                    *)
(*                                              *)
(*   • In \nml, as in full ML and in Typed uScheme, a \ *)
(*  xval definition never mutates a previous binding; *)
(*  it always adds a new binding. (See \exrefpage *)
(*  scheme.ex.val-new-binding of Chapter [->].) *)
(*  If the old binding was used to create a function *)
(*  or other value, that function still refers to the *)
(*  old binding, not the new one. In an interactive *)
(*  interpreter, this behavior can be baffling, *)
(*  particularly if you load a new definition of an *)
(*  old function but you don't also load the    *)
(*  definitions of the functions that depend on it. \ *)
(*  ops Val \nmlevale ==>\nmlevalrv \mltopl\xval(x, *)
(*  e) -->rho{x |->v}                           *)
(*                                              *)
(*   • Like Typed uScheme but unlike micro-Scheme, \nml *)
(*  has \xvalrec. The semantics requires a rho' that *)
(*  binds f to a closure containing rho'. \ops ValRec *)
(*  [*] rho' = rho{f |->\xclo\xlambda(<x_1, ..., *)
(*  x_n>, e)rho'} \mltopl\xvalrec(f, \xlambda(<x_1, *)
(*  ..., x_n>, e)) -->rho' This self-reference is *)
(*  implemented using the same mutable-cell trick *)
(*  used to implement \xletrec.                 *)
(*                                              *)
(*   • In \nml, as in Typed uScheme, \xdefine(f, a, e) *)
(*  is syntactic sugar for \xvalrec(f, \xlambda(a, *)
(*  e)). \ops Define [*] \mltopl\xvalrec(f, \xlambda *)
(*  (<x_1, ..., x_n>, e)) -->rho' \mltopl\define(f, *)
(*  <x_1, ..., x_n>, e) -->rho'                 *)
(*                                              *)
(* As in micro-Scheme, a top-level expression e is *)
(* syntactic sugar for a binding to [[it]]. \ops Exp \ *)
(* mltopl\xval(it, e) -->rho' \mltopl\xexp(e) -->rho' *)
(*                                              *)
(* Type system \chapheadsplitfor \nml           *)
(*                                              *)
(* [*] Like other type systems, the type system of \nml *)
(* determines which terms have types, which in turn *)
(* determines what definitions are accepted by the *)
(* interpreter. As before, the types of terms are *)
(* specified by a formal proof system. The system uses *)
(* the same elements as the type system of Typed *)
(* uScheme*.                                    *)
(*                                              *)
(* \qtrim0.5                                    *)
(*                                              *)
(* Types, type schemes, and type environments   *)
(*                                              *)
(* As in Typed uScheme, types are built using four *)
(* elements:                                    *)
(*                                              *)
(*  \tightlist                                  *)
(*   • Type variables, which are written using alpha *)
(*   • Type constructors, which are written generically *)
(*  using \tycon or specifically using a name such as *)
(*  int or list                                 *)
(*   • Constructor application, which is written using *)
(*  ML notation (\ldotsntau) tau                *)
(*   • Quantification, which is written using \/ *)
(*                                              *)
(* In \nml, unlike in Typed uScheme, quantified types *)
(* are restricted: a type quantified with \/ may appear *)
(* only at top level, never as an argument to a type *)
(* constructor. In \nml, this restriction is built in to *)
(* the syntax of types:                         *)
(*                                              *)
(*   • A type built with type variables, type *)
(*  constructors, and constructor application is *)
(*  written using the metavariable tau. {production} *)
(*  t@tau \alternate*alpha | \tycon | (tau_1, ...,  *)
(*  tau_n) tau {production} A tau is called a type. *)
(*   • A quantified type is written using the *)
(*  metavariable sigma.\notation[sigma]sigmaa type *)
(*  scheme {production}s@sigma \/\ldotsnalpha\alldot *)
(*  tau {production} A sigma is called a type scheme. *)
(*                                              *)
(* In the code, a tau is represented by a [[ty]] and a *)
(* sigma by a [[type_scheme]]: [\Nml's representation *)
(* has only four of the five forms found in \tuscheme. *)
(* The fifth form, [[FUNTY]], is represented in \nml\ as *)
(* a nested application of type constructors    *)
(* [[function]] and [[arguments]] (chunk~\subpageref *)
(* {ml.chunk.funtype}). Coding function types in this *)
(* way simplifies type inference. ] [*] \nmllabel *)
(* tyvar,ty,type_scheme                         *)
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
(* <boxed values 74>=                           *)
val _ = op freetyvars : ty -> name set
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* With [[eqTycon]] defined, type equivalence is *)
(* determined by function [[eqType]], which is carried *)
(* over unchanged from \chunkrefml.chunk.eqType in \cref *)
(* ml.chap.                                     *)
(*                                              *)
(* Generativity is implemented by calling [[freshTycon]] *)
(* whenever a generative type definition is typed. *)
(* Definitions of primitive types are also considered *)
(* generative, and their type constructors are also *)
(* created using [[freshTycon]].                *)
(* <type constructors built into \uml\ and \uhaskell>= *)
val inttycon  = freshTycon "int"
val symtycon  = freshTycon "sym"
(* The second two are used to make function types, which *)
(* we can construct and deconstruct.            *)
(* <type constructors built into \uml\ and \uhaskell>= *)
val funtycon  = freshTycon "function"
val argstycon = freshTycon "arguments"
(* Primitive functions, predefined functions, and the *)
(* initial basis                                *)
(*                                              *)
(* Primitive type \chaptocsplitconstructors in uML *)
(*                                              *)
(* In uML, Booleans, lists, pairs, and other algebraic *)
(* data types are predefined using [[data]] definitions. *)
(* Only four type constructors are defined primitively: *)
(*                                              *)
(*   • Integers and symbols, which give types to literal *)
(*  integers and symbols                        *)
(*   • Function and argument type constructors, which *)
(*  give types to functions                     *)
(*                                              *)
(* The first two type constructors are used to make the *)
(* [[int]] and [[sym]] types.                   *)
(* <types built into \uml\ and \uhaskell>=      *)
val inttype = TYCON inttycon
val symtype = TYCON symtycon
(* <code to construct and deconstruct function types for \uml>= *)
fun funtype (args, result) = 
  CONAPP (TYCON funtycon, [CONAPP (TYCON argstycon, args), result])

fun asFuntype (CONAPP (TYCON mu, [CONAPP (_, args), result])) =
      if eqTycon (mu, funtycon) then
        SOME (args, result)
      else
        NONE
  | asFuntype _ = NONE
(* Functions [[asFuntype]] is the inverse of    *)
(* [[funtype]], satisfying this algebraic law:  *)
(*                                              *)
(*  asFuntype ofuntype = SOME\text.             *)
(*                                              *)
(* <boxed values 155>=                          *)
val _ = op funtype   : ty list * ty -> ty
val _ = op asFuntype : ty -> (ty list * ty) option
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
(* <boxed values 67>=                           *)
type subst = subst
val _ = op dom : subst -> name set
(* To interpret a substitution as a function from type *)
(* variables to types, we apply [[varsubst]] to it: \ *)
(* nmlflabelvarsubst                            *)
(* <boxed values 67>=                           *)
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
(* As the code shows, the function defined by a *)
(* substitution is total. If type variable [[a]] is not *)
(* in the domain of [[theta]], then \monoboxvarsubst *)
(* theta leaves [[a]] unchanged.                *)
(*                                              *)
(* A substitution is most often interpreted as a *)
(* function from types to types. That interpretation is *)
(* provided by function [[tysubst]]. It is almost the *)
(* same as the [[tysubst]] function in the interpreter *)
(* for Typed uScheme (\cpagereftuscheme.code.tysubst), *)
(* but because it has no quantified types to deal with, *)
(* it is simpler. \nmlflabeltysubst [*]         *)
(* <boxed values 68>=                           *)
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
(* <boxed values 69>=                           *)
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
(* <boxed values 70>=                           *)
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
(* <boxed values 71>=                           *)
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
(* <boxed values 72>=                           *)
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
(* <boxed values 72>=                           *)
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
(* Canonical type schemes                       *)
(*                                              *)
(* Type variables like [['t136]] are not suitable for *)
(* use in error messages. A type scheme like \monobox *)
(* (forall ['t136] ((list 't136) -> int)) is unpleasant *)
(* to look at, and it is equivalent to the more readable *)
(* \monobox(forall ['a] ((list 'a) -> int)) When a type *)
(* variable is \/-bound, its name is irrelevant, so *)
(* function [[canonicalize]] renames bound type *)
(* variables using names [['a]], [['b]], and so on. *)
(* <boxed values 75>=                           *)
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
(* <boxed values 76>=                           *)
val _ = op freshtyvar : 'a -> ty
end
(* <shared utility functions on Hindley-Milner types>= *)
fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))
(* Generalization and instantiation             *)
(*                                              *)
(* Calling [[generalize]](tau, \tyvarset) generalizes *)
(* type tau to a type scheme by closing over type *)
(* variables not in \tyvarset. It also puts the type *)
(* scheme into canonical form.                  *)
(* <boxed values 77>=                           *)
val _ = op generalize : ty * name set -> type_scheme
(* <shared utility functions on Hindley-Milner types>= *)
fun freshInstance (FORALL (bound, tau)) =
  instantiate (FORALL (bound, tau), map freshtyvar bound)
(* The dual function, [[instantiate]], is defined in *)
(* chunk [->]. It requires a list of types with which to *)
(* instantiate. That list is often a list of fresh type *)
(* variables, as provided by function [[freshInstance]]. *)
(* <boxed values 78>=                           *)
val _ = op freshInstance : type_scheme -> ty
(* <shared utility functions on Hindley-Milner types>= *)
datatype scheme_shape
  = MONO_FUN of              ty list * ty  (* (tau1 ... tauN -> tau) *)
  | MONO_VAL of              ty            (* tau *)
  | POLY_FUN of tyvar list * ty list * ty
                                         (* (forall (a ...) (tau ... -> tau)) *)
  | POLY_VAL of tyvar list * ty            (* (forall (a ...) tau) *)
(* <shared utility functions on Hindley-Milner types>= *)
fun schemeShape (FORALL (alphas, tau)) =
  case asFuntype tau
    of NONE => if null alphas then MONO_VAL tau
               else                POLY_VAL (alphas, tau)
     | SOME (args, result) =>
               if null alphas then MONO_FUN (args, result)
               else                POLY_FUN (alphas, args, result)
(* <boxed values 163>=                          *)
type scheme_shape = scheme_shape
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* A shape is identified by first looking for a function *)
(* arrow, then checking to see if the list of alpha's is *)
(* empty.                                       *)
(* <boxed values 163>=                          *)
val _ = op schemeShape : type_scheme -> scheme_shape
(* <shared utility functions on Hindley-Milner types>= *)
fun typeSchemeString (FORALL ([], tau)) =
      typeString tau
  | typeSchemeString (FORALL (a's, tau)) =
      "(forall [" ^ spaceSep a's ^ "] " ^ typeString tau ^ ")"
(* <boxed values 276>=                          *)
val _ = op typeString       : ty          -> string
val _ = op typeSchemeString : type_scheme -> string
(* <shared utility functions on Hindley-Milner types ((elided))>= *)
fun substString [] = "idsubst"
  | substString pairs =
      String.concatWith " o " 
      (map (fn (a, t) => a ^ " |--> " ^ typeString t) pairs)
(* <specialized environments for type schemes>= *)
type type_env = type_scheme env * name set
(* <specialized environments for type schemes>= *)
val emptyTypeEnv = 
      (emptyEnv, emptyset)
fun findtyscheme (x, (Gamma, free)) = find (x, Gamma)
(* An empty type environment binds no variables and has *)
(* an empty cache. Looking up a type scheme ignores the *)
(* cache. \nmlflabelfindtyscheme                *)
(* <boxed values 79>=                           *)
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
(* <specialized environments for type schemes>= *)
fun extendTypeEnv (Gamma, bindings) =
  let fun add ((x, sigma), Gamma) = bindtyscheme (x, sigma, Gamma)
  in  foldl add Gamma bindings
  end
(* Extension of type environments               *)
(*                                              *)
(* Because a [[type_env]] has a special representation, *)
(* it can't be extended with the [[<+>]] function from \ *)
(* crefmlscheme.chap. Instead, I define function *)
(* [[extendTypeEnv]], which takes a [[type_env]] on the *)
(* left but a \monoboxtype_scheme env on the right. *)
(* <boxed values 166>=                          *)
val _ = op extendTypeEnv : type_env * type_scheme env -> type_env
(* <extensions that support existential types>= *)
datatype x_type_scheme
  = FORALL_EXISTS of tyvar list * tyvar list * ty list * ty

fun asExistential (FORALL (alphas_and_betas, tau)) =
  let fun asTyvar (TYVAR a) = a
        | asTyvar _ = raise InternalError "GADT"
      fun typeParameters (CONAPP (mu, alphas)) = map asTyvar alphas
        | typeParameters _ = []
  in  case asFuntype tau
        of SOME (args, result) =>
             let val alphas = typeParameters result
                 val betas = diff (alphas_and_betas, alphas)
             in  SOME (FORALL_EXISTS (alphas, betas, args, result))
             end
         | NONE => NONE
  end
(* <boxed values 169>=                          *)
type x_type_scheme = x_type_scheme
val _ = op asExistential : type_scheme -> x_type_scheme option
(* <extensions that support existential types>= *)
fun freshSkolem _ =
  let val { identity = id, printName = T } = freshTycon "skolem type"
  in  TYCON { identity = id + 1
            , printName = "skolem type " ^ intString (id div 2)
            }
  end

fun isSkolem { identity = n, printName = _ } = (n mod 2 = 1)
(* <extensions that support existential types>= *)
fun addFreeSkolems (TYCON mu, mus) =
      if isSkolem mu then insert (mu, mus) else mus
  | addFreeSkolems (TYVAR _,  mus) =
      mus
  | addFreeSkolems (CONAPP (tau, taus), mus) =
      foldl addFreeSkolems (addFreeSkolems (tau, mus)) taus
(* <extensions that support existential types>= *)
fun typeFreeSkolems        tau    = addFreeSkolems (tau, emptyset)
fun typesFreeSkolems       taus   = foldl addFreeSkolems emptyset taus
fun typeSchemesFreeSkolems sigmas =
      typesFreeSkolems (map (fn FORALL (_, tau) => tau) sigmas)
(* Free skolem types are found by examining every type *)
(* constructor. To avoid allocating multiple sets of *)
(* type constructors, I've defined function     *)
(* [[addFreeSkolems]], which adds free skolem types to *)
(* an existing set. This function can be used with *)
(* [[foldl]] and an empty set.                  *)
(* <boxed values 170>=                          *)
val _ = op addFreeSkolems : ty * tycon set -> tycon set
(* <boxed values 170>=                          *)
val _ = op typeFreeSkolems  : ty     -> tycon set
val _ = op typesFreeSkolems : ty set -> tycon set
val _ = op typeSchemesFreeSkolems : type_scheme list -> tycon set
(* <extensions that support existential types>= *)
fun typeEnvSubst theta Gamma' =
  let fun subst (FORALL ([], tau)) = FORALL ([], tysubst theta tau)
        | subst _ = raise InternalError "polytype in pattern"
  in  map (fn (x, sigma) => (x, subst sigma)) Gamma'
  end



(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES FOR \UML                         *)
(*                                                               *)
(*****************************************************************)

(* <abstract syntax and values for \uml>=       *)
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
(* <kinds for typed languages>=                 *)
fun kindString TYPE = "*"
  | kindString (ARROW (ks, k)) =
      "(" ^ spaceSep (map kindString ks @ ["=>", kindString k]) ^ ")"
(* Different interpreters need different utility *)
(* functions, but they all need an implementation of *)
(* equality that can be used in [[check-expect]]. And *)
(* the micro-Scheme interpreter also needs an   *)
(* implementation of primitive equality. Primitive *)
(* equality permits only atoms to be considered equal. *)
(* <definition of [[tyex]] for \uml>=           *)
datatype tyex 
  = TYNAME  of name                (* names type or type constructor *)
  | CONAPPX of tyex * tyex list    (* type-level application *)
  | FUNTYX  of tyex list * tyex
  | FORALLX of name list * tyex
  | TYVARX  of name                (* type variable *)
(* Case expressions include patterns. \umllabelpat\ *)
(* umllabeltyex                                 *)
(* <definition of [[pat]], for patterns>=       *)
type vcon = name   (* a value constructor *)  (*OMIT*)
datatype pat = WILDCARD
             | PVAR     of name
             | CONPAT   of vcon * pat list
(* <definitions of [[exp]] and [[value]] for \uml>= *)
type vcon = name   (* a value constructor *)
datatype exp 
  = VCONX of vcon
  | CASE  of exp * (pat * exp) list
  | (* The extended-definition forms [[USE]] and [[TEST]] *)
    (* are implemented in exactly the same way for every *)
    (* language: internal function [[try]] passes each *)
    (* [[USE]] to [[useFile]], and it adds each [[TEST]] to *)
    (* the mutable list [[unitTests]]—just as in the C code *)
    (* in \crefpage(impcore.readevalprint. Function [[try]] *)
    (* passes each true definition [[DEF]] to function *)
    (* [[processDef]], which does the language-dependent *)
    (* work.                                        *)
    (* <forms of [[exp]] carried over from \nml>=   *)
        LITERAL    of value
      | VAR        of name
      | IFX        of exp * exp * exp (* could be syntactic sugar for CASE *)
      | BEGIN      of exp list
      | APPLY      of exp * exp list
      | LETX       of let_flavor * (name * exp) list * exp
      | LAMBDA     of name list * exp
    and let_flavor = LET | LETREC | LETSTAR
(* \umllabelvalue,vcon                          *)
(* <definitions of [[exp]] and [[value]] for \uml>= *)
and value
  = CONVAL of vcon * value list
  | SYM    of name
  | NUM    of int
  | CLOSURE   of lambda * value env ref
  | PRIMITIVE of primop
 withtype lambda = name list * exp
      and primop = value list -> value
(* <definition of [[def]] for \uml>=            *)
datatype def = DATA of data_def
             | (* <forms of [[def]] carried over from \nml>=   *)
                 VAL    of name * exp
               | VALREC of name * exp
               | EXP    of exp
               | DEFINE of name * (name list * exp)
  withtype data_def = name * kind * (vcon * tyex) list
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \umllabeldef                                 *)
(* <boxed values 143>=                          *)
type data_def = data_def
(* Syntactic sugar for implicit-data            *)
(*                                              *)
(* An implicit data definition gives type parameters, *)
(* the name of the type constructor, and definitions for *)
(* one or more value constructors.              *)
(* <definition of [[implicit_data_def]] for \uml>= *)
datatype implicit_data_def 
  = IMPLICIT_DATA of tyvar list * name * implicit_vcon list
and implicit_vcon 
  = IMPLICIT_VCON of vcon * tyex list
(* Unit tests are like \nml's unit tests, except that *)
(* the type in a [[check-type]] or a            *)
(* [[check-principal-type]] is syntax that has to be *)
(* translated into a [[type_scheme]]. \umllabelunit_test *)
(* <definition of [[unit_test]] for languages with Hindley-Milner types and generated type constructors>= *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_PTYPE       of exp * tyex
                   | CHECK_TYPE_ERROR  of def
(* <definition of [[xdef]] (shared)>=           *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* <definition of [[valueString]] for \uml>=    *)

fun valueString (CONVAL ("cons", [v, vs])) = consString (v, vs)
  | valueString (CONVAL ("'()",  []))      = "()"
  | valueString (CONVAL (c, []))  = c
  | valueString (CONVAL (c, vs))  =
      "(" ^ c ^ " " ^ spaceSep (map valueString vs) ^ ")"
  | valueString (NUM n      )   =
      String.map (fn #"~" => #"-" | c => c) (Int.toString n)
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<function>"
  | valueString (PRIMITIVE _)   = "<function>"
(* Applications of [[cons]] get rendered using Scheme *)
(* syntax for lists.                            *)
(* <definition of [[valueString]] for \uml>=    *)
and consString (v, vs) =
      let fun tail (CONVAL ("cons", [v, vs])) = " " ^ valueString v ^ tail vs
            | tail (CONVAL ("'()", []))       = ")"
            | tail _ =
                raise BugInTypeInference
                  "bad list constructor (or cons/'() redefined)"
      in  "(" ^ valueString v ^ tail vs
	  end
(* The substitution into Gamma' is just good enough for *)
(* use with patterns, where every type scheme in Gamma' *)
(* is a monotype.                               *)
(* <boxed values 171>=                          *)
val _ = op typeEnvSubst : subst -> type_scheme env -> type_scheme env
(* \qbreak                                      *)
(*                                              *)
(* String conversion                            *)
(*                                              *)
(* To print a list of values, function [[valueString]] *)
(* looks only at the name of each value constructor. *)
(* (When [[valueString]] is called, the type of the *)
(* value constructor is no longer available.) This *)
(* heuristic works so long as [[cons]] has its expected *)
(* meaning. If a uML program redefines the [[cons]] *)
(* value constructor, chaos may ensue.          *)
(* <boxed values 171>=                          *)
val _ = op valueString : value -> string
(* CLOSING IN ON CHECK-PRINT:                   *)

(* <definition of [[patString]] for \uml\ and \uhaskell ((uml))>= *)
fun patString WILDCARD    = "_"
  | patString (PVAR x)    = x
  | patString (CONPAT (vcon, []))   = vcon
  | patString (CONPAT (vcon, pats)) =
      "(" ^ spaceSep (vcon :: map patString pats) ^ ")"
(* \qbreak                                      *)
(* <definition of [[expString]] for \nml\ and \uml>= *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      fun sqbracket s = "[" ^ s ^ "]"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = sqbracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL v => valueString v
         | VAR name => name
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ["lambda",
                                              bracketSpace xs, expString body]
         (* <extra cases of [[expString]] for \uml>=     *)
         | VCONX vcon => vcon
         | CASE (e, matches) =>
             let fun matchString (pat, e) =
                   sqbracket (spaceSep [patString pat, expString e])
             in  bracketSpace ("case" :: expString e :: map matchString matches)
             end
         (* <extra cases of [[expString]] for \uml>=     *)
         (* this space is filled in by the uML appendix *)
  end
(* <definitions of [[defString]] and [[defName]] for \nml\ and \uml>= *)
fun defString d =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun formal (x, t) = "[" ^ x ^ " : " ^ typeString t ^ "]"
  in  case d
        of EXP e         => expString e
         | VAL    (x, e) => bracketSpace ["val",     x, expString e]
         | VALREC (x, e) => bracketSpace ["val-rec", x, expString e]
         | DEFINE (f, (formals, body)) =>
             bracketSpace ["define", f, bracketSpace formals, expString body]

        (* <cases for [[defString]] for forms found only in \uml ((elided))>= *)
         | DATA (t, kind, _) => bracketSpace ["data", kindString kind, t, "..."]

        (* <cases for [[defString]] for forms found only in \uml ((elided))>= *)
         (*empty*)
  end
fun defName (VAL (x, _))       = x
  | defName (VALREC (x, _)) = x
  | defName (DEFINE (x, _)) = x
  | defName (EXP _) = raise InternalError "asked for name defined by expression"
  (* <clauses for [[defName]] for forms found only in \uml ((elided))>= *)
  | defName (DATA (t, _, _)) = t
  (* <clauses for [[defName]] for forms found only in \uml ((elided))>= *)
  (*empty*)
(* <definition of [[tyexString]] for \uml>=     *)
fun tyexString (TYNAME t) = t
  | tyexString (CONAPPX (tx, txs)) =
      "(" ^ tyexString tx ^ " " ^ spaceSep (map tyexString txs) ^ ")"
  | tyexString (FORALLX (alphas, tx)) =
      "(forall (" ^ spaceSep alphas ^ ") " ^ tyexString tx ^ ")"
  | tyexString (TYVARX a) = a
  | tyexString (FUNTYX (args, result)) =
      "(" ^ spaceSep (map tyexString args) ^ " -> " ^ tyexString result ^ ")"


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \UML\ SYNTAX                           *)
(*                                                               *)
(*****************************************************************)

(* <utility functions on \uml\ syntax>=         *)
fun isPolymorphicFuntyex (FORALLX (_, tau)) = isPolymorphicFuntyex tau
  | isPolymorphicFuntyex (FUNTYX _)         = true
  | isPolymorphicFuntyex _                  = false


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \UML\ VALUES                           *)
(*                                                               *)
(*****************************************************************)

(* <utility functions on \uml\ values ((uml))>= *)
fun primitiveEquality (v, v') =
  let fun noFun () = raise RuntimeError "compared functions for equality"
  in  case (v, v')
        of (NUM  n1,  NUM  n2)  => (n1 = n2)
         | (SYM  v1,  SYM  v2)  => (v1 = v2)
         | (CONVAL (vcon, vs), CONVAL (vcon', vs')) =>
             vcon = vcon' andalso ListPair.allEq primitiveEquality (vs, vs')
         | (CLOSURE   _, _) => noFun ()
         | (PRIMITIVE _, _) => noFun ()
         | (_, CLOSURE   _) => noFun ()
         | (_, PRIMITIVE _) => noFun ()
         | _ => raise BugInTypeInference
                        ("compared incompatible values " ^ valueString v ^
                         " and " ^ valueString v' ^ " for equality")
  end
val testEquals = primitiveEquality
(* <utility functions on \uml\ values ((uml))>= *)
fun embedList []      = CONVAL ("'()", [])
  | embedList (v::vs) = CONVAL ("cons", [v, embedList vs])
(* The parser for literal S-expressions uses    *)
(* [[embedList]] to convert a list of S-expressions into *)
(* an S-expression. The \nml version (\chunkref *)
(* mlscheme.chunk.embedList) uses Standard ML value *)
(* constructors [[PAIR]] and [[NIL]], but the uML *)
(* version uses uML value constructors [[cons]] and [[' *)
(* ()]].                                        *)
(* <boxed values 157>=                          *)
val _ = op embedList : value list -> value
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <utility functions on \uml\ values ((uml))>= *)
fun embedBool b =
      CONVAL (if b then "#t" else "#f", [])
fun projectBool (CONVAL ("#t", [])) = true
  | projectBool _                   = false
(* <boxed values 158>=                          *)
val _ = op projectBool : value -> bool
val _ = op embedBool   : bool  -> value



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UML, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* Parsing                                      *)
(*                                              *)
(* [*] Where possible, uML's parsers reuse code from *)
(* other interpreters.                          *)
(* <lexical analysis and parsing for \uml, providing [[filexdefs]] and [[stringsxdefs]]>= *)
(* <lexical analysis for \uscheme\ and related languages>= *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* <boxed values 23>=                           *)
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
(* <boxed values 24>=                           *)
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
  (* <boxed values 26>=                           *)
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
(* <boxed values 25>=                           *)
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
(* <boxed values 27>=                           *)
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

(* Identifying uML tokens                       *)
(*                                              *)
(* From the implementation of micro-Scheme in \cref *)
(* mlschemea.chap, uML inherits the token parsers *)
(* [[name]], [[booltok]], [[quote]], and [[int]]. But *)
(* unlike micro-Scheme, uML has at least three different *)
(* species of names: value constructors, value  *)
(* variables, and type variables. To avoid mistakes, *)
(* I don't use the [[name]] parser anywhere. A name will *)
(* always be parsed with a parser that knows what *)
(* species of name it's looking for.            *)
(*                                              *)
(* The [[name]] parser is inherited from the    *)
(* implementation of micro-Scheme, so I disable it by *)
(* rebinding [[name]] to a useless value.       *)
(* <parsers for \uml\ tokens>=                  *)
val name = () (* don't use me as a parser; too confusing *)
(* The name of a type variable begins with a quote mark. *)
(* <parsers for \uml\ tokens>=                  *)
val tyvar =
  quote *> (  curry op ^ "'" <$> namelike
          <?> "type variable (got quote mark)"
           )
(* <parsers for \uml\ value constructors and value variables>= *)
fun isVcon x =
  let val lastPart = List.last (String.fields (curry op = #".") x)
      val firstAfterdot = String.sub (lastPart, 0) handle Subscript => #" "
  in  x = "cons" orelse x = "'()" orelse
      Char.isUpper firstAfterdot orelse firstAfterdot = #"#" orelse
      String.isPrefix "make-" x
  end
fun isVvar x = x <> "->" andalso not (isVcon x)
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
(* <boxed values 28>=                           *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
(* <boxed values 28>=                           *)
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
(* <boxed values 29>=                           *)
val _ = op asLambda : string -> exp parser -> exp parser
(* <parsers and parser builders for formal parameters and bindings>= *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* <boxed values 30>=                           *)
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
(* <boxed values 32>=                           *)
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
(* <boxed values 31>=                           *)
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
(* <boxed values 34>=                           *)
val _ = op fullSchemeExpOf : exp parser -> (exp parser -> exp parser) -> exp
                                                                          parser
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
(* <boxed values 58>=                           *)
val _ = op tyvar : name parser
(* <boxed values 58>=                           *)
val _ = op distinctTyvars : name list parser
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

(* <boxed values 58>=                           *)
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
(* <boxed values 62>=                           *)
val _ = op distinctTBsIn : ((name * 't) * 'e) list parser -> string -> ((name *
                                                           't) * 'e) list parser
(* <parser builders for typed languages>=       *)
fun typedFormalOf name colon ty =
      bracket ("[x : ty]", pair <$> name <* colon <*> ty)
fun typedFormalsOf name colon ty context = 
  let val formal = typedFormalOf name colon ty
  in  distinctBsIn (bracket("(... [x : ty] ...)", many formal)) context
  end                            
(* <boxed values 264>=                          *)
val _ = op typedFormalsOf : string parser -> 'b parser -> 'a parser  -> string
                                                    -> (string * 'a) list parser
(* <parsers for HM types with generated type constructors>= *)
fun tyex tokens = (
     TYNAME <$> tyname
 <|> TYVARX <$> tyvar
 <|> usageParsers
        [("(forall (tyvars) type)",
          curry FORALLX <$> bracket ("('a ...)", distinctTyvars) <*> tyex)]
 <|> bracket("(ty ty ... -> ty)",
        arrowsOf CONAPPX FUNTYX <$> many tyex <*>! many (arrow *> many tyex))
) tokens
(* \qtrim1.0                                    *)
(*                                              *)
(* Parsing types and kinds                      *)
(*                                              *)
(* Parsers for types and kinds are as in Typed uScheme, *)
(* except the type parser produces a [[tyex]], not a  *)
(* [[ty]].                                      *)
(* <boxed values 172>=                          *)
val _ = op tyvar : string parser
val _ = op tyex  : tyex   parser
(* <parsers for HM types with generated type constructors>= *)
fun kind tokens = (
      TYPE <$ eqx "*" vvar
  <|> bracket ("arrow kind",
               curry ARROW <$> many kind <* eqx "=>" vvar <*> kind)
) tokens

val kind = kind <?> "kind"
(* <boxed values 173>=                          *)
val _ = op kind : kind parser
(* <parsers and [[xdef]] streams for \uml>=     *)
fun pattern tokens =  (
                WILDCARD    <$  eqx "_" vvar
      <|>       PVAR        <$> vvar
      <|> curry CONPAT      <$> vcon <*> pure []
      <|> bracket ( "(C x1 x2 ...) in pattern"
                  , curry CONPAT <$> vcon <*> many pattern
                  )
       ) tokens
(* \qbreak                                      *)
(*                                              *)
(* Parsing patterns                             *)
(*                                              *)
(* The distinction between value variable and value *)
(* constructor is most important in patterns.   *)
(* <boxed values 174>=                          *)
val _ = op pattern : pat parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val vvarFormalsIn = formalsOf "(x1 x2 ...)" vvar
val patFormals    = bracket ("(p1 p2 ...)", many pattern)
(* Parsing expressions                          *)
(*                                              *)
(* uML's parser is more elaborate then other parsers *)
(* because it for each binding construct found in \nml, *)
(* it supports two flavors: the standard flavor, which *)
(* binds variables, and the ``patterns everywhere'' *)
(* flavor, which binds patterns. (The case expression, *)
(* of course, binds only patterns.) To begin, the formal *)
(* parameters to a function may be variables or *)
(* patterns. A [[vvarFormalsIn]] parser takes a string *)
(* giving the context, because the parser may detect *)
(* duplicate names. The [[patFormals]] parser doesn't *)
(* take the context, because when patterns are used, *)
(* duplicate names are detected during type checking. *)
(* <boxed values 175>=                          *)
val _ = op vvarFormalsIn : string -> name list parser
val _ = op patFormals    :            pat list parser
(* <parsers and [[xdef]] streams for \uml>=     *)
(* <utility functions that help implement \uml's syntactic sugar ((prototype))>= *)
fun freeIn exp y =
  let fun has_y (CASE (e, choices)) = 
            has_y e orelse (List.exists choice_has_y) choices
        | has_y _ = 
            raise LeftAsExercise "free variable of an expression"
      and choice_has_y (p, e) = not (pat_has_y p) andalso has_y e
      and pat_has_y (PVAR x) = x = y
        | pat_has_y (CONPAT (_, ps)) = List.exists pat_has_y ps
        | pat_has_y WILDCARD = false
  in  has_y exp
  end
(* <utility functions that help implement \uml's syntactic sugar>= *)
val varsupply = 
  streamMap (fn n => "x" ^ intString n) naturals
fun freshVar e =
  case streamGet (streamFilter (not o freeIn e) varsupply)
    of SOME (x, _) => x
     | NONE => raise InternalError "unable to create a fresh variable"
(* To desugar the new syntax, you will sometimes need to *)
(* find a variable that is not free in a given  *)
(* expression. If you have done \cref           *)
(* mlscheme.ex.closure-code in \crefmlscheme.chap (\ *)
(* cpagerefmlscheme.ex.closure-code), you're almost *)
(* there. Use that code to complete function [[freeIn]] *)
(* here.                                        *)
(* <boxed values 187>=                          *)
val _ = op freeIn : exp -> name -> bool
(* \qbreak Once [[freeIn]] is implemented, you can come *)
(* up with fresh variables by using the helper functions *)
(* below. Function [[freshVar]] returns a variable that *)
(* is not free in a given expression. The supply of *)
(* variables is infinite, so the exception should never *)
(* be raised.                                   *)
(* <boxed values 187>=                          *)
val _ = op varsupply : name stream
val _ = op freshVar  : exp -> name
(* <utility functions that help implement \uml's syntactic sugar>= *)
fun freshVars e xs =
  streamTake (length xs, streamFilter (not o freeIn e) varsupply)
(* Function [[freshVars]] returns as many fresh *)
(* variables as there are elements in [[xs]].   *)
(* <boxed values 188>=                          *)
val _ = op freshVars : exp -> 'a list -> name list
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <utility functions that help implement \uml's syntactic sugar>= *)
fun tupleVcon xs = case length xs
                     of 2 => "PAIR"
                      | 3 => "TRIPLE"
                      | n => "T" ^ intString n
(* <utility functions that help implement \uml's syntactic sugar>= *)
fun tupleexp [x] = VAR x
  | tupleexp xs  = APPLY (VCONX (tupleVcon xs), map VAR xs)

fun tuplepat [x] = x
  | tuplepat xs  = CONPAT (tupleVcon xs, xs)
(* In the concrete syntax of [[lambda]], [[lambda*]], *)
(* and [[define*]], a sequence of names stands for a *)
(* tuple, and similarly a sequence of patterns stands *)
(* for a tuple pattern. Sequences are turned into *)
(* expressions or patterns, respectively, by functions *)
(* [[tupleexp]] and [[tuplepat]]. Each tuple expression *)
(* or pattern has to use an appropriate value   *)
(* constructor as defined on \cpageref          *)
(* adta.predef-tuples. That value constructor is chosen *)
(* by calling [[tupleVcon]] with the list of names or *)
(* patterns involved.                           *)
(* <boxed values 189>=                          *)
val _ = op tupleexp  : name list -> exp
val _ = op tuplepat  : pat  list -> pat
val _ = op tupleVcon : 'a   list -> vcon
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <utility functions that help implement \uml's syntactic sugar>= *)
fun freePatVars (PVAR x)         = insert (x, emptyset)
  | freePatVars (WILDCARD)       = emptyset
  | freePatVars (CONPAT (_, ps)) = foldl union emptyset (map freePatVars ps)
(* The free variables in a pattern can be found by *)
(* function [[freePatVars]].                    *)
(* <boxed values 190>=                          *)
val _ = op freePatVars : pat -> name set
fun exptable exp =
  let (* <parsers used in both flavors>=              *)
      val choice =
        bracket ("[pattern exp]", pair <$> pattern <*> exp)
      val letrecBs =
        distinctBsIn (bindingsOf "(x e)" vvar (asLambda "letrec" exp)) "letrec"
      (* Choices use only patterns, and [[letrec]] uses only *)
      (* names.                                       *)
      (* <boxed values 176>=                          *)
      val _ = op choice   : (pat  * exp)      parser
      val _ = op letrecBs : (name * exp) list parser
      (* \qbreak When syntactic sugar for patterns is *)
      (* introduced, the parsers that might change are *)
      (* [[formals]], [[letBs]], and [[letstarBs]].   *)
      (* <parsers for bindings to names>=             *)
      val letBs     = distinctBsIn (bindingsOf "(x e)" vvar exp) "let"
      val letstarBs = bindingsOf "(x e)" vvar exp
      val formals   = vvarFormalsIn "lambda"
      (* The syntactic sugar for patterns is recognized by *)
      (* these parsers:                               *)
      (* <parsers for bindings to patterns>=          *)
      val patBs       = bindingsOf "(p e)" pattern exp
      val patLetrecBs = map (fn (x, e) => (PVAR x, e)) <$> letrecBs
      val patLetBs =
        let fun patVars (WILDCARD)       = []
              | patVars (PVAR x)         = [x]
              | patVars (CONPAT (_, ps)) = List.concat (map patVars ps)
            fun check (loc, bs) =
              let val xs = List.concat (map (patVars o fst) bs)
              in  nodups ("bound name", "let") (loc, xs) >>=+ (fn _ => bs)
              end
        in  check <$>! @@ patBs
        end
      val patFormals = patFormals (* defined above *)
      (* <expression builders that expect to bind names>= *)
      fun letx letkind bs e = LETX (letkind, bs, e)
      fun lambda xs e = LAMBDA (xs, e)
      fun lambdastar clauses = ERROR "lambda* is left as an exercise"
      (* When syntactic sugar for patterns is introduced, it *)
      (* will be necessary to define new versions of the *)
      (* expression-builders [[lambda]], [[lambdastar]], and *)
      (* [[letx]].                                    *)
      (* <boxed values 177>=                          *)
      val _ = op letx       : let_flavor -> (name * exp) list -> exp -> exp
      val _ = op lambda     : name list -> exp -> exp
      val _ = op lambdastar : (pat list * exp) list -> exp error
      (* \qbreak                                      *)
      (*                                              *)
      (* Support for syntactic-sugar exercises        *)
      (*                                              *)
      (* uML can support ``patterns everywhere'' through *)
      (* syntactic sugar, which is left for you to add *)
      (* (exercises, \crefadt.chap). You just have to fill in *)
      (* some code chunks with appropriate parsers:   *)
      (* <\uml\ expression builders that expect to bind patterns>= *)
      (* you can redefine letx, lambda, and lambdastar here *)
  in  (* <parsers for expressions that begin with keywords>= *)
      usageParsers
        [ ("(if e1 e2 e3)",            curry3 IFX    <$> exp  <*> exp <*> exp)
        , ("(begin e1 ...)",                  BEGIN  <$> many exp)
        , ("(lambda (names) body)",           lambda <$> formals <*> exp)
        , ("(lambda* (pats) exp ...)",
             lambdastar <$>!
             many1 (bracket
                      ( "[(pat ...) e]"
                      , pair <$> (bracket ("(pat ...)", many pattern)) <*> exp
                      )))
        , ("(let (bindings) body)",    letx   LET     <$> letBs     <*> exp)
        , ("(letrec (bindings) body)", letx   LETREC  <$> letrecBs  <*> exp)
        , ("(let* (bindings) body)",   letx   LETSTAR <$> letstarBs <*> exp)
        , ("(case exp [pattern exp] ...)", curry CASE <$> exp <*> many choice)

        , ("(while e1 e2)", exp  *> exp <!>
                                    "uML does not include 'while' expressions")
        , ("(set x e)",     vvar *> exp <!>
                                    "uML does not include 'set' expressions")
        (* In the exercises, you'll add more forms of [[exp]]. *)
        (* <rows added to \uml's [[exptable]] in exercises>= *)
        (* you add this bit *)
        ]
  end
(* <parsers and [[xdef]] streams for \uml>=     *)
val atomicExp =  VAR               <$> vvar
             <|> VCONX             <$> vcon
             <|> (LITERAL o NUM)   <$> int

fun exp tokens = (
     atomicExp
 <|> quote *> (LITERAL <$> sexp)
 <|> exptable exp
 <|> leftCurly <!> "curly brackets are not supported"
 <|> left *> right <!> "empty application"
 <|> bracket ("function application", curry APPLY <$> exp <*> many exp)
) tokens
(* \qbreak With the keyword expressions parsed by *)
(* [[exptable]], parsers for atomic expressions and full *)
(* expressions follow.                          *)
(* <boxed values 178>=                          *)
val _ = op atomicExp : exp parser
val _ = op exp       : exp parser
(* <parsers and [[xdef]] streams for \uml>=     *)
(* <definition of [[makeExplicit]], to translate [[implicit-data]] to [[data]]>= *)
fun makeExplicit (IMPLICIT_DATA ([], t, vcons)) =
      let val tx = TYNAME t
          fun convertVcon (IMPLICIT_VCON (K, []))  = (K, tx)
            | convertVcon (IMPLICIT_VCON (K, txs)) = (K, FUNTYX (txs, tx))
      in  (t, TYPE, map convertVcon vcons)
      end
  | makeExplicit (IMPLICIT_DATA (alphas, t, vcons)) =
      let val kind = ARROW (map (fn _ => TYPE) alphas, TYPE)
          val tx   = CONAPPX (TYNAME t, map TYVARX alphas)
          fun close tau = FORALLX (alphas, tau)
          fun vconType (vcon, [])  = tx
            | vconType (vcon, txs) = FUNTYX (txs, tx)
          fun convertVcon (IMPLICIT_VCON (K, []))  =
                                     (K, close tx)
            | convertVcon (IMPLICIT_VCON (K, txs)) =
                                     (K, close (FUNTYX (txs, tx)))
  in  (t, kind, map convertVcon vcons)
  end
(* An implicit data definition is translated into an *)
(* explicit data definition by function         *)
(* [[makeExplicit]]. In this translation, as long as all *)
(* the t's elaborate to sigma's, each sigma satisfies *)
(* the compatibility judgment \vconcompatsigmaµkappa. \ *)
(* umlflabelmakeExplicit                        *)
(* <boxed values 191>=                          *)
val _ = op makeExplicit : implicit_data_def -> data_def
val tyvarlist = bracket ("('a ...)", many1 tyvar)
val optionalTyvars = (fn alphas => getOpt (alphas, [])) <$> optional tyvarlist
val implicitData =
  let fun vc c taus = IMPLICIT_VCON (c, taus)
      val vconDef =  vc <$> vcon <*> pure []
                 <|> bracket ("(vcon of ty ...)",
                              vc <$> vcon <* eqx "of" vvar <*> many1 tyex)
  in  usageParsers
      [("(implicit-data [('a ...)] t vcon ... (vcon of ty ...) ...)"
       , (DATA o makeExplicit) <$>
         (curry3 IMPLICIT_DATA <$> optionalTyvars <*> tyname <*> many vconDef)
       )]
  end
(* \qvfilbreak1.5in                             *)
(*                                              *)
(* Parsing definitions                          *)
(*                                              *)
(* The parser for [[implicit-data]] is defined  *)
(* separately. The syntax is passed to function *)
(* [[makeExplicit]] (\cpagerefadt.fun.makeExplicit), *)
(* which desugars it into what [[DATA]] expects. *)
(* <boxed values 179>=                          *)
val _ = op makeExplicit : implicit_data_def -> data_def
val _ = op implicitData : def parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val def = 
  let (* <parser for binding to names>=               *)
      val formals = vvarFormalsIn "define"
      (* Definitions are parsed using a small collection of *)
      (* internal functions.                          *)
      (* <boxed values 180>=                          *)
      val _ = op formals : name list parser
      (* <parsers for clausal definitions, a.k.a. [[define*]]>= *)
      val lhs = 
        bracket ("(f p1 p2 ...)", pair <$> vvar <*> many pattern)
      val clause =
        bracket ("[(f p1 p2 ...) e]",
                 (fn (f, ps) => fn e => (f, (ps, e))) <$> lhs <*> exp)
      (* <boxed values 181>=                          *)
      val _ = op lhs    : (name * pat list) parser
      val _ = op clause : (name * (pat list * exp)) parser
      (* <definition builders that expect to bind names>= *)
      fun define f xs body = DEFINE (f, (xs, body))
      fun definestar _ = ERROR "define* is left as an exercise"
      (* <definition builders used in all parsers>=   *)
      val Kty = typedFormalOf vcon (kw ":") tyex
      fun data kind name vcons = DATA (name, kind, vcons)
      (* <boxed values 182>=                          *)
      val _ = op Kty  : (vcon * tyex) parser
      val _ = op data : kind -> name -> (vcon * tyex) list -> def

   (* <\uml\ definition builders that expect to bind patterns ((prototype))>= *)
      (* you can redefine 'define' and 'definestar' here *)
  in  usageParsers
      [ ("(define f (args) body)",    define       <$> vvar <*> formals <*> exp)
      , ("(define* (f pats) e ...)",  definestar   <$>! many1 clause)
      , ("(val x e)",                 curry VAL    <$> vvar <*> exp)
      , ("(val-rec x e)",             curry VALREC <$> vvar <*> asLambda
                                                                  "val-rec" exp)
      , ("(data kind t [vcon : type] ...)", 
                                      data <$> kind <*> tyname <*> many Kty)
      ]
  end
(* Function [[atLoc]] is often called by the    *)
(* higher-order function [[located]], which converts a *)
(* function that expects [['a]] into a function that *)
(* expects \monobox'a located. Function [[leftLocated]] *)
(* does something similar for a pair in which only the *)
(* left half must include a source-code location. *)
(* <boxed values 183>=                          *)
val _ = op def : def parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)",          curry CHECK_EXPECT     <$> exp <*> exp)
  , ("(check-assert e)",                    CHECK_ASSERT     <$> exp)
  , ("(check-error e)",                     CHECK_ERROR      <$> exp)
  , ("(check-type e tau)",            curry CHECK_TYPE       <$> exp <*> tyex)
  , ("(check-principal-type e tau)",  curry CHECK_PTYPE      <$> exp <*> tyex)
  , ("(check-type-error e)",                CHECK_TYPE_ERROR <$> (def <|>
                                                                    implicitData
                                                                 <|> EXP <$> exp
                                                                              ))
  ]
(* \qbreak Unit tests are parsed by [[testtable]]. *)
(* <boxed values 184>=                          *)
val _ = op testtable : unit_test parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> namelike)
  (* <rows added to \uml's [[xdeftable]] in exercises>= *)
  (* you add this bit *)
  (* <rows added to \uml's [[xdeftable]] in exercises ((prototype))>= *)
  (* you can add a row for 'val' here *)
  ]
(* <boxed values 185>=                          *)
val _ = op xdeftable : xdef parser
(* <parsers and [[xdef]] streams for \uml>=     *)
val xdef  =  TEST <$> testtable
         <|>          xdeftable
         <|> DEF  <$> (def <|> implicitData)
         <|> badRight "unexpected right bracket"
         <|> DEF <$> EXP <$> exp
         <?> "definition"
val xdefstream = interactiveParsedStream (schemeToken, xdef)
(* And as usual, all the definitions together are parsed *)
(* by [[xdef]].                                 *)
(* <boxed values 186>=                          *)
val _ = op xdef : xdef parser
(* <parsers and [[xdef]] streams for \uml>=     *)
local
  fun sxb b = CONVAL ("Sx.B", [embedBool b])
  fun sxs s = CONVAL ("Sx.S", [SYM s])
  fun sxn n = CONVAL ("Sx.N", [NUM n])
  fun sxlist sxs = CONVAL("Sx.L", [embedList sxs])

  fun sexp tokens = (
         sxb <$> booltok
     <|> sxs <$> (notDot <$>! @@ namelike)
     <|> sxn <$> int
     <|> leftCurly <!> "curly brackets may not be used in S-expressions"
     <|> (fn v => sxlist [sxs "quote", v]) <$> (quote *> sexp)
     <|> sxlist <$> bracket ("list of S-expressions", many sexp)
    ) tokens
  val sexp = sexp <?> "S-expression"
in
  val sxstream = interactiveParsedStream (schemeToken, sexp)
end
(* These S-expressions are read by a little parser. *)
(* <boxed values 192>=                          *)
val _ = op sxstream : string * line stream * prompts -> value stream
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)

(* <shared definitions of [[filexdefs]] and [[stringsxdefs]]>= *)
fun filexdefs (filename, fd, prompts) =
      xdefstream (filename, filelines fd, prompts)
fun stringsxdefs (name, strings) =
      xdefstream (name, streamOfList strings, noPrompts)
(* <boxed values 125>=                          *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   DEFINITION OF [[BASIS]] FOR \UML                            *)
(*                                                               *)
(*****************************************************************)

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
(* <definition of [[basis]] for \uml>=          *)
type basis = type_env * (ty * kind) env * value env


(*****************************************************************)
(*                                                               *)
(*   TRANSLATION OF {\UML} TYPE SYNTAX INTO TYPES                *)
(*                                                               *)
(*****************************************************************)

(* <translation of {\uml} type syntax into types ((elided))>= *)
local
  val known_vcons = ref [] : (tycon * (vcon * type_scheme) list) list ref
in
  fun vconsOf mu =
    case List.find (fn (mu', _) => eqTycon (mu, mu')) (!known_vcons)
      of SOME (_, vcons) => vcons
       | NONE => raise BugInTypeInference "missing vcons"

  fun addVcons (mu, vcons) =
    known_vcons := (mu, vcons) :: !known_vcons
end
(* <translation of {\uml} type syntax into types>= *)
fun txType (TYNAME t, Delta) =
      (find (t, Delta)
       handle NotFound _ => raise TypeError ("unknown type name " ^ t))
(* Function [[revapp]] is written exactly as we wrote it *)
(* in micro-Scheme.                             *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \csynlabelnml \productionwidthlet-keyword \advance\ *)
(* minusproductiongluesizeby 2pt {production}def [[(val *)
(* ]]\termvariable-name exp[[)]] | [[(val-rec ]]\term *)
(* variable-name exp[[)]] | exp | [[(define ]]\term *)
(* function-name (formals) exp[[)]] | [[(use ]]\term *)
(* file-name[[)]] | unit-test \newruleunit-test *)
(* [[(check-expect]] exp exp[[)]] | [[(check-assert]] *)
(* exp[[)]] | [[(check-error]] exp[[)]] |       *)
(* [[(check-type]] exp type-exp[[)]] |          *)
(* [[(check-principal-type]] exp type-exp[[)]] | *)
(* [[(check-type-error]] exp[[)]] \newruleexp literal | *)
(* \termvariable-name | [[(if ]]exp exp exp[[)]] | *)
(* [[(begin ]]\sequenceexp[[)]] | [[(]]exp \sequenceexp *)
(* [[)]] | [[(]]let-keyword [[(]]\sequence[[(]]\term *)
(* variable-name exp[[)]][[) ]]exp[[)]] | [[(lambda (]] *)
(* formals[[) ]]exp[[)]] \newrulelet-keyword \alternate *)
(* *let | let* | letrec \newruletype-exp \term  *)
(* type-constructor-name | '\termtype-variable-name | *)
(* [[(forall []]\sequence'\termtype-variable-name[[] ]] *)
(* type-exp[[)]] | [[(]]\sequencetype-exp[[ -> ]] *)
(* type-exp[[)]] | [[(]]type-exp \sequencetype-exp[[)]] *)
(* \newruleformals \sequence\termvariable-name \newrule *)
(* literal \alternate*\termnumeral | [[#t]] | [[#f]] | *)
(* [[']]S-exp | [[(quote ]]S-exp[[)]] \newruleS-exp \ *)
(* alternate*literal | \termsymbol-name | [[(]]\sequence *)
(* S-exp[[)]] \newrulenumeral@\termnumeral \grammarbox *)
(* token composed only of digits, possibly prefixed with *)
(* a plus or minus sign \newrulename@\term*-name \ *)
(* grammarboxtoken that is not a bracket, a numeral, or *)
(* one of the ``reserved'' words shown in typewriter *)
(* font {production}                            *)
(*                                              *)
(* The concrete syntax of \nml [*]              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \noproductionwidth                           *)
(*                                              *)
(* Abstract syntax and values \maintocsplitof \nml *)
(*                                              *)
(* \Nml's abstract syntax is the same as micro-Scheme's, *)
(* minus [[WHILEX]] and [[SET]]. \nmllabelexp   *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (TYVARX a, Delta) =
      (find (a, Delta)
       handle NotFound _ =>
         raise TypeError ("type variable " ^ a ^ " is not in scope"))
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* --- #3 --- \mono#5                           *)
(*                                              *)
(*  Syntax    Concept Semantics                 *)
(*  \typerows                                   *)
(*                                              *)
(* Representational correspondence between type syntax *)
(* and types [*]                                *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* \qbreak Constructor application must be well-kinded. *)
(* \usetyKindApp                                *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (CONAPPX (tx, txs), Delta) =
      let val (tau,  kind)  = txType (tx, Delta)
          val (taus, kinds) =
            ListPair.unzip (map (fn tx => txType (tx, Delta)) txs)
      in  case kind
            of ARROW (argks, resultk) =>
                 if eqKinds (kinds, argks) then
                   (CONAPP (tau, taus), resultk)
                 else
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
                   (* <applied type constructor [[tx]] has the wrong kind>= *)
                   if length argks <> length kinds then
                     raise TypeError ("type constructor " ^ typeString tau ^
                                                              " is expecting " ^
                                      countString argks "argument" ^
                                                                  ", but got " ^
                                      Int.toString (length taus))
                   else
                     let fun findBad n (k::ks) (k'::ks') =
                               if eqKind (k, k') then
                                 findBad (n+1) ks ks'
                               else
                                 raise TypeError ("argument " ^ Int.toString n ^
                                                  " to type constructor " ^
                                                                typeString tau ^
                                                  " should have kind " ^
                                                                  kindString k ^
                                                  ", but it has kind " ^
                                                                  kindString k')
                           | findBad _ _ _ = raise InternalError
                                                    "undetected length mismatch"
                     in  findBad 1 argks kinds
                     end
             | TYPE =>
                   (* <type [[tau]] is not expecting any arguments>= *)
                   raise TypeError ("type " ^ typeString tau ^
                                                " is not a type constructor, " ^
                                    "but it was applied to " ^ countString taus
                                                                   "other type")
      end
(* A function type may be formed only when the argument *)
(* and result types have kind [[TYPE]]. \usety  *)
(* KindFunction                                 *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (FUNTYX (txs, tx), Delta) =
      let val tks = map (fn tx => txType (tx, Delta)) txs
          val tk  = txType (tx, Delta)
          fun notAType (ty, kind) = not (eqKind (kind, TYPE))
          fun thetype  (ty, kind) = ty
      in  if notAType tk then
            raise TypeError ("in result position, " ^
                             typeString (thetype tk) ^ " is not a type")
          else
            case List.find notAType tks
              of SOME tk =>
                   raise TypeError ("in argument position, " ^
                                    typeString (thetype tk) ^ " is not a type")
               | NONE => (funtype (map thetype tks, thetype tk), TYPE)
      end
(* A [[forall]] quantifier is impermissible in a *)
(* type---this restriction is what makes the type system *)
(* a Hindley-Milner type system.                *)
(* <translation of {\uml} type syntax into types>= *)
  | txType (FORALLX _, _) =
      raise TypeError ("'forall' is permissible only at top level")
(* In Typed uScheme, the syntax is the type; there's no *)
(* separate representation. But if you study the *)
(* representations of [[tyex]] and [[ty]] on \cpageref *)
(* adt.type.tyex,ml.type.ty, you might guess what has to *)
(* be done to convert [[tyex]] to [[ty]]:       *)
(*                                              *)
(*   • Convert function-type syntax to an application of *)
(*  [[funty]]                                   *)
(*   • Convert each type name to a [[tycon]]  *)
(*                                              *)
(* The rest of the conversion is structural, plus a *)
(* little extra work to check that kinds are right. *)
(* To make the [[name]]-to-[[tycon]] conversion easy, *)
(* and to keep track of kinds, I use a single   *)
(* environment Delta. The environment Delta maps each *)
(* name both to the type that it stands for and to the *)
(* kind of that type. The name of a type constructor *)
(* maps to \monoboxTYCON µ (along with the kind of µ), *)
(* and the name of a type variable maps to \monoboxTYVAR *)
(* alpha (along with the kind of alpha). The full *)
(* mapping of [[tyex]] to [[ty]] is done by function *)
(* [[txType]].                                  *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* --- \mono#2 --- #3 --- #4 --- \mono#5        *)
(* --- #3 --- #4                                *)
(*                                              *)
(*  Syntax    Concept Semantics                 *)
(*  \typerows                                   *)
(*                                              *)
(* Notational correspondence between type syntax and *)
(* types [*]                                    *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* The type theory that specifies [[txType]] is a *)
(* conservative extension of theory of kind checking *)
(* from Typed uScheme (function [[kindof]] on \cpageref *)
(* tuscheme.fun.kindof). Typed uScheme* uses the kinding *)
(* judgment \kindistau\kind, which says that in *)
(* environment Delta, type tau has kind \kind.  *)
(* uML extends that judgment to \txTypet tau\kind, which *)
(* says that in environment Delta, type syntax t *)
(* translates to type tau, which has kind \kind. If the *)
(* types were erased from environment Delta and the *)
(* syntax t \leadsto were erased from the judgment \ *)
(* txTypet tau\kind, the result would be Typed uScheme's *)
(* kind system. You can prove it for yourself in \cref *)
(* adt.ex.kind-conservative-extension from \cref *)
(* adt.chap.                                    *)
(*                                              *)
(* \qbreak Each clause of [[txType]] implements the *)
(* translation rule that corresponds to its syntax. The *)
(* translation rules, which are shown in \crefpage *)
(* (adt.fig.kinding, extend Typed uScheme's kinding *)
(* rules. To start, a type name is looked up in the *)
(* environment Delta. \usetyKindIntroCon [*] \umlflabel *)
(* txType                                       *)
(* <boxed values 167>=                          *)
val _ = op txType : tyex * (ty * kind) env -> ty * kind
(* <translation of {\uml} type syntax into types>= *)
fun txTyScheme (FORALLX (alphas, tx), Delta) =
      let val Delta' = Delta <+> map (fn a => (a, (TYVAR a, TYPE))) alphas
          val (tau, kind) = txType (tx, Delta')
      in  if eqKind (kind, TYPE) then
            FORALL (alphas, tau)
          else
            raise TypeError ("in " ^ typeSchemeString (FORALL (alphas, tau)) ^
                             ", type " ^ typeString tau ^
                             " has kind " ^ kindString kind)
      end
(* \qbreak If there's no [[forall]] in the syntax, *)
(* a type is also a type scheme (with an empty \/). \ *)
(* usetySchemeKindMonotype                      *)
(* <translation of {\uml} type syntax into types>= *)
  | txTyScheme (tx, Delta) =
      case txType (tx, Delta)
        of (tau, TYPE) => FORALL ([], tau)
         | (tau, kind) =>
             raise TypeError ("expected a type, but got type constructor " ^
                              typeString tau ^ " of kind " ^ kindString kind)
(* The elaboration judgment for a type scheme is \ *)
(* txTySchemet sigma. (Because the kind of a type scheme *)
(* is always \ktype, writing it explicitly is redundant, *)
(* but it makes the judgment easy to compare with the *)
(* corresponding judgment for a type.)          *)
(*                                              *)
(* \qbreak In a type scheme, [[forall]] is permitted. *)
(* Each type variable is given kind \ktype. \usety *)
(* SchemeKindAll [*]                            *)
(* <boxed values 168>=                          *)
val _ = op txTyScheme : tyex * (ty * kind) env -> type_scheme
(* Variables \ldotsnalpha are guaranteed to be distinct *)
(* by the parser, so no check is required here. *)



(*****************************************************************)
(*                                                               *)
(*   TYPING AND EVALUATION OF [[DATA]] DEFINITIONS               *)
(*                                                               *)
(*****************************************************************)

(* <typing and evaluation of [[data]] definitions>= *)
fun typeDataDef ((T, kind, vcons), Gamma, Delta) =
  let
(* <definition of [[validate]], for the types of the value constructors of [[T]]>= *)
      fun validate (K, sigma as FORALL (alphas, _), mu, kind) =
        let (* Function [[validateTyvarArguments]], which checks to *)
            (* make sure that the arguments to a constructor *)
            (* application are distinct type variables, is defined *)
            (* as follows:                                  *)
            (* <definition of [[validateTyvarArguments]]>=  *)
            fun validateTyvarArguments (CONAPP (_, taus)) =
                  let fun asTyvar (TYVAR a) = a
                        | asTyvar tau =
                            raise TypeError ("in type of " ^ K ^
                                                           ", type parameter " ^
                                             typeString tau ^ " passed to " ^ T
                                                                               ^
                                             " is not a type variable")
                  in  case duplicatename (map asTyvar taus)
                        of NONE => ()
                         | SOME a => 
                             raise TypeError ("in type of " ^ K ^
                                                          ", type parameters " ^
                                              "to " ^ T ^
                                                 " must be distinct, but " ^ a ^
                                              " is passed to " ^ T ^
                                                              " more than once")
                  end
              | validateTyvarArguments (TYCON _) =
                  ()  (* happens only when uML is extended with existentials *)
              | validateTyvarArguments _ =
                  raise InternalError "impossible type arguments"
            fun appliesMu (CONAPP (tau, _)) = eqType (tau, TYCON mu) 
              | appliesMu _ = false
            val desiredType =
              case kind of TYPE    => "type " ^ tyconString mu
                         | ARROW _ => "a type made with " ^ tyconString mu
            fun validateLengths (alphas, argkinds) =
              if length alphas <> length argkinds then
                (* When validation fails, much of the code that issues *)
                (* error messages is here.                      *)

      (* <for [[K]], complain that [[alphas]] is inconsistent with [[kind]]>= *)
                (case kind
                   of TYPE =>
                        raise TypeError ("datatype " ^ T ^
                                              " takes no type parameters, so " ^
                                         "value constructor " ^ K ^
                                                     " must not be polymorphic")
                    | ARROW (kinds, _) => 
                          raise TypeError ("datatype constructor " ^ T ^
                                                                   " expects " ^
                                           intString (length kinds) ^
                                                             " type parameter" ^
                                           (case kinds of [_] => "" | _ => "s")
                                                                               ^
                                           ", but value constructor " ^ K ^
                                           (if null alphas then
                                                           " is not polymorphic"
                                            else " expects " ^ Int.toString (
                                                                length alphas) ^
                                                 " type parameter" ^
                                                 (case alphas of [_] => "" | _
                                                                      => "s"))))
              else
                ()
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
      (* <boxed values 164>=                          *)
      val _ = op appliesMu             : ty -> bool
      val _ = op validateTyvarArguments : ty -> unit
      val _ = op validateLengths       : tyvar list * kind list -> unit
        in
      (* <validation by case analysis on [[schemeShape shape]] and [[kind]]>= *)
            case (schemeShape sigma, kind)
              of (MONO_VAL tau, TYPE) =>
                   if eqType (tau, TYCON mu) then
                     ()
                   else

               (* <type of [[K]] should be [[desiredType]] but is [[sigma]]>= *)
                     raise TypeError ("value constructor " ^ K ^ " should have "
                                                                 ^ desiredType ^
                                      ", but it has type " ^ typeSchemeString
                                                                          sigma)
               | (MONO_FUN (_, result), TYPE) =>
                   if eqType (result, TYCON mu) then
                     ()
                   else

       (* <result type of [[K]] should be [[desiredType]] but is [[result]]>= *)
                     raise TypeError ("value constructor " ^ K ^
                                               " should return " ^ desiredType ^
                                      ", but it returns type " ^ typeString
                                                                         result)
               | (POLY_VAL (alphas, tau), ARROW (argkinds, _)) => 
                   if appliesMu tau then
                     ( validateLengths (alphas, argkinds)
                     ; validateTyvarArguments tau
                     )
                   else

               (* <type of [[K]] should be [[desiredType]] but is [[sigma]]>= *)
                     raise TypeError ("value constructor " ^ K ^ " should have "
                                                                 ^ desiredType ^
                                      ", but it has type " ^ typeSchemeString
                                                                          sigma)
            (* \qbreak                                      *)

      (* <validation by case analysis on [[schemeShape shape]] and [[kind]]>= *)
               | (POLY_FUN (alphas, _, result), ARROW (argkinds, _)) => 
                   if appliesMu result then
                     ( validateLengths (alphas, argkinds)
                     ; validateTyvarArguments result
                     )
                   else

       (* <result type of [[K]] should be [[desiredType]] but is [[result]]>= *)
                     raise TypeError ("value constructor " ^ K ^
                                               " should return " ^ desiredType ^
                                      ", but it returns type " ^ typeString
                                                                         result)
               | _ =>
                   (* When validation fails, much of the code that issues *)
                   (* error messages is here.                      *)

      (* <for [[K]], complain that [[alphas]] is inconsistent with [[kind]]>= *)
                   (case kind
                      of TYPE =>
                           raise TypeError ("datatype " ^ T ^
                                              " takes no type parameters, so " ^
                                            "value constructor " ^ K ^
                                                     " must not be polymorphic")
                       | ARROW (kinds, _) => 
                             raise TypeError ("datatype constructor " ^ T ^
                                                                   " expects " ^
                                              intString (length kinds) ^
                                                             " type parameter" ^
                                              (case kinds of [_] => "" | _ =>
                                                                          "s") ^
                                              ", but value constructor " ^ K ^
                                              (if null alphas then
                                                           " is not polymorphic"
                                               else " expects " ^ Int.toString (
                                                                length alphas) ^
                                                    " type parameter" ^
                                                    (case alphas of [_] => "" |
                                                                    _ => "s"))))
        end                   
      val mu      = freshTycon T
      val Delta'  = bind (T, (TYCON mu, kind), Delta)
      fun translateVcon (K, tx) = (K, txTyScheme (tx, Delta'))
            handle TypeError msg =>
                                                                        (*OMIT*)
              raise TypeError ("in type of value constructor " ^ K ^ ", " ^ msg)
                                                                        (*OMIT*)
      val Ksigmas = map translateVcon vcons
      val ()      = app (fn (K, sigma) => validate (K, sigma, mu, kind))
                        Ksigmas
      val ()      = addVcons (mu, Ksigmas) (* OMIT *)
      val Gamma'  = extendTypeEnv (Gamma, Ksigmas)
      val strings = kindString kind :: map (typeSchemeString o snd) Ksigmas
  in  (Gamma', Delta', strings)
  end
(* A monomorphic value constructor is compatible if it *)
(* is a fish or it returns a fish. A polymorphic *)
(* constructor is the same, but with more fiddly detail: *)
(* [*] {mathpar} \vconcompatsigmaµ\akind       *)
(*                                              *)
(* \tyrule MonoIsCompat \vconcompatµµ\ktype   *)
(*                                              *)
(* \tyruleMonoReturnsCompat \vconcompat\crossdotsntau--> *)
(* µ µ\ktype                                  *)
(*                                              *)
(* \tyrule PolyIsCompat \ldotskprimealpha all distinct \ *)
(* vconcompat\/\ldotskalpha. (\ldotskprimealpha) µ µ\ *)
(* cdotsk\ktype\karrow\ktype                    *)
(*                                              *)
(* \tyrule PolyReturnsCompat \ldotskprimealpha all *)
(* distinct \vconcompat\/\ldotskalpha. \crossdotsntau--> *)
(* (\ldotskprimealpha) µ µ\cdotsk\ktype\karrow\ktype *)
(* {mathpar} The type variables \ldotskprimealpha, which *)
(* are the parameters to µ, are actually a permutation *)
(* of the quantified type variables \ldotskalpha. But in *)
(* the compatibility judgment, it's enough for the alpha *)
(* '_i's to be distinct. If they are, the translation *)
(* judgment \txtyscheme[Delta'] t_i sigma_i (below) *)
(* ensures that they are a permutation of the alpha_i's. *)
(*                                              *)
(* The compatibility rules are implemented by function *)
(* [[validate]] in \crefadta.chap. Using that function, *)
(* and using function [[txTyScheme]] to implement the *)
(* translation judgment described below, function *)
(* [[typeDataDef]] types a [[data]] definition. *)
(* It returns Gamma', Delta', and a list of strings: the *)
(* name \stycon followed by names [\ldotsnK]. \umlflabel *)
(* typeDataDef                                  *)
(* <boxed values 144>=                          *)
val _ = op typeDataDef : data_def * type_env * (ty * kind) env -> type_env * (ty
                                                       * kind) env * string list
(* <typing and evaluation of [[data]] definitions>= *)
fun evalDataDef ((_, _, typed_vcons), rho) =
  let fun valFor (K, t) = if isPolymorphicFuntyex t then
                            PRIMITIVE (fn vs => CONVAL (K, vs))
                          else
                            CONVAL (K, [])
      fun addVcon ((K, t), rho) = bind (K, valFor (K, t), rho)
(* \typesystemuml                               *)
(*                                              *)
(* \deftyruleSchemeKindAll \multiline \ldotsnalpha are *)
(* all distinct \txType [Delta{alpha_1 |->(alpha_1, \ *)
(* ktype), ..., alpha_n |->(alpha_n, \ktype) }] t tau\ *)
(* ktype \txTyScheme\mathttbracketforall \mathttbracket\ *)
(* cdotsnalpha t \/\ldotsnalpha. tau            *)
(*                                              *)
(* \deftyruleSchemeKindMonotype \txTypet tau\ktype \ *)
(* txTySchemet \/.tau                           *)
(*                                              *)
(* \deftyrule KindIntroCon \styconin dom Delta  *)
(* Delta(\stycon) = (tau, \akind) \txType\stycontau\ *)
(* akind                                        *)
(*                                              *)
(* \deftyrule KindIntroVar alphain dom Delta    *)
(* Delta(alpha) = (tau, \akind) \txTypealphatau\akind *)
(*                                              *)
(* \deftyrule KindApp \twoline \txTypet tau\crossdotsn\ *)
(* kind\karrow\kind \txTypet_i tau_i \kind_i, 1 <=i <=n *)
(* \txType\mathttbrackett \cdotsnt (\ldotsntau) tau \ *)
(* kind                                         *)
(*                                              *)
(* \deftyrule KindFunction \twoquad \txTypet_i tau_i \ *)
(* ktype, 1 <=i <=n \txTypet tau\ktype \txType\ *)
(* mathttbracket\cdotsnt -> t \crossdotsntau-->tau \ *)
(* ktype                                        *)
(*                                              *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(* \typesystemuml {mathpar} \txTypet tau\akind  *)
(*                                              *)
(* \usetysKindIntroCon,KindIntroVar,KindApp,KindFunction *)
(*                                              *)
(* \txTySchemet sigma                           *)
(*                                              *)
(* \usetysSchemeKindAll,SchemeKindMonotype {mathpar} *)
(*                                              *)
(* Translation of uML's type syntax, with kinds [*] *)
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
(*                                              *)
(* Translating type syntax into types           *)
(*                                              *)
(* [*] \typesystemuml                           *)
(*                                              *)
(* Just as in Typed uScheme, type syntax written by a *)
(* programmer isn't trusted; nonsense like \monobox(int *)
(* int) and \monobox(list -> list) is rejected by the *)
(* type system. In Typed uScheme*, it suffices to check *)
(* the kind of each type, then pass the syntax on to the *)
(* type checker. But in uML, the syntax of types is *)
(* different from the internal representation used for *)
(* type inference, so the syntax can't be passed on. *)
(* Instead, it is translated.                   *)
(*                                              *)
(* The translation is expressed using two judgment *)
(* forms, \txTypet tau\akind and \txTySchemet sigma, *)
(* which translate uML type syntax into either a *)
(* Hindley-Milner type or a type scheme, respectively. *)
(* The rules are shown in \crefadt.fig.kinding. *)
(* The implementation is so similar to Typed uScheme's *)
(* kind checking that I doubt you need to see it. But if *)
(* you do, you will find functions [[txType]] and *)
(* [[txTyScheme]] in \crefadta.chap.            *)
(*                                              *)
(* Run-time representation of value constructors *)
(*                                              *)
(* Datatype definitions are not only typed; they are *)
(* also evaluated. Evaluating a datatype definition *)
(* introduces a function or value for every value *)
(* constructor. A value constructor with a function type *)
(* gets a function; a value constructor with a  *)
(* non-function type is a value by itself. To tell which *)
(* is which, function [[isPolymorphicFuntyex]] looks at *)
(* the type syntax. As with \nml, evaluation isn't *)
(* formalized. \umlflabelevalDataDef            *)
(* <boxed values 145>=                          *)
val _ = op evalDataDef : data_def * value env -> value env * string list
val _ = op isPolymorphicFuntyex : tyex -> bool
  in  (foldl addVcon rho typed_vcons, map fst typed_vcons)
  end
(* <typing and evaluation of [[data]] definitions>= *)
fun processDataDef (dd, (Gamma, Delta, rho), interactivity) =
  let val (Gamma', Delta', tystrings) = typeDataDef (dd, Gamma, Delta)
      val (rho', vcons)               = evalDataDef (dd, rho)
      val _ = if echoes interactivity then
                (* The name of the new type constructor is printed with *)
                (* its kind, and the name of each value constructor is *)
                (* printed with its type.                       *)
                (* <print the new type and each of its value constructors>= *)
                let val (T, _, _) = dd
                    val (mu, _)   = find (T, Delta')
                    val (kind, vcon_types) =
                      case tystrings
                        of s :: ss => (s, ss)
                         | [] => raise InternalError "no kind string"
                in  ( println (typeString mu ^ " :: " ^ kind)
                    ; ListPair.appEq (fn (K, tau) => println (K ^ " : " ^ tau))
                                                             (vcons, vcon_types)
                    )
                end
              else
                ()
  in  (Gamma', Delta', rho')
  end
(* <boxed values 154>=                          *)
val _ = op processDataDef : data_def * basis * interactivity -> basis


(*****************************************************************)
(*                                                               *)
(*   DEFINITIONS OF [[EMPTYBASIS]] AND [[PREDEFINEDTYPEBASIS]]   *)
(*                                                               *)
(*****************************************************************)

(* <definitions of [[emptyBasis]] and [[predefinedTypeBasis]]>= *)
val emptyBasis =
  (emptyTypeEnv, emptyEnv, emptyEnv)
fun addTycon ((t, tycon, kind), (Gamma, Delta, rho)) =
  (Gamma, bind (t, (TYCON tycon, kind), Delta), rho)
val primTyconBasis : basis = 
  foldl addTycon emptyBasis ((* Primitive types and functions                *)
                             (*                                              *)

                      (* Like Typed uScheme, uML has both primitive types and *)

                     (* primitive values. Primitive types [[int]] and [[sym]] *)

                          (* are bound into the kind environment Delta. Other *)

                      (* built-in types are either defined in user code, like *)

                     (* [[list]] and [[bool]], or they don't have names, like *)
                             (* the function type.                           *)

                           (* <primitive type constructors for \uml\ [[::]]>= *)
                             ("int", inttycon, TYPE) :: 
                             ("sym", symtycon, TYPE) :: nil)
(* <definitions of [[emptyBasis]] and [[predefinedTypeBasis]]>= *)
val predefinedTypeBasis =
  let val predefinedTypes = 
                             [
                              ";  The [[option]] type is not primitive; it is  "
                             ,
                       ";  predefined using ordinary user code. Its definition "
                             ,
                     ";  uses the explicit [[data]] form, which gives the kind "
                             ,
                     ";  of the [[option]] type constructor and the full types "
                             ,
                      ";  of the [[SOME]] and [[NONE]] value constructors: [*] "
                             , ";  <predefined uML types>=                   "
                             , "(data (* => *) option"
                             , "  [SOME : (forall ['a] ('a -> (option 'a)))]"
                             , "  [NONE : (forall ['a] (option 'a))])"
                             ,
                      ";  If you prefer less notation, any algebraic data type "
                             ,
                       ";  in uML can also be defined using [[implicit-data]]: "
                             , ""
                             ,
                            ";  m0.30\\linewidth-8.6pt @\\mskip                "
                             ,
                           ";  3mu \\definedas\\mskip20mu m0.30\\              "
                             ,
                              ";  linewidth-8.6pt@                             "
                             ,
                              ";                                               "
                             ,
                             ";   (implicit-data \\stycon                      "
                             ,
                              ";  K_1                                          "
                             ,
                              ";  ...                                          "
                             ,
                             ";  [K_i of \\cdotsnt]                            "
                             ,
                              ";  ...                                          "
                             ,
                       ";                                     (data * \\stycon "
                             ,
                       ";                                     [K_1 : \\stycon] "
                             ,
                              ";                                     ...       "
                             ,
                    ";  )                                     [K_i : (\\cdotsn "
                             ,
                        ";                                     t -> \\stycon)] "
                             ,
                              ";                                     ...       "
                             ,
                              ";                                     )         "
                             ,
                              ";                                               "
                             ,
                              ";            (a) Without type parameters        "
                             ,
                       ";                                     (data (\\cdotsm* "
                             ,
                        ";                                     => * ) \\stycon "
                             ,
                         ";                                     [K_1 : (forall "
                             ,
                       ";   (implicit-data (\\cdotsmalpha)     (\\cdotsmalpha) "
                             ,
                      ";   \\stycon                           (T \\cdotsmalpha "
                             ,
                              ";   K_1                               ))]       "
                             ,
                              ";   ...                               ...       "
                             ,
                        ";   [K_i of \\cdotsnt]                 [K_i : (forall "
                             ,
                        ";                                     (\\cdotsmalpha) "
                             ,
                          ";   ...                               (\\cdotsnt -> "
                             ,
                       ";   )                                 (T \\cdotsmalpha "
                             ,
                              ";                                     )))]      "
                             ,
                              ";                                     ...       "
                             ,
                              ";                                     )         "
                             ,
                              ";                                               "
                             ,
                              ";             (b) With type parameters          "
                             ,
                              ";                                               "
                             ,
                              ";  Desugaring [[implicit-data]] [*]             "
                             ,
";  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ "
                             ,
                              ";                                               "
                             ,
                        ";  Formally, [[implicit-data]] is syntactic sugar for "
                             ,
                        ";  [[data]]. It is desugared by one of two equations, "
                             ,
                      ";  depending on whether type parameters are present (\\ "
                             ,
                           ";  crefadt.fig.implicit-data-desugared). When type "
                             ,
                     ";  parameters are absent, every value constructor gets a "
                             ,
                       ";  monomorphic type. When type parameters are present, "
                             ,
                         ";  every value constructor is polymorphic in exactly "
                             ,
                    ";  those parameters. Type name \\stycon is applied to the "
                             ,
                      ";  parameters and is given a kind consistent with them. "
                             ,
                      ";  In the interpreter, the desugaring is implemented by "
                             ,
                    ";  function [[makeExplicit]] (\\crefadta.chap, \\cpageref "
                             ,
                              ";  adt.fun.makeExplicit).                       "
                             ,
                              ";                                               "
                             ,
                             ";  Predefined algebraic \\chaptocsplitdata types "
                             ,
                              ";                                               "
                             ,
                     ";  Many of the algebraic data types found in Standard ML "
                             ,
                        ";  are also predefined in uML. They are defined using "
                             ,
                     ";  [[data]] or [[implicit-data]]. In most cases, to make "
                             ,
                       ";  the types of the value constructors explicit, I use "
                             ,
                              ";  the [[data]] form.                           "
                             ,
                              ";                                               "
                             ,
                              ";  A Boolean is either [[#t]] or [[#f]].        "
                             , ";  <predefined uML types>=                   "
                             , "(data * bool"
                             , "  [#t : bool]"
                             , "  [#f : bool])"
                             ,
                          ";  A list is made using either [['()]] or [[cons]]. "
                             , ";  <predefined uML types>=                   "
                             , "(data (* => *) list"
                             , "  ['()  : (forall ['a] (list 'a))]"
                             ,
                         "  [cons : (forall ['a] ('a (list 'a) -> (list 'a)))])"
                             ,
                            ";  The [[unit]] type, which is the result type of "
                             ,
                     ";  [[println]] and [[print]], is inhabited by the single "
                             ,
                              ";  value [[UNIT]].                              "
                             , ";  <predefined uML types>=                   "
                             , "(data * unit [UNIT : unit])"
                             ,
                      ";  Type [[pair]] is defined on \\cpagerefadt.type.pair, "
                             ,
                        ";  with accompanying functions [[pair]], [[fst]], and "
                             ,
                     ";  [[snd]]. Types for triples and larger tuples are also "
                             ,
                     ";  predefined; the value constructor for type [[triple]] "
                             ,
                       ";  is [[TRIPLE]], and the value constructors for types "
                             ,
                           ";  [[4-tuple]] through [[10-tuple]] are [[T4]] to  "
                             ,
                      ";  [[T10]], respectively (\\crefadta.chap). These types "
                             ,
                           ";  are defined without any accompanying functions. "
                             ,
                          ";  If you want functions, define them using pattern "
                             ,
                       ";  matching. Or do \\crefadt.ex.let-patterns and match "
                             ,
                              ";  tuples directly in [[let]] forms.            "
                             ,
                              ";                                               "
                             ,
                          ";  The [[order]] type is a standard result type for "
                             ,
                            ";  comparison functions. It represents a relation "
                             ,
                            ";  between elements of a totally ordered set: one "
                             ,
                       ";  element may be less than, equal to, or greater than "
                             ,
                              ";  another.                                     "
                             , ";  <predefined uML types>=                   "
                             , "(implicit-data order LESS EQUAL GREATER)"
                             , ";  <predefined uML types>=                   "
                             , "(data (* * * => *) triple"
                             ,
             "  [TRIPLE : (forall ['a 'b 'c] ('a 'b 'c -> (triple 'a 'b 'c)))])"
                             , ";  <predefined uML types>=                   "
                             , "(implicit-data ('a1 'a2 'a3 'a4) 4-tuple"
                             , "         [T4 of 'a1 'a2 'a3 'a4])"
                             , "(implicit-data ('a1 'a2 'a3 'a4 'a5) 5-tuple"
                             , "         [T5 of 'a1 'a2 'a3 'a4 'a5])"
                             ,
                              "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6) 6-tuple"
                             , "         [T6 of 'a1 'a2 'a3 'a4 'a5 'a6])"
                             ,
                          "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7) 7-tuple"
                             , "         [T7 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7])"
                             ,
                      "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8) 8-tuple"
                             ,
                             "         [T8 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8])"
                             ,
                  "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9) 9-tuple"
                             ,
                         "         [T9 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9])"
                             ,
            "(implicit-data ('a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9 'a10) 10-tuple"
                             ,
                    "        [T10 of 'a1 'a2 'a3 'a4 'a5 'a6 'a7 'a8 'a9 'a10])"
                             , ";  <predefined uML types>=                   "
                             , "(data * sx"
                             , "   [Sx.B : (bool -> sx)]"
                             , "   [Sx.S : (sym  -> sx)]"
                             , "   [Sx.N : (int  -> sx)]"
                             , "   [Sx.L : ((list sx)  -> sx)])"
                              ] 
      val xdefs = stringsxdefs ("built-in types", predefinedTypes)
      fun process (DEF (DATA dd), b) = processDataDef (dd, b, noninteractive)
        | process _ = raise InternalError "predefined definition is not DATA"
  in  streamFold process primTyconBasis xdefs
  end
(* Building the initial basis and providing access to *)
(* predefined types                             *)
(*                                              *)
(* Other interpreters build an initial basis by starting *)
(* with an empty basis, adding primitives, and adding *)
(* predefined functions. But the initial basis for the *)
(* uML interpreter is built in five stages, not three: *)
(*                                              *)
(*  1. Start with an empty basis                *)
(*  2. Add the primitive type constructors [[int]] and *)
(*  [[sym]], producing [[primTyconBasis]]       *)
(*  3. [*] Add the predefined types, producing  *)
(*  [[predefinedTypeBasis]]                     *)
(*                                              *)
(*  (At this point, it is possible to implement type *)
(*  inference, which uses the predefined types  *)
(*  [[list]] and [[bool]] to infer the types of list *)
(*  literals and Boolean literals.)             *)
(*  4. Add the primitives, some of whose types refer to *)
(*  predefined types, producing [[primFunBasis]] *)
(*  5. Add the predefined functions, some of whose *)
(*  bodies refer to primitives, producing       *)
(*  [[initialBasis]]                            *)
(*                                              *)
(* After step [<-], the predefined types [[list]] and *)
(* [[bool]] need to be exposed to the type-inference *)
(* engine, and all the predefined types need to be *)
(* exposed to the implementations of the primitives. *)
(* The basis holding the predefined types is called *)
(* [[predefinedTypeBasis]], and the code for the first *)
(* two steps is implemented here. First, the primitive *)
(* type constructors:                           *)
(* <boxed values 156>=                          *)
val _ = op emptyBasis     : basis
val _ = op primTyconBasis : basis
(* The [[xdeftable]] is shared with the Impcore parser. *)
(* Function [[reduce_to_xdef]] is almost shareable as *)
(* well, but not quite---the abstract syntax of *)
(* [[DEFINE]] is different.                     *)

(* Next, the predefined types. Internal function *)
(* [[process]] accepts only [[data]] definitions, which *)
(* can be checked without type inference. \     *)
(* makenowebnotdef (from \LApredefined uML types \ *)
(* upshape[->]\RA)                              *)
(* <boxed values 156>=                          *)
val _ = op predefinedTypeBasis : basis
(* [[funty]] stand for \tau, [[actualtypes]]    *)
(* stand for \ldotsntau, and [[rettype]] stand for alpha *)
(* . The first premise is implemented by a call to *)
(* [[typesof]] and the second by a call to      *)
(* [[freshtyvar]]. The constraint is represented just as *)
(* written in the rule.                         *)



(*****************************************************************)
(*                                                               *)
(*   DEFINITIONS OF [[BOOLTYPE]], [[LISTTYPE]], AND [[UNITTYPE]] *)
(*                                                               *)
(*****************************************************************)

(* Once the predefined types have been added to *)
(* [[predefinedTypeBasis]], some of them need to be *)
(* exposed to the rest of the interpreter. Types *)
(* [[bool]], [[list]], [[unit]], and [[sx]] are used to *)
(* infer types, to write the types of primitive *)
(* functions, or both. Two more types, [[alpha]] and *)
(* [[beta]], are used to write the types of polymorphic *)
(* primitives.                                  *)
(* <definitions of [[booltype]], [[listtype]], and [[unittype]]>= *)
local
  val (_, Delta, _) = predefinedTypeBasis
  fun predefined t = fst (find (t, Delta))
  val listtycon = predefined "list"
in
  val booltype     = predefined "bool"
  fun listtype tau = CONAPP (listtycon, [tau])
  val unittype     = predefined "unit"
  val sxtype       = predefined "sx"
  val alpha = TYVAR "'a"
  val beta  = TYVAR "'b"
end


(*****************************************************************)
(*                                                               *)
(*   TYPE INFERENCE FOR \NML\ AND \UML                           *)
(*                                                               *)
(*****************************************************************)

(* <type inference for \nml\ and \uml>=         *)
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
(* <boxed values 80>=                           *)
val _ = op bindtyscheme : name * type_scheme * type_env -> type_env
(* Free type variables are found in the cache in *)
(* constant time. \nmlflabelfreetyvarsGamma \   *)
(* nwnarrowboxes                                *)
(* <boxed values 80>=                           *)
val _ = op freetyvarsGamma : type_env -> name set
(* <boxed values 80>=                           *)
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
(* The \bigwedge{ ...} notation is implemented by ML *)
(* function [[conjoinConstraints]]. To preserve the *)
(* number and order of sub-constraints, it avoids using *)
(* [[foldl]] or [[foldr]]. \nmlflabelconjoinConstraints *)
(* \nwnarrowboxes                               *)
(* <boxed values 81>=                           *)
val _ = op conjoinConstraints : con list -> con
(* <utility functions on type constraints>=     *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau,tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)
(* For debugging, it can be useful to see if a  *)
(* substitution solves a constraint. \nmlflabelisSolved *)
(* <boxed values 83>=                           *)
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
(* <boxed values 267>=                          *)
val _ = op constraintString : con -> string
val _ = op untriviate       : con -> con
(* Now micro-Scheme primitives like [[+]] and [[*]] can *)
(* be defined by applying first [[arithOp]] and then *)
(* [[inExp]] to their ML counterparts.          *)
(*                                              *)
(* The micro-Scheme primitives are organized into a list *)
(* of (name, function) pairs, in Noweb code chunk *)
(* [[]].                                        *)
(* Each primitive on the list has type \monoboxvalue *)
(* list -> value. In \chunkrefmlscheme.inExp-applied, *)
(* each primitive is passed to [[inExp]], and the *)
(* results are used build micro-Scheme's initial *)
(* environment. [Actually, the list contains all the *)
(* primitives except one: the [[error]] primitive, which *)
(* must not be wrapped in [[inExp]].] The list of *)
(* primitives begins with these four elements:  *)
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
(* <boxed values 82>=                           *)
val _ = op solve : con -> subst
(* <constraint solving ((elided))>=             *)
fun hasNoSolution c = (solve c; false) handle TypeError _ => true
fun hasGoodSolution c = solves (solve c, c) handle TypeError _ => false
val hasSolution = not o hasNoSolution : con -> bool
fun solutionEquivalentTo (c, theta) = eqsubst (solve c, theta)
(* <utility functions for {\uml}>=              *)
(* filled in when implementing uML *)
(* <exhaustiveness analysis for {\uml}>=        *)
datatype simple_vset = ALL of ty
                     | ONE of vcon * simple_vset list
type vset = simple_vset collection
(* <exhaustiveness analysis for {\uml} ((elided))>= *)
fun vsetString (ALL tau) = "_" (* (ALL : " ^ typeString tau ^ ")" *)
  | vsetString (ONE (K, [])) = K
  | vsetString (ONE (K, vsets)) = "(" ^ K ^ " " ^ spaceSep (map vsetString vsets
                                                                         ) ^ ")"
(* <exhaustiveness analysis for {\uml} ((elided))>= *)
fun hasAll (ALL _) = true
  | hasAll (ONE (_, vs)) = List.exists hasAll vs
(* <exhaustiveness analysis for {\uml} ((prototype))>= *)
fun classifyVset   p vs = joinC (mapC (classifySimple p) vs)
and classifySimple p vs = raise LeftAsExercise "match classification"
(* <exhaustiveness analysis for {\uml}>=        *)
fun unroll tau =
  let val mu = case tau of TYCON mu => mu
                         | CONAPP (TYCON mu, _) => mu
                         | _ => raise BugInTypeInference "not ADT"
      fun ofVcon (name, sigma) =
        let val tau'   = freshInstance sigma
            val argTys = case asFuntype tau'
                           of SOME (args, res) =>
                                let val theta = solve (res ~ tau)
                                in  map (tysubst theta) args
                                end
                            | NONE => []
        in  ONE (name, map ALL argTys) : simple_vset
        end
  in  C (map ofVcon (vconsOf mu))
  end
(* Function call \monoboxvconsOf mu returns the name and *)
(* type scheme of each value constructor associated *)
(* with [[mu]]. Function [[vconsOf]] is omitted from *)
(* this book.                                   *)

(* <exhaustiveness analysis for {\uml} ((prototype))>= *)
fun exhaustivenessCheck (ps, tau) =
  eprintln "(Case expression not checked for exhaustiveness.)"
(* <exhaustiveness analysis for {\uml}>=        *)
(* filled in when implementing uML *)
(* <definitions of [[typeof]] and [[typdef]] for \nml\ and \uml>= *)
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
      (* The uML interpreter builds on the \nml interpreter of *)
      (* \crefml.chap, but in \crefml.chap I don't want to *)
      (* reveal the existence of code meant to handle pattern *)
      (* matching. To make the relevant functions available in *)
      (* an acceptable part of the interpreter, I pretend they *)
      (* are part of the [[literal]] function.        *)
      (* <function [[literal]], to infer the type of a literal constant>= *)
      (* <definition of function [[pvconType]]>=      *)
      fun pvconType (K, Gamma) =
        freshInstance (findtyscheme (K, Gamma))
        handle NotFound x => raise TypeError ("no value constructor named " ^ x)
      (* <definition of function [[pattype]]>=        *)
      fun pattype (p as CONPAT (vcon, pats as _ :: _), Gamma) = 
            let val vcon_tau = pvconType (vcon, Gamma)
                val (Gamma'_is, tau_is, c_is) = pattypes (pats, Gamma)
                val alpha  = freshtyvar ()
                val c      = vcon_tau ~ funtype (tau_is, alpha)
                val c'     = conjoinConstraints c_is
                val Gamma' = disjointUnion Gamma'_is
                             handle DisjointUnionFailed x =>
                               raise TypeError ("name " ^ x ^
                                                         " is bound multiple " ^
                                                "times in pattern " ^ patString
                                                                              p)
            in  (Gamma', alpha, c /\ c')
            end
        | pattype (CONPAT (K, []), Gamma) =
            (emptyEnv, pvconType (K, Gamma), TRIVIAL)
        | pattype (WILDCARD, _) =
            (emptyEnv, freshtyvar(), TRIVIAL)
        | pattype (PVAR x, _) =
            let val alpha = freshtyvar ()
      (* The combination Gamma+Gamma' is implemented by *)
      (* function [[extendTypeEnv]], which is defined in the *)
      (* Supplement. (Because Gamma is a [[type_env]], not a \ *)
      (* monoboxty env, it can't be extended using the [[<+>]] *)
      (* function.)                                   *)
      (*                                              *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
      (* to                                           *)
      (*                                              *)
      (*  \typesystemuml-constraints {mathpar}        *)
      (*                                              *)
      (*  \shiftjudgment-60pt4.3\typeisc\tyce tau     *)
      (*                                              *)
      (*  \usetyCase                                  *)
      (*                                              *)
      (*  \usetyVcon                                  *)
      (*  \shiftjudgment-30pt1.3\typeisc\tyc\choicep e tau *)
      (*  -->tau'                                     *)
      (*                                              *)
      (*  \usetyChoice                                *)
      (*                                              *)
      (*  \shiftjudgment-20pt4.5\pattypeisc\tycp Gamma' tau *)
      (*                                              *)
      (*  \usetyPatVcon                               *)
      (*                                              *)
      (*  \usetyPatBareVcon                           *)
      (*                                              *)
      (*  \usetyPatWildcard                           *)
      (*                                              *)
      (*  \usetyPatVar {mathpar}                      *)
      (*                                              *)
      (*  Constraint-based rules for case expressions, *)
      (*  choices, and patterns [*]                   *)
      (*                                              *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
      (*                                              *)
      (* \qtrim1.3                                    *)
      (*                                              *)
      (* Function [[pattype]] has four cases: one for each *)
      (* rule.                                        *)
      (* <boxed values 150>=                          *)
      val _ = op pattype : pat * type_env -> type_scheme env * ty * con
      (* \setcodemargin0pt \advance\nwdefspaceby -8pt \ *)
      (* umlflabelpattype                             *)

            in  (bind (x, FORALL ([], alpha), emptyEnv), alpha, TRIVIAL)
            end
      and pattypes (ps, Gamma) = unzip3 (map (fn p => pattype (p, Gamma)) ps)
      (* <definition of function [[choicetype]]>=     *)
      fun choicetype ((p, e), Gamma) =
            let val (Gamma', tau, c) = pattype (p, Gamma)
                val (tau', c') = typeof (e, extendTypeEnv (Gamma, Gamma'))
                val (ty,  con) = (funtype ([tau], tau'), c /\ c')
                val _ =
(* <check [[p]], [[e]], [[Gamma']], [[Gamma]], [[ty]], and [[con]] for escaping skolem types>= *)
                        ()          
      (* <boxed values 149>=                          *)
      val _ = op choicetype : (pat * exp) * type_env -> ty * con
            in  (ty, con)
            end

(* <function [[ty]], to infer the type of a \nml\ expression, given [[Gamma]]>= *)
      fun ty (LITERAL n) = literal n
        (* To infer the type of a variable, we use fresh type *)
        (* variables to create a most general instance of the *)
        (* variable's type scheme in Gamma. No constraint is *)
        (* needed.                                      *)
        (* <more alternatives for [[ty]]>=              *)
          | ty (VAR x) = (freshInstance (findtyscheme (x, Gamma)), TRIVIAL)
        (* To infer the type of a function application, we need *)
        (* a rule that uses constraints. By rewriting the *)
        (* nondeterministic rule as described in Section [->], *)
        (* we get this rule: \usety.ApplyC This rule is *)
        (* implemented by letting [[funty]] stand for \tau, *)
        (* [[actualtypes]] stand for \ldotsntau, and [[rettype]] *)
        (* stand for alpha. The first premise is implemented by *)
        (* a call to [[typesof]] and the second by a call to *)
        (* [[freshtyvar]]. The constraint is formed just as *)
        (* specified in the rule. [*]                   *)
        (* <more alternatives for [[ty]]>=              *)
        | ty (APPLY (f, actuals)) = 
             (case typesof (f :: actuals, Gamma)
                of ([], _) => raise InternalError "pattern match"
                 | (funty :: actualtypes, c) =>
                      let val rettype = freshtyvar ()
                      in  (rettype, c /\ (funty ~ funtype (actualtypes, rettype)
                                                                              ))
                      end)
        (* To infer the type of a [[LETSTAR]] form, we desugar *)
        (* it into nested [[LET]]s.                     *)
        (* <more alternatives for [[ty]]>=              *)
        | ty (LETX (LETSTAR, [], body)) = ty body
        | ty (LETX (LETSTAR, (b :: bs), body)) = 
            ty (LETX (LET, [b], LETX (LETSTAR, bs, body)))
        (* Inference for the remaining expression forms is left *)
        (* as an exercise.                              *)
        (* <more alternatives for [[ty]] ((prototype))>= *)
        | ty (IFX (e1, e2, e3))        = raise LeftAsExercise "type for IFX"
        | ty (BEGIN es)                = raise LeftAsExercise "type for BEGIN"
        | ty (LAMBDA (formals, body))  = raise LeftAsExercise "type for LAMBDA"
        | ty (LETX (LET, bs, body))    = raise LeftAsExercise "type for LET"
        | ty (LETX (LETREC, bs, body)) = raise LeftAsExercise "type for LETREC"
        (* <more alternatives for [[ty]]>=              *)
        | ty (CASE (e, choices)) = 
            let val (tau, c_e) =
                      typeof (e, Gamma)
                val (tau_i's, c_i's) =
                      ListPair.unzip (map (fn ch => choicetype (ch,Gamma))
                                                                        choices)
                val alpha = freshtyvar ()
                fun constrainArrow tau_i = tau_i ~ funtype ([tau], alpha)
                val c' = conjoinConstraints (map constrainArrow tau_i's)
                val c = c_e /\ c' /\ conjoinConstraints c_i's
                val () = exhaustivenessCheck (map fst choices, tysubst (solve c)
                                                                 tau) (* OMIT *)
        (* \qbreak                                      *)
        (*                                              *)
        (* Type inference for case expressions and pattern *)
        (* matching                                     *)
        (*                                              *)
        (* Like any other type system, uML's type system *)
        (* guarantees that run-time computations do not *)
        (* ``go wrong.'' uML's type system extends \nml's type *)
        (* system to provide these guarantees:          *)
        (*                                              *)
        (*   • When a value is constructed using [[CONVAL]], the *)
        (*  value constructor in question is applied to an *)
        (*  appropriate number of values of appropriate *)
        (*  types.                                      *)
        (*   • In every pattern, every value constructor is *)
        (*  applied to an appropriate number of sub-patterns *)
        (*  of appropriate types.                       *)
        (*   • In every case expression, every pattern in every *)
        (*  choice has a type consistent with the type of the *)
        (*  scrutinee. And every variable in every pattern is *)
        (*  bound to a value that is consistent with the type *)
        (*  and value of the scrutinee.                 *)
        (*   • In every case expression, the right-hand sides *)
        (*  all have the same type, which is the type of the *)
        (*  case expression.                            *)
        (*                                              *)
        (* As in any type system, these guarantees are provided *)
        (* by a combination of type-formation rules,    *)
        (* introduction rules, and elimination rules. uML uses *)
        (* all of the rules used in \nml, plus rules for *)
        (* constructed data.                            *)
        (*                                              *)
        (*   • As in other languages that support algebraic data *)
        (*  types, type formation is governed by a kinding *)
        (*  system like the one used in Typed uScheme; both *)
        (*  [[implicit-data]] and [[data]] definition forms *)
        (*  specify the kind of each new type.          *)
        (*   • The introduction form for constructed data is the *)
        (*  named value constructor. Its typing rule is just *)
        (*  like the typing rule for a named variable: look *)
        (*  up the type in Gamma.                       *)
        (*   • The elimination form is the case expression, *)
        (*  which includes patterns. The typing rules for *)
        (*  case expressions and patterns are the subject of *)
        (*  this section.                               *)
        (*                                              *)
        (* Like \nml, uML has two sets of typing rules: *)
        (* nondeterministic rules and constraint-based rules. *)
        (*                                              *)
        (* Nondeterministic typing rules for case expressions, *)
        (* choices, and patterns                        *)
        (*                                              *)
        (* [*] uML inherits all the judgment forms and rules *)
        (* from \nml. Its basic nondeterministic judgment form *)
        (* is still \jform[uml.type.exp]\typeise tau. But case *)
        (* expressions and pattern matching call for new *)
        (* judgment forms. The easiest form to explain is the *)
        (* one that deals with a choice within a case   *)
        (* expression; the form of the judgment is \jform\typeis *)
        (* \choicep e tau-->tau'. In this form, type tau is the *)
        (* type of pattern p and type tau' is the type of *)
        (* expression e. Informally, the judgment says that if a *)
        (* case expression is scrutinizing an expression of *)
        (* type tau, and if pattern p matches the value of that *)
        (* expression, then in the context of that match, *)
        (* expression e has type tau'. The judgment is used in *)
        (* the nondeterministic typing rule for a case  *)
        (* expression: \tyrule. Case \twoline \typeise tau \ *)
        (* typeis\choicep_i e_i tau-->tau', 1 <=i <=n \typeis\ *)
        (* xcase(e, \choicep_1 e_1, ..., \choicep_n e_n)tau' *)
        (* Every pattern p_i has the same type as the scrutinee  *)
        (* e, and every right-hand side e_i has type tau', which *)
        (* is the type of the whole case expression.    *)
        (*                                              *)
        (* As in the dynamic semantics, the key judgment is a *)
        (* pattern-matching judgment. \qbreak And also as in the *)
        (* dynamic semantics, pattern matching produces an *)
        (* environment—a type environment, not a value *)
        (* environment. But compared with the dynamic semantics, *)
        (* the type system is more complicated:         *)
        (*                                              *)
        (*   • Type inference produces not only an output *)
        (*  environment Gamma', which gives the types of the *)
        (*  variables that appear in the pattern, but also a *)
        (*  type tau, which is the type of the whole pattern. *)
        (*   • As inputs, the dynamic semantics requires only *)
        (*  the pattern and the value to be matched. \qbreak *)
        (*  In particular, the dynamic semantics requires no *)
        (*  environment. But type inference requires an input *)
        (*  environment, which tells the system the type of *)
        (*  every value constructor that appears in the *)
        (*  pattern.                                    *)
        (*                                              *)
        (* The typing judgment for pattern matching therefore *)
        (* requires inputs p and Gamma and produces outputs tau *)
        (*  and Gamma'. This two-input, two-output judgment form *)
        (* is written \jform[uml.type.pat]\pattypeisp Gamma' tau *)
        (* . The notation is inspired by the notation for *)
        (* typechecking an expression (\cref            *)
        (* adt.ex.pat-exp-type).                        *)
        (*                                              *)
        (* When p is a bare value constructor, it has the type *)
        (* it is given in the input environment, and it produces *)
        (* an empty output environment. \tyrulePatBareVcon \ *)
        (* typeis\avcontau \pattypeis\avcon\emptyenv tau The *)
        (* premise is a typing judgment for the expression \ *)
        (* avcon, which is a value constructor. Just like a *)
        (* value variable, a value constructor is looked up and *)
        (* instantiated: \tyrule. Vcon Gamma(\avcon) = sigma\ *)
        (* qquadtau' <:sigma \typeis\avcontau' A wildcard *)
        (* pattern has any type and produces the empty output *)
        (* environment. \tyrulePatWildcard \pattypeis\ast *)
        (* wildcard \emptyenv tau A variable pattern also has *)
        (* any type, and it produces an output environment that *)
        (* binds itself to its type. \tyrulePatVar \pattypeisx *)
        (* {x |->tau} tau The most important pattern is one that *)
        (* applies a value constructor \avcon to a list of *)
        (* sub-patterns: p = \applyvcon\cdotsmp. The types of *)
        (* the sub-patterns must be the argument types of the *)
        (* value constructor, and the type of the whole pattern *)
        (* is the result type of the value constructor. Each *)
        (* sub-pattern p_i can introduce new variables; \ *)
        (* stdbreak the environment produced by the whole *)
        (* pattern is the disjoint union of the environments *)
        (* produced by the sub-patterns (\cpageref      *)
        (* adt.disjoint-union). If the disjoint union isn't *)
        (* defined, the pattern doesn't typecheck. \tyrule *)
        (* PatVcon \threeline \typeis\avcon\crossdotsmtau-->tau *)
        (* \pattypeisp_i Gamma'_i tau_i, 1 <=i <=m Gamma' = *)
        (* Gamma'_1 \dunion...\dunionGamma'_m \pattypeis\ *)
        (* applyvcon\cdotsmp Gamma' tau                 *)
        (*                                              *)
        (* The judgment for patterns is used in the rule for a *)
        (* choice \choicep e. Typing pattern p produces a set of *)
        (* variable bindings Gamma', and the right-hand side e *)
        (* is checked in a context formed by extending Gamma *)
        (* with Gamma', which holds p's bindings: \tyrule.Choice *)
        (* \twoquad \pattypeisp Gamma' tau \typeis[+Gamma'] e *)
        (* tau' \typeis\choicep e tau-->tau' The \rulenameChoice *)
        (* rule concludes the nondeterministic type theory of *)
        (* case expressions.                            *)
        (*                                              *)
        (* \typesystemuml-constraints \deftyrule Case \fourline *)
        (* \typeisc\tyc_e e tau \typeisc\tyc_i \choicep_i e_i *)
        (* tau_i, 1 <=i <=n \tyc' = \bigwedge_i {tau_i \eqty(tau *)
        (* -->alpha)}, where alpha is fresh \tyc= \tyc_e \land\ *)
        (* tyc' \land\tyc_1 \land...\land\tyc_n \typeisc\tyc\ *)
        (* xcase(e, \choicep_1 e_1, ..., \choicep_n e_n)alpha *)
        (*                                              *)
        (* \deftyrule Vcon \twoline Gamma(\avcon) = \/\ldotsn *)
        (* alpha. tau \ldotsnprimealpha are fresh and distinct \ *)
        (* typeisc\trivc\avcon (\xsubsnalpha_1alpha'_1\subsndots *)
        (* alpha_nalpha'_n) tau                         *)
        (*                                              *)
        (* \deftyruleChoice \twoquad \pattypeisc\tycp Gamma' tau *)
        (* \typeisc[+Gamma'] \tyc' e tau' \typeisc\tyc\land\tyc' *)
        (* \choicep e tau-->tau'                        *)
        (*                                              *)
        (* \deftyrulePatVcon \fourline \typeisc\trivc\avcontau_\ *)
        (* avcon \pattypeisc\tyc_i p_i Gamma'_i tau_i, 1 <=i <=m *)
        (* \tyc= tau_\avcon \eqty\crossdotsmtau-->alpha, where *)
        (* alpha is fresh \twoquad \tyc' = \tyc_1 \land...\land\ *)
        (* tyc_m Gamma' = Gamma'_1 \dunion...\dunionGamma'_m \ *)
        (* pattypeisc\tyc\land\tyc' \applyvcon\cdotsmp Gamma' *)
        (* alpha                                        *)
        (*                                              *)
        (* \deftyrulePatBareVcon \typeisc\trivc\avcontau \ *)
        (* pattypeisc\trivc\avcon\emptyenv tau          *)
        (*                                              *)
        (* \deftyrulePatWildcard alpha is fresh \pattypeisc\ *)
        (* trivc\astwildcard \emptyenv alpha            *)
        (*                                              *)
        (* \deftyrulePatVar alpha is fresh \pattypeisc\trivcx {x *)
        (* |->alpha} alpha                              *)
        (*                                              *)
        (* Constraint-based type inference for case expressions, *)
        (* choices, and patterns                        *)
        (*                                              *)
        (* To turn the nondeterministic rules into an inference *)
        (* algorithm, I introduce constraints. Just as in \cref *)
        (* ml.chap, each occurrence of an unknown type is *)
        (* represented by a fresh type variable, and within each *)
        (* rule, multiple occurrences are constrained to be *)
        (* equal. The rules are shown in \vref          *)
        (* adt.fig.exp-constraint-rules. They are implemented *)
        (* using the same [[ty]] representation used in \nml. *)
        (*                                              *)
        (* \typesystemuml-constraints                   *)
        (*                                              *)
        (* The \rulenameCase rule checks the scrutinee and each *)
        (* choice. \usetyCase The scrutinee judgment \typeisc\ *)
        (* tyc_e e tau is implemented by function [[typeof]] (\ *)
        (* crefml.chap), and the choice judgment \typeisc\tyc_i  *)
        (* \choicep_i e_i tau_i is implemented by function *)
        (* [[choicetype]] (bottom of this page). Each constraint *)
        (* in the set {tau_i \eqty(tau-->alpha)} is built by *)
        (* applying internal function [[constrainArrow]] to tau *)
        (* _i. \umlflabelty [*]                         *)
        (* <boxed values 148>=                          *)
        val _ = op ty         : exp                    -> ty * con
        val _ = op typeof     : exp         * type_env -> ty * con
        val _ = op choicetype : (pat * exp) * type_env -> ty * con
            in  (alpha, c)
            end
        (* Function [[ListPair.unzip]] converts a list of pairs *)
        (* to a pair of lists.                          *)
        (*                                              *)
        (* The rule for a choice infers a type for the pattern *)
        (* and the expression: \usety.Choice \umlflabel *)
        (* choicetype \umlflabeldisjointUnion Function  *)
        (* [[choicetype]] returns a pair containing the arrow *)
        (* type tau-->tau' and the conjoined constraint \tyc\ *)
        (* land\tyc'.                                   *)

        (* Type inference for value constructors        *)
        (*                                              *)
        (* The type of a value constructor is inferred in the *)
        (* same way as the type of a variable: the value *)
        (* constructor's type scheme is instantiated with fresh *)
        (* type variables. [*]                          *)
        (* <more alternatives for [[ty]]>=              *)
        | ty (VCONX vcon) =
            let val tau =
                  freshInstance (findtyscheme (vcon, Gamma))
                  handle NotFound _ => raise TypeError (
                                           "no value constructor named " ^ vcon)
            in  (tau, TRIVIAL)
            end
(* Type inference                               *)
(*                                              *)
(* Type inference builds on constraint solving. *)
(* It comprises two functions: [[typeof]], which *)
(* implements the typing rules for expressions, and *)
(* [[typdef]], which implements the rules for   *)
(* definitions.                                 *)
(*                                              *)
(* Type inference for expressions               *)
(*                                              *)
(* Given an expression e and type environment Gamma, *)
(* function \monoboxtypeof(e, Gamma) returns a pair (tau *)
(* , \tyc) such that \nomathbreak\typeisc\tyce tau. It *)
(* uses internal functions [[typesof]], [[literal]], *)
(* and [[ty]]. \nmlflabeltypeof,ty \makenowebnotdef(left *)
(* as an exercise)                              *)
(* <boxed values 84>=                           *)
val _ = op typeof  : exp      * type_env -> ty      * con
val _ = op typesof : exp list * type_env -> ty list * con
val _ = op literal : value -> ty * con
val _ = op ty      : exp   -> ty * con
  in  ty e
  end
(* <definitions of [[typeof]] and [[typdef]] for \nml\ and \uml>= *)
fun typdef (d, Gamma) =
  case d
    of VAL    (x, e)      =>
                     (* <infer and bind type for [[VAL    (x, e)]] for \nml>= *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | VALREC (x, e)      =>
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

                     (* <infer and bind type for [[VALREC (x, e)]] for \nml>= *)
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
     | EXP e              => typdef (VAL ("it", e), Gamma)
     | DEFINE (x, lambda) => typdef (VALREC (x, LAMBDA lambda), Gamma)
     (* <extra case for [[typdef]] used only in \uml>= *)
     | DATA _ => raise InternalError "DATA reached typdef"
     (* \qbreak                                      *)
     (*                                              *)
     (* Cases and code \chaptocsplitfor Chapter \adtchapnum *)
     (*                                              *)
     (* \Nml is the foundation for uML (\crefadt.chap), which *)
     (* adds cases for pattern matching and algebraic data *)
     (* types. The following code chunks are placeholders for *)
     (* code that is added in \crefadt.chap.         *)
     (* <extra case for [[typdef]] used only in \uml>= *)
     (* filled in when implementing uML *)
(* Typing and type inference for definitions    *)
(*                                              *)
(* A definition extends the top-level type environment. *)
(* Function [[typdef]] infers the type of the thing *)
(* defined, generalizes it to a type scheme, and adds a *)
(* binding to the environment. This step types the *)
(* definition. Function [[typdef]] returns the new type *)
(* environment, plus a string that describes the type *)
(* scheme of the new binding. \nmlflabeltypdef  *)
(* <boxed values 85>=                           *)
val _ = op typdef : def * type_env -> type_env * string
(* <boxed values 266>=                          *)
val _ = op typeof  : exp * type_env -> ty * con
val _ = op typdef : def * type_env -> type_env * string



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UML  *)
(*                                                               *)
(*****************************************************************)

(* The components of the evaluator and read-eval-print *)
(* loop are organized as follows:               *)
(* <evaluation, testing, and the read-eval-print loop for \uml>= *)
(* <definition of [[namedValueString]] for functional bridge languages>= *)
fun namedValueString x v =
  case v of CLOSURE _ => x
          | PRIMITIVE _ => x
          | _ => valueString v
(* <boxed values 57>=                           *)
val _ = op namedValueString : name -> value -> string
(* <definitions of [[match]] and [[Doesn'tMatch]]>= *)
exception Doesn'tMatch    (* pattern-match failure *)
fun match (CONPAT (k, ps), CONVAL (k', vs)) =
     if k = k' then
       disjointUnion (ListPair.mapEq match (ps, vs))
     else
       raise Doesn'tMatch
  | match (CONPAT _, _) = raise Doesn'tMatch
  | match (WILDCARD, _) = emptyEnv
  | match (PVAR x,   v) = bind (x, v, emptyEnv)
(* <boxed values 147>=                          *)
val _ = op match         : pat * value -> value env (* or raises Doesn'tMatch *)
val _ = op disjointUnion : 'a env list -> 'a env
(* If patterns [[ps]] and values [[vs]] were lists of *)
(* different lengths, function [[ListPair.mapEq]] would *)
(* raise an exception, but uML's type system ensures *)
(* that this can't happen.                      *)

(* <definitions of [[eval]] and [[evaldef]] for \nml\ and \uml>= *)
fun eval (e, rho) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let fun ev (LITERAL v)        = v
        | ev (VAR x)            = find (x, rho)
        | ev (IFX (e1, e2, e3)) = ev (if projectBool (ev e1) then e2 else e3)
        | ev (LAMBDA l)         = CLOSURE (l, ref rho)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, embedBool false)
            end
        | ev (APPLY (f, args)) = 
           (case ev f
              of PRIMITIVE prim => prim (map ev args)
               | CLOSURE clo =>
                            (* To apply a closure, the evaluator binds formal *)

                              (* parameters directly to the values of actual  *)

                       (* parameters, not to mutable cells. Environment rho_c *)

                      (* is extended with the formal-parameter bindings using *)

                              (* the [[<+>]] function.                        *)

                              (* <apply closure [[clo]] to [[args]] ((ml))>=  *)
                                let val ((formals, body), ref rho_c) = clo
                                    val actuals = map ev args
                                in  eval (body, rho_c <+> mkEnv (formals,
                                                                       actuals))
                                    handle BindListLength => 
                                        raise BugInTypeInference
                                          "Wrong number of arguments to closure"
                                end
               | _ => raise BugInTypeInference "Applied non-function"
               )
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (CASE (LITERAL v, 
                        (p, e) :: choices)) =
            (let val rho' = match (p, v)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
        (* \newskip\myskip \myskip=6pt                  *)
        (*                                              *)
        (*    Type system    Concept       Interpreter  *)
        (*         d         Definition    def (\cpagerefadt.type.def) *)
        (*         e         Expression    \umltypeexp  *)
        (*         t         Type syntax   \umltypetyex *)
        (*         x         Variable      \mlstypename *)
        (*       \avcon      Value         \umltypevcon *)
        (*                   constructor                *)
        (*         p         Pattern       \umltypepat  *)
        (*    [\myskip] \    Syntactic                  *)
        (*       stycon      type name                  *)
        (*       alpha       Type variable \nmltypetyvar *)
        (*         µ         Type          \umltypetycon *)
        (*                   constructor                *)
        (*     \atyconset    Set of type   Hidden inside \umlfun *)
        (*                   constructors  freshTycon   *)
        (*        tau        Type          \nmltypety   *)
        (*  sigma, \/alpha.  Type scheme   [[type_scheme]] \nmltypepage *)
        (*        tau                      type_scheme  *)
        (*  [\myskip] Gamma  Type          [[type_env]] \nmltypepage *)
        (*                   environment   type_env     *)
        (*   Gamma_1 \uplus  Disjoint      \umlfundisjointUnion *)
        (*      Gamma_2      union                      *)
        (*   Gamma+ Gamma'   Extension     \mlsfun<+>   *)
        (*   [\myskip] \tyc  Constraint    \nmltypecon  *)
        (*  tau_1 \eqtytau_2 Equality      tau_1 [[ ]] tau_2 \ *)
        (*                   constraint    nmlfunpagetilde *)
        (*  \tyc_1 \land\tyc Conjunction   \tyc_1 [[/] \tyc_2 \ *)
        (*         _2                      nmlfunpagecand *)
        (*       \trivc      Trivial       \nmlfunTRIVIAL *)
        (*                   constraint                 *)
        (*     \bigwedge\    Conjunction   \monoconjoinConstraints [\ *)
        (*  nolimits_i \tyc                ldotsn\tyc] \nmlfunpage *)
        (*         _i                      conjoinConstraints *)
        (*    [\myskip] \    Type          \umlfuntxType *)
        (*    txTypet tau\   elaboration                *)
        (*       akind                                  *)
        (*  \vconcompatsigma Type          \umlfunvalidate *)
        (*      µ\akind      compatibility             *)
        (*    [\myskip] \    Type          \monoboxty e = (tau, \tyc) ( *)
        (*  typeisc\tyce tau inference     \chunkrefadt.chunk.ty-case) *)
        (*   \typeisc\tyc\   Type          \monoboxchoicetype((p, e),  *)
        (*  choicep e tau--> inference     Gamma) = \break\qquad\ *)
        (*        tau'                     nomathbreak(tau-->tau', \tyc *)
        (*                                 ) \umlfunpagechoicetype *)
        (*  \pattypeisc\tycp Type          \monoboxpattype(p, Gamma) = *)
        (*     Gamma' tau    inference     (Gamma', tau, \tyc) \ *)
        (*                                 umlfunpagepattype *)
        (*                   Type          \monoboxtypdef(d, Gamma) = \ *)
        (*  [\myskip] \toptd inference     monobox(Gamma', s) \ *)
        (*     -->Gamma'                   nmlfunpagetypdef and also \ *)
        (*                                 umlfuntypeDataDef *)
        (*                                              *)
        (* [*]                                          *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
        (*                                              *)
        (* Theory and \chaptocsplitimplementation \     *)
        (* maintocsplitof \chaptocsplitcase expressions *)
        (*                                              *)
        (* [*] The operational semantics and type theory of *)
        (* pattern matching apply to any language with  *)
        (* pattern-matching case expressions.           *)
        (*                                              *)
        (* Evaluation of case expressions and pattern matching *)
        (*                                              *)
        (* [*] As illustrated in \                      *)
        (* crefadt.case-evaluation-by-example, a case expression *)
        (* is evaluated by trying one choice after another, *)
        (* selecting the first choice whose pattern matches the *)
        (* scrutinee. If an attempt at pattern matching succeeds *)
        (* , it produces an environment rho', which binds the *)
        (* variables that appear in the pattern. If the attempt *)
        (* at pattern matching fails, I say it produces \FAIL *)
        (* (pronounced ``failure,'' but think of a dagger in the *)
        (* heart). The \FAIL is not an environment or a value or *)
        (* an expression or anything we have encountered before; *)
        (* it is a new symbol that means pattern-match failure. *)
        (* To stand for the result of a pattern match, which is *)
        (* either an environment rho' or \FAIL, I use the *)
        (* metavariable \matchresult. The matching judgment *)
        (* therefore takes the form \jform[uml.eval.match]\ *)
        (* patmatchesp v \matchresult:                  *)
        (*                                              *)
        (*  \patmatchesp  Pattern p matches value v,    *)
        (*  v rho'        producing bindings rho';      *)
        (*  \patfailsp v  Pattern p does not match value v. *)
        (*                                              *)
        (* Since patterns are matched only in the context of a *)
        (* case expression, understanding of pattern matching *)
        (* begins with case expressions.                *)
        (*                                              *)
        (* A case expression is evaluated by first evaluating *)
        (* the scrutinee e, which involves no pattern matching. *)
        (* Once e is evaluated to produce a value v, the *)
        (* operational semantics puts v back into the scrutinee *)
        (* position as a literal expression. This trick avoids *)
        (* the need for a special form of judgment that would *)
        (* otherwise try to match v with each choice in turn. *)
        (* The trick is implemented by this rule: \     *)
        (* jlabeluml.eval.exp<e, rho> ==>v \qops. CaseScrutinee *)
        (* \evale ==>v \eval\xcase(\xliteral(v), \achoice[_1], *)
        (* ..., \achoice[_n]) ==>v' \eval\xcase(e, \achoice[_1], *)
        (* ..., \achoice[_n]) ==>v' Once the scrutinee is a *)
        (* literal v, pattern matching can begin.       *)
        (*                                              *)
        (* The value of the scrutinee is always matched against *)
        (* the first pattern p_1. If that match succeeds, *)
        (* producing environment rho', \qbreak the value of the *)
        (* case expression is the value produced by evaluating *)
        (* the corresponding right-hand side e_1 in the extended *)
        (* environment rho+ rho'. \qops CaseMatch \patmatchesp_1 *)
        (* v rho' \eval[+rho'] e_1 ==>v' \eval\xcase(\xliteral *)
        (* (v), \achoice[_1], ..., \achoice[_n]) ==>v' [*] The *)
        (* extension rho+rho' is defined for any two    *)
        (* environments rho and rho'; environment rho is *)
        (* extended by adding rho''s bindings to it. {align*} *)
        (* dom (rho+ rho') --- = dom rho\cupdom rho'    *)
        (* (rho+ rho')(x) --- = {                       *)
        (*                                              *)
        (* rho'(x), if x in dom rho'                    *)
        (* rho(x), if x \notindom rho'                  *)
        (*                                              *)
        (* . {align*} The + operation can also express an *)
        (* environment extended with bindings:          *)
        (*                                              *)
        (*  rho{\ldotsmapstonx v}= rho+ {\ldotsmapstonx v}\ *)
        (*  text.                                       *)
        (*                                              *)
        (* Back to the case expression: what if the first *)
        (* pattern doesn't match? Evaluation continues with the *)
        (* next pattern. The operational semantics drops choice *)
        (* \choicep_1e_1 from the case expression, and  *)
        (* it evaluates a new case expression whose first choice *)
        (* is \choicep_2e_2: \qops. CaseFail \patfailsp_1 v \ *)
        (* eval\xcase(\xliteral(v), \achoice[_2], ..., \achoice *)
        (* [_n]) ==>v' \eval\xcase(\xliteral(v), \achoice[_1], *)
        (* ..., \achoice[_n]) ==>v' What if there are no more *)
        (* choices—that is, what if n=1? Then no rule applies. *)
        (* The operational semantics gets stuck, and the *)
        (* interpreter raises the [[RuntimeError]] exception. *)
        (*                                              *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
        (* \newskip\myskip \myskip=6pt                  *)
        (*                                              *)
        (*     Semantics     Concept     Interpreter    *)
        (*         e         Expression  \umltypeexp    *)
        (*         x         Variable    \mlstypename   *)
        (*       \avcon      Value       \umltypevcon   *)
        (*                   constructor                *)
        (*         p         Pattern     \umltypepat    *)
        (*         v         Value       \umltypevalue  *)
        (*  [\myskip] rho+   Extension   \mlsfun<+>     *)
        (*        rho'                                  *)
        (*  rho_1 \uplusrho  Disjoint    \umlfundisjointUnion *)
        (*         _2        union                      *)
        (*    [\myskip] \    Pattern     match(p, v) = rho' \ *)
        (*   patmatchesp v   matches     umlfunpagematch *)
        (*        rho'                                  *)
        (*    \patfailsp v   Pattern     match(p, v) raises *)
        (*                   match fails [[Doesn'tMatch]] *)
        (*                                              *)
        (* [*]                                          *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)
        (*                                              *)
        (* Each rule is implemented as a clause for function  *)
        (* [[ev]]. To avoid an infinite loop, the clauses for \ *)
        (* xcase(\xliteral(v), cs) must precede the clause for \ *)
        (* xcase(e, cs). The matching judgment is implemented by *)
        (* function [[match]]: if matching succeeds, it returns  *)
        (* rho', and if not, it raises the exception    *)
        (* [[Doesn'tMatch]]. In that case it's time to try the *)
        (* remaining choices. \umlflabelev              *)
        (* <boxed values 146>=                          *)
        val _ = op match : pat * value -> value env
        val _ = op <+>   : 'a env * 'a env -> 'a env
             in  eval (e, rho <+> rho')
             end
             handle Doesn'tMatch => ev (CASE (LITERAL v, choices)))
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (CASE (LITERAL v, [])) =
            raise RuntimeError ("'case' does not match " ^ valueString v)
        (* If the scrutinee [[e]] hasn't yet been evaluated, *)
        (* [[ev]] calls itself recursively to evaluate [[e]], *)
        (* places the resulting value into a [[LITERAL]] *)
        (* expression, then tail-calls itself to select a *)
        (* choice.                                      *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (CASE (e, choices)) =
            ev (CASE (LITERAL (ev e), choices))
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (VCONX vcon) = find (vcon, rho)
        (* \xlet evaluates all right-hand sides in rho, then *)
        (* extends rho to evaluate the body.            *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, rho <+> mkEnv (names, map ev values))
            end
        (* \xletstar evaluates pairs in sequence, adding a *)
        (* binding to rho after each evaluation.        *)
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, eval (e, rho), rho)
            in  eval (body, foldl step rho bs)
            end
        (* <more alternatives for [[ev]] for \nml\ and \uml>= *)
        | ev (LETX (LETREC, bs, body)) =
            let fun asLambda (LAMBDA l) = l
                  | asLambda _ = raise InternalError "parser guaranteed lambda"
                                                                                
                val newref = ref emptyEnv
                val rho' = foldl (fn ((x, e), rho) =>
                                   bind (x, CLOSURE (asLambda e, newref), rho))
                                 rho
                                 bs
                val () = newref := rho'
            in  eval (body, rho')
            end
  in  ev e
  end
(* Evaluation                                   *)
(*                                              *)
(* Evaluation of \rlapheaderexpressions         *)
(*                                              *)
(* Syntactically, the \nml expressions are a subset of *)
(* the micro-Scheme expressions. Therefore, the \nml *)
(* evaluator is almost a subset of the micro-Scheme *)
(* evaluator. But because \nml doesn't have mutation, *)
(* environments map names to values, instead of mapping *)
(* them to mutable cells. And fewer errors should be *)
(* possible at evaluation time, because type inference *)
(* should rule them out. If one of those errors occurs *)
(* anyway, the evaluator raises the exception   *)
(* [[BugInTypeInference]]. \nmlflabeleval       *)
(* <boxed values 274>=                          *)
val _ = op eval : exp * value env -> value
(* <definitions of [[eval]] and [[evaldef]] for \nml\ and \uml>= *)
fun evaldef (VAL (x, e), rho) =
      let val v   = eval (e, rho)
          val rho = bind (x, v, rho)
      in  (rho, namedValueString x v)
      end
  | evaldef (VALREC (f, LAMBDA lambda), rho) =
      let val newref = ref emptyEnv
          val rho = bind (f, CLOSURE (lambda, newref), rho)
          val () = newref := rho
      in  (rho, f)
      end
  | evaldef (VALREC _, rho) =
      raise InternalError "expression in val-rec is not lambda"
  | evaldef (EXP e, rho) = 
      let val v   = eval (e, rho)
          val rho = bind ("it", v, rho)
      in  (rho, valueString v)
      end
(* The implementation of [[VALREC]] works only for *)
(* [[LAMBDA]] expressions because these are the only *)
(* expressions whose value can be computed without *)
(* having the environment.                      *)
(*                                              *)
(* As in the type system, [[DEFINE]] is syntactic sugar *)
(* for a combination of [[VALREC]] and [[LAMBDA]]. *)
(* <definitions of [[eval]] and [[evaldef]] for \nml\ and \uml>= *)
  | evaldef (DEFINE (f, lambda), rho) =
      evaldef (VALREC (f, LAMBDA lambda), rho)
  (* <clause for [[evaldef]] for datatype definition (\uml\ only)>= *)
  | evaldef (DATA _, _) = raise InternalError "DATA reached evaldef"
  (* uML, which is the subject of \crefadt.chap, is like \ *)
  (* nml but with one additional definition form, for *)
  (* defining an algebraic data type. \Nml lacks that *)
  (* form, so the corresponding clause in [[evaldef]] is *)
  (* empty.                                       *)
  (* <clause for [[evaldef]] for datatype definition (\uml\ only)>= *)
  (* code goes here in Appendix S *)
(* Evaluation of definitions                    *)
(*                                              *)
(* Evaluating a definition can produce a new    *)
(* environment. Function [[evaldef]] also returns a *)
(* string that identifies the name or value being *)
(* defined. [*] \nmlflabelevaldef               *)
(* <boxed values 275>=                          *)
val _ = op evaldef : def * value env -> value env * string
(* <definition of [[processDef]] for \uml>=     *)
fun processDef (DATA dd, basis, interactivity) =
      processDataDef (dd, basis, interactivity)
  | processDef (d, (Gamma, Delta, rho), interactivity) =
      let val (Gamma', tystring)  = typdef  (d, Gamma)
          val (rho',   valstring) = evaldef (d, rho)
          val _ =
            if echoes interactivity then
              println (valstring ^ " : " ^ tystring)
            else
              ()
(* <boxed values 153>=                          *)
val _ = op processDef : def * basis * interactivity -> basis
      in  (Gamma', Delta, rho')
      end
fun dump_names (_, _, values) = app (println o fst) values  (*OMIT*)
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


(* <other handlers that catch non-fatal exceptions and pass messages to [[caught]] ((type-inference))>= *)
       | TypeError          msg => caught ("type error <at loc>: " ^ msg)
       | BugInTypeInference msg => caught ("bug in type inference: " ^ msg)
(* <shared unit-testing utilities>=             *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* <boxed values 105>=                          *)
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
(* A transformer can be complemented, turning success *)
(* into failure and vice versa. Transformer \monobox *)
(* notFollowedBy t succeeds if and only if [[t]] fails. *)
(* Transformer \monoboxnotFollowedBy t may look at *)
(* input, but it never consumes any input. This *)
(* transformer is used when trying to read an integer *)
(* literal, to make sure that the digits are not *)
(* followed by a letter or other non-delimiting symbol. *)
(* <definition of [[testIsGood]] for \uml>=     *)
(* <definition of [[skolemTypes]] for languages with generated type constructors>= *)
val skolemTypes =
  streamOfEffects (fn () => SOME (TYCON (freshTycon "skolem type")))
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
(* <boxed values 273>=                          *)
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
fun testIsGood (test, (Gamma, Delta, rho)) =
  let fun ty e = typeof (e, Gamma)
                 handle NotFound x =>
                   raise TypeError ("name " ^ x ^ " is not defined")
      fun ddtystring dd =
        case typeDataDef (dd, Gamma, Delta)
          of (_, _, kind :: _) => kind
           | _ => "???"
      fun deftystring d =
        (case d of DATA dd => ddtystring dd
                 | _ => snd (typdef (d, Gamma)))
        handle NotFound x =>
          raise TypeError ("name " ^ x ^ " is not defined")
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

      fun withTranslatedSigma check form (e, sigmax) =
        check (e, txTyScheme (sigmax, Delta))
        handle TypeError msg =>
          failtest ["In (", form, " ", expString e, " ",
                    tyexString sigmax, "), ", msg]
(* <definition of [[testIsGood]] for \uml>=     *)
      val checkTxTypeChecks =
        withTranslatedSigma (checkTypeChecks "check-type") "check-type"
      val checkTxPtypeChecks =
        withTranslatedSigma (checkTypeChecks "check-principal-type")
                            "check-principal-type"

      fun checks (CHECK_EXPECT (e1, e2))    = checkExpectChecks (e1, e2)
        | checks (CHECK_ASSERT e)           = checkAssertChecks e
        | checks (CHECK_ERROR e)            = checkErrorChecks  e
        | checks (CHECK_TYPE  (e, sigmax))  = checkTxTypeChecks (e, sigmax)
        | checks (CHECK_PTYPE (e, sigmax))  = checkTxPtypeChecks (e, sigmax)
        | checks (CHECK_TYPE_ERROR e)       = true

      fun outcome e =
        withHandlers (fn () => OK (eval (e, rho))) () (ERROR o stripAtLoc)
      (* <[[asSyntacticValue]] for \uml>=             *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue (VCONX c)   = SOME (CONVAL (c, []))
        | asSyntacticValue (APPLY (e, es)) =
            (case (asSyntacticValue e, optionList (map asSyntacticValue es))
               of (SOME (CONVAL (c, [])), SOME vs) => SOME (CONVAL (c, vs))
                | _ => NONE)
        | asSyntacticValue _ = NONE
      (* If a test fails, the syntax of the offending *)
      (* expression is shown, unless it's a syntactic value, *)
      (* in which case the value is shown. In uML, a syntactic *)
      (* value is either a literal or a value constructor *)
      (* applied to zero or more syntactic values.    *)
      (* <boxed values 159>=                          *)
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
      (* <boxed values 101>=                          *)
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
      (* <boxed values 102>=                          *)
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
      (* <boxed values 103>=                          *)
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
      (* <boxed values 104>=                          *)
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
(* \qbreak And a good test has to pass.         *)
(* <definition of [[testIsGood]] for \uml>=     *)
      val checkTxTypePasses =
        withTranslatedSigma checkTypePasses          "check-type"
      val checkTxPtypePasses =
        withTranslatedSigma checkPrincipalTypePasses "check-principal-type"

      fun passes (CHECK_EXPECT (c, e))     = checkExpectPasses    (c, e)
        | passes (CHECK_ASSERT c)          = checkAssertPasses    c
        | passes (CHECK_ERROR c)           = checkErrorPasses     c
        | passes (CHECK_TYPE (c, sigmax))  = checkTxTypePasses    (c, sigmax)
        | passes (CHECK_PTYPE (c, sigmax)) = checkTxPtypePasses   (c, sigmax)
        | passes (CHECK_TYPE_ERROR d)      = checkTypeErrorPasses d

  in  checks test andalso passes test
  end
(* <definition of [[testIsGood]] for \uml>=     *)
fun assertPtype (x, t, (Gamma, Delta, _)) = 
  let val sigma_x = findtyscheme (x, Gamma)
      val sigma   = txTyScheme (t, Delta)
      fun fail ss = raise TypeError (concat ss)
  in  if typeSchemeIsEquivalent (VAR x, sigma_x, sigma) then
        ()
      else
        fail ["In (check-principal-type* ", x, " ", typeSchemeString sigma,
                                                                           "), "
             , x, " has principal type ", typeSchemeString sigma_x]
  end
(* <shared definition of [[processTests]]>=     *)
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
and numberOfGoodTests (tests, rho) =
      let val testIsGood = fn args => (resetComputationLimits (); testIsGood
                                                               args) in (*OMIT*)
      foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
      end
                                                                                
                                                                        (*OMIT*)
(* \qbreak Function [[processTests]] is shared among all *)
(* bridge languages. For each test, it calls the *)
(* language-dependent [[testIsGood]], adds up the number *)
(* of good tests, and reports the result. [*]   *)
(* <boxed values 106>=                          *)
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
            val () = resetComputationLimits ()     (* OMIT *)
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
      (* <boxed values 133>=                          *)
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
(* <boxed values 132>=                          *)
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
(* <boxed values 132>=                          *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
(* The [[Located]] exception is raised by function *)
(* [[atLoc]]. Calling \monoboxatLoc f x applies [[f]] *)
(* to [[x]] within the scope of handlers that convert *)
(* recognized exceptions to the [[Located]] exception: *)

  in  basis
  end



(*****************************************************************)
(*                                                               *)
(*   IMPLEMENTATIONS OF \UML\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* To complete the construction of the initial basis, *)
(* the next step is to add the primitive functions. \ *)
(* makenowebnotdef (from \LApredefined uML functions \ *)
(* upshape[->]\RA)                              *)
(* <implementations of \uml\ primitives and definition of [[initialBasis]]>= *)
(* <shared utility functions for building primitives in languages with type inference>= *)
fun binaryOp f = (fn [a, b] => f (a, b)
                   | _ => raise BugInTypeInference "arity 2")
fun unaryOp  f = (fn [a]    => f  a
                   | _ => raise BugInTypeInference "arity 1")
(* <shared utility functions for building primitives in languages with type inference>= *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeInference "arithmetic on non-numbers")
val arithtype = funtype ([inttype, inttype], inttype)
(* <boxed values 269>=                          *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* <boxed values 269>=                          *)
val _ = op arithOp   : (int * int -> int) -> (value list -> value)
val _ = op arithtype : ty
(* <utility functions for building \nml\ primitives>= *)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeInference "comparing non-numbers")
fun comptype x = funtype ([x, x], booltype)
(* \Nml's comparison primitives take two arguments each. *)
(* Some comparisons apply only to integers. The *)
(* supporting functions reuse [[embedBool]].    *)
(* <boxed values 270>=                          *)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
val _ = op comptype   : ty -> ty
(* The predicates are similar to micro-Scheme   *)
(* predicates. As in micro-Scheme, values of any type *)
(* can be compared for equality. Equality has type alpha *)
(* *alpha-->bool, which gets generalized to type scheme *)
(* \/alpha\alldotalpha*alpha-->bool. As a consequence; \ *)
(* nml's type system permits code to compare functions *)
(* for equality. (If the code is evaluated, it causes a *)
(* checked run-time error.) In Standard ML, by contrast, *)
(* the type system prevents code from comparing values *)
(* of function types.                           *)

val primFunBasis =
  let fun addPrim ((name, prim, tau), (Gamma, Delta, rho)) = 
        ( bindtyscheme (name, generalize (tau, freetyvarsGamma Gamma), Gamma)
        , Delta
        , bind (name, PRIMITIVE prim, rho)
        )
  in  foldl addPrim predefinedTypeBasis (
                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("error", unaryOp (fn v => raise
                                                  RuntimeError (valueString v)),
                                                   funtype ([alpha], beta)) ::

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("read", unaryOp (fn (SYM s) =>
                                                                let val fd =
                                                                 TextIO.openIn s
                                                                      handle _
                                                                              =>
                                                                        raise
                                          RuntimeError ("Cannot read file " ^ s)
                                                                    val sxs =
                                           sxstream (s, filelines fd, noPrompts)
                                                                in  embedList (
                                                               listOfStream sxs)
                                                                    before
                                                               TextIO.closeIn fd
                                                                end
                                                             | _ => raise
                                       BugInTypeInference "read got non-symbol")
                                                , funtype ([symtype], listtype
                                                                     sxtype)) ::

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("+", arithOp op +,   arithtype) :: 
                                         ("-", arithOp op -,   arithtype) :: 
                                         ("*", arithOp op *,   arithtype) :: 
                                         ("/", arithOp op div, arithtype) ::

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("<", intcompare op <,
                                                            comptype inttype) ::
                                         (">", intcompare op >,
                                                            comptype inttype) ::
                                         ("=", comparison primitiveEquality,
                                                              comptype alpha) ::

                          (* \qbreak Primitives [[print]] and [[println]] are *)

                              (* polymorphic.                                 *)

                              (* <primitives for \nml\ and \uml\ [[::]]>=     *)
                                         ("println", unaryOp (fn v => (print (
                                                     valueString v ^ "\n"); v)),
                                                        funtype ([alpha],
                                                                   unittype)) ::
                                         ("print",   unaryOp (fn v => (print (
                                                     valueString v);        v)),
                                                        funtype ([alpha],
                                                                   unittype)) ::
                                         ("printu",  unaryOp (fn NUM n => (
                                                             printUTF8 n; NUM n)
                                                               | _ => raise
                                     BugInTypeInference "printu of non-number"),
                                                        funtype ([inttype],
                                                              unittype)) :: nil)
  end
(* <implementations of \uml\ primitives and definition of [[initialBasis]]>= *)
val initialBasis =
  let val predefinedFuns =
        
         [ ";  <predefined uML functions>=               "
         , "(define null? (xs)"
         , "   (case xs"
         , "      [(cons y ys) #f]"
         , "      ['()         #t]))"
         , ";;unboxuml"
         , ";  A fold function for a binary search tree is part of \\ "
         , ";  crefadt.ex.bst.                              "
         , ";                                               "
         , ";  Data types with just one value constructor each "
         , ";                                               "
         , ";  Most often, an algebraic data type defines more than "
         , ";  one way to form a constructed value—that is, more "
         , ";  than one value constructor. But an algebraic datatype "
         , ";  can usefully have just a single constructor, as shown "
         , ";  by the [[pair]] type: \\umllabelpair          "
         , ";  <predefined uML functions>=               "
         , "(data (* * => *) pair"
         , "  [PAIR : (forall ['a 'b] ('a 'b -> (pair 'a 'b)))])"
         , ";  Now you modify [[pre-cast]] by supplying an identity "
         , ";  function in the right place (\\creftuscheme.ex.capture "
         , ";  again). Use [[flip-apply]] to define a function "
         , ";  [[cast]] of type \\nomathbreak\\/alpha,beta\\alldotalpha "
         , ";  -->beta:                                     "
         , ";  <predefined uML functions>=               "
         , "(val pair PAIR)"
         , "(define fst (p)"
         , "   (case p [(PAIR x _) x]))"
         , "(define snd (p)"
         , "   (case p [(PAIR _ y) y]))"
         , ";  This representation is used in the predefined "
         , ";  function [[Int.compare]]: \\addboxumlInt.compare : "
         , ";  (int int -> order)                           "
         , ";  <predefined uML functions>=               "
         , "(define Int.compare (n1 n2)"
         , "  (if (< n1 n2) LESS"
         , "      (if (< n2 n1) GREATER"
         , "          EQUAL)))"
         , ";;unboxuml"
         , ";  Pattern matching in predefined functions     "
         , ";                                               "
         , ";  In micro-Scheme and \\nml, list values are built in, "
         , ";  so the basic list functions must be built in as "
         , ";  primitives. In uML, the basic list functions are "
         , ";  user-defined. \\addboxumlnull? : (forall ['a] ((list "
         , ";  'a) -> bool)) \\addboxumlcar : (forall ['a] ((list 'a) "
         , ";  -> 'a)) \\addboxumlcdr : (forall ['a] ((list 'a) -> "
         , ";  (list 'a)))                                  "
         , ";  <predefined uML functions>=               "
         , "(define null? (xs)"
         , "   (case xs ['()        #t]"
         , "            [(cons _ _) #f]))"
         , "(define car (xs)"
         , "   (case xs ['()        (error 'car-of-empty-list)]"
         , "            [(cons y _) y]))"
         , "(define cdr (xs)"
         , "   (case xs ['()         (error 'cdr-of-empty-list)]"
         , "            [(cons _ ys) ys]))"
         , ";;unboxuml"
         , ";  <predefined uML functions>=               "
         , "(define append (xs ys)"
         , "  (case xs"
         , "     ['()         ys]"
         , "     [(cons z zs) (cons z (append zs ys))]))"
         , ""
         , "(define revapp (xs ys)"
         , "  (case xs"
         , "     ['()         ys]"
         , "     [(cons z zs) (revapp zs (cons z ys))]))"
         , ";  Function [[bind]] operates on association lists. The "
         , ";  code uses only pattern matching, not [[null?]], "
         , ";  [[car]], or [[cdr]]. I encourage you to compare it "
         , ";  with the \\nml version in \\chunkrefml.chunk.bind. \\ "
         , ";  nwnarrowboxes \\addboxumlbind : (forall ['a 'b] ('a 'b "
         , ";  (list (pair 'a 'b)) -> (list (pair 'a 'b)))) "
         , ";  <predefined uML functions>=               "
         , "(define list1 (x) (cons x '()))"
         , "(define bind (x y alist)"
         , "  (case alist"
         , "     ['() (list1 (pair x y))]"
         , "     [(cons p ps)"
         , "        (if (= x (fst p))"
         , "            (cons (pair x y) ps)"
         , "            (cons p (bind x y ps)))]))"
         , ";;unboxuml"
         , ";  Functions [[find]] and [[bound?]] also improve on "
         , ";  their \\nml versions. When a key is not found, "
         , ";  [[find]] needn't call [[error]]; it can instead "
         , ";  return a value of [[option]] type. Using nested "
         , ";  patterns, [[find]] can be implemented without using "
         , ";  [[fst]] or [[snd]]. \\nwnarrowboxes \\addboxumlfind : "
         , ";  (forall ['a 'b] ('a (list (pair 'a 'b)) -> (option "
         , ";  'b)))                                        "
         , ";  <predefined uML functions>=               "
         , "(define find (x alist)"
         , "  (case alist"
         , "       ['()   NONE]"
         , "       [(cons (PAIR key value) pairs)"
         , "          (if (= x key)"
         , "              (SOME value)"
         , "              (find x pairs))]))"
         , ";;unboxuml"
         , ";  <predefined uML functions>=               "
         , "(define bound? (x alist)"
         , "  (case (find x alist)"
         , "     [(SOME _) #t]"
         , "     [NONE     #f]))"
         , ";;unboxuml"
         , ";  Predefined functions                         "
         , ";                                               "
         , ";  [*] Quite a few predefined functions, including "
         , ";  integer comparison and some list functions, appear in "
         , ";  \\crefadt.chap. The rest are defined here. Some of the "
         , ";  definitions look exactly the same as the     "
         , ";  corresponding definitions in micro-Scheme or \\nml. "
         , ";  <predefined uML functions>=               "
         , "(define and (b c) (if b  c  b))"
         , "(define or  (b c) (if b  b  c))"
         , "(define not (b)   (if b #f #t))"
         , ";  <predefined uML functions>=               "
         , "(define o (f g) (lambda (x) (f (g x))))"
         , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
         , "(define uncurry (f) (lambda (x y) ((f x) y)))"
         , ";  <predefined uML functions>=               "
         , "(define caar (xs) (car (car xs)))"
         , "(define cadr (xs) (car (cdr xs)))"
         , "(define cdar (xs) (cdr (car xs)))"
         , ";  <predefined uML functions>=               "
         , "(define filter (p? xs)"
         , "  (case xs"
         , "     ['()   '()]"
         , "     [(cons y ys)  (if (p? y) (cons y (filter p? ys))"
         , "                              (filter p? ys))]))"
         , ";  <predefined uML functions>=               "
         , "(define map (f xs)"
         , "  (case xs"
         , "     ['() '()]"
         , "     [(cons y ys) (cons (f y) (map f ys))]))"
         , ";  <predefined uML functions>=               "
         , "(define app (f xs)"
         , "  (case xs"
         , "     ['() UNIT]"
         , "     [(cons y ys) (begin (f y) (app f ys))]))"
         , ";  <predefined uML functions>=               "
         , "(define reverse (xs) (revapp xs '()))"
         , ";  <predefined uML functions>=               "
         , "(define exists? (p? xs)"
         , "  (case xs"
         , "     ['() #f]"
         , "     [(cons y ys) (if (p? y) #t (exists? p? ys))]))"
         , "(define all? (p? xs)"
         , "  (case xs"
         , "     ['() #t]"
         , "     [(cons y ys) (if (p? y) (all? p? ys) #f)]))"
         , ";  <predefined uML functions>=               "
         , "(define foldr (op zero xs)"
         , "  (case xs"
         , "     ['() zero]"
         , "     [(cons y ys) (op y (foldr op zero ys))]))"
         , ";  <predefined uML functions>=               "
         , "(define foldl (op zero xs)"
         , "  (case xs"
         , "     ['() zero]"
         , "     [(cons y ys) (foldl op (op y zero) ys)]))"
         , ";  <predefined uML functions>=               "
         , "(define <= (x y) (not (> x y)))"
         , "(define >= (x y) (not (< x y)))"
         , "(define != (x y) (not (= x y)))"
         , ";  <predefined uML functions>=               "
         , "(define max (m n) (if (> m n) m n))"
         , "(define min (m n) (if (< m n) m n))"
         , "(define negated (n) (- 0 n))"
         , "(define mod (m n) (- m (* n (/ m n))))"
         , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
         , "(define lcm (m n) (* m (/ n (gcd m n))))"
         , ";  <predefined uML functions>=               "
         , "(define min* (xs) (foldr min (car xs) (cdr xs)))"
         , "(define max* (xs) (foldr max (car xs) (cdr xs)))"
         , "(define gcd* (xs) (foldr gcd (car xs) (cdr xs)))"
         , "(define lcm* (xs) (foldr lcm (car xs) (cdr xs)))"
         , ";  <predefined uML functions>=               "
         , "(define list1 (x)               (cons x '()))"
         , "(define list2 (x y)             (cons x (list1 y)))"
         , "(define list3 (x y z)           (cons x (list2 y z)))"
         , "(define list4 (x y z a)         (cons x (list3 y z a)))"
         , "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
         , "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
         , "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
         , "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
         , ";  <predefined uML functions>=               "
         , "(define takewhile (p? xs)"
         , "  (case xs"
         , "     ['() '()]"
         , "     [(cons y ys)"
         , "        (if (p? y)"
         , "            (cons y (takewhile p? ys))"
         , "            '())]))"
         , ";  <predefined uML functions>=               "
         , "(define dropwhile (p? xs)"
         , "  (case xs"
         , "     ['() '()]"
         , "     [(cons y ys)"
         , "        (if (p? y)"
         , "            (dropwhile p? ys)"
         , "            xs)]))"
          ]
      val xdefs = stringsxdefs ("predefined functions", predefinedFuns)
  in  readEvalPrintWith predefinedFunctionError
                        (xdefs, primFunBasis, noninteractive)
  end
(* <implementations of \uml\ primitives and definition of [[initialBasis]]>= *)
val primitiveBasis : basis = (* a mockup, but it's the truth *)
  foldl (fn ((name, prim, tau), (Gamma, Delta, rho)) =>
            (Gamma, Delta, bind (name, PRIMITIVE prim, rho)))
        emptyBasis
        ((* <primitives for \nml\ and \uml\ [[::]]>=     *)
         ("error", unaryOp (fn v => raise RuntimeError (valueString v)),
                   funtype ([alpha], beta)) ::
         (* <primitives for \nml\ and \uml\ [[::]]>=     *)
         ("read", unaryOp (fn (SYM s) =>
                                let val fd = TextIO.openIn s
                                      handle _ =>
                                        raise RuntimeError ("Cannot read file "
                                                                            ^ s)
                                    val sxs = sxstream (s, filelines fd,
                                                                      noPrompts)
                                in  embedList (listOfStream sxs)
                                    before TextIO.closeIn fd
                                end
                             | _ => raise BugInTypeInference
                                                          "read got non-symbol")
                , funtype ([symtype], listtype sxtype)) ::
         (* <primitives for \nml\ and \uml\ [[::]]>=     *)
         ("+", arithOp op +,   arithtype) :: 
         ("-", arithOp op -,   arithtype) :: 
         ("*", arithOp op *,   arithtype) :: 
         ("/", arithOp op div, arithtype) ::
         (* <primitives for \nml\ and \uml\ [[::]]>=     *)
         ("<", intcompare op <,              comptype inttype) :: 
         (">", intcompare op >,              comptype inttype) ::
         ("=", comparison primitiveEquality, comptype alpha) ::
         (* \qbreak Primitives [[print]] and [[println]] are *)
         (* polymorphic.                                 *)
         (* <primitives for \nml\ and \uml\ [[::]]>=     *)
         ("println", unaryOp (fn v => (print (valueString v ^ "\n"); v)),
                        funtype ([alpha], unittype)) ::
         ("print",   unaryOp (fn v => (print (valueString v);        v)),
                        funtype ([alpha], unittype)) ::
         ("printu",  unaryOp (fn NUM n => (printUTF8 n; NUM n)
                               | _ => raise BugInTypeInference
                                                        "printu of non-number"),
                        funtype ([inttype], unittype)) :: nil)
val predefs = [] (* not the truth *)


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
(* <boxed values 134>=                          *)
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
(* <boxed values 134>=                          *)
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
(* <boxed values 135>=                          *)
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
(* <boxed values 136>=                          *)
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
(* Now micro-Scheme primitives like [[+]] and [[*]] can *)
(* be defined by applying first [[arithOp]] and then *)
(* [[inExp]] to their ML counterparts.          *)
(*                                              *)
(* The micro-Scheme primitives are organized into a list *)
(* of (name, function) pairs, in Noweb code chunk *)
(* [[]].                                        *)
(* Each primitive on the list has type \monoboxvalue *)
(* list -> value. In \chunkrefmlscheme.inExp-applied, *)
(* each primitive is passed to [[inExp]], and the *)
(* results are used build micro-Scheme's initial *)
(* environment. [Actually, the list contains all the *)
(* primitives except one: the [[error]] primitive, which *)
(* must not be wrapped in [[inExp]].] The list of *)
(* primitives begins with these four elements:  *)
(* <boxed values 137>=                          *)
val _ = op perform: action * string list -> unit
(* <look at command-line arguments, then run>=  *)
fun merge (_, a as FAIL _) = a
  | merge (a as FAIL _, _) = a
  | merge (DEFAULT, a) = a
  | merge (_, DEFAULT) = raise InternalError "DEFAULT on the right in MERGE"
  | merge (RUN_WITH _, right as RUN_WITH _) = right
  | merge (DUMP f, DUMP g) = DUMP (g o f)
  | merge (_, r) = FAIL "Interpret or dump, but don't try to do both"
(* <boxed values 138>=                          *)
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
(* <boxed values 139>=                          *)
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
(* <boxed values 140>=                          *)
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
(* <boxed values 141>=                          *)
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

