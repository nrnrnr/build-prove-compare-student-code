(* mcl.sml S475 *)
exception Unimp of string (* raised if a feature is not implemented *)
fun unimp s = raise Unimp s
fun concatMap f = List.concat o map f  (* List.concatMap is not in Moscow ML *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE CHECKING             *)
(*                                                               *)
(*****************************************************************)

(* exceptions used in languages with type checking S213c *)
exception TypeError of string
exception BugInTypeChecking of string


(*****************************************************************)
(*                                                               *)
(*   \FOOTNOTESIZE SHARED: NAMES, ENVIRONMENTS, STRINGS, ERRORS, PRINTING, INTERACTION, STREAMS, \&\ INITIALIZATION *)
(*                                                               *)
(*****************************************************************)

(* \footnotesize shared: names, environments, strings, errors, printing, interaction, streams, \&\ initialization S213a *)
(* for working with curried functions: [[id]], [[fst]], [[snd]], [[pair]], [[curry]], and [[curry3]] S249b *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* type declarations for consistency checking *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* support for names and environments 303 *)
type name = string
(* support for names and environments 304 *)
type 'a env = (name * 'a) list
(* support for names and environments 305a *)
val emptyEnv = []
(* support for names and environments 305b *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (x, v)::tail) = if name = x then v else find (name, tail)
(* support for names and environments 305c *)
fun isbound (name, []) = false
  | isbound (name, (x, v)::tail) = name = x orelse isbound (name, tail)
(* support for names and environments 305d *)
fun bind (name, v, rho) =
  (name, v) :: rho
(* support for names and environments 305e *)
exception BindListLength
fun bindList (x::vars, v::vals, rho) = bindList (vars, vals, bind (x, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength

fun mkEnv (xs, vs) = bindList (xs, vs, emptyEnv)
(* support for names and environments 305f *)
(* composition *)
infix 6 <+>
fun pairs <+> pairs' = pairs' @ pairs
(* type declarations for consistency checking *)
val _ = op emptyEnv : 'a env
(* type declarations for consistency checking *)
val _ = op find : name * 'a env -> 'a
(* type declarations for consistency checking *)
val _ = op bind : name * 'a * 'a env -> 'a env
(* type declarations for consistency checking *)
val _ = op bindList : name list * 'a list * 'a env -> 'a env
val _ = op mkEnv    : name list * 'a list -> 'a env
(* type declarations for consistency checking *)
val _ = op <+> : 'a env * 'a env -> 'a env
(* support for names and environments S219d *)
fun duplicatename [] = NONE
  | duplicatename (x::xs) =
      if List.exists (fn x' => x' = x) xs then
        SOME x
      else
        duplicatename xs
(* type declarations for consistency checking *)
val _ = op duplicatename : name list -> name option
(* support for names and environments S453b *)
exception DisjointUnionFailed of name
fun disjointUnion envs =
  let val env = List.concat envs
  in  case duplicatename (map fst env)
        of NONE => env
         | SOME x => raise DisjointUnionFailed x
  end
(* type declarations for consistency checking *)
val _ = op disjointUnion : 'a env list -> 'a env
(* exceptions used in every interpreter S213b *)
exception RuntimeError of string (* error message *)
exception LeftAsExercise of string (* string identifying code *)
(* support for detecting and signaling errors detected at run time S219e *)
exception InternalError of string (* bug in the interpreter *)
(* list functions not provided by \sml's initial basis S219a *)
fun unzip3 [] = ([], [], [])
  | unzip3 (trip::trips) =
      let val (x,  y,  z)  = trip
          val (xs, ys, zs) = unzip3 trips
      in  (x::xs, y::ys, z::zs)
      end

fun zip3 ([], [], []) = []
  | zip3 (x::xs, y::ys, z::zs) = (x, y, z) :: zip3 (xs, ys, zs)
  | zip3 _ = raise ListPair.UnequalLengths
(* type declarations for consistency checking *)
val _ = op unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val _ = op zip3   : 'a list * 'b list * 'c list -> ('a * 'b * 'c) list
(* list functions not provided by \sml's initial basis S219b *)
val reverse = rev
(* list functions not provided by \sml's initial basis S219c *)
fun optionList [] = SOME []
  | optionList (NONE :: _) = NONE
  | optionList (SOME x :: rest) =
      (case optionList rest
         of SOME xs => SOME (x :: xs)
          | NONE    => NONE)
(* type declarations for consistency checking *)
val _ = op optionList : 'a option list -> 'a list option
(* utility functions for string manipulation and printing S275a *)
val lower = String.map Char.toLower
val upper = String.map Char.toUpper
(* utility functions for string manipulation and printing S214c *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)
(* type declarations for consistency checking *)
val _ = op intString : int -> string
(* utility functions for string manipulation and printing S214d *)
fun plural what [x] = what
  | plural what _   = what ^ "s"

fun countString xs what =
  intString (length xs) ^ " " ^ plural what xs
(* utility functions for string manipulation and printing S214e *)
val spaceSep = String.concatWith " "   (* list separated by spaces *)
val commaSep = String.concatWith ", "  (* list separated by commas *)
(* type declarations for consistency checking *)
val _ = op spaceSep : string list -> string
val _ = op commaSep : string list -> string
(* utility functions for string manipulation and printing S214f *)
fun nullOrCommaSep empty [] = empty
  | nullOrCommaSep _     ss = commaSep ss                   
(* type declarations for consistency checking *)
val _ = op nullOrCommaSep : string -> string list -> string
(* utility functions for string manipulation and printing S215a *)
fun fnvHash s =
  let val offset_basis = 0wx011C9DC5 : Word.word  (* trim the high bit *)
      val fnv_prime    = 0w16777619  : Word.word
      fun update (c, hash) = Word.xorb (hash, Word.fromInt (ord c)) * fnv_prime
      fun int w =
        Word.toIntX w handle Overflow => Word.toInt (Word.andb (w, 0wxffffff))
  in  int (foldl update offset_basis (explode s))
  end
(* type declarations for consistency checking *)
val _ = op fnvHash : string -> int
(* utility functions for string manipulation and printing S215b *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* utility functions for string manipulation and printing S215c *)
fun predefinedFunctionError s =
  eprintln ("while reading predefined functions, " ^ s)
(* utility functions for string manipulation and printing S215d *)
val xprinter = ref print
fun xprint   s = !xprinter s
fun xprintln s = (xprint s; xprint "\n")
(* utility functions for string manipulation and printing S216a *)
fun tryFinally f x post =
  (f x handle e => (post (); raise e)) before post ()

fun withXprinter xp f x =
  let val oxp = !xprinter
      val ()  = xprinter := xp
  in  tryFinally f x (fn () => xprinter := oxp)
  end
(* type declarations for consistency checking *)
val _ = op withXprinter : (string -> unit) -> ('a -> 'b) -> ('a -> 'b)
val _ = op tryFinally   : ('a -> 'b) -> 'a -> (unit -> unit) -> 'b
(* utility functions for string manipulation and printing S216b *)
fun bprinter () =
  let val buffer = ref []
      fun bprint s = buffer := s :: !buffer
      fun contents () = concat (rev (!buffer))
  in  (bprint, contents)
  end
(* utility functions for string manipulation and printing S216c *)
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
(* utility functions for string manipulation and printing S217a *)
fun stripNumericSuffix s =
      let fun stripPrefix []         = s   (* don't let things get empty *)
            | stripPrefix (#"-"::[]) = s
            | stripPrefix (#"-"::cs) = implode (reverse cs)
            | stripPrefix (c   ::cs) = if Char.isDigit c then stripPrefix cs
                                       else implode (reverse (c::cs))
      in  stripPrefix (reverse (explode s))
      end
(* support for representing errors as \ml\ values S221b *)
datatype 'a error = OK of 'a | ERROR of string
(* support for representing errors as \ml\ values S222a *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* type declarations for consistency checking *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* support for representing errors as \ml\ values S222b *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= (OK o k')
(* type declarations for consistency checking *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* support for representing errors as \ml\ values S223a *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* type declarations for consistency checking *)
val _ = op errorList : 'a error list -> 'a list error
(* support for representing errors as \ml\ values S223b *)
fun errorLabel s (OK x) = OK x
  | errorLabel s (ERROR msg) = ERROR (s ^ msg)
(* type [[interactivity]] plus related functions and value S236a *)
datatype input_interactivity = PROMPTING | NOT_PROMPTING
(* type [[interactivity]] plus related functions and value S236b *)
datatype output_interactivity = ECHOING | NOT_ECHOING
(* type [[interactivity]] plus related functions and value S236c *)
type interactivity = 
  input_interactivity * output_interactivity
val noninteractive = 
  (NOT_PROMPTING, NOT_ECHOING)
fun prompts (PROMPTING,     _) = true
  | prompts (NOT_PROMPTING, _) = false
fun echoes (_, ECHOING)     = true
  | echoes (_, NOT_ECHOING) = false
(* type declarations for consistency checking *)
type interactivity = interactivity
val _ = op noninteractive : interactivity
val _ = op prompts : interactivity -> bool
val _ = op echoes  : interactivity -> bool
(* simple implementations of set operations S217b *)
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
(* type declarations for consistency checking *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* ability to interrogate environment variable [[BPCOPTIONS]] S220a *)
local
  val split = String.tokens (fn c => c = #",")
  val optionTokens =
    getOpt (Option.map split (OS.Process.getEnv "BPCOPTIONS"), [])
in
  fun hasOption s = member s optionTokens
(* type declarations for consistency checking *)
val _ = op hasOption : string -> bool
end
(* collections with mapping and combining functions S218a *)
datatype 'a collection = C of 'a set
fun elemsC (C xs) = xs
fun singleC x     = C [x]
val emptyC        = C []
(* type declarations for consistency checking *)
type 'a collection = 'a collection
val _ = op elemsC  : 'a collection -> 'a set
val _ = op singleC : 'a -> 'a collection
val _ = op emptyC  :       'a collection
(* collections with mapping and combining functions S218b *)
fun joinC     (C xs) = C (List.concat (map elemsC xs))
fun mapC  f   (C xs) = C (map f xs)
fun filterC p (C xs) = C (List.filter p xs)
fun mapC2 f (xc, yc) = joinC (mapC (fn x => mapC (fn y => f (x, y)) yc) xc)
(* type declarations for consistency checking *)
val _ = op joinC   : 'a collection collection -> 'a collection
val _ = op mapC    : ('a -> 'b)      -> ('a collection -> 'b collection)
val _ = op filterC : ('a -> bool)    -> ('a collection -> 'a collection)
val _ = op mapC2   : ('a * 'b -> 'c) -> ('a collection * 'b collection -> 'c
                                                                     collection)
(* suspensions S228a *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref
(* type declarations for consistency checking *)
type 'a susp = 'a susp
(* suspensions S228b *)
fun delay f = ref (PENDING f)
fun demand cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* type declarations for consistency checking *)
val _ = op delay  : (unit -> 'a) -> 'a susp
val _ = op demand : 'a susp -> 'a
(* streams S228c *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* streams S229a *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (demand s)
(* streams S229b *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* type declarations for consistency checking *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* type declarations for consistency checking *)
val _ = op streamOfList : 'a list -> 'a stream
(* streams S229c *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* streams S229d *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* type declarations for consistency checking *)
val _ = op listOfStream : 'a stream -> 'a list
(* type declarations for consistency checking *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* streams S229e *)
fun streamOfEffects action =
  delayedStream (fn () => case action ()
                            of NONE   => EOS
                             | SOME a => a ::: streamOfEffects action)
(* type declarations for consistency checking *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* streams S229f *)
type line = string
fun filelines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* type declarations for consistency checking *)
type line = line
val _ = op filelines : TextIO.instream -> line stream
(* streams S230a *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* type declarations for consistency checking *)
val _ = op streamRepeat : 'a -> 'a stream
(* streams S230b *)
fun streamOfUnfold next state =
  delayedStream
    (fn () => case next state
                of NONE => EOS
                 | SOME (a, state') => a ::: streamOfUnfold next state')
(* type declarations for consistency checking *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* streams S230c *)
val naturals = 
  streamOfUnfold (fn n => SOME (n, n+1)) 0   (* 0 to infinity *)
(* type declarations for consistency checking *)
val _ = op naturals : int stream
(* streams S231a *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* streams S231b *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* type declarations for consistency checking *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* type declarations for consistency checking *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* streams S231c *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* streams S231d *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* type declarations for consistency checking *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* streams S231e *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* type declarations for consistency checking *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b
(* streams S231f *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* streams S232a *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* type declarations for consistency checking *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* type declarations for consistency checking *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* streams S232b *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* streams S232c *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* type declarations for consistency checking *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* streams S232d *)
fun streamTake (0, xs) = []
  | streamTake (n, xs) =
      case streamGet xs
        of SOME (x, xs) => x :: streamTake (n-1, xs)
         | NONE => []
(* type declarations for consistency checking *)
val _ = op streamTake : int * 'a stream -> 'a list
(* streams S232e *)
fun streamDrop (0, xs) = xs
  | streamDrop (n, xs) =
      case streamGet xs
        of SOME (_, xs) => streamDrop (n-1, xs)
         | NONE => EOS
(* type declarations for consistency checking *)
val _ = op streamDrop : int * 'a stream -> 'a stream
(* stream transformers and their combinators S246b *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* type declarations for consistency checking *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* stream transformers and their combinators S247a *)
fun pure y = fn xs => SOME (OK y, xs)
(* type declarations for consistency checking *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* stream transformers and their combinators S247b *)
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
(* type declarations for consistency checking *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S249a *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* type declarations for consistency checking *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S249c *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* type declarations for consistency checking *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S249d *)
fun pzero _ = NONE
(* stream transformers and their combinators S249e *)
fun anyParser ts = 
  foldr op <|> pzero ts
(* type declarations for consistency checking *)
val _ = op pzero : ('a, 'b) xformer
(* type declarations for consistency checking *)
val _ = op anyParser : ('a, 'b) xformer list -> ('a, 'b) xformer
(* stream transformers and their combinators S250a *)
infix 6 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* type declarations for consistency checking *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S250b *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* type declarations for consistency checking *)
val _ = op one : ('a, 'a) xformer
(* stream transformers and their combinators S250c *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op eos : ('a, unit) xformer
(* stream transformers and their combinators S251a *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* type declarations for consistency checking *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* stream transformers and their combinators S251b *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* type declarations for consistency checking *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S251c *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* type declarations for consistency checking *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* stream transformers and their combinators S252a *)
fun eqx y = 
  sat (fn y' => y = y') 
(* type declarations for consistency checking *)
val _ = op eqx : ''b -> ('a, ''b) xformer -> ('a, ''b) xformer
(* stream transformers and their combinators S252b *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* type declarations for consistency checking *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S252c *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* type declarations for consistency checking *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* stream transformers and their combinators S252d *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* stream transformers and their combinators S253a *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* type declarations for consistency checking *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators S253b *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* type declarations for consistency checking *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* stream transformers and their combinators S253c *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* type declarations for consistency checking *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* stream transformers and their combinators S254a *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* type declarations for consistency checking *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)                 * ('a, 'b) xformer -> ('a, 'c
                                                                       ) xformer
(* support for source-code locations and located streams S233b *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ intString line
(* support for source-code locations and located streams S233c *)
datatype error_format = WITH_LOCATIONS | WITHOUT_LOCATIONS
val toplevel_error_format = ref WITH_LOCATIONS
(* support for source-code locations and located streams S233d *)
fun synerrormsg (source, line) strings =
  if !toplevel_error_format = WITHOUT_LOCATIONS
  andalso source = "standard input"
  then
    concat ("syntax error: " :: strings)
  else    
    concat ("syntax error in " :: srclocString (source, line) :: ": " :: strings
                                                                               )

(* support for source-code locations and located streams S234a *)
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
(* support for source-code locations and located streams S234b *)
exception Located of srcloc * exn
(* support for source-code locations and located streams S234c *)
type 'a located = srcloc * 'a
(* type declarations for consistency checking *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* type declarations for consistency checking *)
type 'a located = 'a located
(* support for source-code locations and located streams S234d *)
fun atLoc loc f a =
  f a handle e as RuntimeError _ => raise Located (loc, e)
           | e as NotFound _     => raise Located (loc, e)
           (* more handlers for [[atLoc]] S234e *)
           | e as IO.Io _   => raise Located (loc, e)
           | e as Div       => raise Located (loc, e)
           | e as Overflow  => raise Located (loc, e)
           | e as Subscript => raise Located (loc, e)
           | e as Size      => raise Located (loc, e)
           (* more handlers for [[atLoc]] ((type-checking)) S392d *)
           | e as TypeError _         => raise Located (loc, e)
           | e as BugInTypeChecking _ => raise Located (loc, e)
(* type declarations for consistency checking *)
val _ = op atLoc : srcloc -> ('a -> 'b) -> ('a -> 'b)
(* support for source-code locations and located streams S234f *)
fun located f (loc, a) = atLoc loc f a
fun leftLocated f ((loc, a), b) = atLoc loc f (a, b)
(* type declarations for consistency checking *)
val _ = op located : ('a -> 'b) -> ('a located -> 'b)
val _ = op leftLocated : ('a * 'b -> 'c) -> ('a located * 'b -> 'c)
(* support for source-code locations and located streams S235a *)
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
(* type declarations for consistency checking *)
val _ = op fillComplaintTemplate : string * srcloc option -> string
(* support for source-code locations and located streams S235b *)
fun synerrorAt msg loc = 
  ERROR (synerrormsg loc [msg])
(* support for source-code locations and located streams S235c *)
fun locatedStream (streamname, inputs) =
  let val locations =
        streamZip (streamRepeat streamname, streamDrop (1, naturals))
  in  streamZip (locations, inputs)
  end
(* type declarations for consistency checking *)
val _ = op synerrorAt : string -> srcloc -> 'a error
(* type declarations for consistency checking *)
val _ = op locatedStream : string * line stream -> line located stream
(* streams that track line boundaries S258 *)
datatype 'a eol_marked
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (demand s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(* streams that track line boundaries S259a *)
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
(* type declarations for consistency checking *)
type 'a eol_marked = 'a eol_marked
val _ = op drainLine : 'a eol_marked stream -> 'a eol_marked stream
(* type declarations for consistency checking *)
val _ = op eol      : ('a eol_marked, int) xformer
val _ = op inline   : ('a eol_marked, 'a)  xformer
val _ = op srcloc   : ('a located eol_marked, srcloc) xformer
(* support for lexical analysis S254b *)
type 'a lexer = (char, 'a) xformer
(* type declarations for consistency checking *)
type 'a lexer = 'a lexer
(* support for lexical analysis S254c *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "()[]{};" c
(* type declarations for consistency checking *)
val _ = op isDelim : char -> bool
(* support for lexical analysis S254d *)
val whitespace = many (sat Char.isSpace one)
(* type declarations for consistency checking *)
val _ = op whitespace : char list lexer
(* support for lexical analysis S256a *)
fun intChars isDelim = 
  (curry (op ::) <$> eqx #"-" one <|> pure id) <*>
  many1 (sat Char.isDigit one) <* 
  notFollowedBy (sat (not o isDelim) one)
(* type declarations for consistency checking *)
val _ = op intChars : (char -> bool) -> char list lexer
(* support for lexical analysis S256b *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow =>
        ERROR "this interpreter can't read arbitrarily large integers"
(* type declarations for consistency checking *)
val _ = op intFromChars : char list -> int error
(* support for lexical analysis S256c *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* type declarations for consistency checking *)
val _ = op intToken : (char -> bool) -> int lexer
(* support for lexical analysis S256d *)
datatype bracket_shape = ROUND | SQUARE | CURLY
(* support for lexical analysis S257a *)
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
(* type declarations for consistency checking *)
type 'a plus_brackets = 'a plus_brackets
val _ = op bracketLexer : 'a lexer -> 'a plus_brackets lexer
(* support for lexical analysis S257b *)
fun leftString ROUND  = "("
  | leftString SQUARE = "["
  | leftString CURLY  = "{"
fun rightString ROUND  = ")"
  | rightString SQUARE = "]"
  | rightString CURLY = "}"

fun plusBracketsString _   (LEFT shape)  = leftString shape
  | plusBracketsString _   (RIGHT shape) = rightString shape
  | plusBracketsString pts (PRETOKEN pt)  = pts pt
(* type declarations for consistency checking *)
val _ = op plusBracketsString : ('a -> string) -> ('a plus_brackets -> string)
(* common parsing code S246a *)
(* combinators and utilities for parsing located streams S259b *)
type ('t, 'a) polyparser = ('t located eol_marked, 'a) xformer
(* combinators and utilities for parsing located streams S259c *)
fun token    stream = (snd <$> inline)      stream
fun noTokens stream = (notFollowedBy token) stream
(* type declarations for consistency checking *)
val _ = op token    : ('t, 't)   polyparser
val _ = op noTokens : ('t, unit) polyparser
(* combinators and utilities for parsing located streams S260a *)
fun @@ p = pair <$> srcloc <*> p
(* type declarations for consistency checking *)
val _ = op @@ : ('t, 'a) polyparser -> ('t, 'a located) polyparser
(* combinators and utilities for parsing located streams S260b *)
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
(* combinators and utilities for parsing located streams S260c *)
infix 0 <?>
fun p <?> what = p <|> synerrorAt ("expected " ^ what) <$>! srcloc
(* type declarations for consistency checking *)
val _ = op asAscii : ('t, string) polyparser -> ('t, string) polyparser
(* type declarations for consistency checking *)
val _ = op <?> : ('t, 'a) polyparser * string -> ('t, 'a) polyparser
(* combinators and utilities for parsing located streams S261a *)
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
(* type declarations for consistency checking *)
val _ = op <!> : ('t, 'a) polyparser * string -> ('t, 'b) polyparser
(* combinators and utilities for parsing located streams S265a *)
fun nodups (what, context) (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) =
            if List.exists (fn y => y = x) xs then
              synerrorAt (what ^ " " ^ x ^ " appears twice in " ^ context) loc
            else
              dup xs
  in  dup names
  end
(* type declarations for consistency checking *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* combinators and utilities for parsing located streams S265b *)
fun rejectReserved reserved x =
  if member x reserved then
    ERROR ("syntax error: " ^ x ^ " is a reserved word and " ^
           "may not be used to name a variable or function")
  else
    OK x
(* type declarations for consistency checking *)
val _ = op rejectReserved : name list -> name -> name error
(* transformers for interchangeable brackets S261c *)
fun left  tokens = ((fn (loc, LEFT  s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun right tokens = ((fn (loc, RIGHT s) => SOME (loc, s) | _ => NONE) <$>? inline
                                                                        ) tokens
fun pretoken stream = ((fn PRETOKEN t => SOME t | _ => NONE) <$>? token) stream
(* type declarations for consistency checking *)
val _ = op left  : ('t plus_brackets, bracket_shape located) polyparser
val _ = op right : ('t plus_brackets, bracket_shape located) polyparser
val _ = op pretoken : ('t plus_brackets, 't) polyparser
(* transformers for interchangeable brackets S262a *)
fun badRight msg =
  (fn (loc, shape) => synerrorAt (msg ^ " " ^ rightString shape) loc) <$>! right
(* type declarations for consistency checking *)
val _ = op badRight : string -> ('t plus_brackets, 'a) polyparser
(* transformers for interchangeable brackets S262b *)
fun notCurly (_, CURLY) = false
  | notCurly _          = true
fun leftCurly tokens = sat (not o notCurly) left tokens
(* transformers for interchangeable brackets S262c *)
(* definition of function [[errorAtEnd]] S261b *)
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
(* type declarations for consistency checking *)
val _ = op errorAtEnd : ('t plus_brackets, 'a) polyparser * ('a -> string list)
                                            -> ('t plus_brackets, 'b) polyparser
(* transformers for interchangeable brackets S262d *)
datatype right_result
  = FOUND_RIGHT      of bracket_shape located
  | SCANNED_TO_RIGHT of srcloc  (* location where scanning started *)
  | NO_RIGHT
(* type declarations for consistency checking *)
val _ = op notCurly  : bracket_shape located -> bool
val _ = op leftCurly : ('t plus_brackets, bracket_shape located) polyparser
(* type declarations for consistency checking *)
type right_result = right_result
(* transformers for interchangeable brackets S263a *)
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
(* type declarations for consistency checking *)
val _ = op matchingRight : ('t, right_result) pb_parser
(* transformers for interchangeable brackets S263b *)
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
(* type declarations for consistency checking *)
val _ = op matchBrackets : string -> bracket_shape located -> 'a -> right_result
                                                                     -> 'a error
(* transformers for interchangeable brackets S264a *)

fun liberalBracket (expected, p) =
  matchBrackets expected <$> sat notCurly left <*> p <*>! matchingRight
fun bracket (expected, p) =
  liberalBracket (expected, p <?> expected)
fun curlyBracket (expected, p) =
  matchBrackets expected <$> leftCurly <*> (p <?> expected) <*>! matchingRight
fun bracketKeyword (keyword, expected, p) =
  liberalBracket (expected, keyword *> (p <?> expected))
(* type declarations for consistency checking *)
val _ = op liberalBracket : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op bracket           : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
val _ = op curlyBracket    : string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* type declarations for consistency checking *)
val _ = op bracketKeyword : ('t, 'keyword) pb_parser * string * ('t, 'a)
                                                 pb_parser -> ('t, 'a) pb_parser
(* transformers for interchangeable brackets S264b *)
fun usageParser keyword =
  let val left = eqx #"(" one <|> eqx #"[" one
      val getkeyword = left *> (implode <$> many1 (sat (not o isDelim) one))
  in  fn (usage, p) =>
        case getkeyword (streamOfList (explode usage))
          of SOME (OK k, _) => bracketKeyword (keyword k, usage, p)
           | _ => raise InternalError ("malformed usage string: " ^ usage)
  end
(* type declarations for consistency checking *)
val _ = op usageParser : (string -> ('t, string) pb_parser) ->
                               string * ('t, 'a) pb_parser -> ('t, 'a) pb_parser
(* code used to debug parsers S265c *)
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
(* type declarations for consistency checking *)
val _ = op safeTokens : 'a located eol_marked stream -> 'a list
(* code used to debug parsers S266a *)
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
(* type declarations for consistency checking *)
val _ = op showErrorInput : ('t -> string) -> ('t, 'a) polyparser -> ('t, 'a)
                                                                      polyparser
(* code used to debug parsers S266b *)
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
(* type declarations for consistency checking *)
val _ = op wrapAround : ('t -> string) -> string -> ('t, 'a) polyparser -> ('t,
                                                                  'a) polyparser
(* streams that issue two forms of prompts S267a *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* type declarations for consistency checking *)
val _ = op echoTagStream : line stream -> line stream 
(* streams that issue two forms of prompts S267b *)
fun stripAndReportErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (eprintln msg; next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* type declarations for consistency checking *)
val _ = op stripAndReportErrors : 'a error stream -> 'a stream
(* streams that issue two forms of prompts S267c *)
fun lexLineWith lexer =
  stripAndReportErrors o streamOfUnfold lexer o streamOfList o explode
(* type declarations for consistency checking *)
val _ = op lexLineWith : 't lexer -> line -> 't stream
(* streams that issue two forms of prompts S267d *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) =
            SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* type declarations for consistency checking *)
val _ = op parseWithErrors : ('t, 'a) polyparser ->                     't
                                    located eol_marked stream -> 'a error stream
(* streams that issue two forms of prompts S268 *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* type declarations for consistency checking *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* streams that issue two forms of prompts S269 *)
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
(* type declarations for consistency checking *)
val _ = op interactiveParsedStream : 't lexer * ('t, 'a) polyparser ->
                                     string * line stream * prompts -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> 't located eol_marked stream
  in  
      stripAndReportErrors (preStream (setPrompt ps1, xdefs_with_errors))
  end 
(* common parsing code ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
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
(* shared utility functions for initializing interpreters S240a *)
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
(* function application with overflow checking S220b *)
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
(* function application with overflow checking S221a *)
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

(* prettyprinting combinators S277a *)
(* definition of [[doc]] and functions S285a *)
datatype doc 
  = ^^     of doc * doc
  | TEXT   of string
  | BREAK  of string
  | INDENT of int * doc
  | GROUP  of break_line or_auto * doc
(* definition of [[doc]] and functions S285b *)
and break_line
  = NO      (* hgrp -- every break is a space *)
  | YES     (* vgrp -- every break is a newline *)
  | MAYBE   (* fgrp -- paragraph fill (break is newline only when needed) *)
and 'a or_auto
  = AUTO    (* agrp -- NO if the whole group fits; otherwise YES *)
  | B of 'a
(* definition of [[doc]] and functions S285c *)
val doc    = TEXT
val brk    = BREAK " "
val indent = INDENT
val empty  = TEXT ""
infix 2 ^^

fun hgrp d = GROUP (B NO,    d)
fun vgrp d = GROUP (B YES,   d)
fun agrp d = GROUP (  AUTO,  d)
fun fgrp d = GROUP (B MAYBE, d)
(* definition of [[doc]] and functions S286a *)

fun format w k [] = []
  | format w k (tagged_doc :: z) = 
      let fun reformat item = format w k (item::z)
          fun copyChar 0 c = []
            | copyChar n c = c :: copyChar (n - 1) c
          fun addString s = s :: format w (k + size s) z
          fun breakAndIndent i =
                implode (#"\n" :: copyChar i #" ") :: format w i z
(* type declarations for consistency checking *)
val _ = op format : int -> int -> (int * break_line * doc) list -> string list
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
(* definition of [[doc]] and functions S286b *)
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
(* type declarations for consistency checking *)
val _ = op fits : int * (int * break_line * doc) list -> bool
(* definition of [[doc]] and functions S287 *)
fun layout w doc = concat (format w 0 [(0, NO, GROUP (AUTO, doc))])
infix 2 ^/
fun l ^/ r = l ^^ brk ^^ r
fun addBrk d = d ^^ brk
val semi = doc ";"
fun addSemi d = d ^^ semi
(* type declarations for consistency checking *)
type doc = doc
val _ = op doc    : string -> doc
val _ = op ^^     : doc * doc -> doc
val _ = op empty  : doc
val _ = op indent : int * doc -> doc
val _ = op brk    : doc
(* type declarations for consistency checking *)
val _ = op layout : int -> doc -> string
(* type declarations for consistency checking *)
val _ = op vgrp    : doc -> doc
val _ = op hgrp    : doc -> doc
val _ = op agrp    : doc -> doc
val _ = op fgrp    : doc -> doc
val _ = op ^/      : doc * doc -> doc
val _ = op addBrk  : doc -> doc
val _ = op semi    : doc
val _ = op addSemi : doc -> doc
(* prettyprinting combinators S526d *)
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

(* abstract syntax and values for \mcl S478c *)
(* paths for \mcl S476a *)
type modcon = { printName : name, serial : int }
datatype modident = MODCON of modcon | MODTYPLACEHOLDER of name

(* definition of function [[genmodident]] S476c *)
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
(* type declarations for consistency checking *)
val _ = op genmodident : name -> modident
(* paths for \mcl S476b *)
datatype 'modname path' = PNAME of 'modname
                        | PDOT of 'modname path' * name
                        | PAPPLY of 'modname path' * 'modname path' list

type pathex = name located path'
type path   = modident path'
(* definition of [[ty]] for \mcl S477a *)
datatype 'modname ty' = TYNAME of 'modname path'
                      | FUNTY  of 'modname ty' list * 'modname ty'
                      | ANYTYPE   (* type of (error ...) *)
type tyex = name located ty'
type ty   = modident ty'
(* definition of [[modty]] for \mcl S477d *)
datatype modty
  = MTEXPORTS of (name * component) list
  | MTARROW   of (modident * modty) list * modty
  | MTALLOF   of modty list
and component
  = COMPVAL    of ty
  | COMPMANTY  of ty
  | COMPABSTY  of path
  | COMPMOD    of modty
(* definition of [[modty]] for \mcl S477e *)
type 'a rooted = 'a * path
fun root (_, path) = path
fun rootedMap f (a, path) = (f a, path)
(* definition of [[modty]] for \mcl S478a *)
type print_string = string
datatype binding
  = ENVVAL    of ty
  | ENVMANTY  of ty
  | ENVMOD    of modty rooted
  | ENVOVLN   of ty list  (* overloaded name *)
  | ENVMODTY  of modty
(* definition of [[modty]] for \mcl S478b *)
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
(* definitions of [[exp]] and [[value]] for \mcl S479a *)
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
(* definitions of [[exp]] and [[value]] for \mcl S479b *)
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
(* type declarations for consistency checking *)
type exp = exp
(* type declarations for consistency checking *)
type value = value
val unitVal = CONVAL (PNAME "unit", [])
(* definition of [[def]] for \mcl S479c *)
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
(* type declarations for consistency checking *)
type def = def
type data_def = data_def
(* definition of [[unit_test]] for explicitly typed languages S406a *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_TYPE_ERROR  of def
  | CHECK_MTYPE of pathex * modtyx
(* definition of [[xdef]] (shared) S214b *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
val BugInTypeInference = BugInTypeChecking (* to make \uml utils work *)
(* string conversion of \mcl\ values S525a *)
fun vconString (PNAME c) = c
  | vconString (PDOT (m, c)) = vconString m ^ "." ^ c
  | vconString (PAPPLY _) = "can't happen! (vcon PAPPLY)"
(* type declarations for consistency checking *)
val _ = op vconString : vcon -> string
(* string conversion of \mcl\ values S525b *)
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
(* string conversion of \mcl\ values S525c *)
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
(* type declarations for consistency checking *)
val _ = op valueString : value -> string
(* type declarations for consistency checking *)
val _ = op consString : value * value -> string
(* definition of [[patString]] for \uml\ and \uhaskell ((Molecule)) S462b *)
fun patString WILDCARD    = "_"
  | patString (PVAR x)    = x
  | patString (CONPAT (vcon, []))   = vconString vcon
  | patString (CONPAT (vcon, pats)) =
      "(" ^ spaceSep (vconString vcon :: map patString pats) ^ ")"
(* string conversion of \mcl\ types and module types S525d *)
fun modidentString (MODCON { printName = m, serial = 0 }) = m
  | modidentString (MODCON { printName = m, serial = k }) =
      m ^ "@{" ^ intString k ^ "}" 
  | modidentString (MODTYPLACEHOLDER s) = "<signature: " ^ s ^ ">"
(* type declarations for consistency checking *)
val _ = op modidentString : modident -> string
(* string conversion of \mcl\ types and module types S525e *)
fun pathString (PNAME a) = modidentString a
  | pathString (PDOT (PNAME (MODTYPLACEHOLDER _), x)) = x
  | pathString (PDOT (p, x)) = pathString p ^ "." ^ x
  | pathString (PAPPLY (f, args)) =
      spaceSep ("(@m" :: pathString f :: map pathString args) ^ ")"
(* type declarations for consistency checking *)
val _ = op pathString : path -> string
(* string conversion of \mcl\ types and module types S526a *)
val pathexString =
  let fun s (PNAME a) = snd a
        | s (PDOT (p, x)) = s p ^ "." ^ x
        | s (PAPPLY (f, args)) = spaceSep ("(@m" :: s f :: map s args) ^ ")"
  in  s
  end
(* type declarations for consistency checking *)
val _ = op pathexString : pathex -> string
(* string conversion of \mcl\ types and module types S526b *)
fun typeString' ps (TYNAME p) = ps p
  | typeString' ps (FUNTY (args, res)) = 
      let val ts = typeString' ps
      in  "(" ^ spaceSep (map ts args @ "->" :: [ts res]) ^ ")"
      end
  | typeString' ps ANYTYPE = "<any type>"

val typeString = typeString' pathString
val tyexString = typeString' pathexString
(* type declarations for consistency checking *)
val _ = op tyexString : tyex -> string
val _ = op typeString : ty -> string
(* string conversion of \mcl\ types and module types S526c *)
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
(* type declarations for consistency checking *)
val _ = op mtString : modty -> string
val _ = op ncompString : name * component -> string
(* string conversion of \mcl\ types and module types S528a *)
fun mtxString (MTNAMEDX m) = m
  | mtxString (MTEXPORTSX []) = "(exports)"
  | mtxString (MTEXPORTSX lcomps) = 
      "(exports " ^ spaceSep (map ncompxString lcomps) ^ ")"
  | mtxString (MTALLOFX  mts) =
      "(allof " ^ spaceSep (map (mtxString o snd) mts) ^ ")"
  | mtxString (MTARROWX (args, res)) =
      "(" ^ spaceSep (map modformalString args) ^ " --m-> " ^
            mtxString (snd res) ^ ")"
(* string conversion of \mcl\ types and module types S528b *)
and modformalString (m, t) = "[" ^ snd m ^ " : " ^ mtxString (snd t) ^ "]"
and ncompxString (loc, (x, c)) =
  case c
    of DECVAL tau => "[" ^ x ^ " : " ^ tyexString tau ^ "]"
     | DECABSTY   => "(abstype " ^ x ^ ")"
     | DECMANTY tau => "(type " ^ x ^ " " ^ tyexString tau ^ ")"
     | DECMOD mt => "(module [" ^ x ^ " : " ^ mtxString mt ^ "])"
     | DECMODTY mt => "(module-type " ^ x ^ " " ^ mtxString mt ^ ")"
(* type declarations for consistency checking *)
val _ = op mtxString : modtyex -> string
val _ = op ncompxString : (name * decl) located -> string
(* prettyprinting of \mcl\ types and module types S527a *)
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
(* type declarations for consistency checking *)
val _ = op arrowdoc : doc list -> string -> doc -> doc
(* prettyprinting of \mcl\ types and module types S527b *)
fun typeDoc (TYNAME p) = doc (pathString p)
  | typeDoc (FUNTY (args, res)) =
      arrowdoc (map typeDoc args) "->" (typeDoc res)
  | typeDoc ANYTYPE = doc "<any-type>"
(* type declarations for consistency checking *)
val _ = op typeDoc : ty -> doc
(* prettyprinting of \mcl\ types and module types S527c *)
fun stdindent doc = indent (2, doc)

fun mtDoc (MTEXPORTS []) = doc "(exports)"
  | mtDoc (MTEXPORTS comps) = 
      agrp (doc "(exports" ^^
            stdindent (brk ^^ brkSep (map ncompDoc comps) ^^ doc ")"))
  | mtDoc (MTALLOF  mts) =
      doc "(allof" ^/+ brkSep (map mtDoc mts) ^^ doc ")"
  | mtDoc (MTARROW (args, res)) =
      arrowdoc (map modformalDoc args) "--m->" (mtDoc res)
(* prettyprinting of \mcl\ types and module types S527d *)
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
(* type declarations for consistency checking *)
val _ = op mtDoc : modty -> doc
(* type declarations for consistency checking *)
val _ = op modformalDoc : modident * modty -> doc
val _ = op ncompDoc : name * component -> doc
(* prettyprinting of \mcl\ types and module types S527e *)
fun ndecString (x, c) =
  case c
    of ENVVAL tau => "[" ^ x ^ " : " ^ typeString tau ^ "]"
     | ENVMANTY tau => "(type " ^ x ^ " " ^ typeString tau ^ ")"
     | ENVMOD (mt, _) => "(module [" ^ x ^ " : " ^ mtString mt ^ "])"
     | ENVOVLN _ => "<overloaded name " ^ x ^ " ...>"
     | ENVMODTY mt => "(module-type " ^ x ^ " " ^ mtString mt ^ ")"
(* type declarations for consistency checking *)
val _ = op ndecString : name * binding -> string
(* string conversion of \mcl's abstract syntax S529 *)
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
(* type declarations for consistency checking *)
val _ = op expString : exp -> string
(* string conversion of \mcl's abstract syntax S530a *)
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
(* type declarations for consistency checking *)
val _ = op defString : baredef -> string
(* utility functions on \uml\ values ((Molecule)) S444a *)
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
(* utility functions on \uml\ values ((Molecule)) S444c *)
fun embedList []      = CONVAL (PNAME "'()", [])
  | embedList (v::vs) = CONVAL (PNAME "cons", [ref v, ref (embedList vs)])
(* utility functions on \uml\ values ((Molecule)) S444e *)
fun embedBool b = CONVAL (PNAME (if b then "#t" else "#f"), [])
fun projectBool (CONVAL (PNAME "#t", [])) = true
  | projectBool _                         = false


(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR OPERATOR OVERLOADING IN \MCL                    *)
(*                                                               *)
(*****************************************************************)

(* support for operator overloading in \mcl S517e *)
fun plast (PDOT (_, x)) = x
  | plast (PNAME (_, x)) = x
  | plast (PAPPLY _) = "??last??"
(* support for operator overloading in \mcl S518a *)
val notOverloadedIndex = ~1


(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \MCL, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \mcl, providing [[filexdefs]] and [[stringsxdefs]] S532b *)
(* lexical analysis for {\mcl} S530b *)
datatype pretoken
  = QUOTE
  | INT      of int
  | RESERVED of string
  | DOTTED   of string * string list
  | DOTNAMES of string list (* .x.y and so on *)
type token = pretoken plus_brackets
(* lexical analysis for {\mcl} S530c *)
fun pretokenString (QUOTE)      = "'"
  | pretokenString (INT  n)     = intString n
  | pretokenString (DOTTED (s, ss))  = String.concatWith "." (s::ss)
  | pretokenString (DOTNAMES ss)= (concat o map (fn s => "." ^ s)) ss
  | pretokenString (RESERVED x) = x
val tokenString = plusBracketsString pretokenString
(* lexical analysis for {\mcl} S532a *)
local
  (* functions used in all lexers S384d *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* type declarations for consistency checking *)
  val _ = op noneIfLineEnds : 'a lexer
  (* support for \mcl's reserved words S531a *)
  val reserved = 
    [ (* words reserved for \mcl\ types S533c *)
      "->", ":"
    , (* words reserved for \mcl\ expressions S534e *)
      "@m", "if", "&&", "||", "set", "let", "let*", "letrec", "case", "lambda",
      "val", "set", "while", "begin", "error",
      "when", "unless", "assert"
    , (* words reserved for \mcl\ definitions S539a *)
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
  (* lexing functions for \mcl's dotted names S531b *)
  val isDelim = fn c => isDelim c orelse c = #"."
  (* lexing functions for \mcl's dotted names S531c *)
  datatype part = DOT | NONDELIMS of string
  val nondelims = (NONDELIMS o implode) <$> many1 (sat (not o isDelim) one)
  val dot       = DOT <$ eqx #"." one
  (* lexing functions for \mcl's dotted names S531d *)
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
(* type declarations for consistency checking *)
val _ = op dottedNames : part list -> pretoken error
(* type declarations for consistency checking *)
val _ = op mclToken : token lexer
val _ = op badReserved : name -> 'a error
end
(* parsers for \mcl\ tokens S532c *)
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
(* parsers for \uml\ value constructors and value variables S463c *)
fun isVcon x =
  let val lastPart = List.last (String.fields (curry op = #".") x)
      val firstAfterdot = String.sub (lastPart, 0) handle Subscript => #" "
  in  x = "cons" orelse x = "'()" orelse
      Char.isUpper firstAfterdot orelse firstAfterdot = #"#" orelse
      String.isPrefix "make-" x
  end
fun isVvar x = x <> "->" andalso not (isVcon x)
(* parsers for \uml\ value constructors and value variables S463d *)
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
(* parsers and parser builders for formal parameters and bindings S385c *)
fun formalsOf what name context = 
  nodups ("formal parameter", context) <$>! @@ (bracket (what, many name))

fun bindingsOf what name exp =
  let val binding = bracket (what, pair <$> name <*> exp)
  in  bracket ("(... " ^ what ^ " ...) in bindings", many binding)
  end
(* parsers and parser builders for formal parameters and bindings S386a *)
fun distinctBsIn bindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map fst bs) >>=+ (fn _ => bs)
  in  check <$>! @@ bindings
  end
(* type declarations for consistency checking *)
val _ = op formalsOf  : string -> name parser -> string -> name list parser
val _ = op bindingsOf : string -> 'x parser -> 'e parser -> ('x * 'e) list
                                                                          parser
(* type declarations for consistency checking *)
val _ = op distinctBsIn : (name * 'e) list parser -> string -> (name * 'e) list
                                                                          parser
(* parsers and parser builders for formal parameters and bindings ((higher-order)) S386b *)
fun asLambda inWhat (loc, e as LAMBDA _) = OK e
  | asLambda inWhat (loc, e) = 
      synerrorAt ("in " ^ inWhat ^ ", expression " ^ expString e ^ 
                  " is not a lambda")
                 loc

val asLambda = fn what => fn eparser => asLambda what <$>! @@ eparser
(* type declarations for consistency checking *)
val _ = op asLambda : string -> exp parser -> exp parser
(* parsers and parser builders for formal parameters and bindings S386c *)
fun recordFieldsOf name =
  nodups ("record fields", "record definition") <$>!
                                    @@ (bracket ("(field ...)", many name))
(* type declarations for consistency checking *)
val _ = op recordFieldsOf : name parser -> name list parser
(* parsers and parser builders for formal parameters and bindings S387a *)
fun kw keyword = 
  eqx keyword namelike
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* type declarations for consistency checking *)
val _ = op kw : string -> string parser
val _ = op usageParsers : (string * 'a parser) list -> 'a parser
(* parser builders for typed languages S414c *)
val distinctTyvars = 
  nodups ("quantified type variable", "forall") <$>! @@ (many tyvar)
(* parser builders for typed languages S414d *)
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
(* type declarations for consistency checking *)
val _ = op tyvar : name parser
(* type declarations for consistency checking *)
val _ = op distinctTyvars : name list parser
(* type declarations for consistency checking *)
val _ = op arrowsOf : ('ty * 'ty list -> 'ty) -> ('ty list * 'ty -> 'ty) -> 'ty
                                              list -> 'ty list list -> 'ty error
(* parser builders for typed languages S416b *)
fun distinctTBsIn tbindings context =
  let fun check (loc, bs) =
        nodups ("bound name", context) (loc, map (fst o fst) bs) >>=+ (fn _ =>
                                                                             bs)
  in  check <$>! @@ tbindings
  end
(* type declarations for consistency checking *)
val _ = op distinctTBsIn : ((name * 't) * 'e) list parser -> string -> ((name *
                                                           't) * 'e) list parser
(* parser builders for typed languages S401b *)
fun typedFormalOf name colon ty =
      bracket ("[x : ty]", pair <$> name <* colon <*> ty)
fun typedFormalsOf name colon ty context = 
  let val formal = typedFormalOf name colon ty
  in  distinctBsIn (bracket("(... [x : ty] ...)", many formal)) context
  end                            
(* type declarations for consistency checking *)
val _ = op typedFormalsOf : string parser -> 'b parser -> 'a parser  -> string
                                                    -> (string * 'a) list parser
(* parsers and [[xdef]] streams for \mcl S532d *)
fun kw keyword = eqx keyword reserved
fun usageParsers ps = anyParser (map (usageParser kw) ps)
(* parsers and [[xdef]] streams for \mcl S533a *)
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
(* type declarations for consistency checking *)
val _ = op addDots : pathex -> name list -> pathex
val _ = op path : pathex parser
(* parsers and [[xdef]] streams for \mcl S533b *)
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
(* type declarations for consistency checking *)
val _ = op mkTyex : (string * tyex parser -> tyex parser) -> tyex parser
val _ = op tyex : tyex parser
(* parsers and [[xdef]] streams for \mcl S534a *)
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
(* type declarations for consistency checking *)
val _ = op vcon : vcon parser
(* parsers and [[xdef]] streams for \mcl S534b *)
fun pattern tokens =  (
                WILDCARD    <$  eqx "_" vvar
      <|>       PVAR        <$> vvar
      <|> curry CONPAT      <$> vcon <*> pure []
      <|> bracket ( "(C x1 x2 ...) in pattern"
                  , curry CONPAT <$> vcon <*> many pattern
                  )
       ) tokens
(* type declarations for consistency checking *)
val _ = op pattern : pat parser
(* parsers and [[xdef]] streams for \mcl S534c *)
fun quoteName "#f" = CONVAL (PNAME "#f", [])
  | quoteName "#t" = CONVAL (PNAME "#t", [])
  | quoteName s    = SYM s

fun quotelit tokens = (
         quoteName <$> name
    <|>  NUM <$> int
    <|>  (ARRAY o Array.fromList) <$> bracket ("(literal ...)", many quotelit)
    ) tokens
(* type declarations for consistency checking *)
val _ = op quoteName : string -> value
val _ = op quotelit : value parser
(* parsers and [[xdef]] streams for \mcl S534d *)
val atomicExp =  VAR <$> path
             <|> badReserved <$>! reserved
             <|> dotnames <!> "a qualified name may not begin with a dot"
             <|> LITERAL <$> NUM <$> int
             <|> VCONX <$> vcon
             <|> quote *> (LITERAL <$> quotelit)
(* parsers and [[xdef]] streams for \mcl S535a *)
fun bindTo exp = bracket ("[x e]", pair <$> name <*> exp)
val formal = bracket ("[x : ty]", pair <$> name <* kw ":" <*> tyex)
val lformals = bracket ("([x : ty] ...)", many formal)
fun nodupsty what (loc, xts) = 
  nodups what (loc, map fst xts) >>=+ (fn _ => xts)
                              (* error on duplicate names *)
(* type declarations for consistency checking *)
val _ = op lformals : (name * tyex) list parser
(* parsers and [[xdef]] streams for \mcl S535b *)
fun smartBegin [e] = e
  | smartBegin es = BEGIN es
(* type declarations for consistency checking *)
val _ = op smartBegin : exp list -> exp
(* parsers and [[xdef]] streams for \mcl S535c *)
fun cand [e] = e
  | cand (e::es) = IFX (e, cand es, LITERAL (embedBool false))
  | cand [] = raise InternalError "parsing &&"

fun cor [e] = e
  | cor (e::es) = IFX (e, LITERAL (embedBool true), cor es)
  | cor [] = raise InternalError "parsing ||"
(* type declarations for consistency checking *)
val _ = op cand : exp list -> exp
val _ = op cor  : exp list -> exp
(* parsers and [[xdef]] streams for \mcl S535d *)
fun assert e =
  IFX (e, BEGIN [], ERRORX [LITERAL (SYM "assertion-failure")])
(* type declarations for consistency checking *)
val _ = op assert: exp -> exp
(* parsers and [[xdef]] streams for \mcl S536a *)
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
    <|> surpriseReserved [(* words reserved for \mcl\ types S533c *)
                          "->", ":",
                          (* words reserved for \mcl\ definitions S539a *)
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
(* parsers and [[xdef]] streams for \mcl S536b *)
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
(* type declarations for consistency checking *)
val _ = op exptable : exp parser -> exp parser
val _ = op exp      : exp parser
(* parsers and [[xdef]] streams for \mcl S537a *)
fun formalWith whatTy aTy =
  bracket ("[x : " ^ whatTy ^ "]", pair <$> name <* kw ":" <*> aTy)

val formal = formalWith "ty" tyex
(* type declarations for consistency checking *)
val _ = op formalWith : string -> 'a parser -> (name * 'a) parser
val _ = op formal : (name * tyex) parser
(* parsers and [[xdef]] streams for \mcl S537b *)
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
(* type declarations for consistency checking *)
val _ = op recordOpsType : string -> (name * tyex) list located -> modtyex
(* parsers and [[xdef]] streams for \mcl S537c *)
fun recordModule (loc, name) tyname fields =
  let val t = TYNAME (PNAME (loc, tyname))
      val vcon = "make-" ^ name ^ "." ^ tyname
      val conpat = CONPAT (PNAME vcon, map (PVAR o fst) fields)
      fun var x = VAR (PNAME (loc, x))
      (* definitions of [[getterComponent]] and [[setterComponent]] S538a *)
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
(* type declarations for consistency checking *)
val _ = op recordModule : name located -> name -> (name * tyex) list -> baredef
(* parsers and [[xdef]] streams for \mcl S538b *)
fun prightmap f (x, a) = (x, f a)
fun crightmap f x a = (x, f a)
(* type declarations for consistency checking *)
val _ = op prightmap : ('a -> 'b) -> name * 'a  -> name * 'b
val _ = op crightmap : ('a -> 'b) -> name -> 'a -> name * 'b
(* parsers and [[xdef]] streams for \mcl S538c *)
fun flipPair tx c = (c, tx)
(* type declarations for consistency checking *)
val _ = op flipPair : 'a -> 'b -> 'b * 'a
(* parsers and [[xdef]] streams for \mcl S538d *)
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
(* type declarations for consistency checking *)
val _ = op decl : (name * decl) parser
val _ = op locmodformal : (name located * modtyex located) parser
val _ = op modformal    : (name * modtyex) parser
val _ = op modtype      : modtyex parser
(* parsers and [[xdef]] streams for \mcl S539b *)
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
(* type declarations for consistency checking *)
val _ = op vcon : name parser
val _ = op vvar : name parser
(* parsers and [[xdef]] streams for \mcl S540 *)
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
(* type declarations for consistency checking *)
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
(* parsers and [[xdef]] streams for \mcl S541a *)
val testtable = usageParsers
  [ ("(check-expect e1 e2)",    curry CHECK_EXPECT     <$> exp <*> exp)
  , ("(check-assert e)",              CHECK_ASSERT     <$> exp)
  , ("(check-error e)",               CHECK_ERROR      <$> exp)
  , ("(check-type e tau)",      curry CHECK_TYPE       <$> exp <*> tyex)
  , ("(check-type-error e)",          CHECK_TYPE_ERROR <$> def)
  , ("(check-module-type M T)", curry CHECK_MTYPE      <$> path <*> modtype)
  ]
(* parsers and [[xdef]] streams for \mcl S541b *)
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
(* type declarations for consistency checking *)
val _ = op xdef : xdef parser

val xdefstream = 
  interactiveParsedStream (mclToken, xdef)
(* shared definitions of [[filexdefs]] and [[stringsxdefs]] S233a *)
fun filexdefs (filename, fd, prompts) =
      xdefstream (filename, filelines fd, prompts)
fun stringsxdefs (name, strings) =
      xdefstream (name, streamOfList strings, noPrompts)
(* type declarations for consistency checking *)
val _ = op xdefstream   : string * line stream     * prompts -> xdef stream
val _ = op filexdefs    : string * TextIO.instream * prompts -> xdef stream
val _ = op stringsxdefs : string * string list               -> xdef stream



(*****************************************************************)
(*                                                               *)
(*   ENVIRONMENTS FOR \MCL'S DEFINED NAMES                       *)
(*                                                               *)
(*****************************************************************)

(* environments for \mcl's defined names S496a *)
fun whatcomp (COMPVAL _) = "a value"
  | whatcomp (COMPABSTY _) = "an abstract type"
  | whatcomp (COMPMANTY _) = "a manifest type"
  | whatcomp (COMPMOD _) = "a module"
(* environments for \mcl's defined names S496b *)
fun whatdec (ENVVAL _) = "a value"
  | whatdec (ENVMANTY _) = "a manifest type"
  | whatdec (ENVOVLN _) = "an overloaded name"
  | whatdec (ENVMOD _) = "a module"
  | whatdec (ENVMODTY _) = "a module type"
(* environments for \mcl's defined names S524b *)
fun compString (ENVVAL tau) = "a value of type " ^ typeString tau
  | compString (ENVMANTY tau) = "manifest type " ^ typeString tau
  | compString (ENVOVLN _) = "an overloaded name"
  | compString (ENVMOD (mt, path)) = "module " ^ pathString path ^
                                     " of type " ^ mtString mt
  | compString (ENVMODTY _) = "a module type"
(* type declarations for consistency checking *)
val _ = op compString : binding -> string



(*****************************************************************)
(*                                                               *)
(*   TYPE CHECKING FOR {\MCL}                                    *)
(*                                                               *)
(*****************************************************************)

(* type checking for {\mcl} S496c *)
(* additional operations for composing [[error]] values S498 *)
infix 1 >>
fun (OK ()) >> c = c
  | (ERROR msg) >> _ = ERROR msg

fun firstE []      = OK ()
  | firstE (e::es) = e >> firstE es
(* [[context]] for a {\mcl} definition S509b *)
datatype context
  = TOPLEVEL
  | INMODULE of path

fun contextDot (TOPLEVEL, name) = PNAME (genmodident name)
  | contextDot (INMODULE path, name) = PDOT (path, name)

fun contextString TOPLEVEL = "at top level"
  | contextString (INMODULE p) = "in module " ^ pathString p
(* type declarations for consistency checking *)
type context = context
val _ = op contextDot : context * name -> path
(* type equality for \mcl S477b *)
fun eqType (TYNAME p, TYNAME p') = p = p'
  | eqType (FUNTY (args, res), FUNTY (args', res')) =
      eqTypes (args, args') andalso eqType (res, res')
  | eqType (ANYTYPE, _) = true
  | eqType (_, ANYTYPE) = true
  | eqType _ = false
and eqTypes (taus, tau's) = ListPair.allEq eqType (taus, tau's)
(* type declarations for consistency checking *)
val _ = op eqType  : ty      * ty      -> bool
val _ = op eqTypes : ty list * ty list -> bool
(* recognition of function types S477c *)
fun isfuntype (FUNTY _) = true
  | isfuntype _         = false
(* substitutions for \mcl S513c *)
type rootsubst = (modident * path) list
val idsubst = []
(* type declarations for consistency checking *)
type rootsubst = rootsubst
val _ = op idsubst : rootsubst
(* substitutions for \mcl S513d *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* type declarations for consistency checking *)
val _ = op |--> : modident * path -> rootsubst
(* substitutions for \mcl S514a *)
type tysubst = 
  (path * ty) list
fun associatedWith (x, []) =
      NONE
  | associatedWith (x, (key, value) :: pairs) =
      if x = key then SOME value else associatedWith (x, pairs)

fun hasKey [] x = false
  | hasKey ((key, value) :: pairs) x = x = key orelse hasKey pairs x
(* type declarations for consistency checking *)
type tysubst = tysubst
val _ = op associatedWith : path * tysubst -> ty option
val _ = op hasKey : tysubst -> path -> bool
(* substitutions for \mcl S514b *)
fun pathsubstRoot theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op pathsubstRoot : rootsubst -> path -> path
(* substitutions for \mcl S514c *)
fun tysubstRoot theta (TYNAME p)          = TYNAME (pathsubstRoot theta p)
  | tysubstRoot theta (FUNTY (args, res)) =
      FUNTY (map (tysubstRoot theta) args, tysubstRoot theta res)
  | tysubstRoot theta ANYTYPE = ANYTYPE
(* type declarations for consistency checking *)
val _ = op tysubstRoot : rootsubst -> ty -> ty
(* substitutions for \mcl S514d *)
fun dom theta = 
  map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubstRoot theta2 o pathsubstRoot theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : rootsubst -> modident set
val _ = op compose : rootsubst * rootsubst -> rootsubst
(* substitutions for \mcl S514e *)
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
(* type declarations for consistency checking *)
val _ = op mtsubstRoot   : rootsubst -> modty      -> modty
val _ = op compsubstRoot : rootsubst -> component -> component
(* substitutions for \mcl S515a *)
fun tysubstManifest mantypes =
  let fun r (TYNAME path) = getOpt (associatedWith (path, mantypes), TYNAME path
                                                                               )
        | r (FUNTY (args, res)) = FUNTY (map r args, r res)
        | r (ANYTYPE) = ANYTYPE
  in  r
  end
(* type declarations for consistency checking *)
val _ = op tysubstManifest : tysubst -> ty -> ty
(* substitutions for \mcl S515b *)
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
(* type declarations for consistency checking *)
val _ = op mtsubstManifest : tysubst -> modty -> modty
(* type components of module types S497a *)
fun abstractTypePaths (MTEXPORTS cs, path : path) =
      let fun ats (t, COMPABSTY _) = [PDOT (path, t)]
            | ats (x, COMPMOD mt) = abstractTypePaths (mt, PDOT (path, x))
            | ats _ = []
(* type declarations for consistency checking *)
val _ = op abstractTypePaths : modty rooted -> path list
      in  concatMap ats cs
      end
  | abstractTypePaths (MTALLOF mts, path) =
      concatMap (fn mt => abstractTypePaths (mt, path)) mts
  | abstractTypePaths (MTARROW _, _) = []
                                          (* could be bogus, cf Leroy rule 21 *)
(* utilities for module-type realization S501c *)
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
(* type declarations for consistency checking *)
val _ = op filterdec : (component * path -> bool) -> modty rooted -> modty
(* utilities for module-type realization S502a *)
fun emptyExports (MTEXPORTS []) = true
  | emptyExports _ = false
(* type declarations for consistency checking *)
val _ = op emptyExports : modty -> bool
(* module-type realization S500c *)
(* definition of [[msubsn]] S501a *)
fun msubsn (MTEXPORTS cs, path) =
      let fun mts (x, COMPMANTY tau) = [(PDOT (path, x), tau)]
            | mts (x, COMPMOD mt) = msubsn (mt, PDOT(path, x))
            | mts _ = []
      in  concatMap mts cs
      end
  | msubsn (MTALLOF mts, path) =
      concatMap (fn mt => msubsn (mt, path)) mts
  | msubsn (MTARROW _, path) = []   (* could be bogus, cf Leroy rule 21 *)
(* type declarations for consistency checking *)
val _ = op msubsn : modty rooted -> tysubst
(* definition of [[simpleSyntacticMeet]] S501b *)
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
(* type declarations for consistency checking *)
val _ = op simpleSyntacticMeet : modty -> modty
(* definition of smart constructor [[allofAt]] S497b *)
fun allofAt (mts, path) =
  let val mt = MTALLOF mts
      val mantypes = msubsn (mt, path)
      val abstypes = abstractTypePaths (mt, path)
  in  if List.exists (hasKey mantypes) abstypes then
        simpleSyntacticMeet (mtsubstManifest mantypes mt)
      else
        mt
  end
(* type declarations for consistency checking *)
val _ = op allofAt : modty list rooted -> modty
(* definition of [[unmixTypes]] S497c *)
fun unmixTypes (mt, path) =
  let fun mtype (MTEXPORTS cs) = MTEXPORTS (map comp cs)
        | mtype (MTALLOF mts)  = allofAt (map mtype mts, path)
        | mtype (MTARROW (args, result)) =
            MTARROW (map (fn (x, mt) => (x, mtype mt)) args, mtype result)
      and comp (x, COMPMOD mt) = (x, COMPMOD (unmixTypes (mt, PDOT (path, x))))
        | comp c = c
  in  mtype mt
  end
(* type declarations for consistency checking *)
val _ = op unmixTypes : modty rooted -> modty
(* invariants of \mcl S496d *)
fun mixedManifestations mt =
  let val path = PNAME (MODTYPLACEHOLDER "invariant checking")
      val manifests = msubsn (mt, path)
      val abstracts = abstractTypePaths (mt, path)
  in  List.exists (hasKey manifests) abstracts
  end
(* type declarations for consistency checking *)
val _ = op mixedManifestations : modty -> bool
(* [[implements]] relation, based on [[subtype]] of two module types S499 *)
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
                  let (* definition of [[csubtype]] S500a *)
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
                      (* type declarations for consistency checking *)
                      val _ = op csubtype : component * component -> unit error
(* type declarations for consistency checking *)
val _ = op >> : unit error * unit error -> unit error
val _ = op firstE : unit error list -> unit error
(* type declarations for consistency checking *)
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
(* [[implements]] relation, based on [[subtype]] of two module types ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
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
(* [[implements]] relation, based on [[subtype]] of two module types S500b *)
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
(* type declarations for consistency checking *)
val _ = op implements : path * modty * modty -> unit error
(* path-expression lookup S493b *)
fun asBinding (COMPVAL tau, root) = ENVVAL tau
  | asBinding (COMPABSTY path, root) = ENVMANTY (TYNAME path)
  | asBinding (COMPMANTY tau, root) = ENVMANTY tau
  | asBinding (COMPMOD mt, root) = ENVMOD (mt, root)

fun uproot (ENVVAL tau) = COMPVAL tau
  | uproot (ENVMANTY tau) = COMPMANTY tau
  | uproot (ENVMOD (mt, _)) = COMPMOD mt
  | uproot d = raise InternalError (whatdec d ^ " as component")
(* type declarations for consistency checking *)
val _ = op asBinding : component * path -> binding
val _ = op uproot : binding -> component
(* path-expression lookup S493c *)
fun pathfind (PNAME x, Gamma) = find (snd x, Gamma)
  | pathfind (PDOT (path, x), Gamma) =
      let (* definition of [[mtfind]] S495d *)
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
          (* type declarations for consistency checking *)
          val _ = op mtfind : name * modty -> component option
      in  case pathfind (path, Gamma)
            of ENVMOD (mt, root) =>
                 (asBinding (valOf (mtfind (x, mt)), root) handle Option =>
                   noComponent (path, x, mt))
             | dec =>
            (* tried to select [[path]].[[x]] but [[path]] is a [[dec]] S494a *)
                      raise TypeError ("Tried to select " ^ pathexString (PDOT (
                                                          path, x)) ^ ", but " ^
                                       pathexString path ^ " is " ^ whatdec dec
                                                         ^ ", which does not " ^
                                       " have components")
      end
  | pathfind (PAPPLY (fpx, actualpxs) : pathex, Gamma) =
     (* instantiation of module [[fpx]] to [[actualpxs]] S494c *)
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
                               (* instantiated exporting module [[fpx]] S495a *)
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
                   (* can't pass [[actroot]] as [[formalid]] to [[fpx]] S495b *)
                                     raise TypeError ("module " ^ pathString
                                      actroot ^ " cannot be used as argument " ^
                                                      modidentString formalid ^
                                      " to generic module " ^ pathexString fpx ^
                                                      ": " ^ msg)
               end
           | resty _ = (* wrong number of arguments to [[fpx]] S495c *)
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
(* type declarations for consistency checking *)
val _ = op pathfind : pathex * binding env -> binding
(* path-expression lookup S494b *)
local
  val vconLoc = ("unlocated value constructor", ~99)
in
  fun pathexOfVcon (PNAME x) = PNAME (vconLoc, x)
    | pathexOfVcon (PDOT (path, x)) = PDOT (pathexOfVcon path, x)
    | pathexOfVcon (PAPPLY _) = raise InternalError "application vcon"
end
(* type declarations for consistency checking *)
val _ = op pathexOfVcon : vcon -> pathex
(* path-expression lookup S495e *)
fun findModule (px, Gamma) =
  case pathfind (px, Gamma)
    of ENVMOD (mt, _) => mt
     | dec => raise TypeError ("looking for a module, but " ^
                               pathexString px ^ " is a " ^ whatdec dec)
(* type declarations for consistency checking *)
val _ = op findModule : pathex * binding env -> modty
(* converting bound entities to components S480a *)
fun asComponent (x, ENVVAL tau)     = SOME (x, COMPVAL tau)
  | asComponent (x, ENVMANTY tau)   = SOME (x, COMPMANTY tau)
  | asComponent (m, ENVMOD (mt, _)) = SOME (m, COMPMOD mt)
  | asComponent (_, ENVOVLN _) = NONE
  | asComponent (_, ENVMODTY _) = raise InternalError "module type as component"
(* elaboration of {\mcl} type syntax into types S515c *)
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
(* type declarations for consistency checking *)
val _ = op elabpath : pathex * binding env -> path
(* elaboration of {\mcl} type syntax into types S516a *)
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
(* type declarations for consistency checking *)
val _ = op elabty : tyex * binding env -> ty
(* elaboration of {\mcl} type syntax into types S516b *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of ENVMODTY mt => mt
     | dec => raise TypeError ("Tried to use " ^ whatdec dec ^ " " ^ x ^
                                " as a module type")
(* type declarations for consistency checking *)
val _ = op findModty : name * binding env -> modty
(* elaboration of {\mcl} type syntax into types S516c *)
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
                          (* if [[modty]] is generic, bleat about [[m]] S517a *)
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
(* elaboration of {\mcl} type syntax into types S517b *)
      and export ((x, ctx : decl), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                               " in module type")
            else
              let val c = elabComp ((ctx, PDOT (path, x)), Gamma)
              in  ((x, c) :: theseDecls, bind (x, asBinding (c, path), Gamma))
              end
(* type declarations for consistency checking *)
val _ = op elabmt : modtyx rooted * binding env -> modty
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
(* type declarations for consistency checking *)
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
  in  elab mtx
  end
(* elaboration of {\mcl} type syntax into types S517c *)
and elabComp ((comp : decl, path), Gamma : binding env) : component =
  let fun ty t = elabty (t, Gamma)
(* type declarations for consistency checking *)
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
(* elaboration of {\mcl} type syntax into types S517d *)
val elabmt = fn a =>
  let val mt = elabmt a
  in  if mixedManifestations mt then
        raise BugInTypeChecking
                ("invariant violation (mixed M): " ^ mtString mt)
      else
        mt
  end
(* primitive module identifiers and types used to type literal expressions S491c *)
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
(* primitive module identifiers and types used to type literal expressions S491d *)
fun arraytype tau =
  case tau
    of TYNAME (PDOT (module, "t")) =>
         TYNAME (PDOT (PAPPLY (PNAME arraymodident, [module]), "t"))
     | _ => raise InternalError "unable to form internal array type"

(* utility functions on {\mcl} types S502b *)
fun firstArgType (x, FUNTY (tau :: _, _)) = OK tau
  | firstArgType (x, FUNTY ([], _)) =
      ERROR ("function " ^ x ^ " cannot be overloaded because " ^
             "it does not take any arguments")
  | firstArgType (x, _) =
      ERROR (x ^ " cannot be overloaded because it is not a function")

(* utility functions on {\mcl} types S502c *)
fun okOrTypeError (OK a) = a
  | okOrTypeError (ERROR msg) = raise TypeError msg

fun ok a = okOrTypeError a
           handle _ => raise InternalError "overloaded non-function?"
(* utility functions on {\mcl} types S503a *)
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
(* type declarations for consistency checking *)
val _ = op okOrTypeError : 'a error -> 'a
val _ = op ok            : 'a error -> 'a
(* type declarations for consistency checking *)
val _ = op resolveOverloaded : name * ty * ty list -> (ty * int) error
(* [[typeof]] a {\mcl} expression ((prototype)) S503b *)
fun typeof (e, Gamma) =
  let fun ty e = typeof (e, Gamma)  (* replace with your code *)
      (* definitions of internal functions for [[typeof]] S503c *)
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
      (* definitions of internal functions for [[typeof]] S504a *)
      fun typeOfApply f actuals index =
        let val atys = map ty actuals
            (* definitions of [[maybeNamed]] and [[diagnoseArgs]] S504b *)
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

      (* type declarations for consistency checking *)
      val _ = op typeof : exp * binding env -> ty
      val _ = op ty     : exp                    -> ty
      (* type declarations for consistency checking *)
      val _ = op typeofFunction : exp * ty list -> ty * int
      (* type declarations for consistency checking *)
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
(* principal type of a module S507b *)
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
(* type declarations for consistency checking *)
val _ = op strengthen : modty rooted -> modty
(* elaboration and evaluation of [[data]] definitions for \mcl S511d *)
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

            (* result type of [[K]] should be [[tau]] but is [[result]] S512a *)
              raise TypeError ("value constructor " ^ K ^ " should return " ^
                                                                typeString tau ^
                               ", but it returns type " ^ typeString result)
        | validate (K, tau') =
            if eqType (tau', tau) then
              ()
            else 
              (* type of [[K]] should be [[tau]] but is [[tau']] S512b *)
              raise TypeError ("value constructor " ^ K ^ " should have " ^
                                                                typeString tau ^
                              ", but it has type " ^ typeString tau')
      val () = app validate Ktaus
  in  (T, ENVMANTY tau) :: map (fn (K, tau) => (K, ENVVAL tau)) Ktaus
      (* thin ice here: the type component should be abstract? *)  (*OMIT*)
  end
(* type declarations for consistency checking *)
val _ = op typeDataDef : data_def * context * binding env -> (name * binding)
                                                                            list
(* elaboration and evaluation of [[data]] definitions for \mcl S523a *)
fun evalDataDef ((_, typed_vcons), rho) =
  let fun addVcon ((K, t), rho) =
        let val v = if isfuntype t then
                      PRIMITIVE (fn vs => CONVAL (PNAME K, map ref vs))
                    else
                      CONVAL (PNAME K, [])
        in  bind (K, ref v, rho)
        end
(* type declarations for consistency checking *)
val _ = op evalDataDef : data_def * value ref env -> value ref env * string list
  in  (foldl addVcon rho typed_vcons, map fst typed_vcons)
  end
(* elaborate a {\mcl} definition S507c *)
type value_printer = 
  interactivity -> (name -> ty -> value -> unit) -> value list -> unit
fun printStrings ss _ _ vs = 
  app print ss
(* type declarations for consistency checking *)
type value_printer = value_printer
val _ = op printStrings : string list -> value_printer
(* elaborate a {\mcl} definition S508a *)
val ppwidth =
  getOpt(Option.mapPartial Int.fromString (OS.Process.getEnv "COLS"), 77)

fun printDoc doc interactivity _ _ =
  let val margin = if prompts interactivity then 2 else 0
  in  print (layout ppwidth (indent (margin, agrp doc)))
  end
(* type declarations for consistency checking *)
val _ = op printDoc : doc -> value_printer
(* elaborate a {\mcl} definition S508b *)
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
(* elaborate a {\mcl} definition S508c *)
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
(* type declarations for consistency checking *)
val _ = op defName : baredef -> string
(* type declarations for consistency checking *)
val _ = op defResponse : name * binding -> value_printer
(* elaborate a {\mcl} definition S509a *)
fun defPrinter (d, Gamma) =
      let val x = defName d
      in  defResponse (x, find (x, Gamma))
          handle NotFound _ => raise InternalError "defName not found"
      end
(* type declarations for consistency checking *)
val _ = op defPrinter : baredef * binding env -> value_printer
(* elaborate a {\mcl} definition S509c *)
fun typdef (d : baredef, context, Gamma) =
  let fun toplevel what =
        case context
          of TOPLEVEL => id
           | _ => raise TypeError (what ^ " cannot appear " ^
                                   contextString context)
      (* definition of [[mtypeof]] S513a *)
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
                      (* definition of [[asUnique]] S513b *)
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
                      (* type declarations for consistency checking *)
                      val _ = op asUnique : component env -> name * component ->
                                                       (name * component) option
                  in  List.mapPartial (asUnique comps'') comps' @ comps''
                  end
      (* type declarations for consistency checking *)
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
         (* named bindings for other forms of definition S510a *)
         | MODULETYPE (T, mtx) =>
             let val mt = elabmt ((mtx, PNAME (MODTYPLACEHOLDER T)), Gamma)
             in  toplevel ("a module type (like " ^ T ^ ")")
                 [(T, ENVMODTY mt)]
             end
         (* named bindings for other forms of definition S510b *)
         | MODULE (name, mx) =>
             let val root = contextDot (context, name)
                 val mt   = mtypeof ((mx, root), Gamma)
             in  [(name, ENVMOD (mt, root))]
             end
         (* named bindings for other forms of definition S510c *)
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
         (* named bindings for other forms of definition S510d *)
         | DEFINE (name, tau, lambda as (formals, body)) =>
             let val funty = FUNTY (map (fn (n, ty) => ty) formals, tau)
             in  typdef (VALREC (name, funty, LAMBDA lambda), context, Gamma)
             end
         (* named bindings for other forms of definition S510e *)
         | VAL (x, e) =>
             let val tau = typeof (e, Gamma)
             in  [(x, ENVVAL tau)]
             end
         (* named bindings for other forms of definition S511a *)
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
         (* named bindings for other forms of definition S511b *)
         | TYPE (t, tx) =>
             let val tau = elabty (tx, Gamma)
             in  [(t, ENVMANTY tau)]
             end
         (* named bindings for other forms of definition S511c *)
         | DATA dd => typeDataDef (dd, context, Gamma)
         | OVERLOAD ovl =>
                          (* return bindings from overload list [[ovl]] S512c *)
                           let fun overloadBinding (p, Gamma) = 
                                 let val (tau, first) =
                                       case pathfind (p, Gamma)
                                         of ENVVAL tau =>
                                              (tau, okOrTypeError (firstArgType
                                                         (pathexString p, tau)))
                                          | c =>
                                              (* can't overload a [[c]] S481b *)
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
                           (* type declarations for consistency checking *)
                           val _ = op overloadBinding : pathex * binding env ->
                                                                  name * binding

                          (* return bindings from overload list [[ovl]] S512d *)
                               fun overloadBindings (ps, Gamma) =
                                 let fun add (bs', Gamma, []) = bs'
                                       | add (bs', Gamma, p :: ps) =
                                           let val b = overloadBinding (p, Gamma
                                                                               )
                                           in  add (b :: bs', Gamma <+> [b], ps)
                                           end
                                 in  add ([], Gamma, ps)
                                 end
                           (* type declarations for consistency checking *)
                           val _ = op overloadBindings : pathex list * binding
                                                    env -> (name * binding) list
                           in  overloadBindings (ovl, Gamma)
                           end
  end
(* type declarations for consistency checking *)
val _ = op typdef : baredef * context * binding env -> (name * binding) list


(*****************************************************************)
(*                                                               *)
(*   SUBSTITUTIONS FOR \MCL                                      *)
(*                                                               *)
(*****************************************************************)

(* substitutions for \mcl S513c *)
type rootsubst = (modident * path) list
val idsubst = []
(* type declarations for consistency checking *)
type rootsubst = rootsubst
val _ = op idsubst : rootsubst
(* substitutions for \mcl S513d *)
infix 7 |-->
fun id |--> p = [(id, p)]
(* type declarations for consistency checking *)
val _ = op |--> : modident * path -> rootsubst
(* substitutions for \mcl S514a *)
type tysubst = 
  (path * ty) list
fun associatedWith (x, []) =
      NONE
  | associatedWith (x, (key, value) :: pairs) =
      if x = key then SOME value else associatedWith (x, pairs)

fun hasKey [] x = false
  | hasKey ((key, value) :: pairs) x = x = key orelse hasKey pairs x
(* type declarations for consistency checking *)
type tysubst = tysubst
val _ = op associatedWith : path * tysubst -> ty option
val _ = op hasKey : tysubst -> path -> bool
(* substitutions for \mcl S514b *)
fun pathsubstRoot theta =
  let fun subst (PNAME id) =
            (case List.find (fn (id', p') => id = id') theta
               of SOME (_, p) => p
                | NONE => PNAME id)
        | subst (PDOT (p, x)) = PDOT (subst p, x)
        | subst (PAPPLY (p, ps)) = PAPPLY (subst p, map subst ps)
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op pathsubstRoot : rootsubst -> path -> path
(* substitutions for \mcl S514c *)
fun tysubstRoot theta (TYNAME p)          = TYNAME (pathsubstRoot theta p)
  | tysubstRoot theta (FUNTY (args, res)) =
      FUNTY (map (tysubstRoot theta) args, tysubstRoot theta res)
  | tysubstRoot theta ANYTYPE = ANYTYPE
(* type declarations for consistency checking *)
val _ = op tysubstRoot : rootsubst -> ty -> ty
(* substitutions for \mcl S514d *)
fun dom theta = 
  map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = pathsubstRoot theta2 o pathsubstRoot theta1 o PNAME
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : rootsubst -> modident set
val _ = op compose : rootsubst * rootsubst -> rootsubst
(* substitutions for \mcl S514e *)
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
(* type declarations for consistency checking *)
val _ = op mtsubstRoot   : rootsubst -> modty      -> modty
val _ = op compsubstRoot : rootsubst -> component -> component
(* substitutions for \mcl S515a *)
fun tysubstManifest mantypes =
  let fun r (TYNAME path) = getOpt (associatedWith (path, mantypes), TYNAME path
                                                                               )
        | r (FUNTY (args, res)) = FUNTY (map r args, r res)
        | r (ANYTYPE) = ANYTYPE
  in  r
  end
(* type declarations for consistency checking *)
val _ = op tysubstManifest : tysubst -> ty -> ty
(* substitutions for \mcl S515b *)
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
(* type declarations for consistency checking *)
val _ = op mtsubstManifest : tysubst -> modty -> modty


(*****************************************************************)
(*                                                               *)
(*   ELABORATION OF {\MCL} TYPE SYNTAX INTO TYPES                *)
(*                                                               *)
(*****************************************************************)

(* elaboration of {\mcl} type syntax into types S515c *)
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
(* type declarations for consistency checking *)
val _ = op elabpath : pathex * binding env -> path
(* elaboration of {\mcl} type syntax into types S516a *)
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
(* type declarations for consistency checking *)
val _ = op elabty : tyex * binding env -> ty
(* elaboration of {\mcl} type syntax into types S516b *)
fun findModty (x, Gamma) =
  case find (x, Gamma)
    of ENVMODTY mt => mt
     | dec => raise TypeError ("Tried to use " ^ whatdec dec ^ " " ^ x ^
                                " as a module type")
(* type declarations for consistency checking *)
val _ = op findModty : name * binding env -> modty
(* elaboration of {\mcl} type syntax into types S516c *)
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
                          (* if [[modty]] is generic, bleat about [[m]] S517a *)
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
(* elaboration of {\mcl} type syntax into types S517b *)
      and export ((x, ctx : decl), (theseDecls, Gamma)) =
            if isbound (x, theseDecls) then
              raise TypeError ("duplicate declaration of " ^ x ^
                               " in module type")
            else
              let val c = elabComp ((ctx, PDOT (path, x)), Gamma)
              in  ((x, c) :: theseDecls, bind (x, asBinding (c, path), Gamma))
              end
(* type declarations for consistency checking *)
val _ = op elabmt : modtyx rooted * binding env -> modty
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
(* type declarations for consistency checking *)
val _ = op export : (name * decl) * ((name * component) list * binding env) -> (
                                          (name * component) list * binding env)
  in  elab mtx
  end
(* elaboration of {\mcl} type syntax into types S517c *)
and elabComp ((comp : decl, path), Gamma : binding env) : component =
  let fun ty t = elabty (t, Gamma)
(* type declarations for consistency checking *)
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
(* elaboration of {\mcl} type syntax into types S517d *)
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

(* evaluation, testing, and the read-eval-print loop for \mcl S518b *)
fun basename (PDOT (_, x)) = PNAME x
  | basename (PNAME x) = PNAME x
  | basename (instance as PAPPLY _) = instance
(* definitions of [[matchRef]] and [[Doesn'tMatch]] ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
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
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S518c *)
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
(* type declarations for consistency checking *)
val _ = op plast : pathex -> name
(* type declarations for consistency checking *)
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
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S519a *)
and eval (e, rho : value ref env) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let fun ev (LITERAL n) = n
        (* more alternatives for [[ev]] for {\mcl} S519b *)
        | ev (VAR p) = evalpath (p, rho)
        | ev (SET (n, e)) = 
            let val v = ev e
            in  find (n, rho) := v;
                unitVal
            end
        (* more alternatives for [[ev]] for {\mcl} S519c *)
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
        (* more alternatives for [[ev]] for {\mcl} S519d *)
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
        (* more alternatives for [[ev]] for {\mcl} S520a *)
        | ev (LAMBDA (args, body)) = CLOSURE ((map (fn (x, ty) => x) args, body)
                                                                          , rho)
        (* more alternatives for [[ev]] for {\mcl} S520b *)
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
                       (* apply closure [[clo]] to [[args]] ((mlscheme)) 310c *)
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
        (* more alternatives for [[ev]] for {\mcl} S520c *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        (* more alternatives for [[ev]] for {\mcl} S520d *)
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
        (* more alternatives for [[ev]] for {\mcl} S520e *)
        | ev (MODEXP components) =
            let fun step ((x, e), (results', rho)) =
                  let val loc = ref (eval (e, rho))
                  in  ((x, loc) :: results', bind (x, loc, rho))
                  end
                val (results', _) = foldl step ([], rho) components
            in  MODVAL results'
            end
        (* more alternatives for [[ev]] for {\mcl} S521a *)
        | ev (ERRORX es) =
            raise RuntimeError (spaceSep (map (valueString o ev) es))
        | ev (EXP_AT (loc, e)) = atLoc loc ev e
(* type declarations for consistency checking *)
val _ = op eval : exp * value ref env -> value
val _ = op ev   : exp                 -> value
  in  ev e
  end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S521b *)
fun evaldef (d, rho) =
  let val bindings = defbindings (d, rho)
(* type declarations for consistency checking *)
val _ = op evaldef : baredef * value ref env -> value ref env * value list
  in  (rho <+> bindings, map (! o snd) bindings)
  end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S521c *)
and defbindings (VAL (x, e), rho) =
      [(x, ref (eval (e, rho)))]
  | defbindings (VALREC (x, tau, e), rho) =
      let val this = ref (SYM "placedholder for val rec")
(* type declarations for consistency checking *)
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
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S521d *)
  | defbindings (QNAME _, rho) = 
      []
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S522a *)
  | defbindings (TYPE _, _) =
      []
  | defbindings (MODULETYPE (a, _), rho) = 
      []
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S522b *)
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
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S522c *)
  | defbindings (MODULE (x, m), rho) =
      [(x, ref (evalmod (m, rho)))]
  | defbindings (GMODULE (f, formals, body), rho) =
      [(f, ref (CLOSURE ((map fst formals, modexp body), rho)))]
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S522d *)
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
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S522e *)
and extendOverloadTable (x, v, rho) =
  let val currentVals =
        (case find (x, rho)
           of ref (ARRAY a) => a
            | _ => Array.fromList [])
        handle NotFound _ => Array.fromList []
(* type declarations for consistency checking *)
val _ = op extendOverloadTable : name * value * value ref env -> value array
  in  Array.tabulate (1 + Array.length currentVals,
                      fn 0 => v | i => Array.sub (currentVals, i - 1))
  end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S523b *)
and evalmod (MSEALED (_, ds), rho) = evalmod (MUNSEALED ds, rho)
  | evalmod (MPATH p, rho) = evalpath (p, rho)
  | evalmod (MPATHSEALED (mtx, p), rho) = evalpath (p, rho)
  | evalmod (MUNSEALED defs, rho) = MODVAL (rev (defsbindings (defs, rho)))
(*OMIT*)   (* XXX type checker should ensure there are no duplicates here *)
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S523c *)
and defsbindings ([],   rho) = []
  | defsbindings (d::ds, rho) =
      let val bs   = leftLocated defbindings (d, rho)
          val rho' = foldl (fn ((x, loc), rho) => bind (x, loc, rho)) rho bs
(* type declarations for consistency checking *)
val _ = op evalmod : moddef * value ref env -> value
(* type declarations for consistency checking *)
val _ = op defsbindings : def list * value ref env -> (name * value ref) list
      in  bs @ defsbindings (ds, rho')
      end
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S523d *)
and modexp (MPATH px)            = VAR px
  | modexp (MPATHSEALED (_, px)) = VAR px
  | modexp (MSEALED (_, defs))   = MODEXP (concatMap (located defexps) defs)
  | modexp (MUNSEALED defs)      = MODEXP (concatMap (located defexps) defs)
(* definitions of [[eval]] and [[evaldef]] for {\mcl} S524a *)
and defexps (VAL (x, e)) = [(x, e)]
  | defexps (VALREC (x, tau, e)) = 
      let val nullsrc : srcloc = ("translated name in VALREC", ~1)
(* type declarations for consistency checking *)
val _ = op modexp : moddef -> exp
(* type declarations for consistency checking *)
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
(* definitions of [[basis]] and [[processDef]] for \mcl S480b *)
type basis = binding env * value ref env
val emptyBasis : basis = (emptyEnv, emptyEnv)
fun processDataDef (dd, (Gamma, rho), interactivity) =
  let val bindings      = typeDataDef (dd, TOPLEVEL, Gamma)
      val Gamma'        = Gamma <+> bindings
      val comps         = List.mapPartial asComponent bindings
        (* could convert first component to abstract type here XXX *)
      val (rho', vcons) = evalDataDef (dd, rho)
      val _ = if echoes interactivity then

      (* print the new type and each of its value constructors for \mcl S480c *)
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
(* type declarations for consistency checking *)
val _ = op processDataDef : data_def * basis * interactivity -> basis
(* definitions of [[basis]] and [[processDef]] for \mcl S481a *)
fun processOverloading (ps, (Gamma, rho), interactivity) =
  let fun next (p, (Gamma, rho)) =
        let val (tau, first) =
              case pathfind (p, Gamma)
                of ENVVAL tau =>
                     (tau, okOrTypeError (firstArgType (pathexString p, tau)))
                 | c => (* can't overload a [[c]] S481b *)
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
(* definitions of [[basis]] and [[processDef]] for \mcl S481c *)
type basis = binding env * value ref env
(* definitions of [[basis]] and [[processDef]] for \mcl S481d *)
fun processDef ((loc, DATA dd), (Gamma, rho), interactivity) =
      atLoc loc processDataDef (dd, (Gamma, rho), interactivity)
  | processDef ((loc, OVERLOAD ps), (Gamma, rho), interactivity) =
      atLoc loc processOverloading (ps, (Gamma, rho), interactivity)
(* definitions of [[basis]] and [[processDef]] for \mcl S482 *)
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
(* definitions of [[basis]] and [[processDef]] for \mcl S483 *)
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
(* type declarations for consistency checking *)
val _ = op processDef : def * basis * interactivity -> basis
fun dump_names (Gamma, rho) = app (println o fst) Gamma (*OMIT*)

(* shared definition of [[withHandlers]] S239a *)
fun withHandlers f a caught =
  f a
  handle RuntimeError msg   => caught ("Run-time error <at loc>: " ^ msg)
       | NotFound x         => caught ("Name " ^ x ^ " not found <at loc>")
       | Located (loc, exn) =>
           withHandlers (fn _ => raise exn)
                        a
                        (fn s => caught (fillAtLoc (s, loc)))

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] S239b *)
       | Div                => caught ("Division by zero <at loc>")
       | Overflow           => caught ("Arithmetic overflow <at loc>")
       | Subscript          => caught ("Array index out of bounds <at loc>")
       | Size               => caught (
                                "Array length too large (or negative) <at loc>")
       | IO.Io { name, ...} => caught ("I/O error <at loc>: " ^ name)

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] ((type-checking)) S392c *)
       | TypeError         msg => caught ("type error <at loc>: " ^ msg)
       | BugInTypeChecking msg => caught ("bug in type checking: " ^ msg)
(* shared unit-testing utilities S225a *)
fun failtest strings = 
  (app eprint strings; eprint "\n"; false)
(* type declarations for consistency checking *)
val _ = op failtest : string list -> bool
(* shared unit-testing utilities S225b *)
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
(* definition of [[testIsGood]] for {\mcl} S541c *)
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
(* type declarations for consistency checking *)
val _ = op comparisonIndex : binding env -> ty -> int error
(* definition of [[testIsGood]] for {\mcl} S542a *)
fun testIsGood (test, (E, rho)) =
  let fun ty e = typeof (e, E)
                 handle NotFound x =>
                   raise TypeError ("name " ^ x ^ " is not defined")

 (* shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]] S409b *)
      fun checkTypeChecks (e, tau) =
        let val tau' = ty e
        in  true
        end
        handle TypeError msg => 
          failtest ["In (check-type ", expString e, " " ^ typeString tau, "), ",
                                                                            msg]

 (* shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]] S396c *)
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

 (* shared [[check{Expect,Assert,Error,Type}Checks]], which call [[ty]] S396d *)
      fun checkOneExpChecks inWhat e =
        let val tau1 = ty e
        in  true
        end handle TypeError msg =>
        failtest ["In (", inWhat, " ", expString e, "), ", msg]
      val checkAssertChecks = checkOneExpChecks "check-assert"
      val checkErrorChecks  = checkOneExpChecks "check-error"
      (* definition of [[checks]] for \mcl S542b *)
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
      (* definition of [[deftystring]] for \mcl S543c *)
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
      (* type declarations for consistency checking *)
      val _ = op deftystring : baredef -> string

      fun outcome e =
        withHandlers (fn () => OK (eval (e, rho))) () (ERROR o stripAtLoc)

      (* definition of [[asSyntacticValue]] for \mcl S543b *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue (VCONX c)   = SOME (CONVAL (c, []))
        | asSyntacticValue (APPLY (e, es, _)) =
            (case (asSyntacticValue e, optionList (map asSyntacticValue es))
               of (SOME (CONVAL (c, [])), SOME vs) => SOME (CONVAL (c, map ref
                                                                            vs))
                | _ => NONE)
        | asSyntacticValue _ = NONE
      (* type declarations for consistency checking *)
      val _ = op asSyntacticValue : exp -> value option
      (* shared [[whatWasExpected]] S223c *)
      fun whatWasExpected (e, outcome) =
        case asSyntacticValue e
          of SOME v => valueString v
           | NONE =>
               case outcome
                 of OK v => valueString v ^ " (from evaluating " ^ expString e ^
                                                                             ")"
                  | ERROR _ =>  "the result of evaluating " ^ expString e
      (* type declarations for consistency checking *)
      val _ = op whatWasExpected  : exp * value error -> string
      val _ = op asSyntacticValue : exp -> value option
      (* shared [[checkExpectPassesWith]], which calls [[outcome]] S224a *)
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
      (* type declarations for consistency checking *)
      val _ = op checkExpectPassesWith : (value * value -> bool) -> exp * exp ->
                                                                            bool
      val _ = op outcome  : exp -> value error
      val _ = op failtest : string list -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] S224b *)
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
      (* type declarations for consistency checking *)
      val _ = op checkAssertPasses : exp -> bool

(* shared [[checkAssertPasses]] and [[checkErrorPasses]], which call [[outcome]] S224c *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, " expected evaluating ", expString checkx
                                                                               ,
                             " to cause an error, but evaluation produced ",
                             valueString check]
      (* type declarations for consistency checking *)
      val _ = op checkErrorPasses : exp -> bool
      (* definition of [[checkExpectPasses]] for \mcl S543a *)
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
      (* definition of [[checkMtypePasses]] for \mcl S544a *)
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

(* shared [[checkTypePasses]] and [[checkTypeErrorPasses]], which call [[ty]] S396a *)
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

(* shared [[checkTypePasses]] and [[checkTypeErrorPasses]], which call [[ty]] S396b *)
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
(* type declarations for consistency checking *)
val _ = op testIsGood : unit_test * (binding env * value ref env) -> bool
(* shared definition of [[processTests]] S226 *)
fun processTests (tests, rho) =
      reportTestResults (numberOfGoodTests (tests, rho), length tests)
and numberOfGoodTests (tests, rho) =
  foldr (fn (t, n) => if testIsGood (t, rho) then n + 1 else n) 0 tests
(* type declarations for consistency checking *)
val _ = op processTests : unit_test list * basis -> unit
(* shared read-eval-print loop S237 *)
fun readEvalPrintWith errmsg (xdefs, basis, interactivity) =
  let val unitTests = ref []

(* definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]] S238b *)
      fun processXDef (xd, basis) =
        let (* definition of [[useFile]], to read from a file S238a *)
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
      (* type declarations for consistency checking *)
      val _ = op errmsg     : string -> unit
      val _ = op processDef : def * basis * interactivity -> basis
      val basis = streamFold processXDef basis xdefs
      val _     = processTests (!unitTests, basis)
(* type declarations for consistency checking *)
type basis = basis
val _ = op processDef   : def * basis * interactivity -> basis
val _ = op testIsGood   : unit_test      * basis -> bool
val _ = op processTests : unit_test list * basis -> unit
(* type declarations for consistency checking *)
val _ = op readEvalPrintWith : (string -> unit) ->                     xdef
                                         stream * basis * interactivity -> basis
val _ = op processXDef       : xdef * basis -> basis
  in  basis
  end



(*****************************************************************)
(*                                                               *)
(*   FUNCTIONS FOR BUILDING PRIMITIVES WHEN TYPES ARE CHECKED    *)
(*                                                               *)
(*****************************************************************)

(* functions for building primitives when types are checked S393a *)
fun binaryOp f = (fn [a, b] => f (a, b) | _ => raise BugInTypeChecking "arity 2"
                                                                               )
fun unaryOp  f = (fn [a]    => f a      | _ => raise BugInTypeChecking "arity 1"
                                                                               )
(* type declarations for consistency checking *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* functions for building primitives when types are checked S393b *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeChecking "arithmetic on non-numbers")
(* type declarations for consistency checking *)
val _ = op arithOp : (int * int -> int) -> (value list -> value)


(*****************************************************************)
(*                                                               *)
(*   LIST OF (TYPED) PRIMITIVES FOR EACH PRIMITIVE MODULE        *)
(*                                                               *)
(*****************************************************************)

(* list of (typed) primitives for each primitive module S488d *)
type primitive = name * primop * ty
fun compval (x, v, ty) = (x, COMPVAL ty)
fun decval  (x, v, ty) = (x, ENVVAL  ty)
(* type declarations for consistency checking *)
type primitive = primitive
val _ = op compval : primitive -> name * component
val _ = op decval  : primitive -> name * binding
(* list of (typed) primitives for each primitive module S488e *)
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
(* type declarations for consistency checking *)
val _ = op eqPrintPrims : ty -> (value -> ''a) -> primitive list
(* list of (typed) primitives for each primitive module S489a *)
val symPrims =
  eqPrintPrims symtype
               (fn SYM s => s
                 | _ => raise BugInTypeChecking "comparing non-symbols")
val boolPrims =
  eqPrintPrims booltype
               (fn CONVAL (K, []) => K
                 | _ => raise BugInTypeChecking "comparing non-Booleans")
(* type declarations for consistency checking *)
val _ = op symPrims  : primitive list
val _ = op boolPrims : primitive list
(* list of (typed) primitives for each primitive module S489b *)
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
(* list of (typed) primitives for each primitive module S489c *)
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
(* type declarations for consistency checking *)
val _ = op intPrims : primitive list
(* list of (typed) primitives for each primitive module S490 *)
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
(* type declarations for consistency checking *)
val _ = op arrayPrims  : primitive list
val _ = op uarrayPrims : primitive list
val _ = op arraymodtype  : modty
val _ = op uarraymodtype : modty


(*****************************************************************)
(*                                                               *)
(*   PRIMITIVE MODULES AND DEFINITION OF [[INITIALBASIS]]        *)
(*                                                               *)
(*****************************************************************)

(* primitive modules and definition of [[initialBasis]] S492a *)
local
  fun module id primvals =
    let val typeT = ("t", COMPABSTY (PDOT (PNAME id, "t")))
    in  ENVMOD (MTEXPORTS (typeT :: map compval primvals), PNAME id)
    end
  val unitinfo = ("unit", unitVal, TYNAME (PDOT (PNAME unitmodident, "t")))
(* type declarations for consistency checking *)
val _ = op module : modident -> primitive list -> binding
in
  val intmod  = module intmodident intPrims
  val symmod  = module symmodident symPrims
  val boolmod = module boolmodident boolPrims
  val unitmod = module unitmodident [unitinfo]
  val arraymod  = ENVMOD (arraymodtype,  PNAME arraymodident)
  val uarraymod = ENVMOD (uarraymodtype, PNAME uarraymodident)
end
(* primitive modules and definition of [[initialBasis]] S492b *)
fun addValWith f ((x, v, ty), rho) = bind (x, f v, rho)
val intmodenv    = foldl (addValWith (ref o PRIMITIVE)) emptyEnv intPrims
val arraymodenv  = foldl (addValWith (ref o PRIMITIVE)) emptyEnv arrayPrims
val boolmodenv   = foldl (addValWith (ref o PRIMITIVE)) emptyEnv boolPrims
val unitmodenv = bind ("unit", ref (CONVAL (PNAME "unit", [])), emptyEnv)
val symmodenv  = foldl (addValWith (ref o PRIMITIVE)) emptyEnv symPrims
(* type declarations for consistency checking *)
val _ = op intmodenv : value ref env
(* primitive modules and definition of [[initialBasis]] S492c *)

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
(* type declarations for consistency checking *)
val _ = op modules : (name * binding * value) list
(* primitive modules and definition of [[initialBasis]] S493a *)
val emptyBasis = (emptyEnv, emptyEnv)
val initialBasis = 
  let fun addmod ((x, dbl, v), (Gamma, rho)) =
        (bind (x, dbl, Gamma), bind (x, ref v, rho))
  in  foldl addmod emptyBasis modules
  end

val initialBasis =
  let val predefinedTypes =
            
             [ ";  predefined Molecule types, functions, and modules 529a "
             , "(type int  Int.t)"
             , "(type bool Bool.t)"
             , "(type unit Unit.t)"
             , "(type sym  Sym.t)"
             , ";  predefined Molecule types, functions, and modules 529b "
             , "(module-type ARRAY"
             , " (exports [abstype t]    ;; an array"
             , "          [abstype elem] ;; one element of the array"
             , "          [new    : (int elem -> t)]"
             , "          [empty  : (         -> t)]"
             , "          [size   : (t -> int)]     "
             , "          [at     : (t int -> elem)]"
             , "          [at-put : (t int elem -> unit)]))"
             , ";  predefined Molecule types, functions, and modules 540a "
             , "(module-type GENERIC-ARRAY"
             , " ([Elem : (exports [abstype t])] --m->"
             , "     (exports [abstype t]        ;; an array"
             , "              [type elem Elem.t] ;; one element of the array"
             , "              [new    : (int elem -> t)]"
             , "              [empty  : (         -> t)]"
             , "              [size   : (t -> int)]     "
             , "              [at     : (t int -> elem)]"
             , "              [at-put : (t int elem -> unit)])))"
             , ";  predefined Molecule types, functions, and modules 541b "
             , "(module [IntArray : (allof ARRAY (exports [type elem Int.t]))]"
             , "   (@m Array Int))"
             ,
";  predefined Molecule types, functions, and modules ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
             , ";  definition of module [[Char]] S484b "
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
             , ";    definitions inside module [[Char]] 558a "
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
             , ";  definition of module [[String]] 541a "
             , "(module String (@m Array Char))"
             , ";  predefined Molecule types, functions, and modules 543a "
             , "(overload Int.+ Int.-  Int.* Int./ Int.negated"
             , "          Int.= Int.!= Int.< Int.> Int.<= Int.>="
             , "          Int.print Int.println)"
             , "(overload Bool.= Bool.!= Bool.print Bool.println)"
             , "(overload Sym.=  Sym.!=  Sym.print  Sym.println)"
             , "(overload Char.= Char.!= Char.print Char.println)"
             , ";  predefined Molecule types, functions, and modules S484a "
             , ";  \\mcl's predefined module types S485a "
             , "(module-type PRINTS"
             , "   (exports [abstype t]"
             , "            [print : (t -> unit)]"
             , "            [println : (t -> unit)]))"
             , ";  \\mcl's predefined module types S485b "
             , "(module-type BOOL"
             , "   (exports [abstype t]"
             , "            [#f : t]"
             , "            [#t : t]))"
             , " ;;;; omitted: and, or, not, similar?, copy, print, println"
             , ";  \\mcl's predefined module types S485c "
             , "(module-type SYM"
             , "   (exports [abstype t]"
             , "            [=  : (t t -> Bool.t)]"
             , "            [!= : (t t -> Bool.t)]))"
             , " ;;;; omitted: hash, similar?, copy, print, println"
             , ";  \\mcl's predefined module types S485d "
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
             , ";  \\mcl's predefined module types S485e "
             , "(module-type RELATIONS"
             , "  (exports [abstype t]"
             , "           [<  : (t t -> Bool.t)]"
             , "           [<= : (t t -> Bool.t)]"
             , "           [>  : (t t -> Bool.t)]"
             , "           [>= : (t t -> Bool.t)]"
             , "           [=  : (t t -> Bool.t)]"
             , "           [!= : (t t -> Bool.t)]))"
             , ";  \\mcl's predefined module types S485f "
             , "(generic-module [Relations : ([M : (exports [abstype t]"
             ,
    "                                            [compare : (t t -> Order.t)])]"
             , "                               --m-> (allof RELATIONS"
             ,
         "                                            (exports [type t M.t])))]"
             , "  (type t M.t)"
             , ";    definitions of the six relational predicates S486a "
             , "  (define bool < ([x : t] [y : t])"
             , "     (case (M.compare x y)"
             , "        [Order.LESS #t]"
             , "        [_    #f]))"
             , "  (define bool > ([x : t] [y : t])"
             , "     (case (M.compare y x)"
             , "        [Order.LESS #t]"
             , "        [_    #f]))"
             , ";    definitions of the six relational predicates S486b "
             , "  (define bool <= ([x : t] [y : t])"
             , "     (case (M.compare x y)"
             , "        [Order.GREATER #f]"
             , "        [_       #t]))"
             , "  (define bool >= ([x : t] [y : t])"
             , "     (case (M.compare y x)"
             , "        [Order.GREATER #f]"
             , "        [_       #t]))"
             , ";    definitions of the six relational predicates S486c "
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
             , ";  \\mcl's predefined module types S21 "
             , "(module-type INT"
             , "  (exports [abstype t]                 [<  : (t t -> Bool.t)]"
             , "           [+ : (t t -> t)]            [<= : (t t -> Bool.t)]"
             , "           [- : (t t -> t)]            [>  : (t t -> Bool.t)]"
             , "           [* : (t t -> t)]            [>= : (t t -> Bool.t)]"
             , "           [/ : (t t -> t)]            [=  : (t t -> Bool.t)]"
             , "           [negated : (t -> t)]        [!= : (t t -> Bool.t)]"
             , "           [print   : (t -> Unit.t)]"
             , "           [println : (t -> Unit.t)]))"
             , "(define bool and ([b : bool] [c : bool]) (if b c b))"
             , "(define bool or  ([b : bool] [c : bool]) (if b b c))"
             ,
              "(define bool not ([b : bool])            (if b (= 1 0) (= 0 0)))"
             , "(define int mod ([m : int] [n : int]) (- m (* n (/ m n))))"
             , ";  predefined Molecule types, functions, and modules S486d "
             , ";  arraylist.mcl 547 "
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
             , ";  arraylist.mcl S486e "
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
             , ";     definitions of operations in [[ArrayList]] S486f "
             , "   (define t from ([i : int])"
             , "     (Rep.make (U.new 3) i 0 0))"
             , ""
             , "   (define int size ([a : t]) (Rep.population a))"
             , ";     definitions of operations in [[ArrayList]] S487a "
             , "   (define bool in-bounds? ([a : t] [i : int])"
             , "     (if (>= i (Rep.low-index a))"
             , "         (< (- i (Rep.low-index a)) (Rep.population a))"
             , "         #f))"
             , ";     definitions of operations in [[ArrayList]] S487b "
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
             , ";     definitions of operations in [[ArrayList]] S487c "
             , "   (define int lo     ([a : t]) (Rep.low-index a))   "
             ,
     "   (define int nexthi ([a : t]) (+ (Rep.low-index a) (Rep.population a)))"
             , ";     definitions of operations in [[ArrayList]] S487d "
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
             , ";     definitions of operations in [[ArrayList]] S487e "
             , "   (define unit addhi ([a : t] [v : elem])"
             , "     (maybe-grow a)"
             , "     (let ([i (nexthi a)])"
             , "        (Rep.set-population! a (+ (Rep.population a) 1))"
             , "        (at-put a i v)))"
             , ";     definitions of operations in [[ArrayList]] S487f "
             , "   (define unit addlo ([a : t] [v : elem])"
             , "     (maybe-grow a)"
             , "     (Rep.set-population! a (+ (Rep.population a) 1))"
             , "     (Rep.set-low-index!  a (- (Rep.low-index a)  1))"
             , "     (Rep.set-low-stored! a (- (Rep.low-stored a) 1))"
             , "     (when (< (Rep.low-stored a) 0)"
             ,
 "       (Rep.set-low-stored! a (+ (Rep.low-stored a) (A.size (Rep.elems a)))))"
             , "     (at-put a (Rep.low-index a) v))"
             , ";     definitions of operations in [[ArrayList]] S488a "
             , "   (define elem remhi ([a : t])"
             , "     (if (<= (Rep.population a) 0)"
             , "         (error 'removal-from-empty-array)"
             , "         (let* ([v (at a (- (nexthi a) 1))]"
             ,
         "                [_ (Rep.set-population! a (- (Rep.population a) 1))])"
             , "           v)))"
             , ";     definitions of operations in [[ArrayList]] S488b "
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
             , ";     definitions of operations in [[ArrayList]] S488c "
             , "   (define unit setlo ([a : t] [i : int])"
             , "     (Rep.set-low-index! a i))"
             , ""
             , ")"
             , ";  predefined Molecule types, functions, and modules S491a "
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
             , ";  predefined Molecule types, functions, and modules S491b "
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
(* type declarations for consistency checking *)
val _ = op initialBasis : basis
(* primitive modules and definition of [[initialBasis]] ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
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

(* function [[runStream]], which evaluates input given [[initialBasis]] S240b *)
fun runStream inputName input interactivity basis = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs (inputName, input, prompts)
  in  readEvalPrintWith eprintln (xdefs, basis, interactivity)
  end 
(* type declarations for consistency checking *)
val _ = op withHandlers : ('a -> 'b) -> 'a -> (string -> 'b) -> 'b
(* type declarations for consistency checking *)
val _ = op runStream : string -> TextIO.instream -> interactivity -> basis ->
                                                                           basis


(*****************************************************************)
(*                                                               *)
(*   LOOK AT COMMAND-LINE ARGUMENTS, THEN RUN                    *)
(*                                                               *)
(*****************************************************************)

(* look at command-line arguments, then run S240c *)
fun runPathWith interactivity ("-", basis) =
      runStream "standard input" TextIO.stdIn interactivity basis
  | runPathWith interactivity (path, basis) =
      let val fd = TextIO.openIn path
      in  runStream path fd interactivity basis
          before TextIO.closeIn fd
      end 
(* type declarations for consistency checking *)
val _ = op runPathWith : interactivity -> (string * basis -> basis)
(* look at command-line arguments, then run S240d *)
val usage = ref (fn () => ())
(* type declarations for consistency checking *)
val _ = op usage : (unit -> unit) ref
(* look at command-line arguments, then run S241a *)
datatype action
  = RUN_WITH of interactivity  (* call runPathWith on remaining arguments *)
  | DUMP     of unit -> unit   (* dump information *)
  | FAIL     of string         (* signal a bad command line *)
  | DEFAULT                    (* no command-line options were given *)
(* look at command-line arguments, then run S241b *)
val default_action = RUN_WITH (PROMPTING, ECHOING)
(* look at command-line arguments, then run S241c *)
fun perform (RUN_WITH interactivity, []) =
      perform (RUN_WITH interactivity, ["-"])
  | perform (RUN_WITH interactivity, args) =
      ignore (foldl (runPathWith interactivity) initialBasis args)
  | perform (DUMP go, [])     = go ()
  | perform (DUMP go, _ :: _) = perform (FAIL "Dump options take no files", [])
  | perform (FAIL msg, _)     = (eprintln msg; !usage())
  | perform (DEFAULT, args)   = perform (default_action, args)
(* type declarations for consistency checking *)
val _ = op perform: action * string list -> unit
(* look at command-line arguments, then run S241d *)
fun merge (_, a as FAIL _) = a
  | merge (a as FAIL _, _) = a
  | merge (DEFAULT, a) = a
  | merge (_, DEFAULT) = raise InternalError "DEFAULT on the right in MERGE"
  | merge (RUN_WITH _, right as RUN_WITH _) = right
  | merge (DUMP f, DUMP g) = DUMP (g o f)
  | merge (_, r) = FAIL "Interpret or dump, but don't try to do both"
(* type declarations for consistency checking *)
val _ = op merge: action * action -> action
(* look at command-line arguments, then run S241e *)
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
(* type declarations for consistency checking *)
val _ = op actions : (string * action) list
(* look at command-line arguments, then run S242a *)
val _ = usage := (fn () =>
  ( app eprint ["Usage:\n"]
  ; app (fn (option, action) =>
         app eprint ["       ", CommandLine.name (), " ", option, "\n"]) actions
  ))
(* look at command-line arguments, then run S242b *)
fun action option =
  case List.find (curry op = option o fst) actions
    of SOME (_, action) => action
     | NONE => FAIL ("Unknown option " ^ option)
(* type declarations for consistency checking *)
val _ = op action : string -> action
(* look at command-line arguments, then run S242c *)
fun strip_options a [] = (a, [])
  | strip_options a (arg :: args) =
      if String.isPrefix "-" arg andalso arg <> "-" then
          strip_options (merge (a, action arg)) args
      else
          (a, arg :: args)

val _ = if hasOption "NORUN" then ()
        else perform (strip_options DEFAULT (CommandLine.arguments ()))
(* type declarations for consistency checking *)
val _ = op strip_options : action -> string list -> action * string list
