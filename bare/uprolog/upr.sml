(* upr.sml S593a *)


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
(* streams S114a *)
fun every xs () k = streamConcatMap k xs
val run = ()
(* streams S114b *)
fun cartesian xs ys =
  every xs run (fn x => 
    every ys run (fn y =>
      streamOfList [(x, y)]))
(* type declarations for consistency checking *)
val _ = op every : 'a stream -> unit -> ('a -> 'b stream) -> 'b stream
(* type declarations for consistency checking *)
val _ = op cartesian : 'a stream -> 'b stream -> ('a * 'b) stream
(* streams S114c *)
fun streamOfCPS cpsSource =
  cpsSource (fn theta => fn resume => theta ::: resume ()) (fn () => EOS)
val _ = streamOfCPS : (('a -> (unit->'a stream) -> 'a stream) -> (unit->'a
                                     stream) -> 'a stream) -> 'a stream (*OMIT*)
val _ = streamOfCPS (fn succ => fn fail => succ [("a", 3)] (fn () => EOS)) : (
                                             string * int) list stream  (*OMIT*)
(* streams S114d *)
fun cpsStream answers succ fail =
  case streamGet answers
    of NONE => fail ()
     | SOME (theta, answers) =>
         succ theta (fn () => cpsStream answers succ fail)
(* type declarations for consistency checking *)
val _ = op cpsStream : 'subst stream -> ('subst -> (unit->'a) -> 'a) -> (unit->
                                                                       'a) -> 'a
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
(*   ABSTRACT SYNTAX FOR \UPROLOG                                *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax for \uprolog S55c *)
(* definitions of [[term]], [[goal]], and [[clause]] for \uprolog S54c *)
datatype term = VAR     of name
              | LITERAL of int
              | APPLY   of name * term list
(* definitions of [[term]], [[goal]], and [[clause]] for \uprolog S54d *)
type goal = name * term list
(* definitions of [[term]], [[goal]], and [[clause]] for \uprolog S54e *)
datatype clause = :- of goal * goal list
infix 3 :-
(* definitions of [[def]] and [[unit_test]] for \uprolog S55a *)
datatype cq
  = ADD_CLAUSE of clause
  | QUERY      of goal list
type def = cq
(* definitions of [[def]] and [[unit_test]] for \uprolog S55b *)
datatype unit_test 
  = CHECK_SATISFIABLE   of goal list
  | CHECK_UNSATISFIABLE of goal list
  | CHECK_SATISFIED     of goal list * (name * term) list
(* definition of [[xdef]] (shared) S214b *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* definitions of [[termString]], [[goalString]], and [[clauseString]] S597d *)
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
(* definitions of [[termString]], [[goalString]], and [[clauseString]] S597e *)
fun goalString g = termString (APPLY g)
fun clauseString (g :- []) = goalString g
  | clauseString (g :- (h :: t)) =
      String.concat (goalString g :: " :- " :: goalString h ::
                     (foldr (fn (g, tail) => ", " :: goalString g :: tail)) [] t
                                                                               )
(* definitions of [[termString]], [[goalString]], and [[clauseString]] S598a *)
fun substString pairs =
      nullOrCommaSep "no substitution"
      (map (fn (x, t) => x ^ " = " ^ termString t) pairs)


(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR TRACING \UPROLOG\ COMPUTATION                   *)
(*                                                               *)
(*****************************************************************)

(* support for tracing \uprolog\ computation S609c *)
val tracer = ref (app print)
val _ = tracer := (fn _ => ())
fun trace l = !tracer l
(* type declarations for consistency checking *)
val _ = op trace : string list -> unit


(*****************************************************************)
(*                                                               *)
(*   SUBSTITUTION AND UNIFICATION                                *)
(*                                                               *)
(*****************************************************************)

(* substitution and unification ((upr)) S78b *)
datatype con = ~  of term * term
             | /\ of con  * con
             | TRIVIAL
infix 4 ~
infix 3 /\

(* free variables (terms/goals/clauses) S79a *)
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
(* type declarations for consistency checking *)
val _ = op termFreevars   : term   -> name set
val _ = op goalFreevars   : goal   -> name set
val _ = op clauseFreevars : clause -> name set
(* substitutions for \uprolog S596b *)
type subst = term env
val idsubst = emptyEnv
(* type declarations for consistency checking *)
type subst = subst
val _ = op idsubst : subst
(* substitutions for \uprolog S596c *)
fun varsubst theta = 
  (fn x => find (x, theta) handle NotFound _ => VAR x)
(* type declarations for consistency checking *)
val _ = op varsubst : subst -> (name -> term)
(* substitutions for \uprolog S596d *)
fun termsubst theta =
  let fun subst (VAR x)         = varsubst theta x
        | subst (LITERAL n)     = LITERAL n
        | subst (APPLY (f, ts)) = APPLY (f, map subst ts)
(* type declarations for consistency checking *)
val _ = op termsubst : subst -> (term -> term)
  in  subst
  end
(* substitutions for \uprolog S596e *)
fun goalsubst   theta (f, ts)   = (f, map (termsubst theta) ts)
fun clausesubst theta (c :- ps) = (goalsubst theta c :- map (goalsubst theta) ps
                                                                               )
(* type declarations for consistency checking *)
val _ = op goalsubst   : subst -> (goal   -> goal)
val _ = op clausesubst : subst -> (clause -> clause)
(* substitutions for \uprolog S597a *)
fun consubst theta =
  let fun subst (t1 ~  t2) = termsubst theta t1 ~ termsubst theta t2
        | subst (c1 /\ c2) = subst c1 /\ subst c2
        | subst TRIVIAL    = TRIVIAL
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op consubst : subst -> (con -> con)
(* substitutions for \uprolog S597b *)
infix 7 |-->
fun x |--> (VAR x') = if x = x' then idsubst else bind (x, VAR x', emptyEnv)
  | x |--> t        = if member x (termFreevars t) then
                        raise InternalError "non-idempotent substitution"
                      else
                        bind (x, t, emptyEnv)
(* type declarations for consistency checking *)
val _ = op |--> : name * term -> subst
(* substitutions for \uprolog S597c *)
fun dom theta =
  map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = termsubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end

(* type declarations for consistency checking *)
type subst = subst
val _ = op idsubst : subst
val _ = op |-->    : name * term -> subst
val _ = op varsubst    : subst -> (name   -> term)
val _ = op termsubst   : subst -> (term   -> term)
val _ = op goalsubst   : subst -> (goal   -> goal)
val _ = op clausesubst : subst -> (clause -> clause)
type con = con
val _ = op consubst    : subst -> (con -> con)
(* substitution and unification ((upr)) S79d *)
exception Unsatisfiable
(* constraint solving ((prototype)) S79e *)
fun solve c = raise LeftAsExercise "solve"
(* type declarations for consistency checking *)
val _ = op solve : con -> subst
fun unify ((f, ts), (f', ts')) =
  solve (APPLY (f, ts) ~ APPLY (f', ts'))
(* type declarations for consistency checking *)
val _ = op unify : goal * goal -> subst


(*****************************************************************)
(*                                                               *)
(*   RENAMING \UPROLOG\ VARIABLES                                *)
(*                                                               *)
(*****************************************************************)

(* renaming \uprolog\ variables S79b *)
local
  val n = ref 1
in
  fun freshVar s = VAR ("_" ^ s ^ intString (!n) before n := !n + 1)
(* type declarations for consistency checking *)
val _ = op freshVar : string -> term
end
(* renaming \uprolog\ variables S79c *)
fun freshen c =
  let val renamings = map (fn x => x |--> freshVar x) (clauseFreevars c)
      val renaming  = foldl compose idsubst renamings
  in  clausesubst renaming c
  end
(* type declarations for consistency checking *)
val _ = op freshen : clause -> clause


(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UPROLOG, PROVIDING [[XDEFSINMODE]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \uprolog, providing [[xdefsInMode]] S598b *)
(* lexical analysis for \uprolog S598c *)
datatype token 
  = UPPER     of string
  | LOWER     of string
  | SYMBOLIC  of string
  | INT_TOKEN of int
  | RESERVED  of string
  | EOF
(* type declarations for consistency checking *)
val _ = op compose : subst * subst -> subst
(* type declarations for consistency checking *)
type token = token
(* lexical analysis for \uprolog S598d *)
fun tokenString (UPPER s)     = s
  | tokenString (LOWER s)     = s
  | tokenString (INT_TOKEN n) = intString n
  | tokenString (SYMBOLIC  s) = s
  | tokenString (RESERVED  s) = s
  | tokenString EOF           = "<end-of-file>"
(* lexical analysis for \uprolog S599b *)
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
(* type declarations for consistency checking *)
val _ = op symbolic : string -> token
val _ = op lower    : string -> token
(* lexical analysis for \uprolog S599c *)
fun anonymousVar () =
  case freshVar ""
    of VAR v => UPPER v
     | _ => raise InternalError "\"fresh variable\" is not a VAR"
local
  (* character-classification functions for \uprolog S599a *)
  val symbols = explode "!%^&*-+:=|~<>/?`$\\"
  fun isSymbol c = List.exists (fn c' => c' = c) symbols
  fun isIdent  c = Char.isAlphaNum c orelse c = #"_"
  fun isSpace  c = Char.isSpace c
  fun isDelim  c = not (isIdent c orelse isSymbol c)
  (* lexical utility functions for \uprolog S599d *)
  fun underscore _ [] = OK (anonymousVar ())
    | underscore c cs = ERROR ("name may not begin with underscore at " ^
                                   implode (c::cs))

  fun int cs [] = intFromChars cs >>=+ INT_TOKEN
    | int cs ids = 
        ERROR ("integer literal " ^ implode cs ^
               " may not be followed by '" ^ implode ids ^ "'")
  (* type declarations for consistency checking *)
  val _ = op underscore : char      -> char list -> token error
  val _ = op int        : char list -> char list -> token error
  (* lexical utility functions for \uprolog S600a *)
  fun unrecognized (ERROR _) = raise InternalError "this can't happen"
    | unrecognized (OK cs) =
        case cs
          of []        => NONE
           | #";" :: _ => raise InternalError "this can't happen"
           | _ =>
               SOME (ERROR ("invalid initial character in `" ^ implode cs ^ "'")
                                                                          , EOS)
  (* type declarations for consistency checking *)
  val _ = op unrecognized : char list error -> ('a error * 'a error stream)
                                                                          option
  (* lexical utility functions for \uprolog S600b *)
  fun nextline (file, line) = (file, line+1)
  (* type declarations for consistency checking *)
  val _ = op nextline : srcloc -> srcloc
in
  (* lexical analyzers for for \uprolog S600c *)
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
  (* lexical analyzers for for \uprolog S600d *)
  fun manySat p =
    many (sat p char)

  val whitespace =
    manySat isSpace
  val intChars = 
    (curry op :: <$> eqx #"-" char <|> pure id) <*> many1 (sat Char.isDigit char
                                                                               )
  (* type declarations for consistency checking *)
  type 'a prolog_lexer = 'a prolog_lexer
  val _ = op char : char prolog_lexer
  val _ = op eol  : unit prolog_lexer
  (* type declarations for consistency checking *)
  val _ = op manySat    : (char -> bool) -> char list prolog_lexer
  val _ = op whitespace : char list prolog_lexer
  val _ = op intChars   : char list prolog_lexer
  (* lexical analyzers for for \uprolog S600e *)
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
  (* lexical analyzers for for \uprolog S601a *)
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
  (* type declarations for consistency checking *)
  val _ = op ordinaryToken : token prolog_lexer
  (* type declarations for consistency checking *)
  val _ = op tokenAt     : srcloc -> token located prolog_lexer
  val _ = op skipComment : srcloc -> srcloc -> token located prolog_lexer
  end
end
(* lexical analysis and parsing for \uprolog, providing [[xdefsInMode]] S601b *)
(* parsers and streams for \uprolog S601c *)
type 'a parser = (token, 'a) polyparser
val symbol = asAscii ((fn SYMBOLIC  s => SOME s | _ => NONE) <$>? token)
val upper  = asAscii ((fn UPPER     s => SOME s | _ => NONE) <$>? token)
val lower  = asAscii ((fn LOWER     s => SOME s | _ => NONE) <$>? token)
val int    =          (fn INT_TOKEN n => SOME n | _ => NONE) <$>? token
fun reserved s = eqx s ((fn RESERVED s => SOME s | _ => NONE) <$>? token)
(* type declarations for consistency checking *)
val _ = op symbol : string parser
val _ = op upper  : string parser
val _ = op lower  : string parser
val _ = op int    : int    parser
(* parsers and streams for \uprolog S602a *)
val notSymbol =
  symbol <!> "arithmetic expressions must be parenthesized" <|>
  pure ()
(* type declarations for consistency checking *)
val _ = op notSymbol : unit parser
(* parsers and streams for \uprolog S602b *)
val nilt = APPLY ("nil", [])
fun cons (x, xs) = APPLY ("cons", [x, xs])
(* type declarations for consistency checking *)
val _ = op nilt : term
val _ = op cons : term * term -> term
(* parsers and streams for \uprolog S602c *)
val variable        = upper
val binaryPredicate = symbol
val functr          = lower
fun commas p = 
  curry op :: <$> p <*> many (reserved "," *> p)
(* type declarations for consistency checking *)
val _ = op variable        : string parser
val _ = op binaryPredicate : string parser
val _ = op functr          : string parser
val _ = op commas : 'a parser -> 'a list parser
(* parsers and streams for \uprolog S602d *)
fun closing bracket = reserved bracket <?> bracket
fun wrap left right p = reserved left *> p <* closing right
(* type declarations for consistency checking *)
val _ = op wrap : string -> string -> 'a parser -> 'a parser
(* parsers and streams for \uprolog S603a *)
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
(* type declarations for consistency checking *)
val _ = op term   : term parser
val _ = op atom   : term parser
val _ = op commas : 'a parser -> 'a list parser
end
(* parsers and streams for \uprolog S603b *)
fun asGoal _   (APPLY g) = OK g
  | asGoal loc (VAR v)   = 
      synerrorAt ("Variable " ^ v ^ " cannot be a predicate") loc
  | asGoal loc (LITERAL n) =
      synerrorAt ("Integer " ^ intString n ^ " cannot be a predicate") loc

val goal = asGoal <$> srcloc <*>! term 
(* type declarations for consistency checking *)
val _ = op asGoal : srcloc -> term -> goal error
val _ = op goal   : goal parser
(* parsers and streams for \uprolog S604a *)
datatype concrete
  = BRACKET of string 
  | CLAUSE  of goal * goal list option
  | GOALS   of goal list
  | CTEST   of unit_test
(* type declarations for consistency checking *)
type concrete = concrete
(* parsers and streams for \uprolog S604b *)
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
(* type declarations for consistency checking *)
val _ = op checkSatisfied : goal list -> unit_test error
(* parsers and streams for \uprolog S604c *)
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
(* type declarations for consistency checking *)
val _ = op unit_test : unit_test parser
(* parsers and streams for \uprolog S605a *)
val notClosing =
  sat (fn RESERVED "]" => false | _ => true) token
val concrete = 
     (BRACKET o concat o map tokenString) <$> wrap "[" "]" (many notClosing)
 <|> CTEST <$> unit_test
 <|> curry CLAUSE <$> goal <*> reserved ":-" *> (SOME <$> commas goal)
 <|> GOALS <$> commas goal
(* type declarations for consistency checking *)
val _ = op concrete : concrete parser
(* parsers and streams for \uprolog S605b *)
datatype mode = QMODE | RMODE
fun mprompt RMODE = "-> "
  | mprompt QMODE = "?- "
(* type declarations for consistency checking *)
type mode = mode
val _ = op mprompt : mode -> string
(* parsers and streams for \uprolog S605c *)
datatype xdef_or_mode
  = XDEF of xdef
  | NEW_MODE of mode
(* type declarations for consistency checking *)
type xdef_or_mode = xdef_or_mode
(* parsers and streams for \uprolog S606a *)
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
(* type declarations for consistency checking *)
val _ = op interpretConcrete : mode -> concrete -> xdef_or_mode error
(* parsers and streams for \uprolog S606b *)
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
(* type declarations for consistency checking *)
val _ = op xdef_or_mode : mode -> xdef_or_mode parser
(* parsers and streams for \uprolog S607a *)
fun xdefsInMode initialMode (name, lines, prompts) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref (if ps1 = "" then "" else mprompt initialMode)
      val setPrompt = if ps1 = "" then (fn _ => ()) else (fn s => thePrompt := s
                                                                               )

      type read_state = string * mode * token located eol_marked stream
      (* utility functions for [[xdefsInMode]] S607b *)
      fun startsWithEOF tokens =
        case streamGet tokens
          of SOME (INLINE (_, EOF), _) => true
           | _ => false
      (* type declarations for consistency checking *)
      val _ = op startsWithEOF : token located eol_marked stream -> bool
      (* utility functions for [[xdefsInMode]] S607c *)
      fun skipPastDot tokens =
        case streamGet tokens
          of SOME (INLINE (_, RESERVED "."), tokens) => tokens
           | SOME (INLINE (_, EOF), tokens) => tokens
           | SOME (_, tokens) => skipPastDot tokens
           | NONE => tokens
      (* type declarations for consistency checking *)
      val _ = op skipPastDot : token located eol_marked stream -> token located
                                                               eol_marked stream
      (* utility functions for [[xdefsInMode]] S608a *)
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
                    (* fail epically with a diagnostic about [[tokens]] S608b *)
                         let val tokensStrings =
                               map (fn t => " " ^ tokenString t) o valOf o peek
                                                                    (many token)
                             val _ = app print (tokensStrings tokens)
                         in  raise InternalError "cq parser failed"
                         end
        )                 
      (* type declarations for consistency checking *)
      val _ = op getXdef : read_state -> (xdef * read_state) option

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

(* type declarations for consistency checking *)
val _ = op xdefsInMode : mode -> string * line stream * prompts -> xdef stream
type read_state  = read_state  fun zz__checktyperead_state (x : read_state ) = (
                           x :  string * mode * token located eol_marked stream)
val _ = op getXdef : read_state -> (xdef * read_state) option
  in  streamOfUnfold getXdef (!thePrompt, initialMode, streamMap INLINE tokens)
  end 
val xdefstream = xdefsInMode RMODE
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
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UPROLOG *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \uprolog S593b *)
(* \uprolog's database of clauses S78a *)
type database = clause list
val emptyDatabase = []
fun addClause (r, rs) = rs @ [r] (* must maintain order *)
fun potentialMatches (_, rs) = rs
(* type declarations for consistency checking *)
type database = database
val _ = op emptyDatabase    : database
val _ = op addClause        : clause * database -> database
val _ = op potentialMatches : goal * database -> clause list
(* functions [[eval]], [[is]], and [[compare]], used in primitive predicates S594b *)
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
(* type declarations for consistency checking *)
val _ = op eval : term -> int
(* functions [[eval]], [[is]], and [[compare]], used in primitive predicates S594c *)
fun is [x, e] succ fail = (succ (solve (x ~ LITERAL (eval e))) fail
                           handle Unsatisfiable => fail())
  | is _      _    fail = fail ()
(* functions [[eval]], [[is]], and [[compare]], used in primitive predicates S594e *)
fun compare name cmp [LITERAL n, LITERAL m] succ fail =
      if cmp (n, m) then succ idsubst fail else fail ()
  | compare name _ [_, _] _ _ =
      raise RuntimeError ("Used comparison " ^ name ^ " on non-integer term")
  | compare name _ _ _ _ =
      raise InternalError ("this can't happen---non-binary comparison?!")
(* tracing functions S111 *)
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
(* search ((prototype)) S80 *)
fun 'a query database =
  let val primitives = foldl (fn ((n, p), rho) => bind (n, p, rho))
                       emptyEnv (
                              (* \uprolog's primitive predicates [[::]] S593c *)
                                 ("true", fn args => fn succ => fn fail =>
                                            if null args then succ idsubst fail
                                                                else fail ()) ::

                              (* \uprolog's primitive predicates [[::]] S593d *)
                                 ("atom", fn args => fn succ => fn fail =>
                                             case args of [APPLY(f, [])] => succ
                                                                    idsubst fail
                                                        | _ => fail ()) ::

                              (* \uprolog's primitive predicates [[::]] S594a *)
                                 ("print", fn args => fn succ => fn fail =>
                                             ( app (fn x => (print (termString x
                                                             ); print " ")) args
                                             ; print "\n"
                                             ; succ idsubst fail
                                             )) ::

                              (* \uprolog's primitive predicates [[::]] S594d *)
                                 ("is", is) ::

                              (* \uprolog's primitive predicates [[::]] S594f *)
                                 ("<",  compare "<"  op < ) ::
                                 (">",  compare ">"  op > ) ::
                                 ("=<", compare "=<" op <= ) ::
                                 (">=", compare ">=" op >= ) ::

                              (* \uprolog's primitive predicates [[::]] S594g *)
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
(* type declarations for consistency checking *)
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
(* interaction S81c *)
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
(* type declarations for consistency checking *)
val _ = op showAndContinue : interactivity -> subst -> goal list -> bool
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
(* definitions of [[basis]] and [[processDef]] for \uprolog S81a *)
type basis = database
fun processDef (cq, database, interactivity) =
  let fun process (ADD_CLAUSE c) = addClause (c, database)
        | process (QUERY gs) = (
                              (* query goals [[gs]] against [[database]] S81b *)
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
(* type declarations for consistency checking *)
type basis = basis
val _ = op processDef : cq * database * interactivity -> database
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
(* definition of [[testIsGood]] for \uprolog S595a *)
fun testIsGood (test, database) =
  let
(* definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]] S595b *)
      type query = goal list
      val qstring = 
        nullOrCommaSep "?" o map goalString
      (* type declarations for consistency checking *)
      type query = query
      val _ = op qstring : query -> string

(* definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]] S595c *)
      fun stripSubst theta =
        List.filter (fn (x, _) => String.sub (x, 0) <> #"_") theta
      fun checkUnsatisfiablePasses (gs) =
        let fun succ theta' _ =
              failtest ["check_unsatisfiable failed: ", qstring gs,
                          " is satisfiable with ", substString theta']
            fun fail () = true
        in  query database gs (succ o stripSubst) fail
        end

(* definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]] S595d *)
      fun checkSatisfiablePasses (gs) =
        let fun succ _ _ = true
            fun fail () = failtest ["check_unsatisfiable failed: ", qstring gs,
                                    " is not satisfiable"]
        in  query database gs succ fail
        end

(* definitions of [[checkSatisfiedPasses]] and [[checkUnsatisfiablePasses]] S596a *)
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
(* type declarations for consistency checking *)
val _ = op testIsGood : unit_test * basis -> bool
  in  passes test
  end
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
            (* definition of [[useFile]], to read from a file S608c *)
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
(*   FUNCTION [[RUNAS]] FOR \UPROLOG                             *)
(*                                                               *)
(*****************************************************************)

(* function [[runAs]] for \uprolog S609a *)
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
(* type declarations for consistency checking *)
val _ = op runAs : interactivity -> unit


(*****************************************************************)
(*                                                               *)
(*   CODE THAT LOOKS AT \UPROLOG'S COMMAND-LINE ARGUMENTS AND CALLS [[RUNAS]] *)
(*                                                               *)
(*****************************************************************)

(* code that looks at \uprolog's command-line arguments and calls [[runAs]] S609b *)
fun runmain ["-q"]          = runAs (NOT_PROMPTING, ECHOING)
  | runmain []              = runAs (PROMPTING,     ECHOING)
  | runmain ("-trace" :: t) = (tracer := app eprint; runmain t)
  | runmain _  =
      TextIO.output (TextIO.stdErr,
                     "Usage: " ^ CommandLine.name() ^ " [trace] [-q]\n")
val _ = runmain (CommandLine.arguments())
