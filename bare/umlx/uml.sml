(* uml.sml S437a *)


(*****************************************************************)
(*                                                               *)
(*   EXCEPTIONS USED IN LANGUAGES WITH TYPE INFERENCE            *)
(*                                                               *)
(*****************************************************************)

(* exceptions used in languages with type inference S213d *)
exception TypeError of string
exception BugInTypeInference of string


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
           (* more handlers for [[atLoc]] ((type-inference)) S421c *)
           | e as TypeError _          => raise Located (loc, e)
           | e as BugInTypeInference _ => raise Located (loc, e)
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
(*   HINDLEY-MILNER TYPES WITH GENERATED TYPE CONSTRUCTORS       *)
(*                                                               *)
(*****************************************************************)

(* Hindley-Milner types with generated type constructors S438d *)
(* foundational definitions for generated type constructors 485a *)
type tycon_identity = int
(* foundational definitions for generated type constructors 485b *)
type tycon = { printName : name, identity : tycon_identity }
(* foundational definitions for generated type constructors 485c *)
fun eqTycon ( { identity = id,  printName = _ }
            , { identity = id', printName = _ }) = 
  id = id'
(* type declarations for consistency checking *)
val _ = op eqTycon : tycon * tycon -> bool
(* utility functions for generated type constructors S448d *)
fun tyconString { identity = _, printName = T } = T
(* type declarations for consistency checking *)
val _ = op tyconString : tycon -> string
(* utility functions for generated type constructors S449a *)
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
(* type declarations for consistency checking *)
val _ = op freshPrintName : string -> string
(* utility functions for generated type constructors S449b *)
local
  val nextIdentity = ref 0
  fun freshIdentity () =
    !nextIdentity before nextIdentity := !nextIdentity + 2
in
  fun freshTycon t =
    { identity = freshIdentity(), printName = freshPrintName t }
end
(* type declarations for consistency checking *)
val _ = op freshTycon : name -> tycon
(* representation of Hindley-Milner types 408 *)
type tyvar  = name
datatype ty = TYVAR  of tyvar               (* type variable alpha *)
            | TYCON  of tycon               (* type constructor mu *)
            | CONAPP of ty * ty list        (* type-level application *)

datatype type_scheme = FORALL of tyvar list * ty
(* sets of free type variables in Hindley-Milner types 433a *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
  in  reverse (f (t, emptyset))
  end  
(* type declarations for consistency checking *)
val _ = op freetyvars : ty -> name set
(* type constructors built into \uml\ and \uhaskell 485d *)
val inttycon  = freshTycon "int"
val symtycon  = freshTycon "sym"
(* type constructors built into \uml\ and \uhaskell S440d *)
val funtycon  = freshTycon "function"
val argstycon = freshTycon "arguments"
(* types built into \uml\ and \uhaskell S440c *)
val inttype = TYCON inttycon
val symtype = TYCON symtycon
(* code to construct and deconstruct function types for \uml S440e *)
fun funtype (args, result) = 
  CONAPP (TYCON funtycon, [CONAPP (TYCON argstycon, args), result])

fun asFuntype (CONAPP (TYCON mu, [CONAPP (_, args), result])) =
      if eqTycon (mu, funtycon) then
        SOME (args, result)
      else
        NONE
  | asFuntype _ = NONE
(* type declarations for consistency checking *)
val _ = op funtype   : ty list * ty -> ty
val _ = op asFuntype : ty -> (ty list * ty) option
(* definition of [[typeString]] for Hindley-Milner types S431b *)
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
(* shared utility functions on Hindley-Milner types 410a *)
type subst = ty env
fun dom theta = map (fn (a, _) => a) theta
(* shared utility functions on Hindley-Milner types 410b *)
fun varsubst theta = 
  (fn a => find (a, theta) handle NotFound _ => TYVAR a)
(* type declarations for consistency checking *)
type subst = subst
val _ = op dom : subst -> name set
(* type declarations for consistency checking *)
val _ = op varsubst : subst -> (name -> ty)
(* shared utility functions on Hindley-Milner types 410c *)
fun tysubst theta =
  let fun subst (TYVAR a) = varsubst theta a
        | subst (TYCON c) = TYCON c
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
(* type declarations for consistency checking *)
val _ = op tysubst : subst -> (ty -> ty)
val _ = op subst   :           ty -> ty
  in  subst
  end
(* shared utility functions on Hindley-Milner types 411a *)
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  mkEnv (domain, map replace domain)
  end
(* type declarations for consistency checking *)
val _ = op compose : subst * subst -> subst
(* shared utility functions on Hindley-Milner types ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun eqsubst (theta1, theta2) =
  let val domain  = union (dom theta2, dom theta1)
      fun eqOn a = (varsubst theta1 a = varsubst theta2 a)
  in  List.all eqOn domain
  end
(* shared utility functions on Hindley-Milner types 411b *)
fun instantiate (FORALL (formals, tau), actuals) =
  tysubst (mkEnv (formals, actuals)) tau
  handle BindListLength =>
    raise BugInTypeInference "number of types in instantiation"
(* type declarations for consistency checking *)
val _ = op instantiate : type_scheme * ty list -> ty
(* shared utility functions on Hindley-Milner types 411c *)
infix 7 |-->
fun a |--> (TYVAR a') = if a = a' then emptyEnv
                        else bind (a, TYVAR a', emptyEnv)
  | a |--> tau        = bind (a, tau, emptyEnv)
(* type declarations for consistency checking *)
val _ = op |--> : name * ty -> subst
(* shared utility functions on Hindley-Milner types 411d *)
val idsubst = emptyEnv
(* shared utility functions on Hindley-Milner types 412a *)
fun eqType (TYVAR a, TYVAR a') = a = a'
  | eqType (TYCON c, TYCON c') = eqTycon (c, c')
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType _ = false
and eqTypes (taus, taus') = ListPair.allEq eqType (taus, taus')
(* type declarations for consistency checking *)
val _ = op idsubst : subst
(* type declarations for consistency checking *)
val _ = op eqType : ty * ty -> bool
(* shared utility functions on Hindley-Milner types 433b *)
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
(* type declarations for consistency checking *)
val _ = op canonicalize : type_scheme -> type_scheme
val _ = op newBoundVars : int * name list -> name list
  in  FORALL (newBound,
              tysubst (mkEnv (bound, map TYVAR newBound)) ty)
  end
(* shared utility functions on Hindley-Milner types 434a *)
local
  val n = ref 1
in
  fun freshtyvar _ = TYVAR ("'t" ^ intString (!n) before n := !n + 1)
(* type declarations for consistency checking *)
val _ = op freshtyvar : 'a -> ty
end
(* shared utility functions on Hindley-Milner types 434b *)
fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))
(* type declarations for consistency checking *)
val _ = op generalize : ty * name set -> type_scheme
(* shared utility functions on Hindley-Milner types 434c *)
fun freshInstance (FORALL (bound, tau)) =
  instantiate (FORALL (bound, tau), map freshtyvar bound)
(* type declarations for consistency checking *)
val _ = op freshInstance : type_scheme -> ty
(* shared utility functions on Hindley-Milner types S449c *)
datatype scheme_shape
  = MONO_FUN of              ty list * ty  (* (tau1 ... tauN -> tau) *)
  | MONO_VAL of              ty            (* tau *)
  | POLY_FUN of tyvar list * ty list * ty
                                         (* (forall (a ...) (tau ... -> tau)) *)
  | POLY_VAL of tyvar list * ty            (* (forall (a ...) tau) *)
(* shared utility functions on Hindley-Milner types S449d *)
fun schemeShape (FORALL (alphas, tau)) =
  case asFuntype tau
    of NONE => if null alphas then MONO_VAL tau
               else                POLY_VAL (alphas, tau)
     | SOME (args, result) =>
               if null alphas then MONO_FUN (args, result)
               else                POLY_FUN (alphas, args, result)
(* type declarations for consistency checking *)
type scheme_shape = scheme_shape
(* type declarations for consistency checking *)
val _ = op schemeShape : type_scheme -> scheme_shape
(* shared utility functions on Hindley-Milner types S431d *)
fun typeSchemeString (FORALL ([], tau)) =
      typeString tau
  | typeSchemeString (FORALL (a's, tau)) =
      "(forall [" ^ spaceSep a's ^ "] " ^ typeString tau ^ ")"
(* type declarations for consistency checking *)
val _ = op typeString       : ty          -> string
val _ = op typeSchemeString : type_scheme -> string
(* shared utility functions on Hindley-Milner types ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun substString [] = "idsubst"
  | substString pairs =
      String.concatWith " o " 
      (map (fn (a, t) => a ^ " |--> " ^ typeString t) pairs)
(* specialized environments for type schemes 435a *)
type type_env = type_scheme env * name set
(* specialized environments for type schemes 435b *)
val emptyTypeEnv = 
      (emptyEnv, emptyset)
fun findtyscheme (x, (Gamma, free)) = find (x, Gamma)
(* type declarations for consistency checking *)
val _ = op emptyTypeEnv : type_env
val _ = op findtyscheme : name * type_env -> type_scheme
(* specialized environments for type schemes 435c *)
fun bindtyscheme (x, sigma as FORALL (bound, tau), (Gamma, free)) = 
  (bind (x, sigma, Gamma), union (diff (freetyvars tau, bound), free))
(* specialized environments for type schemes 435d *)
fun freetyvarsGamma (_, free) = free
(* specialized environments for type schemes S453c *)
fun extendTypeEnv (Gamma, bindings) =
  let fun add ((x, sigma), Gamma) = bindtyscheme (x, sigma, Gamma)
  in  foldl add Gamma bindings
  end
(* type declarations for consistency checking *)
val _ = op extendTypeEnv : type_env * type_scheme env -> type_env
(* extensions that support existential types S458a *)
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
(* type declarations for consistency checking *)
type x_type_scheme = x_type_scheme
val _ = op asExistential : type_scheme -> x_type_scheme option
(* extensions that support existential types S458b *)
fun freshSkolem _ =
  let val { identity = id, printName = T } = freshTycon "skolem type"
  in  TYCON { identity = id + 1
            , printName = "skolem type " ^ intString (id div 2)
            }
  end

fun isSkolem { identity = n, printName = _ } = (n mod 2 = 1)
(* extensions that support existential types S460c *)
fun addFreeSkolems (TYCON mu, mus) =
      if isSkolem mu then insert (mu, mus) else mus
  | addFreeSkolems (TYVAR _,  mus) =
      mus
  | addFreeSkolems (CONAPP (tau, taus), mus) =
      foldl addFreeSkolems (addFreeSkolems (tau, mus)) taus
(* extensions that support existential types S460d *)
fun typeFreeSkolems        tau    = addFreeSkolems (tau, emptyset)
fun typesFreeSkolems       taus   = foldl addFreeSkolems emptyset taus
fun typeSchemesFreeSkolems sigmas =
      typesFreeSkolems (map (fn FORALL (_, tau) => tau) sigmas)
(* type declarations for consistency checking *)
val _ = op addFreeSkolems : ty * tycon set -> tycon set
(* type declarations for consistency checking *)
val _ = op typeFreeSkolems  : ty     -> tycon set
val _ = op typesFreeSkolems : ty set -> tycon set
val _ = op typeSchemesFreeSkolems : type_scheme list -> tycon set
(* extensions that support existential types S460e *)
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

(* abstract syntax and values for \uml S437b *)
(* kinds for typed languages 355a *)
datatype kind = TYPE                          (* kind of all types *)
              | ARROW of kind list * kind     (* kind of many constructors *)
(* kinds for typed languages 355b *)
fun eqKind (TYPE, TYPE) = true
  | eqKind (ARROW (args, result), ARROW (args', result')) =
      eqKinds (args, args') andalso eqKind (result, result')
  | eqKind (_, _) = false
and eqKinds (ks, ks') = ListPair.allEq eqKind (ks, ks')
(* kinds for typed languages (BUG: software can't tell where this code came from [NW4fx3pn-2bdUVX-1]) *)
fun kindString TYPE = "*"
  | kindString (ARROW (ks, k)) =
      "(" ^ spaceSep (map kindString ks @ ["=>", kindString k]) ^ ")"
(* definition of [[tyex]] for \uml S454c *)
datatype tyex 
  = TYNAME  of name                (* names type or type constructor *)
  | CONAPPX of tyex * tyex list    (* type-level application *)
  | FUNTYX  of tyex list * tyex
  | FORALLX of name list * tyex
  | TYVARX  of name                (* type variable *)
(* definition of [[pat]], for patterns 486c *)
type vcon = name   (* a value constructor *)  (*OMIT*)
datatype pat = WILDCARD
             | PVAR     of name
             | CONPAT   of vcon * pat list
(* definitions of [[exp]] and [[value]] for \uml 486b *)
type vcon = name   (* a value constructor *)
datatype exp 
  = VCONX of vcon
  | CASE  of exp * (pat * exp) list
  | (* forms of [[exp]] carried over from \nml S438a *)
        LITERAL    of value
      | VAR        of name
      | IFX        of exp * exp * exp (* could be syntactic sugar for CASE *)
      | BEGIN      of exp list
      | APPLY      of exp * exp list
      | LETX       of let_flavor * (name * exp) list * exp
      | LAMBDA     of name list * exp
    and let_flavor = LET | LETREC | LETSTAR
(* definitions of [[exp]] and [[value]] for \uml 486d *)
and value
  = CONVAL of vcon * value list
  | SYM    of name
  | NUM    of int
  | CLOSURE   of lambda * value env ref
  | PRIMITIVE of primop
 withtype lambda = name list * exp
      and primop = value list -> value
(* definition of [[def]] for \uml 486a *)
datatype def = DATA of data_def
             | (* forms of [[def]] carried over from \nml S438b *)
                 VAL    of name * exp
               | VALREC of name * exp
               | EXP    of exp
               | DEFINE of name * (name list * exp)
  withtype data_def = name * kind * (vcon * tyex) list
(* type declarations for consistency checking *)
type data_def = data_def
(* definition of [[implicit_data_def]] for \uml S469c *)
datatype implicit_data_def 
  = IMPLICIT_DATA of tyvar list * name * implicit_vcon list
and implicit_vcon 
  = IMPLICIT_VCON of vcon * tyex list
(* definition of [[unit_test]] for languages with Hindley-Milner types and generated type constructors S438c *)
datatype unit_test = CHECK_EXPECT      of exp * exp
                   | CHECK_ASSERT      of exp
                   | CHECK_ERROR       of exp
                   | CHECK_TYPE        of exp * tyex
                   | CHECK_PTYPE       of exp * tyex
                   | CHECK_TYPE_ERROR  of def
(* definition of [[xdef]] (shared) S214b *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)
(* definition of [[valueString]] for \uml S461b *)

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
(* definition of [[valueString]] for \uml S461c *)
and consString (v, vs) =
      let fun tail (CONVAL ("cons", [v, vs])) = " " ^ valueString v ^ tail vs
            | tail (CONVAL ("'()", []))       = ")"
            | tail _ =
                raise BugInTypeInference
                  "bad list constructor (or cons/'() redefined)"
      in  "(" ^ valueString v ^ tail vs
	  end
(* type declarations for consistency checking *)
val _ = op typeEnvSubst : subst -> type_scheme env -> type_scheme env
(* type declarations for consistency checking *)
val _ = op valueString : value -> string
(* definition of [[patString]] for \uml\ and \uhaskell ((uml)) S462a *)
fun patString WILDCARD    = "_"
  | patString (PVAR x)    = x
  | patString (CONPAT (vcon, []))   = vcon
  | patString (CONPAT (vcon, pats)) =
      "(" ^ spaceSep (vcon :: map patString pats) ^ ")"
(* definition of [[expString]] for \nml\ and \uml S432a *)
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
         (* extra cases of [[expString]] for \uml S461d *)
         | VCONX vcon => vcon
         | CASE (e, matches) =>
             let fun matchString (pat, e) =
                   sqbracket (spaceSep [patString pat, expString e])
             in  bracketSpace ("case" :: expString e :: map matchString matches)
             end
         (* extra cases of [[expString]] for \uml S432b *)
         (* this space is filled in by the uML appendix *)
  end
(* definitions of [[defString]] and [[defName]] for \nml\ and \uml S432c *)
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

(* cases for [[defString]] for forms found only in \uml ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
         | DATA (t, kind, _) => bracketSpace ["data", kindString kind, t, "..."]

(* cases for [[defString]] for forms found only in \uml ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
         (*empty*)
  end
fun defName (VAL (x, _))       = x
  | defName (VALREC (x, _)) = x
  | defName (DEFINE (x, _)) = x
  | defName (EXP _) = raise InternalError "asked for name defined by expression"

(* clauses for [[defName]] for forms found only in \uml ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
  | defName (DATA (t, _, _)) = t

(* clauses for [[defName]] for forms found only in \uml ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
  (*empty*)
(* definition of [[tyexString]] for \uml S462c *)
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

(* utility functions on \uml\ syntax S452b *)
fun isPolymorphicFuntyex (FORALLX (_, tau)) = isPolymorphicFuntyex tau
  | isPolymorphicFuntyex (FUNTYX _)         = true
  | isPolymorphicFuntyex _                  = false


(*****************************************************************)
(*                                                               *)
(*   UTILITY FUNCTIONS ON \UML\ VALUES                           *)
(*                                                               *)
(*****************************************************************)

(* utility functions on \uml\ values ((uml)) S443d *)
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
(* utility functions on \uml\ values ((uml)) S444b *)
fun embedList []      = CONVAL ("'()", [])
  | embedList (v::vs) = CONVAL ("cons", [v, embedList vs])
(* type declarations for consistency checking *)
val _ = op embedList : value list -> value
(* utility functions on \uml\ values ((uml)) S444d *)
fun embedBool b =
      CONVAL (if b then "#t" else "#f", [])
fun projectBool (CONVAL ("#t", [])) = true
  | projectBool _                   = false
(* type declarations for consistency checking *)
val _ = op projectBool : value -> bool
val _ = op embedBool   : bool  -> value



(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS AND PARSING FOR \UML, PROVIDING [[FILEXDEFS]] AND [[STRINGSXDEFS]] *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis and parsing for \uml, providing [[filexdefs]] and [[stringsxdefs]] S462e *)
(* lexical analysis for \uscheme\ and related languages S383d *)
datatype pretoken = QUOTE
                  | INT     of int
                  | SHARP   of bool
                  | NAME    of string
type token = pretoken plus_brackets
(* type declarations for consistency checking *)
type pretoken = pretoken
type token = token
(* lexical analysis for \uscheme\ and related languages S384a *)
fun pretokenString (QUOTE)     = "'"
  | pretokenString (INT  n)    = intString n
  | pretokenString (SHARP b)   = if b then "#t" else "#f"
  | pretokenString (NAME x)    = x
val tokenString = plusBracketsString pretokenString
(* type declarations for consistency checking *)
val _ = op pretokenString : pretoken -> string
val _ = op tokenString    : token    -> string
(* lexical analysis for \uscheme\ and related languages S384b *)
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
  (* functions used in the lexer for \uscheme S384c *)
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
(* type declarations for consistency checking *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> pretoken
end
(* parsers for single tokens for \uscheme-like languages S385a *)
type 'a parser = (token, 'a) polyparser
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token : pretoken
                                                                          parser
val quote     = (fn (QUOTE)     => SOME () | _ => NONE) <$>? pretoken
val int       = (fn (INT   n)   => SOME n  | _ => NONE) <$>? pretoken
val booltok   = (fn (SHARP b)   => SOME b  | _ => NONE) <$>? pretoken
val namelike  = (fn (NAME  n)   => SOME n  | _ => NONE) <$>? pretoken
val namelike  = asAscii namelike
(* type declarations for consistency checking *)
val _ = op booltok  : bool parser
val _ = op int      : int  parser
val _ = op namelike : name parser
(* parsers for \uml\ tokens S463a *)
val name = () (* don't use me as a parser; too confusing *)
(* parsers for \uml\ tokens S463b *)
val tyvar =
  quote *> (  curry op ^ "'" <$> namelike
          <?> "type variable (got quote mark)"
           )
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
(* parsers and parser builders for \scheme-like syntax S386d *)
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
(* type declarations for consistency checking *)
val _ = op sexp : value parser
(* parsers and parser builders for \scheme-like syntax S386e *)
fun atomicSchemeExpOf name =  VAR                   <$> name
                          <|> LITERAL <$> NUM       <$> int
                          <|> LITERAL <$> embedBool <$> booltok
(* parsers and parser builders for \scheme-like syntax S388a *)
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
(* type declarations for consistency checking *)
val _ = op fullSchemeExpOf : exp parser -> (exp parser -> exp parser) -> exp
                                                                          parser
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
(* parsers for HM types with generated type constructors S463e *)
fun tyex tokens = (
     TYNAME <$> tyname
 <|> TYVARX <$> tyvar
 <|> usageParsers
        [("(forall (tyvars) type)",
          curry FORALLX <$> bracket ("('a ...)", distinctTyvars) <*> tyex)]
 <|> bracket("(ty ty ... -> ty)",
        arrowsOf CONAPPX FUNTYX <$> many tyex <*>! many (arrow *> many tyex))
) tokens
(* type declarations for consistency checking *)
val _ = op tyvar : string parser
val _ = op tyex  : tyex   parser
(* parsers for HM types with generated type constructors S463f *)
fun kind tokens = (
      TYPE <$ eqx "*" vvar
  <|> bracket ("arrow kind",
               curry ARROW <$> many kind <* eqx "=>" vvar <*> kind)
) tokens

val kind = kind <?> "kind"
(* type declarations for consistency checking *)
val _ = op kind : kind parser
(* parsers and [[xdef]] streams for \uml S464a *)
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
(* parsers and [[xdef]] streams for \uml S464b *)
val vvarFormalsIn = formalsOf "(x1 x2 ...)" vvar
val patFormals    = bracket ("(p1 p2 ...)", many pattern)
(* type declarations for consistency checking *)
val _ = op vvarFormalsIn : string -> name list parser
val _ = op patFormals    :            pat list parser
(* parsers and [[xdef]] streams for \uml S464c *)
(* utility functions that help implement \uml's syntactic sugar ((prototype)) S468d *)
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
(* utility functions that help implement \uml's syntactic sugar S468e *)
val varsupply = 
  streamMap (fn n => "x" ^ intString n) naturals
fun freshVar e =
  case streamGet (streamFilter (not o freeIn e) varsupply)
    of SOME (x, _) => x
     | NONE => raise InternalError "unable to create a fresh variable"
(* type declarations for consistency checking *)
val _ = op freeIn : exp -> name -> bool
(* type declarations for consistency checking *)
val _ = op varsupply : name stream
val _ = op freshVar  : exp -> name
(* utility functions that help implement \uml's syntactic sugar S468f *)
fun freshVars e xs =
  streamTake (length xs, streamFilter (not o freeIn e) varsupply)
(* type declarations for consistency checking *)
val _ = op freshVars : exp -> 'a list -> name list
(* utility functions that help implement \uml's syntactic sugar S468g *)
fun tupleVcon xs = case length xs
                     of 2 => "PAIR"
                      | 3 => "TRIPLE"
                      | n => "T" ^ intString n
(* utility functions that help implement \uml's syntactic sugar S469a *)
fun tupleexp [x] = VAR x
  | tupleexp xs  = APPLY (VCONX (tupleVcon xs), map VAR xs)

fun tuplepat [x] = x
  | tuplepat xs  = CONPAT (tupleVcon xs, xs)
(* type declarations for consistency checking *)
val _ = op tupleexp  : name list -> exp
val _ = op tuplepat  : pat  list -> pat
val _ = op tupleVcon : 'a   list -> vcon
(* utility functions that help implement \uml's syntactic sugar S469b *)
fun freePatVars (PVAR x)         = insert (x, emptyset)
  | freePatVars (WILDCARD)       = emptyset
  | freePatVars (CONPAT (_, ps)) = foldl union emptyset (map freePatVars ps)
(* type declarations for consistency checking *)
val _ = op freePatVars : pat -> name set
fun exptable exp =
  let (* parsers used in both flavors S464d *)
      val choice =
        bracket ("[pattern exp]", pair <$> pattern <*> exp)
      val letrecBs =
        distinctBsIn (bindingsOf "(x e)" vvar (asLambda "letrec" exp)) "letrec"
      (* type declarations for consistency checking *)
      val _ = op choice   : (pat  * exp)      parser
      val _ = op letrecBs : (name * exp) list parser
      (* parsers for bindings to names S465a *)
      val letBs     = distinctBsIn (bindingsOf "(x e)" vvar exp) "let"
      val letstarBs = bindingsOf "(x e)" vvar exp
      val formals   = vvarFormalsIn "lambda"
      (* parsers for bindings to patterns S465b *)
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
      (* expression builders that expect to bind names S465c *)
      fun letx letkind bs e = LETX (letkind, bs, e)
      fun lambda xs e = LAMBDA (xs, e)
      fun lambdastar clauses = ERROR "lambda* is left as an exercise"
      (* type declarations for consistency checking *)
      val _ = op letx       : let_flavor -> (name * exp) list -> exp -> exp
      val _ = op lambda     : name list -> exp -> exp
      val _ = op lambdastar : (pat list * exp) list -> exp error
      (* \uml\ expression builders that expect to bind patterns S468a *)
      (* you can redefine letx, lambda, and lambdastar here *)
  in  (* parsers for expressions that begin with keywords S465d *)
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
        (* rows added to \uml's [[exptable]] in exercises S466a *)
        (* you add this bit *)
        ]
  end
(* parsers and [[xdef]] streams for \uml S466b *)
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
(* type declarations for consistency checking *)
val _ = op atomicExp : exp parser
val _ = op exp       : exp parser
(* parsers and [[xdef]] streams for \uml S466c *)
(* definition of [[makeExplicit]], to translate [[implicit-data]] to [[data]] S469d *)
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
(* type declarations for consistency checking *)
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
(* type declarations for consistency checking *)
val _ = op makeExplicit : implicit_data_def -> data_def
val _ = op implicitData : def parser
(* parsers and [[xdef]] streams for \uml S467c *)
val def = 
  let (* parser for binding to names S466d *)
      val formals = vvarFormalsIn "define"
      (* type declarations for consistency checking *)
      val _ = op formals : name list parser
      (* parsers for clausal definitions, a.k.a. [[define*]] S466e *)
      val lhs = 
        bracket ("(f p1 p2 ...)", pair <$> vvar <*> many pattern)
      val clause =
        bracket ("[(f p1 p2 ...) e]",
                 (fn (f, ps) => fn e => (f, (ps, e))) <$> lhs <*> exp)
      (* type declarations for consistency checking *)
      val _ = op lhs    : (name * pat list) parser
      val _ = op clause : (name * (pat list * exp)) parser
      (* definition builders that expect to bind names S467b *)
      fun define f xs body = DEFINE (f, (xs, body))
      fun definestar _ = ERROR "define* is left as an exercise"
      (* definition builders used in all parsers S467a *)
      val Kty = typedFormalOf vcon (kw ":") tyex
      fun data kind name vcons = DATA (name, kind, vcons)
      (* type declarations for consistency checking *)
      val _ = op Kty  : (vcon * tyex) parser
      val _ = op data : kind -> name -> (vcon * tyex) list -> def

(* \uml\ definition builders that expect to bind patterns ((prototype)) S468b *)
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
(* type declarations for consistency checking *)
val _ = op def : def parser
(* parsers and [[xdef]] streams for \uml S467d *)
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
(* type declarations for consistency checking *)
val _ = op testtable : unit_test parser
(* parsers and [[xdef]] streams for \uml S467e *)
val xdeftable = usageParsers
  [ ("(use filename)", USE <$> namelike)
  (* rows added to \uml's [[xdeftable]] in exercises S467f *)
  (* you add this bit *)
  (* rows added to \uml's [[xdeftable]] in exercises ((prototype)) S468c *)
  (* you can add a row for 'val' here *)
  ]
(* type declarations for consistency checking *)
val _ = op xdeftable : xdef parser
(* parsers and [[xdef]] streams for \uml S467h *)
val xdef  =  TEST <$> testtable
         <|>          xdeftable
         <|> DEF  <$> (def <|> implicitData)
         <|> badRight "unexpected right bracket"
         <|> DEF <$> EXP <$> exp
         <?> "definition"
val xdefstream = interactiveParsedStream (schemeToken, xdef)
(* type declarations for consistency checking *)
val _ = op xdef : xdef parser
(* parsers and [[xdef]] streams for \uml S470b *)
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
(* type declarations for consistency checking *)
val _ = op sxstream : string * line stream * prompts -> value stream
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
(*   DEFINITION OF [[BASIS]] FOR \UML                            *)
(*                                                               *)
(*****************************************************************)

(* definition of [[basis]] for \uml S439a *)
type basis = type_env * (ty * kind) env * value env


(*****************************************************************)
(*                                                               *)
(*   TRANSLATION OF {\UML} TYPE SYNTAX INTO TYPES                *)
(*                                                               *)
(*****************************************************************)

(* translation of {\uml} type syntax into types ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
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
(* translation of {\uml} type syntax into types S455a *)
fun txType (TYNAME t, Delta) =
      (find (t, Delta)
       handle NotFound _ => raise TypeError ("unknown type name " ^ t))
(* translation of {\uml} type syntax into types S455b *)
  | txType (TYVARX a, Delta) =
      (find (a, Delta)
       handle NotFound _ =>
         raise TypeError ("type variable " ^ a ^ " is not in scope"))
(* translation of {\uml} type syntax into types S455c *)
  | txType (CONAPPX (tx, txs), Delta) =
      let val (tau,  kind)  = txType (tx, Delta)
          val (taus, kinds) =
            ListPair.unzip (map (fn tx => txType (tx, Delta)) txs)
      in  case kind
            of ARROW (argks, resultk) =>
                 if eqKinds (kinds, argks) then
                   (CONAPP (tau, taus), resultk)
                 else

                  (* applied type constructor [[tx]] has the wrong kind S457b *)
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
                   (* type [[tau]] is not expecting any arguments S457c *)
                   raise TypeError ("type " ^ typeString tau ^
                                                " is not a type constructor, " ^
                                    "but it was applied to " ^ countString taus
                                                                   "other type")
      end
(* translation of {\uml} type syntax into types S456a *)
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
(* translation of {\uml} type syntax into types S456b *)
  | txType (FORALLX _, _) =
      raise TypeError ("'forall' is permissible only at top level")
(* type declarations for consistency checking *)
val _ = op txType : tyex * (ty * kind) env -> ty * kind
(* translation of {\uml} type syntax into types S456c *)
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
(* translation of {\uml} type syntax into types S457a *)
  | txTyScheme (tx, Delta) =
      case txType (tx, Delta)
        of (tau, TYPE) => FORALL ([], tau)
         | (tau, kind) =>
             raise TypeError ("expected a type, but got type constructor " ^
                              typeString tau ^ " of kind " ^ kindString kind)
(* type declarations for consistency checking *)
val _ = op txTyScheme : tyex * (ty * kind) env -> type_scheme


(*****************************************************************)
(*                                                               *)
(*   TYPING AND EVALUATION OF [[DATA]] DEFINITIONS               *)
(*                                                               *)
(*****************************************************************)

(* typing and evaluation of [[data]] definitions 489 *)
fun typeDataDef ((T, kind, vcons), Gamma, Delta) =
  let
(* definition of [[validate]], for the types of the value constructors of [[T]] S450a *)
      fun validate (K, sigma as FORALL (alphas, _), mu, kind) =
        let (* definition of [[validateTyvarArguments]] S451b *)
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

   (* for [[K]], complain that [[alphas]] is inconsistent with [[kind]] S451c *)
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
      (* type declarations for consistency checking *)
      val _ = op appliesMu             : ty -> bool
      val _ = op validateTyvarArguments : ty -> unit
      val _ = op validateLengths       : tyvar list * kind list -> unit
        in
(* validation by case analysis on [[schemeShape shape]] and [[kind]] ((existentials)) S452a *)
            case (schemeShape sigma, kind)
              of (MONO_VAL tau, TYPE) =>
                   if eqType (tau, TYCON mu) then
                     ()
                   else

            (* type of [[K]] should be [[desiredType]] but is [[sigma]] S451d *)
                     raise TypeError ("value constructor " ^ K ^ " should have "
                                                                 ^ desiredType ^
                                      ", but it has type " ^ typeSchemeString
                                                                          sigma)
               | (MONO_FUN (_, result), TYPE) =>
                   if eqType (result, TYCON mu) then
                     ()
                   else

    (* result type of [[K]] should be [[desiredType]] but is [[result]] S451e *)
                     raise TypeError ("value constructor " ^ K ^
                                               " should return " ^ desiredType ^
                                      ", but it returns type " ^ typeString
                                                                         result)
               | (POLY_VAL (alphas, tau), _) => 
                   if appliesMu tau orelse eqType (tau, TYCON mu) then
                     validateTyvarArguments tau
                   else

            (* type of [[K]] should be [[desiredType]] but is [[sigma]] S451d *)
                     raise TypeError ("value constructor " ^ K ^ " should have "
                                                                 ^ desiredType ^
                                      ", but it has type " ^ typeSchemeString
                                                                          sigma)
               | (POLY_FUN (alphas, _, result), _) => 
                   if appliesMu result orelse eqType (result, TYCON mu) then
                     validateTyvarArguments result
                   else

    (* result type of [[K]] should be [[desiredType]] but is [[result]] S451e *)
                     raise TypeError ("value constructor " ^ K ^
                                               " should return " ^ desiredType ^
                                      ", but it returns type " ^ typeString
                                                                         result)
               | _ =>

   (* for [[K]], complain that [[alphas]] is inconsistent with [[kind]] S451c *)
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
(* type declarations for consistency checking *)
val _ = op typeDataDef : data_def * type_env * (ty * kind) env -> type_env * (ty
                                                       * kind) env * string list
(* typing and evaluation of [[data]] definitions 490 *)
fun evalDataDef ((_, _, typed_vcons), rho) =
  let fun valFor (K, t) = if isPolymorphicFuntyex t then
                            PRIMITIVE (fn vs => CONVAL (K, vs))
                          else
                            CONVAL (K, [])
      fun addVcon ((K, t), rho) = bind (K, valFor (K, t), rho)
(* type declarations for consistency checking *)
val _ = op evalDataDef : data_def * value env -> value env * string list
val _ = op isPolymorphicFuntyex : tyex -> bool
  in  (foldl addVcon rho typed_vcons, map fst typed_vcons)
  end
(* typing and evaluation of [[data]] definitions S439c *)
fun processDataDef (dd, (Gamma, Delta, rho), interactivity) =
  let val (Gamma', Delta', tystrings) = typeDataDef (dd, Gamma, Delta)
      val (rho', vcons)               = evalDataDef (dd, rho)
      val _ = if echoes interactivity then

               (* print the new type and each of its value constructors S439d *)
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
(* type declarations for consistency checking *)
val _ = op processDataDef : data_def * basis * interactivity -> basis


(*****************************************************************)
(*                                                               *)
(*   DEFINITIONS OF [[EMPTYBASIS]] AND [[PREDEFINEDTYPEBASIS]]   *)
(*                                                               *)
(*****************************************************************)

(* definitions of [[emptyBasis]] and [[predefinedTypeBasis]] S441b *)
val emptyBasis =
  (emptyTypeEnv, emptyEnv, emptyEnv)
fun addTycon ((t, tycon, kind), (Gamma, Delta, rho)) =
  (Gamma, bind (t, (TYCON tycon, kind), Delta), rho)
val primTyconBasis : basis = 
  foldl addTycon emptyBasis (
                        (* primitive type constructors for \uml\ [[::]] S443c *)
                             ("int", inttycon, TYPE) :: 
                             ("sym", symtycon, TYPE) :: nil)
(* definitions of [[emptyBasis]] and [[predefinedTypeBasis]] S442a *)
val predefinedTypeBasis =
  let val predefinedTypes = 
                             [ ";  predefined uML types 463a "
                             , "(data (* => *) option"
                             , "  [SOME : (forall ['a] ('a -> (option 'a)))]"
                             , "  [NONE : (forall ['a] (option 'a))])"
                             , ";  predefined uML types 473a "
                             , "(data * bool"
                             , "  [#t : bool]"
                             , "  [#f : bool])"
                             , ";  predefined uML types 473b "
                             , "(data (* => *) list"
                             , "  ['()  : (forall ['a] (list 'a))]"
                             ,
                         "  [cons : (forall ['a] ('a (list 'a) -> (list 'a)))])"
                             , ";  predefined uML types 474a "
                             , "(data * unit [UNIT : unit])"
                             , ";  predefined uML types 474b "
                             , "(implicit-data order LESS EQUAL GREATER)"
                             , ";  predefined uML types S440f "
                             , "(data (* * * => *) triple"
                             ,
             "  [TRIPLE : (forall ['a 'b 'c] ('a 'b 'c -> (triple 'a 'b 'c)))])"
                             , ";  predefined uML types S441a "
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
                             , ";  predefined uML types S470a "
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
(* type declarations for consistency checking *)
val _ = op emptyBasis     : basis
val _ = op primTyconBasis : basis
(* type declarations for consistency checking *)
val _ = op predefinedTypeBasis : basis


(*****************************************************************)
(*                                                               *)
(*   DEFINITIONS OF [[BOOLTYPE]], [[LISTTYPE]], AND [[UNITTYPE]] *)
(*                                                               *)
(*****************************************************************)

(* definitions of [[booltype]], [[listtype]], and [[unittype]] S442b *)
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

(* type inference for \nml\ and \uml S420b *)
(* representation of type constraints 436a *)
datatype con = ~  of ty  * ty
             | /\ of con * con
             | TRIVIAL
infix 4 ~
infix 3 /\
(* utility functions on type constraints 436b *)
fun freetyvarsConstraint (t ~  t') = union (freetyvars t, freetyvars t')
  | freetyvarsConstraint (c /\ c') = union (freetyvarsConstraint c,
                                             freetyvarsConstraint c')
  | freetyvarsConstraint TRIVIAL   = emptyset
(* utility functions on type constraints 436c *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)    = subst c1 /\ subst c2
        | subst TRIVIAL       = TRIVIAL
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op bindtyscheme : name * type_scheme * type_env -> type_env
(* type declarations for consistency checking *)
val _ = op freetyvarsGamma : type_env -> name set
(* type declarations for consistency checking *)
val _ = op consubst : subst -> con -> con
(* utility functions on type constraints 436d *)
fun conjoinConstraints []      = TRIVIAL
  | conjoinConstraints [c]     = c
  | conjoinConstraints (c::cs) = c /\ conjoinConstraints cs
(* type declarations for consistency checking *)
val _ = op conjoinConstraints : con list -> con
(* utility functions on type constraints 437c *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau,tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)
(* type declarations for consistency checking *)
val _ = op isSolved : con -> bool
val _ = op solves : subst * con -> bool
(* utility functions on type constraints S420c *)
(* definitions of [[constraintString]] and [[untriviate]] S431c *)
fun constraintString (c /\ c') =
      constraintString c ^ " /\\ " ^ constraintString c'
  | constraintString (t ~  t') = typeString t ^ " ~ " ^ typeString t'
  | constraintString TRIVIAL = "TRIVIAL"

fun untriviate (c /\ c') = (case (untriviate c, untriviate c')
                              of (TRIVIAL, c) => c
                               | (c, TRIVIAL) => c
                               | (c, c') => c /\ c')
  | untriviate atomic = atomic
(* type declarations for consistency checking *)
val _ = op constraintString : con -> string
val _ = op untriviate       : con -> con
(* constraint solving 437a *)
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
(* constraint solving ((prototype)) 437b *)
fun solve c = raise LeftAsExercise "solve"
(* type declarations for consistency checking *)
val _ = op solve : con -> subst
(* constraint solving ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun hasNoSolution c = (solve c; false) handle TypeError _ => true
fun hasGoodSolution c = solves (solve c, c) handle TypeError _ => false
val hasSolution = not o hasNoSolution : con -> bool
fun solutionEquivalentTo (c, theta) = eqsubst (solve c, theta)
(* utility functions for {\uml} S435c *)
(* filled in when implementing uML *)
(* exhaustiveness analysis for {\uml} 520 *)
datatype simple_vset = ALL of ty
                     | ONE of vcon * simple_vset list
type vset = simple_vset collection
(* exhaustiveness analysis for {\uml} ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun vsetString (ALL tau) = "_" (* (ALL : " ^ typeString tau ^ ")" *)
  | vsetString (ONE (K, [])) = K
  | vsetString (ONE (K, vsets)) = "(" ^ K ^ " " ^ spaceSep (map vsetString vsets
                                                                         ) ^ ")"
(* exhaustiveness analysis for {\uml} ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
fun hasAll (ALL _) = true
  | hasAll (ONE (_, vs)) = List.exists hasAll vs
(* exhaustiveness analysis for {\uml} ((prototype)) 521a *)
fun classifyVset   p vs = joinC (mapC (classifySimple p) vs)
and classifySimple p vs = raise LeftAsExercise "match classification"
(* exhaustiveness analysis for {\uml} 521b *)
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
(* exhaustiveness analysis for {\uml} ((prototype)) 522a *)
fun exhaustivenessCheck (ps, tau) =
  eprintln "(Case expression not checked for exhaustiveness.)"
(* exhaustiveness analysis for {\uml} S435b *)
(* filled in when implementing uML *)
(* definitions of [[typeof]] and [[typdef]] for \nml\ and \uml 437d *)
fun typeof (e, Gamma) =
  let
(* shared definition of [[typesof]], to infer the types of a list of expressions 438a *)
      fun typesof ([],    Gamma) = ([], TRIVIAL)
        | typesof (e::es, Gamma) =
            let val (tau,  c)  = typeof  (e,  Gamma)
                val (taus, c') = typesof (es, Gamma)
            in  (tau :: taus, c /\ c')
            end

(* function [[literal]], to infer the type of a literal constant ((prototype)) 438b *)
      fun literal _ = raise LeftAsExercise "literal"
      (* function [[literal]], to infer the type of a literal constant S453a *)
      (* definition of function [[pvconType]] ((existentials)) S459a *)
      fun pvconType (K, Gamma) = 
        let val sigma = findtyscheme (K, Gamma)
            val sigma' =
              case asExistential sigma
                of NONE => sigma
                 | SOME (FORALL_EXISTS (alphas, betas, args, result)) =>
                     let val skolems = map freshSkolem betas
                         val theta = tysubst (mkEnv (betas, skolems))
                     in  FORALL (alphas, theta (funtype (args, result)))
                     end
        in  freshInstance sigma'
        end handle NotFound x => raise TypeError ("no value constructor named "
                                                                            ^ x)
      (* definition of function [[pattype]] 498 *)
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
      (* type declarations for consistency checking *)
      val _ = op pattype : pat * type_env -> type_scheme env * ty * con
            in  (bind (x, FORALL ([], alpha), emptyEnv), alpha, TRIVIAL)
            end
      and pattypes (ps, Gamma) = unzip3 (map (fn p => pattype (p, Gamma)) ps)
      (* definition of function [[choicetype]] 497b *)
      fun choicetype ((p, e), Gamma) =
            let val (Gamma', tau, c) = pattype (p, Gamma)
                val (tau', c') = typeof (e, extendTypeEnv (Gamma, Gamma'))
                val (ty,  con) = (funtype ([tau], tau'), c /\ c')
                val _ =
(* check [[p]], [[e]], [[Gamma']], [[Gamma]], [[ty]], and [[con]] for escaping skolem types ((existentials)) S459b *)
                        let val theta = solve con
                                (* if exn is raised here, we're doomed anyway *)
                            val patSkolems =
                                  typeSchemesFreeSkolems (map snd (typeEnvSubst
                                                                  theta Gamma'))
                            val envSkolems =
                                  typesFreeSkolems (map (varsubst theta) (
                                                         freetyvarsGamma Gamma))
                            val tySkolems  =
                                  typeFreeSkolems (tysubst theta ty)

            (* definitions of skolem functions [[fail]] and [[badType]] S460b *)
                            fun fail ss =
                              raise TypeError (concat (["in choice [", patString
                                                            p, " ", expString e,
                                                        "], "] @ ss))
                            fun badType tau =
                              concat ["type ", typeString tau, ", which ",
                                      (case tau of TYCON _ => "is" | _ =>
                                                                    "includes"),
                                      " an escaping skolem type"]
                        in  case (inter (patSkolems, tySkolems), inter (
                                                        patSkolems, envSkolems))
                              of (mu :: _,  _) =>
                                 (* fail with skolem escaping into type S459c *)
                                                  (case asFuntype (tysubst theta
                                                                             ty)
                                                     of SOME ([tau], tau') =>
                                                          if not (null (inter (
                                        patSkolems, typeFreeSkolems tau'))) then
                                                            fail [
                                           "right-hand side has ", badType tau']
                                                          else
                                                            fail [
                               "scrutinee is constrained to have ", badType tau]
                                                      | _ => raise InternalError
                                           "choice type is not a function type")
                               | ([], mu :: _) =>
                          (* fail with skolem escaping into environment S460a *)
                                                  fail ["skolem type " ^
                                                                tyconString mu ^

                                    " constrains a variable in the environment"]
                               | ([], []) => ()
                        end          
      (* type declarations for consistency checking *)
      val _ = op choicetype : (pat * exp) * type_env -> ty * con
            in  (ty, con)
            end

(* function [[ty]], to infer the type of a \nml\ expression, given [[Gamma]] 438c *)
      fun ty (LITERAL n) = literal n
        (* more alternatives for [[ty]] 438d *)
          | ty (VAR x) = (freshInstance (findtyscheme (x, Gamma)), TRIVIAL)
        (* more alternatives for [[ty]] 438e *)
        | ty (APPLY (f, actuals)) = 
             (case typesof (f :: actuals, Gamma)
                of ([], _) => raise InternalError "pattern match"
                 | (funty :: actualtypes, c) =>
                      let val rettype = freshtyvar ()
                      in  (rettype, c /\ (funty ~ funtype (actualtypes, rettype)
                                                                              ))
                      end)
        (* more alternatives for [[ty]] 438f *)
        | ty (LETX (LETSTAR, [], body)) = ty body
        | ty (LETX (LETSTAR, (b :: bs), body)) = 
            ty (LETX (LET, [b], LETX (LETSTAR, bs, body)))
        (* more alternatives for [[ty]] ((prototype)) 438g *)
        | ty (IFX (e1, e2, e3))        = raise LeftAsExercise "type for IFX"
        | ty (BEGIN es)                = raise LeftAsExercise "type for BEGIN"
        | ty (LAMBDA (formals, body))  = raise LeftAsExercise "type for LAMBDA"
        | ty (LETX (LET, bs, body))    = raise LeftAsExercise "type for LET"
        | ty (LETX (LETREC, bs, body)) = raise LeftAsExercise "type for LETREC"
        (* more alternatives for [[ty]] 497a *)
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
        (* type declarations for consistency checking *)
        val _ = op ty         : exp                    -> ty * con
        val _ = op typeof     : exp         * type_env -> ty * con
        val _ = op choicetype : (pat * exp) * type_env -> ty * con
            in  (alpha, c)
            end
        (* more alternatives for [[ty]] S452c *)
        | ty (VCONX vcon) =
            let val tau =
                  freshInstance (findtyscheme (vcon, Gamma))
                  handle NotFound _ => raise TypeError (
                                           "no value constructor named " ^ vcon)
            in  (tau, TRIVIAL)
            end
(* type declarations for consistency checking *)
val _ = op typeof  : exp      * type_env -> ty      * con
val _ = op typesof : exp list * type_env -> ty list * con
val _ = op literal : value -> ty * con
val _ = op ty      : exp   -> ty * con
  in  ty e
  end
(* definitions of [[typeof]] and [[typdef]] for \nml\ and \uml 439a *)
fun typdef (d, Gamma) =
  case d
    of VAL    (x, e)      =>
                   (* infer and bind type for [[VAL    (x, e)]] for \nml 439b *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | VALREC (x, e)      =>
                   (* infer and bind type for [[VALREC (x, e)]] for \nml 439c *)
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
     (* extra case for [[typdef]] used only in \uml S440a *)
     | DATA _ => raise InternalError "DATA reached typdef"
     (* extra case for [[typdef]] used only in \uml S435a *)
     (* filled in when implementing uML *)
(* type declarations for consistency checking *)
val _ = op typdef : def * type_env -> type_env * string
(* type declarations for consistency checking *)
val _ = op typeof  : exp * type_env -> ty * con
val _ = op typdef : def * type_env -> type_env * string



(*****************************************************************)
(*                                                               *)
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \UML  *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \uml S438e *)
(* definition of [[namedValueString]] for functional bridge languages S413a *)
fun namedValueString x v =
  case v of CLOSURE _ => x
          | PRIMITIVE _ => x
          | _ => valueString v
(* type declarations for consistency checking *)
val _ = op namedValueString : name -> value -> string
(* definitions of [[match]] and [[Doesn'tMatch]] 494b *)
exception Doesn'tMatch    (* pattern-match failure *)
fun match (CONPAT (k, ps), CONVAL (k', vs)) =
     if k = k' then
       disjointUnion (ListPair.mapEq match (ps, vs))
     else
       raise Doesn'tMatch
  | match (CONPAT _, _) = raise Doesn'tMatch
  | match (WILDCARD, _) = emptyEnv
  | match (PVAR x,   v) = bind (x, v, emptyEnv)
(* type declarations for consistency checking *)
val _ = op match         : pat * value -> value env (* or raises Doesn'tMatch *)
val _ = op disjointUnion : 'a env list -> 'a env
(* definitions of [[eval]] and [[evaldef]] for \nml\ and \uml S429a *)
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
                            (* apply closure [[clo]] to [[args]] ((ml)) S429b *)
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
        (* more alternatives for [[ev]] for \nml\ and \uml 492 *)
        | ev (CASE (LITERAL v, 
                        (p, e) :: choices)) =
            (let val rho' = match (p, v)
        (* type declarations for consistency checking *)
        val _ = op match : pat * value -> value env
        val _ = op <+>   : 'a env * 'a env -> 'a env
             in  eval (e, rho <+> rho')
             end
             handle Doesn'tMatch => ev (CASE (LITERAL v, choices)))
        (* more alternatives for [[ev]] for \nml\ and \uml 493a *)
        | ev (CASE (LITERAL v, [])) =
            raise RuntimeError ("'case' does not match " ^ valueString v)
        (* more alternatives for [[ev]] for \nml\ and \uml 493b *)
        | ev (CASE (e, choices)) =
            ev (CASE (LITERAL (ev e), choices))
        (* more alternatives for [[ev]] for \nml\ and \uml S461a *)
        | ev (VCONX vcon) = find (vcon, rho)
        (* more alternatives for [[ev]] for \nml\ and \uml S429c *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
            in  eval (body, rho <+> mkEnv (names, map ev values))
            end
        (* more alternatives for [[ev]] for \nml\ and \uml S429d *)
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((x, e), rho) = bind (x, eval (e, rho), rho)
            in  eval (body, foldl step rho bs)
            end
        (* more alternatives for [[ev]] for \nml\ and \uml S430a *)
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
(* type declarations for consistency checking *)
val _ = op eval : exp * value env -> value
(* definitions of [[eval]] and [[evaldef]] for \nml\ and \uml S430b *)
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
(* definitions of [[eval]] and [[evaldef]] for \nml\ and \uml S430c *)
  | evaldef (DEFINE (f, lambda), rho) =
      evaldef (VALREC (f, LAMBDA lambda), rho)
  (* clause for [[evaldef]] for datatype definition (\uml\ only) S440b *)
  | evaldef (DATA _, _) = raise InternalError "DATA reached evaldef"
  (* clause for [[evaldef]] for datatype definition (\uml\ only) S431a *)
  (* code goes here in Appendix S *)
(* type declarations for consistency checking *)
val _ = op evaldef : def * value env -> value env * string
(* definition of [[processDef]] for \uml S439b *)
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
(* type declarations for consistency checking *)
val _ = op processDef : def * basis * interactivity -> basis
      in  (Gamma', Delta, rho')
      end
fun dump_names (_, _, values) = app (println o fst) values  (*OMIT*)
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

(* other handlers that catch non-fatal exceptions and pass messages to [[caught]] ((type-inference)) S421b *)
       | TypeError          msg => caught ("type error <at loc>: " ^ msg)
       | BugInTypeInference msg => caught ("bug in type inference: " ^ msg)
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
(* definition of [[testIsGood]] for \uml S447a *)
(* definition of [[skolemTypes]] for languages with generated type constructors S448b *)
val skolemTypes =
  streamOfEffects (fn () => SOME (TYCON (freshTycon "skolem type")))
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] S427c *)
fun asGeneralAs (sigma_g, sigma_i as FORALL (a's, tau)) =
  let val theta = mkEnv (a's, streamTake (length a's, skolemTypes))
      val skolemized = tysubst theta tau
      val tau_g = freshInstance sigma_g
  in  (solve (tau_g ~ skolemized); true) handle _ => false
  end
(* type declarations for consistency checking *)
val _ = op asGeneralAs : type_scheme * type_scheme -> bool
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] S427d *)
fun typeSchemeIsAscribable (e, sigma_e, sigma) =
  if asGeneralAs (sigma_e, sigma) then
    true
  else
    failtest ["check-type failed: expected ", expString e,
              " to have type ", typeSchemeString sigma,
              ", but it has type ", typeSchemeString sigma_e]
(* shared definitions of [[typeSchemeIsAscribable]] and [[typeSchemeIsEquivalent]] S428a *)
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

(* definitions of [[check{Expect,Assert,Error}Checks]] that use type inference S426b *)
      fun checkExpectChecks (e1, e2) = 
        let val (tau1, c1) = ty e1
            val (tau2, c2) = ty e2
            val c = tau1 ~ tau2
            val theta = solve (c1 /\ c2 /\ c)
        in  true
        end handle TypeError msg =>
            failtest ["In (check-expect ", expString e1, " ", expString e2,
                                                                     "), ", msg]

(* definitions of [[check{Expect,Assert,Error}Checks]] that use type inference S426c *)
      fun checkExpChecksIn what e =
        let val (tau, c) = ty e
            val theta = solve c
        in  true
        end handle TypeError msg =>
            failtest ["In (", what, " ", expString e, "), ", msg]
      val checkAssertChecks = checkExpChecksIn "check-assert"
      val checkErrorChecks  = checkExpChecksIn "check-error"
      (* definition of [[checkTypeChecks]] using type inference S427a *)
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
(* definition of [[testIsGood]] for \uml S447b *)
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
      (* [[asSyntacticValue]] for \uml S448c *)
      fun asSyntacticValue (LITERAL v) = SOME v
        | asSyntacticValue (VCONX c)   = SOME (CONVAL (c, []))
        | asSyntacticValue (APPLY (e, es)) =
            (case (asSyntacticValue e, optionList (map asSyntacticValue es))
               of (SOME (CONVAL (c, [])), SOME vs) => SOME (CONVAL (c, vs))
                | _ => NONE)
        | asSyntacticValue _ = NONE
      (* type declarations for consistency checking *)
      val _ = op asSyntacticValue : exp -> value option

 (* shared [[check{Expect,Assert,Error}Passes]], which call [[outcome]] S224d *)
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
      fun checkExpectPasses (cx, ex) = checkExpectPassesWith testEquals (cx, ex)
      (* definitions of [[check*Type*Passes]] using type inference S428b *)
      fun checkTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsAscribable (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-type ", expString e,
                      " ", typeSchemeString sigma, "), ", msg]
      (* definitions of [[check*Type*Passes]] using type inference S428c *)
      fun checkPrincipalTypePasses (e, sigma) =
        let val (tau, c) = ty e
            val theta    = solve c
            val sigma_e  = generalize (tysubst theta tau, freetyvarsGamma Gamma)
        in  typeSchemeIsEquivalent (e, sigma_e, sigma)
        end handle TypeError msg =>
            failtest ["In (check-principal-type ", expString e, " ",
                      typeSchemeString sigma, "), ", msg]
      (* definitions of [[check*Type*Passes]] using type inference S428d *)
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
(* definition of [[testIsGood]] for \uml S448a *)
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
(* definition of [[testIsGood]] for \uml S457e *)
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
(*   IMPLEMENTATIONS OF \UML\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \uml\ primitives and definition of [[initialBasis]] S442c *)
(* shared utility functions for building primitives in languages with type inference S421d *)
fun binaryOp f = (fn [a, b] => f (a, b)
                   | _ => raise BugInTypeInference "arity 2")
fun unaryOp  f = (fn [a]    => f  a
                   | _ => raise BugInTypeInference "arity 1")
(* shared utility functions for building primitives in languages with type inference S421e *)
fun arithOp f =
      binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                 | _ => raise BugInTypeInference "arithmetic on non-numbers")
val arithtype = funtype ([inttype, inttype], inttype)
(* type declarations for consistency checking *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* type declarations for consistency checking *)
val _ = op arithOp   : (int * int -> int) -> (value list -> value)
val _ = op arithtype : ty
(* utility functions for building \nml\ primitives S422b *)
fun comparison f = binaryOp (embedBool o f)
fun intcompare f = 
      comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                   | _ => raise BugInTypeInference "comparing non-numbers")
fun comptype x = funtype ([x, x], booltype)
(* type declarations for consistency checking *)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)
val _ = op comptype   : ty -> ty
val primFunBasis =
  let fun addPrim ((name, prim, tau), (Gamma, Delta, rho)) = 
        ( bindtyscheme (name, generalize (tau, freetyvarsGamma Gamma), Gamma)
        , Delta
        , bind (name, PRIMITIVE prim, rho)
        )
  in  foldl addPrim predefinedTypeBasis (
                                (* primitives for \nml\ and \uml\ [[::]] 440b *)
                                         ("error", unaryOp (fn v => raise
                                                  RuntimeError (valueString v)),
                                                   funtype ([alpha], beta)) ::

                               (* primitives for \nml\ and \uml\ [[::]] S470c *)
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

                               (* primitives for \nml\ and \uml\ [[::]] S422a *)
                                         ("+", arithOp op +,   arithtype) :: 
                                         ("-", arithOp op -,   arithtype) :: 
                                         ("*", arithOp op *,   arithtype) :: 
                                         ("/", arithOp op div, arithtype) ::

                               (* primitives for \nml\ and \uml\ [[::]] S422c *)
                                         ("<", intcompare op <,
                                                            comptype inttype) ::
                                         (">", intcompare op >,
                                                            comptype inttype) ::
                                         ("=", comparison primitiveEquality,
                                                              comptype alpha) ::

                               (* primitives for \nml\ and \uml\ [[::]] S423a *)
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
(* implementations of \uml\ primitives and definition of [[initialBasis]] S443a *)
val initialBasis =
  let val predefinedFuns =
        
         [ ";  predefined uML functions 458 "
         , "(define null? (xs)"
         , "   (case xs"
         , "      [(cons y ys) #t]"
         , "      ['()         #f]))"
         , ";;unboxuml"
         , ";  predefined uML functions 464d "
         , "(data (* * => *) pair"
         , "  [PAIR : (forall ['a 'b] ('a 'b -> (pair 'a 'b)))])"
         , ";  predefined uML functions 464e "
         , "(val pair PAIR)"
         , "(define fst (p)"
         , "   (case p [(PAIR x _) x]))"
         , "(define snd (p)"
         , "   (case p [(PAIR _ y) y]))"
         , ";  predefined uML functions 474c "
         , "(define Int.compare (n1 n2)"
         , "  (if (< n1 n2) LESS"
         , "      (if (< n2 n1) GREATER"
         , "          EQUAL)))"
         , ";;unboxuml"
         , ";  predefined uML functions 475a "
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
         , ";  predefined uML functions 475b "
         , "(define append (xs ys)"
         , "  (case xs"
         , "     ['()         ys]"
         , "     [(cons z zs) (cons z (append zs ys))]))"
         , ""
         , "(define revapp (xs ys)"
         , "  (case xs"
         , "     ['()         ys]"
         , "     [(cons z zs) (revapp zs (cons z ys))]))"
         , ";  predefined uML functions 475c "
         , "(define list1 (x) (cons x '()))"
         , "(define bind (x y alist)"
         , "  (case alist"
         , "     ['() (list1 (pair x y))]"
         , "     [(cons p ps)"
         , "        (if (= x (fst p))"
         , "            (cons (pair x y) ps)"
         , "            (cons p (bind x y ps)))]))"
         , ";;unboxuml"
         , ";  predefined uML functions 475d "
         , "(define find (x alist)"
         , "  (case alist"
         , "       ['()   NONE]"
         , "       [(cons (PAIR key value) pairs)"
         , "          (if (= x key)"
         , "              (SOME value)"
         , "              (find x pairs))]))"
         , ";;unboxuml"
         , ";  predefined uML functions 476 "
         , "(define bound? (x alist)"
         , "  (case (find x alist)"
         , "     [(SOME _) #t]"
         , "     [NONE     #f]))"
         , ";;unboxuml"
         , ";  predefined uML functions S445a "
         , "(define and (b c) (if b  c  b))"
         , "(define or  (b c) (if b  b  c))"
         , "(define not (b)   (if b #f #t))"
         , ";  predefined uML functions S445b "
         , "(define o (f g) (lambda (x) (f (g x))))"
         , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
         , "(define uncurry (f) (lambda (x y) ((f x) y)))"
         , ";  predefined uML functions S445c "
         , "(define caar (xs) (car (car xs)))"
         , "(define cadr (xs) (car (cdr xs)))"
         , "(define cdar (xs) (cdr (car xs)))"
         , ";  predefined uML functions S445d "
         , "(define filter (p? xs)"
         , "  (case xs"
         , "     ['()   '()]"
         , "     [(cons y ys)  (if (p? y) (cons y (filter p? ys))"
         , "                              (filter p? ys))]))"
         , ";  predefined uML functions S445e "
         , "(define map (f xs)"
         , "  (case xs"
         , "     ['() '()]"
         , "     [(cons y ys) (cons (f y) (map f ys))]))"
         , ";  predefined uML functions S445f "
         , "(define app (f xs)"
         , "  (case xs"
         , "     ['() UNIT]"
         , "     [(cons y ys) (begin (f y) (app f ys))]))"
         , ";  predefined uML functions S445g "
         , "(define reverse (xs) (revapp xs '()))"
         , ";  predefined uML functions S445h "
         , "(define exists? (p? xs)"
         , "  (case xs"
         , "     ['() #f]"
         , "     [(cons y ys) (if (p? y) #t (exists? p? ys))]))"
         , "(define all? (p? xs)"
         , "  (case xs"
         , "     ['() #t]"
         , "     [(cons y ys) (if (p? y) (all? p? ys) #f)]))"
         , ";  predefined uML functions S445i "
         , "(define foldr (op zero xs)"
         , "  (case xs"
         , "     ['() zero]"
         , "     [(cons y ys) (op y (foldr op zero ys))]))"
         , ";  predefined uML functions S446a "
         , "(define foldl (op zero xs)"
         , "  (case xs"
         , "     ['() zero]"
         , "     [(cons y ys) (foldl op (op y zero) ys)]))"
         , ";  predefined uML functions S446b "
         , "(define <= (x y) (not (> x y)))"
         , "(define >= (x y) (not (< x y)))"
         , "(define != (x y) (not (= x y)))"
         , ";  predefined uML functions S446c "
         , "(define max (m n) (if (> m n) m n))"
         , "(define min (m n) (if (< m n) m n))"
         , "(define negated (n) (- 0 n))"
         , "(define mod (m n) (- m (* n (/ m n))))"
         , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
         , "(define lcm (m n) (* m (/ n (gcd m n))))"
         , ";  predefined uML functions S446d "
         , "(define min* (xs) (foldr min (car xs) (cdr xs)))"
         , "(define max* (xs) (foldr max (car xs) (cdr xs)))"
         , "(define gcd* (xs) (foldr gcd (car xs) (cdr xs)))"
         , "(define lcm* (xs) (foldr lcm (car xs) (cdr xs)))"
         , ";  predefined uML functions S446e "
         , "(define list1 (x)               (cons x '()))"
         , "(define list2 (x y)             (cons x (list1 y)))"
         , "(define list3 (x y z)           (cons x (list2 y z)))"
         , "(define list4 (x y z a)         (cons x (list3 y z a)))"
         , "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
         , "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
         , "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
         , "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
         , ";  predefined uML functions S446f "
         , "(define takewhile (p? xs)"
         , "  (case xs"
         , "     ['() '()]"
         , "     [(cons y ys)"
         , "        (if (p? y)"
         , "            (cons y (takewhile p? ys))"
         , "            '())]))"
         , ";  predefined uML functions S446g "
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
(* implementations of \uml\ primitives and definition of [[initialBasis]] S443b *)
val primitiveBasis : basis = (* a mockup, but it's the truth *)
  foldl (fn ((name, prim, tau), (Gamma, Delta, rho)) =>
            (Gamma, Delta, bind (name, PRIMITIVE prim, rho)))
        emptyBasis
        ((* primitives for \nml\ and \uml\ [[::]] 440b *)
         ("error", unaryOp (fn v => raise RuntimeError (valueString v)),
                   funtype ([alpha], beta)) ::
         (* primitives for \nml\ and \uml\ [[::]] S470c *)
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
         (* primitives for \nml\ and \uml\ [[::]] S422a *)
         ("+", arithOp op +,   arithtype) :: 
         ("-", arithOp op -,   arithtype) :: 
         ("*", arithOp op *,   arithtype) :: 
         ("/", arithOp op div, arithtype) ::
         (* primitives for \nml\ and \uml\ [[::]] S422c *)
         ("<", intcompare op <,              comptype inttype) :: 
         (">", intcompare op >,              comptype inttype) ::
         ("=", comparison primitiveEquality, comptype alpha) ::
         (* primitives for \nml\ and \uml\ [[::]] S423a *)
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
