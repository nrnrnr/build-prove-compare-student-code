(* usm.sml S547 *)


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
(*   ABSTRACT SYNTAX AND VALUES FOR \USMALLTALK                  *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values for \usmalltalk S548a *)
(* support for \usmalltalk\ stack frames S590a *)
datatype frame = FRAME_NUMBER of int
local
  val next_f = ref 0
in
  fun newFrame () = FRAME_NUMBER (!next_f) before next_f := !next_f + 1
end
(* support for \usmalltalk\ stack frames S590b *)
val noFrame = newFrame () (* top level, unit tests, etc... *)
(* support for \usmalltalk\ stack frames S590c *)
type active_send = { method : name, class : name, loc : srcloc }
(* support for \usmalltalk\ stack frames S590f *)
fun activeSendString { method, class, loc = (file, line) } =
  let val obj = if String.isPrefix "class " class then class
                else "an object of class " ^ class
  in  concat [file, ", line ", intString line, ": ", "sent `", method, "` to ",
                                                                            obj]
  end
(* definitions of [[exp]], [[rep]], and [[class]] for \usmalltalk 686a *)
datatype rep
  = USER     of value ref env (* ordinary object *)
  | ARRAY    of value Array.array
  | NUM      of int
  | SYM      of name
  | CLOSURE  of name list * exp list * value ref env * class * frame
  | CLASSREP of class
  | METHODV  of method        (* compiled method *)
(* definitions of [[exp]], [[rep]], and [[class]] for \usmalltalk 686c *)
and class
  = CLASS of { name    : name            (* name of the class *)
             , super   : class option    (* superclass, if any *)
             , ivars   : string list     (* instance variables *)
             , methods : method env ref  (* both exported and private *)
             , class   : metaclass ref   (* class of the class object *)
             }
and metaclass = PENDING | META of class
(* definitions of [[exp]], [[rep]], and [[class]] for \usmalltalk 688a *)
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
(* definitions of [[value]] and [[method]] for \usmalltalk 685 *)
withtype value = class * rep
(* definitions of [[value]] and [[method]] for \usmalltalk 686d *)
and method = { name : name, formals : name list, locals : name list
             , body : exp, superclass : class
             }
(* definition of [[def]] for \usmalltalk 687b *)
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
(* definition of [[unit_test]] for \usmalltalk S548b *)
(* definition of [[unit_test]] for untyped languages (shared) S214a *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ASSERT of exp
                   | CHECK_ERROR  of exp
             | CHECK_PRINT of exp * string
(* definition of [[xdef]] (shared) S214b *)
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
              | DEFS   of def list  (*OMIT*)

fun className (CLASS {name, ...}) = name

(* definition of [[valueString]] for \usmalltalk S574a *)
fun valueString (c, NUM n) = intString n ^ valueString(c, USER [])
  | valueString (_, SYM v) = v
  | valueString (c, _) = "<" ^ className c ^ ">"
(* definition of [[expString]] for \usmalltalk S573b *)
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

(* support for logging (for coverage analysis) (BUG: software can't tell where this code came from [NW3jRhuk-2UCV57-1]) *)
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

(* utility functions on \usmalltalk\ classes, methods, and values 686b *)
fun instanceVars (_, USER rep) = rep
  | instanceVars self = bind ("self", ref self, emptyEnv)
(* type declarations for consistency checking *)
val _ = op instanceVars : value -> value ref env
(* utility functions on \usmalltalk\ classes, methods, and values S587c *)
fun valueSelector [] = "value"
  | valueSelector args = concat (map (fn _ => "value:") args)
(* utility functions on \usmalltalk\ classes, methods, and values S588a *)
fun className (CLASS {name,  ...}) = name
fun classId   (CLASS {class, ...}) = class
(* type declarations for consistency checking *)
val _ = op className : class -> name
val _ = op classId   : class -> metaclass ref
(* utility functions on \usmalltalk\ classes, methods, and values S588b *)
fun methodName ({ name, ... } : method) = name
fun methodsEnv ms = foldl (fn (m, rho) => bind (methodName m, m, rho)) emptyEnv
                                                                              ms
(* type declarations for consistency checking *)
val _ = op methodName : method -> name
val _ = op methodsEnv : method list -> method env
(* utility functions on \usmalltalk\ classes, methods, and values S588e *)
fun mkClass name meta super ivars ms =
  (
(* if any name in [[ivars]] repeats a name declared in a superclass, raise [[RuntimeError]] S589a *)
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
(* type declarations for consistency checking *)
val _ = op mkClass : name -> metaclass -> class -> name list -> method list ->
                                                                           class



(*****************************************************************)
(*                                                               *)
(*   SUPPORT FOR BOOTSTRAPPING CLASSES/VALUES USED DURING PARSING *)
(*                                                               *)
(*****************************************************************)

(* support for bootstrapping classes/values used during parsing 698a *)
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
(* type declarations for consistency checking *)
val _ = op mkInteger : int        -> value
val _ = op mkSymbol  : string     -> value
val _ = op mkArray   : value list -> value
(* support for bootstrapping classes/values used during parsing 698b *)
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
(* type declarations for consistency checking *)
val _ = op findClass : string * value ref env -> class
(* support for bootstrapping classes/values used during parsing 699a *)
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
(* type declarations for consistency checking *)
val _ = op mkBoolean : bool -> value
(* support for bootstrapping classes/values used during parsing S561a *)
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
(* type declarations for consistency checking *)
val _ = op mkBlock : name list * exp list * value ref env * class * frame ->
                                                                           value
(* support for bootstrapping classes/values used during parsing S570c *)
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

(* lexical analysis and parsing for \usmalltalk, providing [[filexdefs]] and [[stringsxdefs]] S574b *)
(* lexical analysis for \usmalltalk S574c *)
datatype pretoken = INTCHARS of char list
                  | NAME     of name
                  | QUOTE    of string option (* symbol or array *)
type token = pretoken plus_brackets
(* lexical analysis for \usmalltalk S574d *)
fun pretokenString (INTCHARS ds)    = implode ds
  | pretokenString (NAME    x)      = x
  | pretokenString (QUOTE NONE)     = "'"
  | pretokenString (QUOTE (SOME s)) = "'" ^ s
(* lexical analysis for \usmalltalk S575a *)
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
(* type declarations for consistency checking *)
val _ = op smalltalkToken : token lexer
end
(* parsers for single \usmalltalk\ tokens S576b *)
type 'a parser = (token, 'a) polyparser
val token : token parser = token (* make it monomorphic *)
val pretoken  = (fn (PRETOKEN t)=> SOME t  | _ => NONE) <$>? token
val namelike  = (fn (NAME s)         => SOME s  | _ => NONE) <$>? pretoken
val intchars  = (fn (INTCHARS ds)=> SOME ds | _ => NONE) <$>? pretoken
val sym   = (fn (QUOTE (SOME s)) => SOME s  | _ => NONE) <$>? pretoken
val quote = (fn (QUOTE NONE    ) => SOME () | _ => NONE) <$>? pretoken

val namelike = asAscii namelike

val int = intFromChars <$>! intchars
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
(* parsers and [[xdef]] streams for \usmalltalk S575b *)
fun arity name =
      let val cs = explode name
          fun isColon c = (c = #":")
      in  if Char.isAlpha (hd cs) then
            length (List.filter isColon cs)
          else
            1
      end
(* parsers and [[xdef]] streams for \usmalltalk S575c *)
fun arityOk name args = arity name = length args

fun arityErrorAt loc what msgname args =
  let fun argn n = if n = 1 then "1 argument" else intString n ^ " arguments"
  in  synerrorAt ("in " ^ what ^ ", message " ^ msgname ^ " expects " ^
                         argn (arity msgname) ^ ", but gets " ^
                         argn (length args)) loc
  end
(* parsers and [[xdef]] streams for \usmalltalk S576a *)
fun colonsOK name =
  Char.isAlpha (String.sub (name, 0)) orelse not (Char.contains name #":")
  handle Subscript => false

val badColon = not o colonsOK

fun badColonErrorAt msgname loc =
  synerrorAt ("a symbolic method name like " ^ msgname ^
              " may not contain a colon")
             loc
(* parsers and [[xdef]] streams for \usmalltalk S576c *)
val reserved = [ "set", "begin", "primitive", "return", "block", "quote"
               , "compiled-method" , "subclass-of", "ivars", "method"
               , "class-method", "locals", "val", "define", "use"
               , "check-error", "check-expect", "check-assert"
               ]
val name = rejectReserved reserved <$>! namelike
(* parsers and [[xdef]] streams for \usmalltalk S577a *)
fun isImmutable x =
  List.exists (fn x' => x' = x) ["true", "false", "nil", "self", "super"] 
val immutable = sat isImmutable name

val mutable =
  let fun can'tMutate (loc, x) =
        ERROR (srclocString loc ^
               ": you cannot set or val-bind pseudovariable " ^ x)
  in  can'tMutate <$>! @@ immutable <|> OK <$>! name
  end
(* type declarations for consistency checking *)
val _ = op name : string parser
val _ = op int  : int    parser
(* type declarations for consistency checking *)
val _ = op mutable   : name parser
val _ = op immutable : name parser
(* parsers and [[xdef]] streams for \usmalltalk S577b *)
val atomicExp
  =  LITERAL <$> NUM    <$> int
 <|> LITERAL <$> SYM    <$> (sym <|> (quote *> name)
                                 <|> (quote *> (intString <$> int)))
 <|> SUPER              <$  eqx "super" name
 <|> VAR                <$> name
(* parsers and [[xdef]] streams for \usmalltalk S577c *)
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
(* type declarations for consistency checking *)
val _ = op quotelit : value parser
(* parsers and [[xdef]] streams for \usmalltalk S578a *)
(* definition of function [[bodyWarning]] S581c *)
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
(* parsers and [[xdef]] streams for \usmalltalk S578b *)
(* definition of function [[curlyWarning]] S581a *)
fun curlyWarning (loc, es) =
  let fun nameFollowed (VAR _ :: _ :: _) = true
        | nameFollowed (_ :: es) = nameFollowed es
        | nameFollowed [] = false
      val () = if nameFollowed es then
                 warnAt loc ["inside {...} it looks like (...) was forgotten"]
               else
                 (* warn about blocks of the form [[{exp name}]] S581b *)
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
(* parsers and [[xdef]] streams for \usmalltalk S579a *)
val printable = name <|> implode <$> intchars

val testtable = usageParsers
  [ ("(check-expect e1 e2)", curry CHECK_EXPECT <$> exp <*> exp)
  , ("(check-assert e)",           CHECK_ASSERT <$> exp)
  , ("(check-error e)",            CHECK_ERROR  <$> exp)
  , ("(check-print e chars)", curry CHECK_PRINT <$> exp <*> printable)
  ]
(* type declarations for consistency checking *)
val _ = op exp      : exp parser
(* type declarations for consistency checking *)
val _ = op testtable : unit_test parser
(* parsers and [[xdef]] streams for \usmalltalk S579b *)
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
(* type declarations for consistency checking *)
val _ = op method : method_def parser
val parseMethods = many method <* many eol <* eos
(* parsers and [[xdef]] streams for \usmalltalk S580a *)
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
(* type declarations for consistency checking *)
val _ = op deftable : def parser
(* parsers and [[xdef]] streams for \usmalltalk S580b *)
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
(* type declarations for consistency checking *)
val _ = op xdeftable : xdef parser
val _ = op xdef      : xdef parser
(* parsers and [[xdef]] streams for \usmalltalk S580c *)
val xdefstream = interactiveParsedStream (smalltalkToken, xdef)
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
(*   EVALUATION, TESTING, AND THE READ-EVAL-PRINT LOOP FOR \USMALLTALK *)
(*                                                               *)
(*****************************************************************)

(* evaluation, testing, and the read-eval-print loop for \usmalltalk S548c *)
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
(* definition of [[nullsrc]], for use in \ml~code that sends messages S549b *)
val nullsrc : srcloc = ("internally generated SEND node", 1)
(* support for primitives and built-in classes S550c *)
(* utility functions for building primitives in \usmalltalk S551a *)
type primitive = value list * value ref env -> value
fun arityError n args =
  raise RuntimeError ("primitive expected " ^ intString n ^
                      " arguments; got " ^ intString (length args))
fun unaryPrim  f = (fn ([a],    _) => f  a     | (vs, _) => arityError 0 vs)
fun binaryPrim f = (fn ([a, b], _) => f (a, b) | (vs, _) => arityError 1 vs)
(* type declarations for consistency checking *)
val _ = op unaryPrim  : (value         -> value) -> primitive
val _ = op binaryPrim : (value * value -> value) -> primitive
(* metaclass utilities 695b *)
fun metaclass (CLASS { class = ref meta, ... }) =
  case meta of META c => c
             | PENDING => raise InternalError "pending class"

fun classObject c = (metaclass c, CLASSREP c)
(* type declarations for consistency checking *)
val _ = op metaclass   : class -> class
val _ = op classObject : class -> value
(* metaclass utilities S588d *)
fun setMeta (CLASS { class = m as ref PENDING, ... }, meta) = m := META meta
  | setMeta (CLASS { class = ref (META _), ... }, _) =
      raise InternalError "double patch"
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives S552a *)
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
(* type declarations for consistency checking *)
val _ = op eqRep : value * value -> bool
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives S552c *)
fun memberOf ((c, _), (_, CLASSREP c')) = mkBoolean (classId c = classId c')
  | memberOf _ = raise RuntimeError "argument of isMemberOf: must be a class"

fun kindOf ((c, _), (_, CLASSREP (CLASS {class=u', ...}))) =
      let fun subclassOfClassU' (CLASS {super, class=u, ... }) =
            u = u' orelse (case super of NONE => false
                                       | SOME c => subclassOfClassU' c)
      in  mkBoolean (subclassOfClassU' c)
      end
  | kindOf _ = raise RuntimeError "argument of isKindOf: must be a class"
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives S553c *)
fun error (_, (_, SYM s)) = raise RuntimeError s
  | error (_, (c, _    )) =
      raise RuntimeError ("error: got class " ^ className c ^
                                                            "; expected Symbol")
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives S559a *)
fun errorPrim msg = fn _ => raise RuntimeError msg
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives S588c *)
fun findClassAndMeta (supername, xi) =
  case !(find (supername, xi))
    of (meta, CLASSREP c) => (c, meta)
     | v => raise RuntimeError ("object " ^ supername ^ " = " ^ valueString v ^
                                " is not a class")
(* type declarations for consistency checking *)
val _ = op findClassAndMeta : name * value ref env -> class * class
(* \ml\ functions for [[Object]]'s and [[UndefinedObject]]'s primitives S589b *)
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
(* type declarations for consistency checking *)
val _ = op methodDefns : class * class -> method_def list -> method list *
                                                                     method list
val _ = op method : method_def -> method
  in  foldr addMethodDefn ([], []) ms
  end
(* utility functions for parsing internal method definitions S558a *)
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
(* type declarations for consistency checking *)
val _ = op internalParse : 'a parser -> string list -> 'a
(* utility functions for parsing internal method definitions S558b *)
val bogusSuperclass =
  CLASS { name = "bogus superclass", super = NONE
        , ivars = [], methods = ref [ ], class = ref PENDING
        }
val internalMethodDefns = methodDefns (bogusSuperclass, bogusSuperclass)
fun internalMethods strings =
  case (internalMethodDefns o internalParse parseMethods) strings
    of ([], imethods) => imethods 
     | (_ :: _, _) => raise InternalError "primitive class has class methods"
(* type declarations for consistency checking *)
val _ = op internalMethods : string list -> method list
(* built-in class [[Object]] 696a *)
val objectMethods =
  internalMethods 
                   [ ";  methods of class [[Object]] 634 "
                   ,
                  "(method ==  (anObject) (primitive sameObject self anObject))"
                   , "(method !== (anObject) ((self == anObject) not))"
                   , ";  methods of class [[Object]] 635a "
                   , "(method =  (anObject) (self == anObject))"
                   , "(method != (anObject) ((self = anObject) not))"
                   , ";  methods of class [[Object]] 655a "
                   , "(method isNil  () false)  ;; definitions on Object"
                   , "(method notNil () true)"
                   , ";  methods of class [[Object]] S558c "
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
(* built-in class [[UndefinedObject]] and value [[nilValue]] 696b *)
val nilClass = 
  mkClass "UndefinedObject" PENDING objectClass []
    (internalMethods 
                      [ ";  methods of class [[UndefinedObject]] 654 "
                      ,
                   "(method isNil  () true)   ;; definitions on UndefinedObject"
                      , "(method notNil () false)"
                      , ";  methods of class [[UndefinedObject]] S559d "
                      , "(method print () ('nil print) self)"
                       ])
(* built-in class [[UndefinedObject]] and value [[nilValue]] 696c *)
val nilValue = 
  let val nilCell  = ref (nilClass, USER []) : value ref
      val nilValue = (nilClass, USER (bind ("self", nilCell, emptyEnv)))
      val _        = nilCell := nilValue
  in  nilValue
  end
(* higher-order functions for creating \usmalltalk\ primitives S554h *)
fun arrayPrimitive f ((c, ARRAY a) :: vs, _) = f (a, vs)
  | arrayPrimitive f _ = raise RuntimeError "Array primitive used on non-array"
(* type declarations for consistency checking *)
val _ = op arrayPrimitive : (value array * value list -> value) -> primitive
(* higher-order functions for creating \usmalltalk\ primitives S555e *)
fun classPrim f = 
  unaryPrim (fn (meta, CLASSREP c) => f (meta, c)
              | _ => raise RuntimeError "class primitive sent to non-class")
(* type declarations for consistency checking *)
val _ = op classPrim : (class * class -> value) -> primitive
(* \ml\ code for remaining classes' primitives 691b *)
type closure = name list * exp list * value ref env * class * frame
val applyClosureRef : (closure * value list * value ref env -> value) ref
  = ref (fn _ => raise InternalError "applyClosureRef not set")

fun valuePrim ((_, CLOSURE clo) :: vs, xi) = !applyClosureRef (clo, vs, xi)
  | valuePrim _ = raise RuntimeError "primitive `value` needs a closure"
(* \ml\ code for remaining classes' primitives 699b *)
val classPrimitive = unaryPrim (fn (c, rep) => classObject c)
(* \ml\ code for remaining classes' primitives 700 *)
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
(* type declarations for consistency checking *)
val _ = op mkIvars       : class -> value ref env
val _ = op newUserObject : class -> value
end
(* \ml\ code for remaining classes' primitives S551b *)
fun withOverflow binop ([(_, NUM n), (_, NUM m), ovflw], xi) =
      (internalThunk [VALUE (mkInteger (binop (n, m)))]
       handle Overflow => ovflw)
  | withOverflow _ ([_, _, _], _) =
      raise RuntimeError "numeric primitive with overflow expects numbers"
  | withOverflow _ _ =
      raise RuntimeError "numeric primitive with overflow expects 3 arguments"
and internalThunk exps =
      mkBlock ([], exps, emptyEnv, objectClass, noFrame)
(* type declarations for consistency checking *)
val _ = op internalThunk : exp list -> value
val _ = op withOverflow : (int * int -> int) -> primitive
(* \ml\ code for remaining classes' primitives S553e *)
fun printInt (self as (_, NUM n)) = ( xprint (intString n); self )
  | printInt _ = raise RuntimeError ("printInt primitive on non-SmallInteger")
(* \ml\ code for remaining classes' primitives S553f *)
fun printu (self as (_, NUM n)) = ( printUTF8 n; self )
  | printu _ = raise RuntimeError ("printu primitives on non-SmallInteger")
(* \ml\ code for remaining classes' primitives S553h *)
fun binaryInt mk operator ((_, NUM n), (_, NUM m)) = mk (operator (n, m))
  | binaryInt _ _         ((_, NUM n), (c, _)) =
      raise RuntimeError ("numeric primitive expected numeric argument, got <"
                          ^ className c ^ ">")
  | binaryInt _ _         ((c, _), _) =
      raise RuntimeError ("numeric primitive method defined on <" ^ className c
                                                                          ^ ">")
fun arithop    operator = binaryPrim (binaryInt mkInteger operator)
fun intcompare operator = binaryPrim (binaryInt mkBoolean operator)
(* type declarations for consistency checking *)
val _ = op binaryInt  : ('a -> value) -> (int * int -> 'a)   -> value * value ->
                                                                           value
val _ = op arithop    :                  (int * int -> int)  -> primitive
val _ = op intcompare :                  (int * int -> bool) -> primitive
(* \ml\ code for remaining classes' primitives S554b *)
fun newInteger ((_, CLASSREP c), (_, NUM n)) = (c, NUM n)
  | newInteger _ = raise RuntimeError (
                                   "made new integer with non-int or non-class")
(* \ml\ code for remaining classes' primitives S554d *)
fun printSymbol (self as (_, SYM s)) = (xprint s; self)
  | printSymbol _ = raise RuntimeError
                                 "cannot print when object inherits from Symbol"
(* \ml\ code for remaining classes' primitives S554e *)
fun newSymbol ((_, CLASSREP c), (_, SYM s)) = (c, SYM s)
  | newSymbol _ = raise RuntimeError (
                                 "made new symbol with non-symbol or non-class")
(* \ml\ code for remaining classes' primitives S554g *)
fun newArray ((_, CLASSREP c), (_, NUM n)) = (c, ARRAY (Array.array (n, nilValue
                                                                             )))
  | newArray _ = raise RuntimeError
                                "Array new sent to non-class or got non-integer"
(* \ml\ code for remaining classes' primitives S554i *)
fun arraySize (a, []) = mkInteger (Array.length a)
  | arraySize (a, vs) = arityError 0 vs
(* \ml\ code for remaining classes' primitives S555a *)
fun arrayAt (a, [(_, NUM n)]) = Array.sub (a, n)
  | arrayAt (_, [_])  = raise RuntimeError "Non-integer used as array subscript"
  | arrayAt (_, vs)   = arityError 1 vs

fun arrayUpdate (a, [(_, NUM n), x]) = (Array.update (a, n, x); nilValue)
  | arrayUpdate (_, [_, _]) = raise RuntimeError
                                           "Non-integer used as array subscript"
  | arrayUpdate (_, vs)     = arityError 2 vs
(* \ml\ code for remaining classes' primitives S556a *)
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
(* \ml\ code for remaining classes' primitives S556b *)
fun methodNames (_, CLASS { methods, ... }) = mkArray (map (mkSymbol o fst) (!
                                                                       methods))
(* \ml\ code for remaining classes' primitives S556c *)
fun getMethod ((_, CLASSREP (c as CLASS { methods, name, ... })), (_, SYM s)) =
      (mkCompiledMethod (find (s, !methods))
       handle NotFound _ =>
         raise RuntimeError ("class " ^ className c ^ " has no method " ^ s))
      before (if logging then logGetMethod name s else ())
  | getMethod ((_, CLASSREP _), _) =
      raise RuntimeError "getMethod primitive given non-name"    
  | getMethod _ =
      raise RuntimeError "getMethod primitive given non-class"    
(* \ml\ code for remaining classes' primitives S557a *)
fun removeMethod ((_, CLASSREP (c as CLASS { methods, ... })), (_, SYM s)) =
      (methods := List.filter (fn (m, _) => m <> s) (!methods); nilValue)
  | removeMethod ((_, CLASSREP _), _) =
      raise RuntimeError "removeMethod primitive given non-name"    
  | removeMethod _ =
      raise RuntimeError "removeMethod primitive given non-class"    
(* \ml\ code for remaining classes' primitives S557b *)
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
(* \ml\ code for remaining classes' primitives S559c *)
fun superclassObject (_, CLASS { super = NONE, ... })   = nilValue
  | superclassObject (_, CLASS { super = SOME c, ... }) = classObject c
(* built-in classes [[Class]] and [[Metaclass]] 696d *)
val classClass =
  mkClass "Class" PENDING objectClass []
    (internalMethods 
                      [ ";  methods of class [[Class]] 697a "
                      , "(method new () (primitive newUserObject self))"
                      , ";  methods of class [[Class]] S559b "
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
                      [ ";  methods of class [[Metaclass]] 697b "
                      ,
         "(method new () (self error: 'a-metaclass-may-have-only-one-instance))"
                       ])
(* metaclasses for built-in classes 695c *)
fun metaSuper (CLASS { super = NONE,       ... }) = classClass
  | metaSuper (CLASS { super = SOME c_sup, ... }) = metaclass c_sup
(* type declarations for consistency checking *)
val _ = op metaSuper : class -> class
(* metaclasses for built-in classes 695d *)
fun mkMeta c classmethods =
  mkClass ("class " ^ className c) (META metaclassClass) (metaSuper c)
          [] classmethods
(* type declarations for consistency checking *)
val _ = op mkMeta : class -> method list -> class
(* metaclasses for built-in classes 697c *)
fun patchMeta c = setMeta (c, mkMeta c [])
val () = app patchMeta [objectClass, nilClass, classClass, metaclassClass]
(* definition of [[newClassObject]] and supporting functions 695a *)
fun newClassObject {name, super, ivars, methods} xi =
  let val (super, superMeta) = findClassAndMeta (super, xi)
        handle NotFound s =>
          raise RuntimeError ("Superclass " ^ s ^ " not found")
      val (cmethods, imethods) = methodDefns (superMeta, super) methods
      val class = mkClass name PENDING super ivars imethods
      val ()    = setMeta (class, mkMeta class cmethods)
  in  classObject class
  end
(* functions for managing and printing a \usmalltalk\ stack trace S582a *)
local

(* private state and functions for printing indented message traces ((usm)) S582b *)
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
  (* type declarations for consistency checking *)
  val _ = op curlyWarning : exp list located -> exp list
  (* type declarations for consistency checking *)
  val _ = op traceMe : value ref env -> bool

(* private state and functions for printing indented message traces ((usm)) S584a *)
  val tindent = ref 0
  fun indent 0 = ()
    | indent n = (xprint "  "; indent (n-1))
  (* type declarations for consistency checking *)
  val _ = op indent : int -> unit

(* private state and functions for printing indented message traces ((usm)) S584b *)
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

(* private state and functions for maintaining a stack of source-code locations ((usm)) S584c *)
  val locationStack = ref [] : (string * srcloc * string) list ref
  fun push info = locationStack := info :: !locationStack
  fun pop () = case !locationStack
                 of []     => raise InternalError "tracing stack underflows"
                  | h :: t => locationStack := t
in
  (* exposed message-tracing functions ((usm)) S583a *)
  fun resetTrace ()       = (locationStack := []; tindent := 0)
  fun traceIndent what xi = (push what; tracePrint INDENT_AFTER   xi)
  fun outdentTrace     xi = (pop ();    tracePrint OUTDENT_BEFORE xi)
  (* type declarations for consistency checking *)
  val _ = op resetTrace   : unit -> unit
  val _ = op traceIndent : string * srcloc * string ->       value ref env -> (
                                                    unit -> string list) -> unit
  val _ = op outdentTrace   :               value ref env -> (unit -> string
                                                                   list) -> unit
  (* exposed stack-tracing functions ((usm)) S585a *)
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
  (* type declarations for consistency checking *)
  val _ = op removeRepeat : int -> ''a list -> int * ''a list * ''a list
  (* exposed stack-tracing functions ((usm)) S585b *)
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
  (* type declarations for consistency checking *)
  val _ = op findRepeat : ''a list -> int -> int * ''a list * ''a list
  (* exposed stack-tracing functions ((usm)) S586a *)
  fun findRepeatAfter xs 10 = ([], (0, [], xs))
    | findRepeatAfter xs  m =
        let val (k, header, ys) = findRepeat (List.drop (xs, m)) 1
        in  if k > 0 then
              (List.take(xs, m), (k, header, ys))
            else
              findRepeatAfter xs (m + 1)
        end handle Subscript => ([], (0, [], xs))
  (* type declarations for consistency checking *)
  val _ = op findRepeatAfter : ''a list -> int -> ''a list * (int * ''a list *
                                                                       ''a list)
  (* exposed stack-tracing functions ((usm)) S586b *)
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
      in  (* show the (possibly condensed) stack in [[headerAndRepeat]] S586c *)
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
  (* type declarations for consistency checking *)
  val _ = op showStackTrace : bool -> unit
  (* exposed stack-tracing functions ((usm)) S587a *)
  fun eprintlnTrace s = 
    ( eprintln s
    ; showStackTrace (String.isSubstring "recursion too deep" s
                      orelse String.isSubstring "CPU time exhausted" s)
    ; resetTrace ()
    )
  (* type declarations for consistency checking *)
  val _ = op eprintlnTrace  : string -> unit
end
(* definition of [[primitives]] S550d *)
val primitives = (* primitives for \usmalltalk\ [[::]] S551c *)
                 ("addWithOverflow", withOverflow op + ) ::
                 ("subWithOverflow", withOverflow op - ) ::
                 ("mulWithOverflow", withOverflow op * ) ::
                 (* primitives for \usmalltalk\ [[::]] S551d *)
                 ("hash", unaryPrim
                              (fn (_, SYM s) => mkInteger (fnvHash s)
                                | v => raise RuntimeError
                                          "hash primitive expects a symbol")) ::
                 (* primitives for \usmalltalk\ [[::]] S552b *)
                 ("sameObject", binaryPrim (mkBoolean o eqRep)) ::
                 (* primitives for \usmalltalk\ [[::]] S553a *)
                 ("isKindOf",   binaryPrim kindOf) ::
                 ("isMemberOf", binaryPrim memberOf) ::
                 (* primitives for \usmalltalk\ [[::]] S553b *)
                 ("class", classPrimitive) ::
                 (* primitives for \usmalltalk\ [[::]] S553d *)
                 ("error", binaryPrim error) ::
                 (* primitives for \usmalltalk\ [[::]] S553g *)
                 ("printSmallInteger", unaryPrim printInt) ::     
                 ("printu",            unaryPrim printu)   ::     
                 (* primitives for \usmalltalk\ [[::]] S554a *)
                 ("+",   arithop op +  )  ::
                 ("-",   arithop op -  )  ::
                 ("*",   arithop op *  )  ::
                 ("div", arithop op div)  ::
                 ("<",   intcompare op <) ::
                 (">",   intcompare op >) ::
                 (* primitives for \usmalltalk\ [[::]] S554c *)
                 ("newSmallInteger", binaryPrim newInteger) ::
                 (* primitives for \usmalltalk\ [[::]] S554f *)
                 ("printSymbol", unaryPrim  printSymbol) ::
                 ("newSymbol",   binaryPrim newSymbol  ) ::
                 (* primitives for \usmalltalk\ [[::]] S555b *)
                 ("arrayNew",    binaryPrim     newArray)   ::
                 ("arraySize",   arrayPrimitive arraySize)  ::
                 ("arrayAt",     arrayPrimitive arrayAt)    ::
                 ("arrayUpdate", arrayPrimitive arrayUpdate) ::
                 (* primitives for \usmalltalk\ [[::]] S555c *)
                 ("value", valuePrim) ::
                 (* primitives for \usmalltalk\ [[::]] S555d *)
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
                 (* primitives for \usmalltalk\ [[::]] S558d *)
                 ("subclassResponsibility",
                     errorPrim
              "subclass failed to implement a method it was responsible for") ::
                 ("leftAsExercise", 
                     errorPrim
                     "method was meant to be implemented as an exercise") :: nil
val () =   if isSome (OS.Process.getEnv "USMPRIM") then      (*OMIT*)
           app (println o fst) primitives else ()   (*OMIT*)
(* helper functions for evaluation 690b *)
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
(* type declarations for consistency checking *)
val _ = op findMethod : name * class -> method
val _ = op fm         : class        -> method
  in  fm class
  end
(* helper functions for evaluation S587b *)
fun optimizedBind (x, v, xi) =
  let val loc = find (x, xi)
  in  (loc := v; xi)
  end handle NotFound _ => bind (x, ref v, xi)
(* definition of the [[Return]] exception 687a *)
exception
  Return of { value : value, to : frame, unwound : active_send list }
(* evaluation, [[basis]], and [[processDef]] for \usmalltalk 688b *)
fun eval (e, rho, superclass, F, xi) =
  let val go = applyWithLimits id in go end (* OMIT *)
  let (* definition of function [[invokeMethod]] 690a *)
      fun invokeMethod ({ name, superclass, formals, locals, body },
                      receiver, vs, Fhat) =
            let val ivars  = instanceVars receiver
                val args   = mkEnv (formals, map ref vs)
                val locals = mkEnv (locals,  map (fn _ => ref nilValue) locals)
            in  eval (body, ivars <+> args <+> locals, superclass, Fhat, xi)
            end
            handle BindListLength => raise InternalError
                                             "bad arity in user method" (*OMIT*)
      (* type declarations for consistency checking *)
      val _ = op invokeMethod   : method * value * value list * frame -> value
      (* function [[ev]], the evaluator proper ((usm)) 689a *)
      fun ev (RETURN e) = raise Return { value = ev e, to = F, unwound = [] }
      (* function [[ev]], the evaluator proper ((usm)) 689b *)
        | ev (SEND (srcloc, receiver, msgname, args))  =
            let val obj as (class, rep) = ev receiver
                val vs = map ev args
                val _ = if logging then logSend srcloc msgname else () (*OMIT*)
                val startingClass =
                      case receiver of SUPER => superclass | _ => class
                val checkpoint = checkpointLimit ()  (*OMIT*)
                (* definition of function [[trace]] S583b *)
                fun trace action =
                  let val (file, line) = srcloc
                (* type declarations for consistency checking *)
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

(* reraise [[Return]], adding [[msgname]], [[class]], and [[loc]] to [[unwound]] S590d *)
                           let val this = { method = msgname, class = className
                                                           class, loc = srcloc }
                           in  raise Return { value = v, to = F', unwound = this
                                                                    :: unwound }
                           end
                   end)
            end
      (* function [[ev]], the evaluator proper ((usm)) 691a *)
        | ev (BLOCK (formals, body)) = mkBlock (formals, body, rho, superclass,
                                                                              F)
      (* function [[ev]], the evaluator proper ((usm)) 691d *)
        | ev (LITERAL c) = 
            (case c of NUM n => mkInteger n
                     | SYM s => mkSymbol s
                     | _ => raise InternalError "unexpected literal")
      (* function [[ev]], the evaluator proper ((usm)) 692a *)
        | ev (VALUE v) = v
      (* function [[ev]], the evaluator proper ((usm)) 692b *)
        | ev (VAR x) = !(find (x, rho) handle NotFound _ => find (x, xi))
        | ev (SET (x, e)) =
            let val v = ev e
                val cell = find (x, rho) handle NotFound _ => find (x, xi)
            in  cell := v; v
            end 
      (* function [[ev]], the evaluator proper ((usm)) 692c *)
        | ev (SUPER) = ev (VAR "self")
      (* function [[ev]], the evaluator proper ((usm)) 692d *)
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, nilValue)
            end
      (* function [[ev]], the evaluator proper ((usm)) 693a *)
        | ev (PRIMITIVE (p, args)) =
            let val f = find (p, primitives)
                        handle NotFound n =>
                          raise RuntimeError ("There is no primitive named " ^ n
                                                                               )
            in  f (map ev args, xi)
            end
      (* function [[ev]], the evaluator proper ((usm)) 693b *)
        | ev (METHOD (xs, ys, es)) =
            mkCompiledMethod { name = "", formals = xs, locals = ys
                             , body = BEGIN es, superclass = objectClass }
(* type declarations for consistency checking *)
val _ = op eval: exp * value ref env * class * frame * value ref env -> value
val _ = op ev  : exp -> value
  in  ev e
  end
(* evaluation, [[basis]], and [[processDef]] for \usmalltalk 691c *)
fun applyClosure ((formals, body, rho_c, superclass, frame), vs, xi) =
  eval (BEGIN body, rho_c <+> mkEnv (formals, map ref vs), superclass,
        frame, xi)
  handle BindListLength => 
    raise RuntimeError ("wrong number of arguments to block; expected " ^
                        "(<block> " ^ valueSelector formals ^ " " ^
                        spaceSep formals ^ ")")
(* type declarations for consistency checking *)
val _ = op applyClosure : closure * value list * value ref env -> value
val () = applyClosureRef := applyClosure
(* evaluation, [[basis]], and [[processDef]] for \usmalltalk 693c *)
fun evaldef (d, xi) =
  let fun ev e = eval (e, emptyEnv, objectClass, noFrame, xi)
                 (* handle unexpected [[Return]] in [[evaldef]] S590e *)
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
(* type declarations for consistency checking *)
val _ = op evaldef : def * value ref env -> value ref env * value
val _ = op ev      : exp -> value
  in  (xi', v)
  end
(* evaluation, [[basis]], and [[processDef]] for \usmalltalk S548e *)
type basis = value ref env
(* evaluation, [[basis]], and [[processDef]] for \usmalltalk S549a *)
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
(* definition of [[testIsGood]] for \usmalltalk S571a *)
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

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] S571b *)
      fun whatWasExpected (LITERAL (NUM n), _) = printsAs (mkInteger n)
        | whatWasExpected (LITERAL (SYM x), _) = printsAs (mkSymbol x)
        | whatWasExpected (e, OK v) =
            concat [printsAs v, " (from evaluating ", expString e, ")"]
        | whatWasExpected (e, ERROR _) =
            concat ["the result of evaluating ", expString e]

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] S572a *)
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

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] S572b *)
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

(* definitions of [[check{Expect,Assert,Error}Passes]] that call [[printsAs]] S572c *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   failtest [cefailed, "expected evaluating ", expString checkx,
                             " to cause an error, but evaluation produced ",
                             printsAs check]
      (* definition of [[checkPrintPasses]] S573a *)
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
(*   IMPLEMENTATIONS OF \USMALLTALK\ PRIMITIVES AND DEFINITION OF [[INITIALBASIS]] *)
(*                                                               *)
(*****************************************************************)

(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] S549c *)
val initialXi = emptyEnv

fun addClass (c, xi) = bind (className c, ref (classObject c), xi)
val initialXi =
  foldl addClass initialXi [ objectClass, nilClass, classClass, metaclassClass ]
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] S549d *)
val predefs = 
               [
";  predefined uSmalltalk classes and values ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
               , ";  definition of class [[Boolean]] S561b "
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
               , ";  predefined uSmalltalk classes and values 655b "
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
               ,
";  predefined uSmalltalk classes and values ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
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
               , ";  predefined uSmalltalk classes and values S548d "
               , ";  numeric classes 656a "
               , "(class Magnitude"
               , "    [subclass-of Object] ; abstract class"
               ,
            "    (method = (x) (self subclassResponsibility)) ; may not inherit"
               , "    (method < (x) (self subclassResponsibility))"
               , ";      other methods of class [[Magnitude]] 656b "
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
               ,
";  numeric classes ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) "
               , ";  definition of class [[Number]] 663a "
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
               , ";      other methods of class [[Number]] 663b "
               , "    (method -  (y) (self + (y  negated)))"
               ,
"    (method abs () ((self isNegative) ifTrue:ifFalse: {(self negated)} {self}))"
               , "    (method /  (y) (self * (y reciprocal)))"
               , ""
               , "    (method isZero             () (self  = (self coerce: 0)))"
               , "    (method isNegative         () (self  < (self coerce: 0)))"
               , "    (method isNonnegative      () (self >= (self coerce: 0)))"
               , "    (method isStrictlyPositive () (self  > (self coerce: 0)))"
               , ";      other methods of class [[Number]] S566b "
               , "    (method squared () (self * self))"
               , "    (method raisedToInteger: (anInteger)"
               , "        ((anInteger = 0) ifTrue:ifFalse:"
               , "            {(self coerce: 1)}"
               , "            {((anInteger = 1) ifTrue:ifFalse: {self}"
               ,
      "                {(((self raisedToInteger: (anInteger div: 2)) squared) *"
               ,
          "                    (self raisedToInteger: (anInteger mod: 2)))})}))"
               , ";      other methods of class [[Number]] S566c "
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
               , ";  numeric classes 664a "
               , "(class Integer"
               , "    [subclass-of Number] ; abstract class"
               , "    (method div: (n) (self subclassResponsibility))"
               , "    (method mod: (n) (self - (n * (self div: n))))"
               , "    (method gcd: (n) ((n = (self coerce: 0))"
               ,
       "                      ifTrue:ifFalse: {self} {(n gcd: (self mod: n))}))"
               , "    (method lcm: (n) (self * (n div: (self gcd: n))))"
               , ";      other methods of class [[Integer]] 664b "
               , "    (method reciprocal () (Fraction num:den: 1 self)) "
               , "    (method / (aNumber) ((self asFraction) / aNumber))"
               , ";      other methods of class [[Integer]] 669b "
               , "    (method asFraction () (Fraction num:den:  self 1))"
               , "    (method asFloat    () (Float    mant:exp: self 0))"
               , ";      other methods of class [[Integer]] 669c "
               , "    (method asInteger () self)"
               , "    (method coerce: (aNumber) (aNumber asInteger))"
               , ";      other methods of class [[Integer]] S567a "
               , "    (method timesRepeat: (aBlock) [locals count]"
               ,
    "        ((self isNegative) ifTrue: {(self error: 'negative-repeat-count)})"
               , "        (set count self)"
               , "        ({(count != 0)} whileTrue:"
               , "             {(aBlock value)"
               , "              (set count (count - 1))}))"
               , ")"
               , ";  numeric classes 664c "
               , "(class Fraction"
               , "    [subclass-of Number]"
               , "    [ivars num den]"
               , "    (method print () (num print) ('/ print) (den print) self)"
               , ";      other methods of class [[Fraction]] 664d "
               , "    (method num () num)  ; private"
               , "    (method den () den)  ; private"
               , ";      other methods of class [[Fraction]] 665a "
               , "    (method = (f) ((num = (f num)) and: {(den = (f den))}))"
               , "    (method < (f) ((num * (f den)) < ((f num) * den)))"
               , ";      other methods of class [[Fraction]] 665b "
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
               , ";      other methods of class [[Fraction]] 665c "
               ,
               "    (class-method num:den: (a b) ((self new) initNum:den: a b))"
               ,
         "    (method setNum:den: (a b) (set num a) (set den b) self) ; private"
               , "    (method initNum:den: (a b) ; private"
               , "        (self setNum:den: a b)"
               , "        (self signReduce)"
               , "        (self divReduce))"
               , ";      other methods of class [[Fraction]] 665d "
               , "    (method * (f)"
               ,
"      (((Fraction new) setNum:den: (num * (f num)) (den * (f den))) divReduce))"
               , ";      other methods of class [[Fraction]] 666a "
               , "    (method + (f) [locals temp]"
               , "        (set temp (den lcm: (f den)))"
               , "        (((Fraction new) setNum:den:"
               , "                         ((num     * (temp div: den)) +"
               , "                          ((f num) * (temp div: (f den))))"
               , "                         temp)"
               , "            divReduce))"
               , ";      other methods of class [[Fraction]] 666b "
               , "    (method reciprocal ()"
               , "       (((Fraction new) setNum:den: den num) signReduce))"
               , ";      other methods of class [[Fraction]] 666c "
               ,
        "    (method negated () ((Fraction new) setNum:den: (num negated) den))"
               , ";      other methods of class [[Fraction]] 666d "
               , "    (method isZero             () (num isZero))"
               , "    (method isNegative         () (num isNegative))"
               , "    (method isNonnegative      () (num isNonnegative))"
               , "    (method isStrictlyPositive () (num isStrictlyPositive))"
               , ";      other methods of class [[Fraction]] 668a "
               , "    (method asInteger  () (num div: den))"
               , "    (method asFloat    () ((num asFloat) / (den asFloat)))"
               , "    (method asFraction () self)"
               , ";      other methods of class [[Fraction]] 668b "
               , "    (method coerce: (aNumber) (aNumber asFraction))"
               , ")"
               , ";  numeric classes 670 "
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
               , ";  numeric classes S567b "
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
               , ";  numeric classes S568a "
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
               , ";      other methods of class [[Float]] S568b "
               , "    (method mant () mant)  ; private"
               , "    (method exp  () exp)   ; private"
               , ";      other methods of class [[Float]] S568c "
               , "    (method < (x) ((self - x) isNegative))"
               , "    (method = (x) ((self - x) isZero))"
               , ";      other methods of class [[Float]] S568d "
               , "    (method negated () (Float mant:exp: (mant negated) exp))"
               , ";      other methods of class [[Float]] S568e "
               , "    (method + (x-prime) "
               , "        ((exp >= (x-prime exp)) ifTrue:ifFalse:"
               ,
"            {(Float mant:exp: ((mant * (10 raisedToInteger: (exp - (x-prime exp)))) +"
               , "                                 (x-prime mant))"
               , "                              (x-prime exp))}"
               , "            {(x-prime + self)}))"
               , ";      other methods of class [[Float]] S569a "
               , "    (method * (x-prime) "
               ,
      "        (Float mant:exp: (mant * (x-prime mant)) (exp + (x-prime exp))))"
               , ";      other methods of class [[Float]] S569b "
               , "    (method reciprocal ()"
               , "        (Float mant:exp: (1000000000 div: mant) (-9 - exp)))"
               , ";      other methods of class [[Float]] S569c "
               , "    (method coerce: (aNumber) (aNumber asFloat))"
               , "    (method asFloat () self)"
               , ";      other methods of class [[Float]] S569d "
               , "    (method asInteger ()"
               , "        ((exp isNegative) ifTrue:ifFalse:"
               , "            {(mant div: (10 raisedToInteger: (exp negated)))}"
               , "            {(mant    * (10 raisedToInteger: exp))}))"
               , ";      other methods of class [[Float]] S569e "
               , "    (method asFraction ()"
               , "        ((exp < 0) ifTrue:ifFalse:"
               ,
    "            {(Fraction num:den: mant (10 raisedToInteger: (exp negated)))}"
               ,
      "            {(Fraction num:den: (mant * (10 raisedToInteger: exp)) 1)}))"
               , ";      other methods of class [[Float]] S569f "
               , "    (method isZero             () (mant isZero))"
               , "    (method isNegative         () (mant isNegative))"
               , "    (method isNonnegative      () (mant isNonnegative))"
               , "    (method isStrictlyPositive () (mant isStrictlyPositive))"
               , ";      other methods of class [[Float]] S570a "
               , "    (method print () "
               , "        (self print-normalize) "
               , "        (mant print) ('x10^ print) (exp print)"
               , "        (self normalize))"
               , ""
               , "    (method print-normalize ()"
               , "        ({((exp < 0) and: {((mant mod: 10) = 0)})} whileTrue:"
               , "            {(set exp (exp + 1)) (set mant (mant div: 10))}))"
               , ")"
               ,
  ";  predefined uSmalltalk classes and values that use numeric literals S560c "
               , "(val &trace 0)"
               ,
  ";  predefined uSmalltalk classes and values that use numeric literals S562a "
               , "(class Char"
               , "   [subclass-of Object]"
               , "   [ivars code-point]"
               , "   (class-method new: (n) ((self new) init: n))"
               , "   (method init:      (n) (set code-point n) self) ;; private"
               , "   (method print      ()  (primitive printu code-point))"
               , "   (method =          (c) (code-point = (c code-point)))"
               , "   (method code-point ()  code-point) ;; private"
               , ")"
               ,
  ";  predefined uSmalltalk classes and values that use numeric literals S562b "
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
               , ";  collection classes 656c "
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
               , ";    other methods of class [[Collection]] 657a "
               , "  (class-method with: (anObject)"
               , "      ((self new) add: anObject))"
               , "  (class-method withAll: (aCollection)"
               , "      ((self new) addAll: aCollection))"
               , ";    other methods of class [[Collection]] 657b "
               , "  (method addAll: (aCollection) "
               , "      (aCollection do: [block (x) (self add: x)])"
               , "      self)"
               , ";    other methods of class [[Collection]] 657c "
               , "  (method remove: (oldObject) "
               ,
   "      (self remove:ifAbsent: oldObject {(self error: 'remove-was-absent)}))"
               , "  (method removeAll: (aCollection) "
               , "      (aCollection do: [block (x) (self remove: x)])"
               , "      self)"
               , ";    other methods of class [[Collection]] 657d "
               , "  (method size () [locals n]"
               , "      (set n 0)"
               , "      (self do: [block (_) (set n (n + 1))])"
               , "      n)"
               , "  (method occurrencesOf: (anObject) [locals n]"
               , "      (set n 0)"
               ,
       "      (self do: [block (x) ((x = anObject) ifTrue: {(set n (n + 1))})])"
               , "      n)"
               , ";    other methods of class [[Collection]] 657e "
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
               , ";    other methods of class [[Collection]] 658a "
               , "  (method inject:into: (aValue binaryBlock)"
               ,
  "     (self do: [block (x) (set aValue (binaryBlock value:value: x aValue))])"
               , "     aValue)"
               , ";    other methods of class [[Collection]] 658b "
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
               , ";    other methods of class [[Collection]] 658c "
               , "  (method species () (self class))"
               , ";    other methods of class [[Collection]] 658d "
               , "  (method print ()"
               , "      (self printName)"
               , "      (left-round print)"
               , "      (self do: [block (x) (space print) (x print)])"
               , "      (space print)"
               , "      (right-round print)"
               , "      self)"
               , "  (method printName () (((self class) name) print))"
               , ")"
               , ";  collection classes 659a "
               , "(class KeyedCollection"
               , "    [subclass-of Collection]  ; abstract class"
               ,
 "    (method associationsDo: (aBlock)           (self subclassResponsibility))"
               ,
 "    (method removeKey:ifAbsent: (key exnBlock) (self subclassResponsibility))"
               ,
 "    (method at:put: (key value)                (self subclassResponsibility))"
               , ";      other methods of class [[KeyedCollection]] 659b "
               , "    (method do: (aBlock) "
               ,
"        (self associationsDo: [block (anAssoc) (aBlock value: (anAssoc value))]))"
               , ";      other methods of class [[KeyedCollection]] 660a "
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
               , ";      other methods of class [[KeyedCollection]] 660b "
               , "    (method keyAtValue: (value) "
               ,
   "        (self keyAtValue:ifAbsent: value {(self error: 'value-not-found)}))"
               , "    (method keyAtValue:ifAbsent: (value exnBlock)"
               , "        (self associationsDo: [block (x) "
               ,
                "            (((x value) = value) ifTrue: {(return (x key))})])"
               , "        (exnBlock value))"
               , ";      other methods of class [[KeyedCollection]] 660c "
               , "    (method removeKey: (key)    "
               ,
        "        (self removeKey:ifAbsent: key {(self error: 'key-not-found)}))"
               , ";      other methods of class [[KeyedCollection]] 660d "
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
               , ";  collection classes 661a "
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
               ,
                ";      other methods of class [[SequenceableCollection]] 661b "
               , "    (method associationsDo: (bodyBlock) [locals i last]"
               , "        (set i    (self firstKey))"
               , "        (set last (self lastKey))"
               , "        ({(i <= last)} whileTrue:"
               ,
   "            {(bodyBlock value: (Association withKey:value: i (self at: i)))"
               , "             (set i (i + 1))}))"
               , ")"
               , ";  collection classes 674a "
               , ";  classes that define cons cells and sentinels 675b "
               , "(class Cons"
               , "    [subclass-of Object]"
               , "    [ivars car cdr]"
               , ";      methods of class [[Cons]] 676a "
               , "    (method car ()           car)"
               , "    (method cdr ()           cdr)"
               , "    (method car: (anObject)  (set car anObject) self)"
               , "    (method cdr: (anObject)  (set cdr anObject) self)"
               , "    (method pred: (aCons)    nil)"
               , ";      methods of class [[Cons]] 676b "
               , "    (method deleteAfter () [locals answer]"
               , "        (set answer (cdr car))"
               , "        (set cdr    (cdr cdr))"
               , "        (cdr pred: self)"
               , "        answer)"
               , "    (method insertAfter: (anObject)"
               , "        (set cdr (((Cons new) cdr: cdr) car: anObject))"
               , "        ((cdr cdr) pred: cdr)"
               , "        anObject)"
               , ";      methods of class [[Cons]] 676c "
               ,
             "    (method do: (aBlock)       ; defined on an ordinary cons cell"
               , "        (aBlock value: car)"
               , "        (cdr do: aBlock))"
               , ";      methods of class [[Cons]] 676e "
               ,
               "    (method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)"
               , "        ((aBlock value: self) ifTrue:ifFalse:"
               , "            {(pred deleteAfter)}"
               ,
       "            {(cdr rejectOne:ifAbsent:withPred: aBlock exnBlock self)}))"
               , ")"
               , ";  classes that define cons cells and sentinels 677 "
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
               , ";      iterating methods of class [[ListSentinel]] 676d "
               , "    (method do: (aBlock) nil)  ; defined on a sentinel"
               , ";      iterating methods of class [[ListSentinel]] 676f "
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
               , ";      other methods of class [[List]] 674b "
               ,
      "    (method addLast:  (item)   ((sentinel pred) insertAfter: item) self)"
               ,
      "    (method addFirst: (item)   (sentinel insertAfter: item)        self)"
               , "    (method add:      (item)   (self addLast: item))"
               , ";      other methods of class [[List]] 674c "
               , "    (method removeFirst ()     (sentinel deleteAfter))"
               , "    (method removeLast  ()     (self leftAsExercise))"
               , ";      other methods of class [[List]] 674d "
               , "    (method remove:ifAbsent: (oldObject exnBlock)"
               , "        ((sentinel cdr)"
               , "            rejectOne:ifAbsent:withPred:"
               , "            [block (x) (oldObject = (x car))]"
               , "            exnBlock"
               , "            sentinel))"
               , ";      other methods of class [[List]] 674e "
               ,
           "    (method removeKey:ifAbsent: (n exnBlock) (self leftAsExercise))"
               , ";      other methods of class [[List]] 674f "
               , "    (method firstKey () 0)"
               , "    (method lastKey  () ((self size) - 1))"
               , ";      other methods of class [[List]] 675a "
               , "    (method at:put: (n value) [locals tmp]"
               , "        (set tmp (sentinel cdr))"
               , "        ({(n isZero)} whileFalse:"
               , "           {(set n (n - 1))"
               , "            (set tmp (tmp cdr))})"
               , "        (tmp car: value)"
               , "        self)"
               , ")"
               , ";  collection classes S563 "
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
               , ";  collection classes S564a "
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
               , ";  collection classes S564b "
               , "(class Dictionary"
               , "    [subclass-of KeyedCollection]"
               , "    [ivars table] ; list of Associations"
               , "    (class-method new ()      ((super new) initDictionary))"
               ,
          "    (method initDictionary () (set table (List new)) self) ; private"
               , ";      other methods of class [[Dictionary]] S564c "
               , "    (method associationsDo: (aBlock) (table do: aBlock))"
               , "    (method at:put: (key value) [locals tempassoc]"
               , "        (set tempassoc (self associationAt:ifAbsent: key {}))"
               , "        ((tempassoc isNil) ifTrue:ifFalse:"
               ,
            "             {(table add: (Association withKey:value: key value))}"
               , "             {(tempassoc setValue: value)})"
               , "        self)"
               , ";      other methods of class [[Dictionary]] S565a "
               , "    (method removeKey:ifAbsent: (key exnBlock)"
               , "       [locals value-removed] ; value found if not absent"
               ,
"       (set value-removed (self at:ifAbsent: key {(return (exnBlock value))}))"
               ,
"       (set table (table reject: [block (assn) (key = (assn key))])) ; remove assoc"
               , "       value-removed)"
               , ";      other methods of class [[Dictionary]] S565b "
               , "    (method remove:ifAbsent: (value exnBlock)"
               ,
                "       (self error: 'Dictionary-uses-remove:key:-not-remove:))"
               , ";      other methods of class [[Dictionary]] S565c "
               , "    (method add: (anAssociation)"
               ,
               "      (self at:put: (anAssociation key) (anAssociation value)))"
               , ";      other methods of class [[Dictionary]] S565d "
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
               , ";  collection classes S565e "
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
               , ";      other methods of class [[Array]] 661c "
               , "    (method add:                (x)   (self fixedSizeError))"
               , "    (method remove:ifAbsent:    (x b) (self fixedSizeError))"
               , "    (method removeKey:ifAbsent: (x b) (self fixedSizeError))"
               ,
  "    (method fixedSizeError      ()    (self error: 'arrays-have-fixed-size))"
               , ";      other methods of class [[Array]] 662 "
               , "    (method firstKey () 0)"
               , "    (method lastKey  () ((self size) - 1))"
               , "    (method do: (aBlock) [locals index]"
               , "        (set index (self firstKey))"
               , "        ((self size) timesRepeat:"
               , "           {(aBlock value: (self at: index))"
               , "            (set index (index + 1))}))"
               , ";      other methods of class [[Array]] ((prototype)) S566a "
               , "    (method select:  (_) (self leftAsExercise))"
               , "    (method collect: (_) (self leftAsExercise))"
               , ")"
               , ";  predefined uSmalltalk classes and values S560a "
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
               , ";      tracing methods on class [[Block]] S560b "
               , "    (method messageTraceFor: (n) [locals answer]"
               , "        (set &trace n)"
               , "        (set answer (self value))"
               , "        (set &trace 0)"
               , "        answer)"
               , "    (method messageTrace () (self messageTraceFor: -1))"
               , ")"
               , ";  predefined uSmalltalk classes and values S561c "
               , "(class Symbol"
               , "    [subclass-of Object] ; internal representation"
               ,
            "    (class-method new  () (self error: 'can't-send-new-to-Symbol))"
               ,
          "    (class-method new: (aSymbol) (primitive newSymbol self aSymbol))"
               , "    (method       print  () (primitive printSymbol self))"
               , "    (method       hash   () (primitive hash self))"
               , ")"
               , ";  predefined uSmalltalk classes and values S570b "
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
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] S549e *)
fun addVal x e xi = processDef (VAL (x, e), xi, noninteractive)

local 
  fun newInstance classname = SEND (nullsrc, VAR classname, "new", [])
in
  val initialXi = addVal "true"  (newInstance "True" ) initialXi
  val initialXi = addVal "false" (newInstance "False") initialXi
end
(* type declarations for consistency checking *)
val _ = op addVal : name -> exp -> basis -> basis
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] S550a *)
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
(* implementations of \usmalltalk\ primitives and definition of [[initialBasis]] S550b *)
val initialXi = addVal "nil" (VALUE nilValue) initialXi
val initialBasis = initialXi
val primitiveBasis = primitives


(*****************************************************************)
(*                                                               *)
(*   FUNCTION [[RUNSTREAM]] FOR \USMALLTALK, WHICH PRINTS STACK TRACES *)
(*                                                               *)
(*****************************************************************)

(* function [[runStream]] for \usmalltalk, which prints stack traces S584d *)
fun runStream inputName input interactivity basis = 
  let val _ = setup_error_format interactivity
      val prompts = if prompts interactivity then stdPrompts else noPrompts
      val xdefs = filexdefs (inputName, input, prompts)
  in  readEvalPrintWith eprintlnTrace (xdefs, basis, interactivity)
  end 
(* type declarations for consistency checking *)
val _ = op runStream : string -> TextIO.instream -> interactivity -> basis ->
                                                                           basis
fun dump_global_names () = app (println o fst) initialBasis  (*OMIT*)


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
(*OMIT*)

(*****************************************************************)
(*                                                               *)
(*   TYPE ASSERTIONS FOR \USMALLTALK                             *)
(*                                                               *)
(*****************************************************************)

(* type assertions for \usmalltalk ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
(* type declarations for consistency checking *)
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
(* type assertions for \usmalltalk ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) *)
(* type declarations for consistency checking *)
val _ =   mkBlock : name list * exp list * value ref env * class * frame ->
                                                                           value
val _ =   saveBlockClass : value ref env -> unit
