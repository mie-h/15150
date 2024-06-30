(* ---------------------------------------------------------------------- *)
(* Section 3 - Bases *)
(* Task 3.1 *)
(* toInt : int -> int list -> int *)
(* REQUIRES: base > 1 *)
(* ENSURES: toInt(b)(L) evaluates to n where L is the representation of n in base b *)
fun toInt (b: int) (L:int list): int = 
    let 
        fun toInt' _ [] = 0
          | toInt' i (x::xs) = 
          let
            val num': int = Real.round (Math.pow (Real.fromInt b, Real.fromInt i))
          in
            x * num' + toInt' (i+1) xs
          end 
    in 
        toInt' 0 L
    end

val 1085 = toInt 16 [13, 3, 4]

(* Tests for toInt *)
(* Task 3.2 *)
(* toBase int -> int -> int list *)
(* REQUIRES: base > 1, n>=0 *)
(* ENSURES: toBase(b)(n) evaluates to the representation of n in base b *)
fun toBase _ 0 = []
  | toBase (b: int) (n: int): int list = 
    let
      val n' = n div b
      val r = n mod b
    in
      r::toBase b n'
    end

(* Tests for toBase *)
val toBase3 = toBase 3
val [2, 1] = toBase3 5

(* Task 3.3 *)
(* convert : int * int -> int list -> int list *)
(* REQUIRES: b1 > 1, b2 < 1 *)
(* ENSURES: toInt b2 (convert(b1, b2) L) = toInt b1 L *)
fun convert (b1: int, b2: int) (L: int list): int list = toBase b2 (toInt b1 L)

(* Tests for convert *)


(* ---------------------------------------------------------------------- *)
(* Section 4 - Polymorphism, HOFs, Options *)
(* Task 4.1 *)
(* dotProduct : real list * real list -> real *)
(* REQUIRES: length(a) = length(b) *)
(* ENSURES: dotProduct(a,b) evaluates to the dot product of a and b *)
fun dotProduct (a: real list, b: real list): real = 
  List.foldl (fn (i, acc) => i+acc) 0.0 (List.map (fn (a', b') => a' * b') (zip (a, b)))

val dotProduct' = ListPair.foldl (fn (a, b, c) => c + a * b) 0.0
(* Tests for dotProduct *)

(* Task 4.2 *)
(* magnitudeOfVector : real list -> real *)
(* REQUIRES: true *)
(* ENSURES: magnitudeOfVector(a) evalutes to the magnitude of the vector a *)
fun magnitudeOfVector (xs: real list) = Math.sqrt (List.foldl (fn (x, b) => b + x * x) 0.0 xs) 
  
(* Tests for magnitudeOfVector *)

(* Task 4.3 *)
(* angleBetweenVectors : real list * real list -> real *)
(* REQUIRES: a and b are non-empty *)
(* ENSURES: angleBetweenVectors(a,b) evaluates to the angle between the vectors a and b *)
fun angleBetweenVectors (a, b) = Math.acos (dotProduct (a, b) / (magnitudeOfVector a * magnitudeOfVector b))

(* Task 4.4 *)
(* extract : ('a -> bool) * 'a list -> ('a * 'a list) option *)
(* REQUIRES: true *)
(* ENSURES: extract(p, l) evaluates to SOME(p, l') if there is an x in l such that p(x)=true and l' is l without x and evaluates to NONE otherwise *)
fun extract (_, []) = NONE
  | extract (p: 'a -> bool, (x::xs): 'a list): ('a * 'a list) option = if p x then SOME (x, xs) else 
    case extract (p, xs)
    of NONE => NONE
     | SOME (x', lst) => SOME (x', x::lst)

(* ---------------------------------------------------------------------- *)
(* Section 5 - Blocks World *)
(* Task 5.1 *)
(* extractMany : (’a * ’a -> bool * ’a list * ’a list) -> (’a list) option *)
(* REQUIRES: true *)
(* ENSURES: extractMany(eq,toExtract,from) removes elements in toExtract from from if the elements in toExtract are in from and evaluates to NONE otherwise *)
fun extractMany (_, [], y) = SOME y
  | extractMany (p: 'a * 'a -> bool, x::xs: 'a list, from: 'a list): 'a list option = 
  let 
    val from' = extract (fn z => p (x, z), from)
  in
    case from'
    of NONE => NONE
    | SOME (_, lst) => extractMany (p, xs, lst)
  end
                    
(* Tests for extractMany *)

datatype color = Red | Yellow | Green

datatype Foo = FooA of (bool * color)
             | FooB of bool

fun bar (FooA (b, _)) = b
  | bar (FooB b) = not b

bar (FooA (false, Yellow))
bar (FooB true)

(* Task 5.2 *)
datatype block = A | B | C

datatype move = Pickup of (block * block option) 
              | Put of (block * block option)

datatype fact = Free of block
              | On of (block * block option)
              | Hand of block option

type state = fact list

(* Task 5.3 *)
val initial : state = [Hand NONE, On (A, NONE), On (B, NONE), On (C, NONE), Free A, Free B, Free C]

fun extractManyFacts (toConsume : fact list, s : state) : state option =
    extractMany (fn (x : fact, y : fact) => x = y, toConsume, s)

(* Task 5.4 *)
(* consumeAndAdd : (state * fact list * fact list) -> state option *)
(* REQUIRES: true *)
(* ENSURES: extractMany(s,before,after) removes elements in before from s and adds elements from after to s if the elements in before are in s and evaluates to NONE otherwise *)
fun consumeAndAdd (s: state, b: fact list, after: fact list) : state option = 
    case extractManyFacts(b, s)
    of NONE => NONE
     | SOME s' => SOME (s'@after)

(* Tests for consumeAndAdd *)

(* Task 5.5 *)
(* step : (move * state) -> state option *)
(* REQUIRES: true *)
(* ENSURES: step(m, s) applies the move m to the state s if the before facts hold and evaluates to NONE otherwise *)
fun step (m: move, s: state): state option = 
  let
    val (before', after) = 
      case m of
          Pickup (block, NONE) => ([Free block, On (block, NONE), Hand NONE], [Hand (SOME block)])
        | Pickup (block, SOME block2) => ([Free block, On (block, SOME block2), Hand NONE], [Free block2, Hand (SOME block)])
        | Put (block, NONE) => ([Hand (SOME block)], [Hand NONE, On (block, NONE), Free block])
        | Put (block, SOME block2) => ([Hand (SOME block), Free block2], [Free block, Hand NONE, On (block, SOME block2)])
  in
    consumeAndAdd(s, before', after)
  end


(* Tests for step *)


(* ---------------------------------------------------------------------- *)
(* shrubMap : (’a -> ’b) -> ’a shrub -> ’b shrub *)
(* REUQIRES: f is a total function *)
(* ENSUERS: shrubMap(f)(s) evaluates to a a shrub with f applies to evert leaf *)
(* Task 7.1 *)
fun shrubMap (f: ('a-> 'b), s: 'a shrub): 'b shrub = 
  case s of
      Leaf leaf => Leaf (f leaf)
    | Branch (left, right) => Branch (shrubMap(f, left), shrubMap(f, right))
  
fun shrubMap (f, Leaf leaf) = Leaf (f leaf)
  | shrubMap (f, Branch (left, right)) = Branch (shrubMap(f, left), shrubMap(f, right))
     

(* Tests for shrubMap *)

(* Task 7.4 *)
(* shrubCombine : (’a * ’a -> ’a) -> ’a -> ’a shrub -> ’a *)
(* REQUIRES:  f is a total associative function *)
(* ENSURES: shrubCombine(f)(i)(s) returns the result of recursively combining the shrub with f where i is the identity *)
fun shrubCombine (f, so_far, s) =
  case s of
      Leaf leaf => f (so_far, leaf)
    |  Branch (left, right) => f (shrubCombine(f, so_far, left), shrubCombine(f, so_far, right))
  
(* Tests for shrubCombine *)
