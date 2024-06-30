
(* Remove this when you're done to make sure you didn't miss anything *)
exception Unimplemented

datatype regexp =
    Zero
  | One
  | Char of char
  | Plus of regexp * regexp
  | Times of regexp * regexp
  | Star of regexp
  | Whatever
  | Both of regexp * regexp

fun isEmpty [] = true
  | isEmpty _ = false

(* match : regexp -> char list -> (char list -> bool) -> bool *)
(* REQUIRES; p is a total function *)
(* ENSURES: match R L p evaluates to true if there exists L1, L2 such that L=L1@L2 and L1 is in R and p(L2)=true and false otherwise *)
fun match (R : regexp) (L : char list) (p : char list -> bool) : bool =
    case R of 
      Zero => false
    | One => p L
    | Char c => (case L of
                  [] => false
                | c'::cs' => c = c' andalso p cs')
    | Plus(r1, r2) => match r1 L p orelse match r2 L p
    | Times(r1, r2) => match r1 L (fn cs' => match r2 cs' p)
    | Star r => 
      let 
        fun matchstar cs = p cs orelse match r cs matchstar
      in
        matchstar L 
      end
    | Whatever => (case L of
                    [] => p L
                  | _::cs => p L orelse match Whatever cs p)
    (* ".*a", "bcba" vs ".*a "bcbb" *)
    | Both (r1, r2) => match r1 L (fn L' => match r2 L (fn L'' => L' = L'' andalso p L''))
      

fun accept R s = match R (String.explode s) (fn x => case x of [] => true | _ => false)

(* Task 2.1 Tests *)
val true = accept Whatever "abc"
val true = accept Whatever "Hello World"
val true = accept Whatever ""
val false = match Whatever (String.explode("abc")) (fn x => x = [#"d"])

(* Task 2.2 Tests *)
val R1 = Plus(Char #"a", Plus(Char #"b", Plus(Char #"c", Char #"d")))
val R2 = Plus(Char #"a", Plus(Char #"b", Plus(Char #"x", Char #"y")))
val true = accept (Both(R1,R2)) "a"
val true = accept (Both(R1,R2)) "b"
val false = accept (Both(R1,R2)) "c"
val false = accept (Both(R1,R2)) "y"
val false = accept (Both(R1,R2)) ""

(* Task 3.1 *)
(* halfmatch : regexp -> regexp -> char list -> bool *)
(* REQUIRES: true *)
(* ENSURES: halfmatch R1 R2 L evaluates to true if and only if there exists L1, L2 such that L=L1@L2, length(L1)=length(L2), and L1 is in R1 and L2 is in R2 *)
fun halfmatch (r1 : regexp) (r2 : regexp) (l : char list) : bool = 
  match r1 l (fn l2 => 
    match r2 l2 (fn lst => 
      case lst of 
        [] => (((length l) div 2) = length l2)
      | _ => false))

(* Tests for halfmatch *)
val R1 = Star(Plus(Char #"a", Plus(Char #"b", Char #"c")))
val R2 = Star(Plus(Char #"x", Plus(Char #"y", Char #"z")))
val true = halfmatch R1 R2 (String.explode("abcxyz"))
val true = halfmatch R1 R2 (String.explode("abxz"))
val false = halfmatch R1 R2 (String.explode("axy"))
val false = halfmatch R1 R2 (String.explode("abc"))
val false = halfmatch R1 R2 (String.explode("adxy"))
val true = halfmatch R1 R2 (String.explode(""))
