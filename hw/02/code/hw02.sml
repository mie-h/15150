(* READ THIS COMMENT!
 *
 * In this file there are various lines marked by a comment either like so:
 *
 *    raise Fail "unimplemented"  (* DELETE THIS LINE *)
 *
 * or like so:
 *
 *    _ = raise Fail "unimplemented"  (* DELETE THIS LINE *)
 *
 * You do not need to delete these lines immediately, but they should be gone by
 * the time you hand in your homework. They are placeholders for your
 * implementations of the functions specified in the homework.  Without them,
 * this file would not load.
 *
 * If you remove such a line without implementing the function it is associated
 * with, this file will not load. Only remove such lines when you are ready to
 * implement their associated function.
 *
 *   Also notice that for some functions we specified the left parts of
 *   (one or more) function clauses, but for other functions we did not.
 *   You will need to decide what the function clauses should be in those cases.
 *)

(* evenP : int -> bool
   REQUIRES: true
   ENSURES: evenP n returns true if n is even and false otherwise
*)
fun evenP (n : int) : bool = raise Fail "unimplemented"  (* DELETE THIS LINE *)

(* oddP : int -> bool
   REQUIRES: true
   ENSURES: oddP n returns true if n is odd and false otherwise
*)
fun oddP (n : int) : bool = raise Fail "unimplemented"  (* DELETE THIS LINE *)

(* add : int * int -> int
   REQUIRES:  x, y >= 0
   ENSURES:  add(x,y) ==> x+y
*)
fun add (x : int, y : int) : int = raise Fail "unimplemented"  (* DELETE THIS LINE *)


(* Task 3.1: Implement and document this function. *)
(* Note: You may want to pattern-match for different cases. *)
(* leq : int * int -> bool
   REQUIRES:  x, y >= 0
   ENSURES:  leq(x,y) return true if x <= y and false otherwise
*)
fun leq (0, y) : bool = true
  | leq (x, 0) : bool = false
  | leq (x, y) : bool = leq(x-1, y-1)

val true = leq (3, 4)


(* Task 3.3: Implement and document this function. *)
(* pow : int -> int
   REQUIRES:  x >= 0
   ENSURES:  pow x ==> x ^ n
*)
fun pow x 0 : int = 1
  | pow x n : int = x * (pow x (n-1))

val 2 = pow 2 1

(* halfSum : int -> half
   REQUIRES:  x, y >= 0
   ENSURES:  leq(x,y) return true if x <= y and false otherwise
*)
fun halfSum 0 : real = 0.0
  | halfSum n : real = 0.5 + 0.5 * halfSum(n-1)

val true = Real.== (halfSum 1, 0.5)

(* Example of how to test to a function that returns a real:
   because comparing equality of floating point numbers is fragile,
   SML doesn't let you pattern-match against them,
   so you need to use an explicit equality test.  *)
(*
val true = Real.==(halfSum 1, 1.0)
*)


(* Task 3.5: Implement and document this function. *)
(* altHalfSum : int -> half
   REQUIRES:  x, y >= 0
   ENSURES:  leq(x,y) return true if x <= y and false otherwise
*)
fun altHalfSum 0 : real = 0.0
  | altHalfSum n : real = 0.5 - 0.5 * altHalfSum(n-1)

val x = [altHalfSum 1, altHalfSum 2, altHalfSum 3]

(* Task 3.7: Implement this function. *)
(* div : int * int -> bool
   REQUIRES:  n,m >= 0
   ENSURES:  div(n,m) returns true if n is divisible by nubmers from 2 to m and false otherwise
*)
fun divisible (n, 2): bool = n mod 2 = 0  
  | divisible (n, m): bool = (n mod m = 0) andalso divisible (n, m-1)

val true = divisible (4, 3)


fun is_prime n : bool = 
   let fun is_prime' 1: bool = true 
         | is_prime' x: bool = (n mod x <> 0) andalso is_prime' (x-1)
   in 
      is_prime' (n-1)
   end

val true = is_prime 4