use "lib.sml";

(* ---------------------------------------------------------------------- *)
(* SECTION 2 *)

(* zip : string list * int list -> (string * int) list *)
(* REQUIRES: true *)
(* ENSURES: zip(l1, l2) pairs the nth elements from l1 and l2 *)
fun zip (l1: string list, l2: int list): (string * int) list = error "unimplemented"

(* Tests for zip *)

(* unzip : (string * int) list -> string list * int list *)
(* REQUIRES: true *)
(* ENSURES: zip(unzip(l)) = l *)
fun unzip () = error "unimplemented"

(* Tests for unzip *)

(* ---------------------------------------------------------------------- *)
(* SECTION 3 *)

(* runWith : int * int list -> int list  * int list *)
(* REQUIRES: true *)
(* ENSURES: runWith(x,L) = (L1,L2) where L =  and every element of L1 is equal to x and L2 does not begin with x *)
fun runWith () = error "unimplemented"
    
(* Tests for runWith *)

(* lookSay : int list -> (int * int) list *)
(* REQUIRES: true *)
(* ENSURES: lookSay(l) evalues to the look-and-say list of l *)
fun lookSay () = error "unimplemented"

(* Tests for lookSay *)

(* flatten : (int * int) list -> int list *)
(* REQUIRES: true *)
(* ENSURES: flatten(l) evaluates to a "flattened" list of integers *)
fun flatten () = error "unimplemented"
  
(* Tests for flatten *)


(* ---------------------------------------------------------------------- *)
(* SECTION 4 *)

(* prefixSum : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSum(l) evaluates to the prefix sum of l in O(n^2) time *)
fun prefixSum () = error "unimplemented"

(* Tests for prefixSum *)

(* prefixSumFast : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSumFast(l) evaluates to the prefix sum of l in O(n) time *)
fun prefixSumFast () = error "unimplemented"

(* Tests for prefixSumFast *)

(* ---------------------------------------------------------------------- *)
(* SECTION 5 *)

(* sublist : int * int * int list -> int list *)
(* REQUIRES: 0<=i+j<=length(l) *)
(* ENSURES: sublist(i,j,l) returns the sublist of l starting at i of length j *)
fun sublist () = error "unimplemented"
  
(* Tests for sublist *)

(* ---------------------------------------------------------------------- *)
(* SECTION 6 *)

(* subsetSum : int list * int -> int list *)
(* REQUIRES: true *)
(* ENSURES: subsetSum(l, s) returns true if there is a subset of l whose sum is s and false otherwise *)
fun subsetSum () = error "unimplemented"

(* Tests for subsetSum *)

fun subsetSumCert () = error "unimplemented"
    
(* Tests for subsetSumCert *)