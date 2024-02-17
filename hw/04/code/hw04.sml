  (* Defining Trees *)
datatype tree =
    Node of tree * int * tree
  | Empty

datatype rtree =
    rNode of rtree * real * rtree
  | rEmpty

(* Real trees cannot be compared directly (because reals cannot be compared
 * directly).  Use this function to check if two rtrees are the same, using
 * a small positive epsilon. *)  
fun treeCompare(_ : real, rEmpty : rtree, rEmpty : rtree) = true
  | treeCompare(epsilon, rNode(a, x, b), rNode(c, y, d)) =
  		(y < (x + epsilon)) andalso (y > (x - epsilon)) andalso
  		treeCompare(epsilon, a, c) andalso treeCompare(epsilon, b, d)
  | treeCompare(_, _, _) = false


(* TASK 2 *)

(* fastfib : int -> int *)
fun fastfib () = raise Fail "fastfib not implemented"

(* TASK 3 *)

(* pow : real * int -> real *)
fun pow () = raise Fail "pow not implemented"

(* partial : int * real -> real *)
fun partial () = raise Fail "partial not implemented"

(* geometricTreeLevel : int * int * real -> real *)
fun geometricTreeHelp () = raise Fail "geometricTreeHelp not implemented"

(* geometricTree : int * real -> real *)
fun geometricTree () = raise Fail "geometricTree not implemented"

(* TASK 4 *)

(* part : int * int list -> int list * int list *)
fun part () = raise Fail "part not implemented"

(* quicksort : int list -> int sort *)
fun quicksort () = raise Fail "quicksort not implemented"

(* TASK 5 *)

(* traver : tree * int list -> int list *)
fun traver () = raise Fail "traver not implemented"

(* TASK 7 *)

(* treecompare : tree * tree -> order *)
fun treecompare () = raise Fail "treecompare not implemented"

(* makeSwapDown : int * tree -> bool * int * tree *)
fun makeSwapDown () = raise Fail "makeSwapDown not implemented"

(* swapDown : tree -> tree *)
fun swapDown () = raise Fail "swapDown not implemented"

(* heapify : tree -> tree *)
fun heapify () = raise Fail "heapify not implemented"