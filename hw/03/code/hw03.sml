(* ---------------------------------------------------------------------- *)
(* SECTION 2 *)

(* zip : string list * int list -> (string * int) list *)
(* REQUIRES: true *)
(* ENSURES: zip(l1, l2) pairs the nth elements from l1 and l2 *)
fun zip ([], _) = []
  | zip (_, []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)

(* Tests for zip *)
val [("aa", 1), ("bb", 2), ("cc", 3)] = zip(["aa", "bb", "cc"], [1,2,3])
val [("aa", 1), ("bb", 2)] = zip(["aa", "bb"], [1,2,3])
val [("aa", 1), ("bb", 2)] = zip(["aa", "bb", "cc"], [1,2])
val [] = zip(["aa", "bb", "cc"], [])
val [] = zip([], [1,2,3])
val [] = zip([], [])



(* unzip : (string * int) list -> string list * int list *)
(* REQUIRES: true *)
(* ENSURES: zip(unzip(l)) = l *)
fun unzip [] = ([], [])
  | unzip ((x,y)::tl) = 
    let 
      val (lst_x, lst_y) = unzip tl
    in
      (x::lst_x, y::lst_y)
    end
  
(* Tests for unzip *)
val (["aa", "bb", "cc"], [1,2,3]) = unzip([("aa", 1), ("bb", 2), ("cc", 3)])
val ([], []) = unzip([])

(* ---------------------------------------------------------------------- *)
(* SECTION 3 *)

(* runWith : int * int list -> int list  * int list *)
(* REQUIRES: true *)
(* ENSURES: runWith(x,L) = (L1,L2) where L =  and every element of L1 is equal to x and L2 does not begin with x *)
fun runWith (_: int, []: int list): int list * int list = ([], []) 
  | runWith (x, lst as (y::ys)) = 
    if x <> y then ([], lst)
    else
      let 
        val (lst1, lst2) = runWith (x, ys)
      in 
        (x::lst1, lst2)
      end

(* Tests for runWith *)
val ([1,1,1], [2,3]) = runWith(1, [1,1,1,2,3])
val ([], [1,1,1,2,3,3,3]) = runWith(3, [1,1,1,2,3])

fun runWith' (x: int, []: int list): int * int list = (0, []) 
  | runWith' (x, lst as (y::ys)) = 
    if x <> y then (0, lst)
    else
      let 
        val (num, lst2) = runWith' (x, ys)
      in 
        (num+1, lst2)
      end

(* Tests for runWith *)
val (3, [2,3]) = runWith'(1, [1,1,1,2,3])
val (0, [1,1,1,2,3,3,3]) = runWith'(3, [1,1,1,2,3,3,3])   


(* lookSay : int list -> (int * int) list *)
(* REQUIRES: true *)
(* ENSURES: lookSay(l) evalues to the look-and-say list of l *)
fun lookSay ([]): (int * int) list = []
  | lookSay (lst as (x::xs)) = 
    let 
      val (count, sub_lst) = runWith' (x, lst)
    in 
      (count, x)::lookSay sub_lst
    end

(* Tests for lookSay *)
val [(3,1), (1,2), (3,3)] = lookSay([1,1,1,2,3,3,3])
val [] = lookSay([])


(* flatten : (int * int) list -> int list *)
(* REQUIRES: true *)
(* ENSURES: flatten(l) evaluates to a "flattened" list of integers *)
fun flatten ([]: (int * int) list): int list = []
  | flatten ((x1, x2)::xs) = x1::x2::flatten xs
  
(* Tests for flatten *)
val [1,2] = flatten [(1,2)]
val [1,2,3,4,5,6] = flatten [(1,2),(3,4),(5,6)]


(* ---------------------------------------------------------------------- *)
(* SECTION 4 *)

(* prefixSum : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSum(l) evaluates to the prefix sum of l in O(n^2) time *)
fun prefixSum ([]: int list): int list = []
  | prefixSum (x::xs) = x::addToEach (prefixSum xs, x)

(* Tests for prefixSum *)
val [1,2,3] = prefixSum [1,1,1]


fun prefixSumHelp ([], num) = [num]
  | prefixSumHelp (x::xs: int list, num: int): int list =
    let
      val lst = prefixSumHelp (xs, x + num)
    in
      num::lst
    end

val [1,3,6] = prefixSumHelp ([2,3], 1)

fun prefixSumHelp' ([], num) = []
  | prefixSumHelp' (x::xs: int list, num: int): int list =
    let
      val lst = prefixSumHelp' (xs, num + x)
    in
      (num + x)::lst
    end

val [1,3,6] = prefixSumHelp' ([1,2,3], 0)

(* prefixSumFast : int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: prefixSumFast(l) evaluates to the prefix sum of l in O(n) time *)
fun prefixSumFast nil = []
  | prefixSumFast (x::xs: int list): int list = prefixSumHelp(xs, x)


(* Tests for prefixSumFast *)
val [1,2,3] = prefixSumFast [1,1,1]
val [1,3,6] = prefixSumFast [1,2,3]

(* ---------------------------------------------------------------------- *)
(* SECTION 5 *)

(* sublist : int * int * int list -> int list *)
(* REQUIRES: 0<=i+k<=length(l) *)
(* ENSURES: sublist(i,k,l) returns the sublist of l starting at i of length k *)
fun sublist (_, 0, _) = []
  | sublist (0, k, (x::xs)) = x::sublist(0, k-1, xs)
  | sublist (i: int, k: int, (_::xs): int list) : int list = sublist(i-1, k, xs)
  | sublist (_, _, []) = raise Fail "not possible"
(* Tests for sublist *)
val [1,2,3] = sublist(0, 3, [1,2,3,4,5])
val [2,3] = sublist(1, 2, [1,2,3,4,5])



(* ---------------------------------------------------------------------- *)
(* SECTION 6 *)

(* subsetSum : int list * int -> int list *)
(* REQUIRES: true *)
(* ENSURES: subsetSum(l, s) returns true if there is a subset of l whose sum is s and false otherwise *)
fun subsetSum (_, 0) = true
  | subsetSum ([], _) = false
  | subsetSum ((x::xs): int list, n: int): bool = subsetSum(xs, n-x) orelse subsetSum(xs, n)
(* Tests for subsetSum *)
val true = subsetSum([1,2,1,~6,10], 4)
val false = subsetSum([2,1,~6], 4)

fun subsetSumCert (_, 0) = (true, nil)
  | subsetSumCert ([], _) = (false, nil)
  | subsetSumCert ((x::xs): int list, n: int): bool * int list =
    let
      val (flag, lst) = subsetSumCert(xs, n-x)
      val (flag2, lst2) = subsetSumCert(xs, n)
    in
      case (flag, flag2) of 
         (true, _) => (true, x::lst)
      |  (false, true) => (true, lst2)
      |  (false, false) => (false, nil)
    end
(* Tests for subsetSumCert *)
val (true, _) = subsetSumCert([1,2,1,~6,10], 4)
val (false, _) = subsetSumCert([2,1,~6], 4)