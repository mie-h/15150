  (* Defining Trees *)
datatype tree =
    Node of tree * int * tree
  | Empty

datatype rtree =
    rNode of rtree * real * rtree
  | rEmpty

(* TASK 2 *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

fun fiblin n =
  let
    (* return n and n+1 
       fiblin' n -> (fib n, fib (n+1))
    *)
    fun fiblin' 0 = (0, 1)
      | fiblin' k = let
        val (fib_km1, fib_k) = fiblin' (k - 1)
      in
        (fib_k, fib_k + fib_km1)
      end
    val (_, fib_n) = fiblin' (n-1)
  in
    fib_n
  end

(* fastfib : int -> int *)
fun fastfib n =
    let
      fun fastfib' 0 = (0, 1)
        | fastfib' m = 
          let
            val k = m div 2
            val (fib_k, fib_k1) = fastfib' k
            val fib_2k = fib_k * (2 * fib_k1 - fib_k)
            val fib_2kp1 = fib_k1 * fib_k1 + fib_k * fib_k
          in
            case m mod 2 of
              0 => (fib_2k, fib_2kp1)
            | 1 => (fib_2kp1, fib_2k + fib_2kp1)
            | _ => raise Fail "Not Possible"
          end
          val (result, _) = fastfib' n
    in
      result
    end

(* TASK 3 *)
val 55 = fastfib 10
val 89 = fastfib 11

(* geometricTree : int * real -> rtree *)
fun geometricTree (n: int, r: real): rtree = 
  let 
    fun geometricTree' (i: int, agg: real): rtree = 
      if i = n then rEmpty
      else 

        let 
          val agg' = agg + Math.pow (r, real (i+1))
          val node = geometricTree' (i+1, agg')
        in
          rNode (node, agg, node)
        end
  in
    geometricTree' (0, 1.0)
  end


(* TASK 4 *)

(* part : int * int list -> int list * int list *)
fun part (_, []) = ([], [])
  | part (piv, x::xs) = 
    let
      val (left, right) = part(piv, xs)
    in
      if x < piv then (x::left, right)
      else (left, x::right)
    end
    


(* quicksort : int list -> int list *)
fun quicksort []  = []
  | quicksort (x::xs) = 
    let 
      val (left, right) = part (x, xs)
    in
      quicksort left @ x::quicksort right
    end

(* TASK 5 *)
val t: tree = Node (Node (Node (Empty, 5, Node (Empty, 6, Empty)), 2, Node (Empty, 4, Empty)), 1, Node (Node (Empty, 7, Empty), 3, Node(Empty, 8, Empty)))

(* traver : tree * int list -> int list *)
fun traver (Empty: tree, A: int list): int list = A
  | traver (Node(left, x, right): tree, A: int list): int list = traver (left, x::traver (right, A))

traver (Node (Node (Node (Empty, 5, Node (Empty, 6, Empty)), 2, Node (Empty, 4, Empty)), 1, Node (Node (Empty, 7, Empty), 3, Node(Empty, 8, Empty))), [9])
traver (Node (Empty, 1, Node (Node (Empty, 7, Empty), 3, Node(Empty, 8, Empty))), [9])

(* TASK 7 *)

(* treecompare : tree * tree -> order *)
fun treecompare (Empty, Empty) = EQUAL
  | treecompare (Empty, _) = GREATER
  | treecompare (_, Empty) = LESS
  | treecompare (Node(_, x, _): tree, Node(_, y, _): tree): order = 
    if x = y then EQUAL 
    else if x > y then GREATER 
    else LESS

val EQUAL = treecompare(Node(Empty, 4, Empty), Node(Empty, 4, Empty))
val LESS = treecompare(Node(Empty, 8, Empty), Node(Empty, 4, Empty))
val GREATER = treecompare(Node(Empty, 4, Empty), Node(Empty, 10, Empty))

(* swapDown : tree -> tree *)
fun swapDown Empty = Empty
  | swapDown (t as Node(Empty, _, Empty)) = t
  | swapDown (t as Node(Empty, x, Node(left, c, right))) =
    if x > c then Node(Empty, c, swapDown(Node(left, x, right))) else t
  | swapDown (t as Node(Node(left, c, right), x, Empty)) =
    if x > c then Node(swapDown(Node(left, x, right)), c, Empty) else t
  | swapDown (t as Node(l as Node(l_left, l_c, l_right), x, r as Node(r_left, r_c, r_right))) = 
    case treecompare(l, r) of
      GREATER => if x > r_c then
                   Node(l, l_c, swapDown(Node(r_left, x, r_right)))
                 else t
    | LESS => if x > l_c then 
                Node(swapDown(Node(l_left, x, l_right)), l_c, r)
              else t
    | EQUAL => if x > l_c then 
                Node(swapDown(Node(l_left, x, l_right)), l_c, r)
              else t

swapDown(Node(Node(Empty, 4, Empty), 5, Node(Empty, 4, Empty)))

(* heapify : tree -> tree *)
fun heapify Empty = Empty
  | heapify (Node(left, x, right)) =
  let
    val left' = heapify left
    val right' = heapify right
  in
    swapDown (Node(left', x, right'))
  end