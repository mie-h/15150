structure TreeFind : TREEFIND = 
struct
  datatype 'a ntree = Empty
                    | Node of 'a * 'a ntree list
  type 'a tree = 'a ntree

  exception NoSubtree
  
  (* fun : ('a -> bool) -> 'a tree -> 'a tree *)
  (* REQUIRES: true *)
  (* ENSURES: find p T evaluates to a subtree of T whose root satisfies p
   * and raises NoSubtree if no such subtree exists *)
  fun find p Empty = raise NoSubtree
    | find p (n as (Node (x, children))) = 
      if p x then n
      else let
              fun first [] = raise NoSubtree
                | first (x::xs) = find p x handle NoSubtree => first xs
            in
              first children
            end
end


structure TestTreeFind =
struct

(* Tests for find *)
val testTree1 = TreeFind.Node(1,[TreeFind.Node(2,[]),TreeFind.Node(3,[])])
val testTree2 = TreeFind.Node(1,[])

val (TreeFind.Node(1,[TreeFind.Node(2,[]),TreeFind.Node(3,[])])) =
    TreeFind.find (fn x => x = 1) testTree1 
val (TreeFind.Node(2,[])) = TreeFind.find (fn x => x = 2) testTree1 
val (TreeFind.Node(3,[])) = TreeFind.find (fn x => x = 3) testTree1
val (TreeFind.Node(1,[])) = TreeFind.find (fn x => x = 1) testTree2 

end
