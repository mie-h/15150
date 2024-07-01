(* Task 4.1 *)
functor Stack(T : TYPEDEF) : STACK =
struct
	exception StackUnderflow
	
	type stacktype = T.t
	type stack = T.t list
	
	(* empty : unit -> stack *)
	(* REQUIRES: true *)
	(* ENSURES: empty evaluates to an empty stack *)
	fun empty () = []
	
	(* push : (stack * stacktype) -> stack *)
	(* REQUIRES: true *)
	(* ENSURES: push(S,x) evaluates to S with x pushed onto the top *)
	fun push (S, x) = x::S
	
	(*  *)
	(* REQUIRES: true *)
	(* ENSURES: pop S evalutes to S with the top element removed
	 * and raises StackUnderFlow if S is empty *)
	fun pop [] = raise StackUnderflow
	  | pop (x::xs) = (x, xs) 
	
	(* append : (stack * stack) -> stack *)
	(* REQUIRES: true *)
	(* ENSURES: append(S1,S2) evaluates to S1 put on top of S2 *)
	fun append (S1, S2) = S1 @ S2


	(* find : (stack * (stacktype -> bool)) -> stack option *)
	(* REQUIRES: true *)
	(* ENSURES: find(S,p) evaluates to the largest substack of S
	 * whose top element satisfies p *)
	fun find (S, p) = 
		case S of
			[] => NONE
		|   (l as x::xs) => if p x then SOME l
					      else find (xs, p) 
	        
	(* flip : stack -> stack *)
	(* REQUIRES: true *)
	(* ENSURES: flip S flips the stack S *)
	fun flip s = 
		let
			fun flip' reversed lst = case lst of
									   [] => reversed
									 | x::xs => flip' (x::reversed) xs
		in
			flip' [] s
		end

end


structure TestStack =
struct

  structure IntType : TYPEDEF = 
  struct
    type t = int
  end
    
  structure IntStack : STACK = Stack(IntType)
  
  (* Tests for empty *)
  val [] = IntStack.empty()
  
  (* Tests for push *)
  val [3,1,2] = IntStack.push([1,2], 3)
  val [2,2,2] = IntStack.push([2,2], 2)
  val [4] = IntStack.push([], 4)
  
  (* Tests for pop *)
  val (1, [2,3]) = IntStack.pop([1,2,3])
  val (2, [2,2]) = IntStack.pop([2,2,2])
  val (4, []) = IntStack.pop([4])
  
  (* Tests for flip *)
  val [3,2,1] = IntStack.flip([1,2,3])
  val [2,2,2] = IntStack.flip([2,2,2])
  val [4] = IntStack.flip([4])
  
  (* Tests for append *)
  val [1,2,3,4,5,6] = IntStack.append([1,2,3], [4,5,6])
  val [1,2,3,2,3,4] = IntStack.append([1,2,3], [2,3,4])
  val [1,2,3] = IntStack.append([1,2,3], [])
  val [4,5,6] = IntStack.append([], [4,5,6])
  val [] = IntStack.append([], [])
  
  (* Tests for find *)
  val SOME [1,2,3] = IntStack.find([1,2,3], fn x => x = 1)
  val SOME [2,3] = IntStack.find([1,2,3], fn x => x = 2)
  val SOME [2,2,2] = IntStack.find([2,2,2], fn x => x = 2)
  val NONE = IntStack.find([1,2,3], fn x => x = 4)
  val NONE = IntStack.find([], fn x => x = 1)
  
end
