(********** TASK 4.1 **********)
functor FunDict (K : ORDERED) : DICT =
struct
  
  structure Key = K
  
  datatype 'v func = Func of (Key.t -> 'v option)
  
  type 'v dict = 'v func
  
  val empty = Func (fn _ => NONE)
  
  (* insert : 'v dict -> (Key.t * 'v) -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: insert D (k,v) evaluates to D with the mapping (k,v) added *)
  fun insert (Func f) (k, v) = Func (fn k' => if K.compare (k, k') = EQUAL then SOME v else f k') 
                         
  (* lookup : 'v dict -> Key.t -> 'v option *)
  (* REQUIRES: true *)
  (* ENSURES: lookup D k evaluates to SOME v if k maps to v in D
   * and NONE otherwise *)               
  fun lookup (Func f) k = f k
  
  (* remove : 'v dict -> Key.t -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: remove D k evaluates to D wtih the mapping for k removed *)
  fun remove (Func f) k = Func (fn k' => if K.compare (k, k') = EQUAL then NONE else f k') 
  
  (* map : ('u -> 'v) -> 'u dict -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: map g D evaluates to a dicionary D' with values of type 'v such that
   * if a key is mapped to v in D then it is mapped to g v in D' *)
  fun map g (Func f) = 
    Func (fn k' => case f k' of
                     SOME v => SOME (g v)
                   | NONE => NONE)
      
  
  (* filter : ('v -> bool) -> 'v dict -> 'v dict *)
  (* REQUIRES: true *)
  (* ENSURES: filter p D devalutes to a dictionary D' such that if a key
   * is mapped to v in D and pv evalutes to true then it is mapped to v in D' *)
  fun filter p f = 
    Func (fn k' => case (lookup f k') of
                     NONE => NONE
                   | SOME v => if p v then SOME v else NONE)

                         
end


structure TestFunDict =
struct

  structure IntOrder : ORDERED =
  struct
    type t = int
    val compare : t * t -> order = Int.compare
  end

  structure StringOrder : ORDERED =
  struct
    type t = string
    val compare : t * t -> order = String.compare
  end
  
  structure IntDict = FunDict (IntOrder : ORDERED)    
  
  structure StrDict = FunDict (StringOrder : ORDERED)

  (* Tests on IntDict *)
  val sipairs = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5)]
  val sd5 = List.foldl (fn (p, d) => StrDict.insert d p) StrDict.empty sipairs

  val SOME 3 = StrDict.lookup sd5 "three"
  val SOME 5 = StrDict.lookup sd5 "five"

  val sd4 = StrDict.remove sd5 "four"

  val NONE = StrDict.lookup sd4 "four"
  val SOME 5 = StrDict.lookup sd4 "five"

  val ssd5 = StrDict.map Int.toString sd5

  val NONE = StrDict.lookup ssd5 "six"
  val SOME "5" = StrDict.lookup ssd5 "five"

  val sd3 = StrDict.filter (fn x => x > 2) sd5

  val NONE = StrDict.lookup sd3 "one"
  val SOME 3 = StrDict.lookup sd3 "three"
  val SOME 5 = StrDict.lookup sd3 "five"

  (* Tests on IntDict *)
  val ispairs = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
  val id5 = List.foldl (fn (p, d) => IntDict.insert d p) IntDict.empty ispairs

  val SOME "three" = IntDict.lookup id5 3
  val SOME "five"  = IntDict.lookup id5 5

  val id4 = IntDict.remove id5 4

  val NONE = IntDict.lookup id4 4
  val SOME "five" = IntDict.lookup id4 5

end
