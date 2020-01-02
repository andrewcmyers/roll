structure Sample :> SAMPLE = struct
  open AbSyn
  open Rand

  structure E = Evaluator

  exception RuntimeError;

  fun insert(x,[]) = [x]
    | insert(x,h::t) = if h < x then h::(insert(x,t)) else x::h::t

  val inSort = foldl insert []

  fun valueCompare(x:value, y:value):order = 
    case (x,y) of 
        (Int_v x, Int_v y) => Int.compare(x, y)
      | (Int_v _, Bag_v _) => GREATER 
      | (Bag_v _, Int_v _) => LESS
      (* bags of ints *)
      | (Bag_v x, Bag_v y) => 
          case Int.compare (Multiset.numItems x,Multiset.numItems y) of
              EQUAL => 
                let
                  val x' = inSort(Multiset.toList x)
                  val y' = inSort(Multiset.toList y)
                  (* invariant: this code works only if x' and y' are
                     of the same length *)
                  fun comp(a:int list, b:int list):order =
                    case (a,b) of 
                        ([],[]) => EQUAL
                      | (x::xs,y::ys) => 
                          (case Int.compare(x,y) of
                               EQUAL => comp(xs,ys)
                             | res => res)
                      |   _ => raise Fail "error in comparsion!"
                in
                  comp(x',y')
                end
            |  x => x

  fun eval(n:int, e:exp) =
    Multiset.tabulate(n, fn x=> E.eval e, valueCompare)



end

