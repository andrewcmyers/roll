structure Evaluator :> EVALUATOR = struct
  open AbSyn

  structure MS = Multiset

  exception RuntimeError

  fun evalOper(v1,oper) (v2) =
    case (v1,oper,v2) of
         (Int_v x,Equal,Int_v y) => x = y
       | (Int_v x,Greater,Int_v y) => x > y
       | (Int_v x,Less,Int_v y) => x < y
       | (Int_v x,GreaterEqual,Int_v y) => x >= y
       | (Int_v x,LessEqual,Int_v y) => x <= y
       | (Int_v x,NotEqual,Int_v y) => x <> y
       | _ => raise RuntimeError

  fun insert(x,[]) = [x]
    | insert(x,h::t) = if h < x then h::(insert(x,t)) else x::h::t

  val inSort = foldl insert []

  fun valueCompare(x:value, y:value):order =
    case (x,y) of
        (Int_v x, Int_v y) => Int.compare (x,y)
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


  val emptyBag = Multiset.empty Int.compare

  val now = Time.now()
  val rnd = Random.rand (IntInf.toInt(Time.toSeconds(now)), IntInf.toInt(Time.toMicroseconds(now)))

  fun eval(e:AbSyn.exp):AbSyn.value = (
    case e of
         Count_e(e1) =>
         (case eval(e1) of
               Int_v(v) => Int_v(1)
             | Bag_v(v) => Int_v(Multiset.numItems v))
       | DieRoll_e(e1) =>
           (case eval(e1) of
                 Int_v(v) => if v < 1 then raise RuntimeError else Int_v(Random.randRange(1,v) rnd)
               | Bag_v(v) => raise RuntimeError)
       | Filter_e((oper,e1),e2) =>
           (case eval(e2) of
                 Int_v(x) =>
                   Bag_v(if(evalOper(eval e1,oper)(Int_v x)) then
                             Multiset.add(x,emptyBag) else
                             emptyBag)
               | Bag_v(x) =>
                   let
                     val a = eval e1
                   in
                    (* TODO check this*)
                    Bag_v (Multiset.filter
                              (fn y =>(evalOper(a,oper)(Int_v y))) x)
                   end)
       | If_e((e1,oper,e2),e3,e4) =>
           if evalOper (eval e1,oper) (eval e2) then eval(e3) else eval(e4)
       | Int_e(x) => Int_v(x)
       | Bag_e(x) => Bag_v(x)
       | Least_e(e1,e2) =>
                (case (eval(e1),eval(e2)) of
                     (Int_v(x),Bag_v(y)) =>
                        Bag_v((Multiset.nMin x y))
                | _ => raise RuntimeError)
       | Greatest_e(e1,e2) =>
                (case (eval(e1),eval(e2)) of
                     (Int_v(x),Bag_v(y)) =>
                        Bag_v((Multiset.nMax x y))
                | _ => raise RuntimeError)
       | Let_e(id,e1,e2) => eval(subst(eval(e1),id,e2))
       | NTimes_e(e1,e2) =>
           (case eval(e1) of
                 Int_v(0) => Bag_v(emptyBag)
               | Int_v(n) =>
                    (case (eval e2, eval (NTimes_e(Int_e(n-1),e2))) of
                        (Bag_v x,Bag_v y) => Bag_v(Multiset.union(x,y))
                     | ((Int_v a, Bag_v x)|(Bag_v x,Int_v a)) => Bag_v(Multiset.add(a,x))
                     | (Int_v a,Int_v b) => Bag_v(Multiset.fromList (a::[b],
                     Int.compare )))
               | Bag_v(_) => raise RuntimeError)
       | Plus_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a+b)
              | ((Bag_v a, Int_v b) | (Int_v b,Bag_v a)) =>
                  Bag_v(Multiset.fold (fn (x,mset)=>Multiset.add (x+b, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError)
       | Times_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a*b)
              | ((Bag_v a, Int_v b) | (Int_v b, Bag_v a)) =>
                  Bag_v(Multiset.fold (fn (x,mset)=>Multiset.add (x*b, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError)
       | Div_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a div b)
              | ((Bag_v a, Int_v b) | (Int_v b, Bag_v a)) =>
                  Bag_v(Multiset.fold (fn (x,mset)=>Multiset.add (b div x, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError)
       | Negative_e e =>
	    (case eval e of
		Int_v a => Int_v(~a)
	    |   Bag_v a => Bag_v(Multiset.map (fn x => ~x) a))
       | Sum_e(e) =>
           (case eval(e) of
                 Int_v(v) => Int_v(v)
               | Bag_v(v) => Int_v(Multiset.fold op+ 0 v))
       | Union_e(e1,e2) =>
           (case (eval e1, eval e2) of
                 (Bag_v x,Bag_v y) => Bag_v(Multiset.union(x,y))
               | _ => raise RuntimeError)
       | Var_e(id) => raise RuntimeError
    )

end

