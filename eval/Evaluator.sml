structure Evaluator :> EVALUATOR = struct
  open AbSyn

  structure MS = Multiset

  exception RuntimeError of string

  fun computeOper a oper b =
    case (a,oper,b) of
         (a, Equal, b) => a = b
       | (a, Greater, b) => a > b
       | (a, Less, b) => a < b
       | (a, GreaterEqual, b) => a >= b
       | (a, LessEqual, b) => a <= b
       | (a, NotEqual, b) => a <> b

  fun evalOper v1 oper v2 =
    case (v1, v2) of
        (Int_v a, Int_v b) => computeOper a oper b
    |   _ => raise RuntimeError "Can't compare against a bag"

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
                      |   _ => raise Fail "error in comparison!"
                in
                  comp(x',y')
                end
            |  x => x


  val emptyBag = Multiset.empty Int.compare

  val rnd : Random.rand option ref = ref NONE

  fun getRandom() : Random.rand = (
      case !rnd of
        NONE =>
            let val now = Time.now()
                val r = Random.rand (IntInf.toInt(Time.toSeconds(now)),
                                     IntInf.toInt(Time.toMicroseconds(now)))
            in
                rnd := SOME r;
                r
            end
      | SOME r => r
  )

  fun dieRoll(n) = Random.randRange(1,n) (getRandom())

  fun eval(e:AbSyn.exp):AbSyn.value = (
     case e of
         Count_e(e1) =>
         (case eval(e1) of
               Int_v(v) => Int_v(1)
             | Bag_v(v) => Int_v(Multiset.numItems v))
       | DieRoll_e(e1) =>
           ( case eval(e1) of
                 Int_v(v) => if v < 1 then Bag_v(emptyBag)
                                      else Int_v(dieRoll v)
               | Bag_v(v) => raise RuntimeError "Die number cannot be a bag")
       | Filter_e((oper,e1),e2) =>
           (case eval(e2) of
                 Int_v(x) =>
                   Bag_v(if evalOper (eval e1) oper (Int_v x) then
                             Multiset.add(x,emptyBag) else
                             emptyBag)
               | Bag_v(x) =>
                   let
                     val a = eval e1
                   in
                    (* TODO check this*)
                     Bag_v (Multiset.filter
                              (fn y => evalOper a oper (Int_v y)) x)
                   end)
       | If_e((e1,oper,e2),e3,e4) =>
           if evalOper (eval e1) oper (eval e2) then eval(e3) else eval(e4)
       | Int_e(x) => Int_v(x)
       | Bag_e(x) => Bag_v(x)
       | Least_e(e1,e2) =>
                (case (eval(e1),eval(e2)) of
                     (Int_v(x),Bag_v(y)) =>
                        Bag_v((Multiset.nMin x y))
                | _ => raise RuntimeError "invalid arguments to min: must be number, bag")
       | Greatest_e(e1,e2) =>
                (case (eval(e1),eval(e2)) of
                     (Int_v(x),Bag_v(y)) =>
                        Bag_v((Multiset.nMax x y))
                | _ => raise RuntimeError "invalid arguments to max: must be number, bag")
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
               | Bag_v(_) => raise RuntimeError "Repetition count in # cannot be a bag")
       | Plus_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a+b)
              | ((Bag_v a, Int_v b) | (Int_v b,Bag_v a)) =>
                  Bag_v(Multiset.fold (fn (x,mset)=>Multiset.add (x+b, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError "Cannot add two bags")
       | Times_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a*b)
              | ((Bag_v a, Int_v b) | (Int_v b, Bag_v a)) =>
                  Bag_v(Multiset.fold (fn (x,mset)=>Multiset.add (x*b, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError "Cannot multiply two bags")
       | Div_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a div b)
              | (Bag_v a, Int_v b) =>
                  Bag_v(Multiset.fold (fn (x,mset) => Multiset.add (x div b, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError "Cannot divide by a bag")
       | Mod_e(x,y) =>
           (case (eval x, eval y) of
                (Int_v a, Int_v b) => Int_v(a mod b)
              | (Bag_v a, Int_v b) =>
                  Bag_v(Multiset.fold (fn (x,mset) => Multiset.add (x mod b, mset))
                                      (Multiset.empty Int.compare) a)
              | _ => raise RuntimeError "Cannot divide by a bag")
       | Compare_e(x,oper,y) =>
           (case (eval x, eval y) of
              (Int_v a, Int_v b) =>
                if computeOper a oper b then Int_v(1) else Int_v(0)
            | (Int_v a, Bag_v b) =>
                Bag_v(Multiset.fold (fn (x, mset) => Multiset.add (
                        if computeOper a oper x then 1 else 0, mset))
                    (Multiset.empty Int.compare) b)
            | (Bag_v a, Int_v b) =>
                Bag_v(Multiset.fold (fn (x, mset) => Multiset.add (
                        if computeOper x oper b then 1 else 0, mset))
                    (Multiset.empty Int.compare) a)
            | _ => raise RuntimeError "Comparison not implemented for bags")
       | Negative_e e =>
	    (case eval e of
		Int_v a => Int_v(~a)
	    |   Bag_v a => Bag_v(Multiset.map (fn x => ~x) a))
       | Sum_e(e) =>
           (case eval(e) of
                 Int_v(v) => Int_v(v)
               | Bag_v(v) => Int_v(Multiset.fold op+ 0 v))
       | Union_e(e1,e2) =>
           Bag_v(Multiset.union(evalToBag(e1), evalToBag(e2)))
       | Var_e(id) => raise RuntimeError ("Undefined: " ^ id)
    )
    and evalToBag(e) = case eval e of
        Bag_v b => b
    | Int_v i => Multiset.fromList([i], Int.compare)


end

