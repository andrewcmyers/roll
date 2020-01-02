structure Distribution :> DISTRIBUTION = struct
  open AbSyn
  open Rand

  structure E = Evaluator
  structure MS = Multiset

  exception RuntimeError;

  fun evalOper(v1,oper) (v2) =
    case (v1,oper,v2) of
         (Int_v x,Equal,Int_v y) => x = y
       | (Int_v x,Greater,Int_v y) => x > y
       | (Int_v x,Less,Int_v y) => x < y
       | (Int_v x,GreaterEqual,Int_v y) => x >= y
       | (Int_v x,LessEqual,Int_v y) => x <= y
       | (Int_v x,NotEqual,Int_v y) => x <> y
       | _ => raise RuntimeError

  fun add1 a = a+1

  fun insert(x,[]) = [x]
    | insert(x,h::t) = if h < x then h::(insert(x,t)) else x::h::t

  val inSort = foldl insert []

  fun gcd(a,b) = if b = 0 then a else gcd(b,a mod b)

  fun valueCompare(x:value, y:value):order =
    case (x,y) of
        (Int_v x, Int_v y) => Int.compare (x,y)
      | (Int_v _, Bag_v _) => LESS
      | (Bag_v _, Int_v _) => GREATER
      (* bags of ints *)
      | (Bag_v x, Bag_v y) =>
          case Int.compare (Multiset.numItems x,Multiset.numItems y) of
              EQUAL =>
                let
                  val x' = inSort(MS.toList x)
                  val y' = inSort(MS.toList y)
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

  fun dup(s,b,n) =
    if n = 1
    then b
    else dup(s,MS.union(s,b),n-1)


  fun expCompare(x:exp, y:exp):order =
    case (x,y) of
        (Int_e x, Int_e y) => Int.compare (x,y)
      | (Int_e _, Bag_e _) => GREATER
      | (Bag_e _, Int_e _) => LESS
      (* bags of ints *)
      | (Bag_e x, Bag_e y) =>
          (case Int.compare (Multiset.numItems x,Multiset.numItems y) of
              EQUAL =>
                let
                  val x' = inSort(MS.toList x)
                  val y' = inSort(MS.toList y)
                  (* invariant: this code works only if x' and y' are
                     of the same length *)
                  fun comp(a:int list, b:int list):order =
                    case (a,b) of
                        ([],[]) => EQUAL
                      | (x::xs,y::ys) =>
                          (case Int.compare(x,y) of
                               EQUAL => comp(xs,ys)
                             | res => res)
                      |   _ => raise Fail "error in comparsion for exps!"
                in
                  comp(x',y')
                end
            |  x => x)
      | _ =>
          raise Fail "shouldn't ever see something other than ints or bags"

  (* converts values in a bag to expressions in a bag *)
  fun lift_expr(l:AbSyn.value AbSyn.bag):AbSyn.exp AbSyn.bag =
      MS.map' (fn x =>
               case x of
                   Int_v(y) => Int_e(y)
                 | Bag_v(y) => Bag_e(y)) expCompare l

  val emptyMset:value bag = Multiset.empty valueCompare

  fun adjUnion(runs:value bag list) = let
            val sizes = map MS.numItems runs
            val lcm = foldl (fn (a,b) => a*b div gcd(a,b)) 1 sizes
            val adjSizes = map (fn x => lcm div x) sizes
            val adjRuns = ListPair.map  (fn (a,b) => dup(a,a,b)) (runs,adjSizes)
            val ans = foldl MS.union emptyMset adjRuns
  in
    ans
  end

  fun eval(e:AbSyn.exp):AbSyn.value AbSyn.bag =
    case e of
         Count_e(e) =>
            let
              val v = eval(e)
            in
              MS.map (fn x =>
                case x of
                   Int_v(_) => Int_v 1
                 | Bag_v(b) => Int_v(MS.numItems b)) v
            end
       | DieRoll_e(e) =>
           let
             val v = eval(e)
             val runs =
              map
              (fn x =>
                case x of
                     Int_v(y) =>
                      if y < 1
                      then raise RuntimeError
                      else
                         foldl (fn (z,b) => MS.add(Int_v z,b))
                               emptyMset
                               (List.tabulate(y,fn x => x+1))
                   | Bag_v(_) => raise RuntimeError)
              (MS.toList(v))
           in
             adjUnion(runs)
           end
       | If_e((e1,oper,e2),e3,e4) =>
           let
             val v1 = eval e1
             val v2 = eval e2
             val v3 = eval e3
             val v4 = eval e4
             val s3 = MS.numItems(v3)
             val s4 = MS.numItems(v4)
             val lcm = (s3 * s4) div (gcd(s3,s4))
             val m3 = lcm div s3
             val m4 = lcm div s4
             val s3' = dup(v3,v3,m3);
             val s4' = dup(v4,v4,m4);
           in
             MS.fold
               (fn (x,b) =>
                 case x of
                   Bag_v(_) => raise RuntimeError
                 | Int_v(_) =>
                     MS.fold
                       (fn (y,b) =>
                         case x of
                           Bag_v(_) => raise RuntimeError
                         | Int_v(_) =>
                             if evalOper(x,oper) y
                             then MS.union(s3',b)
                             else MS.union(s4',b))
                       b
                       v2)
                emptyMset
                v1
           end
       | Filter_e((oper,e1),e2) =>
           let
             val e1' = lift_expr (eval e1)
             val e2' = lift_expr (eval e2)
             val runs = map (fn x =>
                      MS.map' (fn y => E.eval(Filter_e((oper,x),y)))
                                       valueCompare
                                       (e2'))
                   (MS.toList e1')
           in
             adjUnion(runs)
           end
       | Int_e(x) => MS.add(Int_v(x),emptyMset)
       | Bag_e(b) => MS.add(Bag_v(b),emptyMset)
       | Least_e(e1,e2) =>
           let
             val v1 = lift_expr (eval e1)
             val v2 = lift_expr (eval e2)
             val runs =
               map (fn x =>
                MS.fold (fn (y,b) => MS.add(E.eval(Least_e(x,y)),b) )
                               emptyMset v2)
                      (MS.toList v1)
           in
             adjUnion(runs)
           end
       | Greatest_e(e1,e2) =>
           let
             val v1 = lift_expr (eval e1)
             val v2 = lift_expr (eval e2)
             val runs =
               map (fn x =>
                MS.fold (fn (y,b) => MS.add(E.eval(Greatest_e(x,y)),b) )
                               emptyMset v2)
                      (MS.toList v1)
           in
             adjUnion(runs)
           end
       | Let_e(id,e1,e2) =>
           let
             val v1 =  eval e1
             val runs = map (fn x => eval(subst(x,id,e2))) (MS.toList v1)
           in
             adjUnion(runs)
           end
       | NTimes_e(e1,e2) =>
           let
             val v2 = eval e2
             val v1 = eval e1

             fun nCartProd n (acc:value bag) (set:value bag) =
               case n of
                    0 => acc
                  | _ => nCartProd (n-1)
                         (MS.fold (fn (bg:value,y:value bag) =>
                         case bg of
                              Int_v(_) => raise Fail "huh?"
                            | Bag_v(x) =>
                           MS.union(
                             MS.fold (fn (z:AbSyn.value,b:AbSyn.value AbSyn.bag) =>
                             case z of
                                  Int_v(a) => MS.add(Bag_v(MS.add(a,x)),b)
                                | Bag_v(a) => MS.add(Bag_v(MS.union(a,x)),b)
                                ) emptyMset set,y))
                         emptyMset acc)
                         set

            val runs:value bag list =
             map
               (fn (x:value) =>
                  case x of
                    Int_v(n) => nCartProd n
                                (MS.add(Bag_v(MS.empty Int.compare),emptyMset))
                                v2
                  | Bag_v(_) => raise RuntimeError)
               (MS.toList(v1))
           in
             adjUnion(runs)
           end
       | Plus_e(x,y) =>
           let
             val e1' = lift_expr (eval x)
             val e2' = lift_expr (eval y)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Plus_e(x,y)),b) )
                               emptyMset e2',b))
              emptyMset e1'
           end
       | Times_e(x,y) =>
           let
             val e1' = lift_expr (eval x)
             val e2' = lift_expr (eval y)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Times_e(x,y)),b) )
                               emptyMset e2',b))
              emptyMset e1'
           end
       | Div_e(x,y) =>
           let
             val e1' = lift_expr (eval x)
             val e2' = lift_expr (eval y)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Div_e(x,y)),b) )
                               emptyMset e2',b))
              emptyMset e1'
           end
       | Negative_e(e) =>
	    let val e' = lift_expr (eval e) in
	      MS.fold (fn (x,b) =>
		MS.add(E.eval(Negative_e x),b) )
		emptyMset e'
	    end
       | Sum_e(e1) => MS.map' (E.eval o Sum_e) valueCompare (lift_expr(eval e1))
       | Union_e(e1,e2) =>
           let
             val e1' = lift_expr (eval e1)
             val e2' = lift_expr (eval e2)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Union_e(x,y)),b))
                               emptyMset e2',b))
             emptyMset e1'
           end
       | Var_e(id') => raise RuntimeError

end

