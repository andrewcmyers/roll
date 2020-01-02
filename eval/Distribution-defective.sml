structure Distribution :> DISTRIBUTION = struct
  open AbSyn
  open Rand

  structure E = Evaluator
  structure MS = Multiset

  exception RuntimeError;

  fun type_error(s:string) =
    (print ("TYPE ERROR: "^s^"\n");
     raise RuntimeError)


  fun subst(v,id,e2) =
    case e2 of
         Count_e(e) => Count_e(subst(v,id,e))
       | DieRoll_e(e) => DieRoll_e(subst(v,id,e))
       | Filter_e((oper,x),y) => Filter_e((oper,subst(v,id,x)),subst(v,id,y))
       | If_e((x,oper,y),a,b) =>
          If_e((subst(v,id,x),oper,subst(v,id,y)),subst(v,id,a),subst(v,id,b))
       | Int_e(x) => e2
       | Bag_e(x) => e2
       | Least_e(x,y) => Least_e(subst(v,id,x),subst(v,id,y))
       | Let_e(id',x,y) =>
           if id' = id then e2
           else Let_e(id',subst(v,id,x),subst(v,id,y))
       | NTimes_e(x,y) => NTimes_e(subst(v,id,x),subst(v,id,y))
       | Plus_e(x,y) => Plus_e(subst(v,id,x),subst(v,id,y))
       | Sum_e(x) => Sum_e(subst(v,id,x))
       | Union_e(x,y) => Union_e(subst(v,id,x),subst(v,id,y))
       | Var_e(id') => (if id' = id then
                         case v of Int_v(x) => Int_e(x) | Bag_v(x) => Bag_e(x)
                       else
                         e2)

  fun add1 a = a+1

  fun insert(x,[]) = [x]
    | insert(x,h::t) = if h < x then h::(insert(x,t)) else x::h::t

  val inSort = foldl insert []

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
  fun listify(l:AbSyn.value AbSyn.bag):AbSyn.exp AbSyn.bag =
      MS.map' (fn x =>
               case x of
                   Int_v(y) => Int_e(y)
                 | Bag_v(y) => Bag_e(y)) expCompare l

  val emptyMset:value bag = Multiset.empty valueCompare

  fun eval(e:AbSyn.exp):AbSyn.value AbSyn.bag =
    case e of
         Count_e(e1) =>
             MS.map' (E.eval o Count_e) valueCompare (listify(eval e1))
       | DieRoll_e(e1) =>
           MS.fold (fn (x,b) =>
                    case x of
                       (* TODO check this *)
                       Int_v(v) =>
                           MS.union(b,
                               (MS.tabulate (v,Int_v o add1, valueCompare)))
                     | Bag_v(v) => type_error "Can't roll a bag-sided die")
                     emptyMset (eval e1)
       | Filter_e((oper,e1),e2) =>
           let
             val e1' = listify (eval e1)
             val e2' = listify (eval e2)
           in
             MS.fold (fn (x,b) =>
                      MS.union(MS.map' (fn y => E.eval(Filter_e((oper,x),y)))
                                       valueCompare
                                       (e2'),b))
                   emptyMset e1'
           end
       | If_e((e1,oper,e2),e3,e4) =>
           let
             val e1' = listify (eval e1)
             val e2' = listify (eval e2)
             val TRUE_E = Int_e(1)            (* HACK *)
             val FALSE_E = Int_e(0)
             val TRUE_V = E.eval(TRUE_E)
             val FALSE_V = E.eval(FALSE_E)
             val e3' = eval e3      (*  No side effects *)
             val e4' = eval e4
           in
             MS.fold
               (fn (x,b) =>
                 MS.union(
                   MS.fold
                     (fn (y,b) =>
                       MS.union(if valueCompare(E.eval(If_e((x,oper,y),
                                                            TRUE_E,
                                                            FALSE_E)),
                                                TRUE_V) = EQUAL
                                then e3'
                                else e4', b))
                     emptyMset e2',
                   b))
               emptyMset e1'
           end
       | Int_e(x) => MS.add(Int_v(x), emptyMset)
       | Bag_e(x) => MS.add(Bag_v(x), emptyMset)
       | Least_e(e1,e2) =>
           let
             val e1' = listify (eval e1)
             val e2' = listify (eval e2)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Least_e(x,y)),b) )
                               emptyMset e2',b))
                      emptyMset e1'
           end
       | Let_e(id,e1,e2) =>
           let
             val e1' =  eval e1
           in
             MS.fold (fn (x,b) => MS.union(eval(subst(x,id,e2)),b))
                     emptyMset e1'
           end
       | NTimes_e(e1,e2) =>
           let
             val e1' = eval e1
             val e2' = eval e2

             fun nCartProd n (acc:AbSyn.value AbSyn.bag) (set:AbSyn.value AbSyn.bag) =
               case n of
                    0 => acc
                  | _ => nCartProd (n-1)
                         (MS.fold (fn (Bag_v(x:int MS.multiset),y:AbSyn.value AbSyn.bag) =>
                           MS.union(
                             MS.fold (fn (z:AbSyn.value,b:AbSyn.value AbSyn.bag) =>
                             case z of
                                  Int_v(a) => MS.add(Bag_v(MS.add(a,x)),b)
                                | Bag_v(a) => MS.add(Bag_v(MS.union(a,x)),b)
                                ) emptyMset set,y))
                         emptyMset acc)
                         set


           in
             MS.fold (fn (x,b) =>
              MS.union((case x of
                    Int_v(n) => nCartProd n
                                (MS.add(Bag_v(MS.empty Int.compare),emptyMset))
                                e2'
                  | Bag_v(_) => type_error "Can't have bag before #"),b))
               emptyMset e1'
           end
       | Plus_e(x,y) =>
           let
             val e1' = listify (eval x)
             val e2' = listify (eval y)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Plus_e(x,y)),b) )
                               emptyMset e2',b))
              emptyMset e1'
           end
       | Sum_e(e) => MS.map' (E.eval o Sum_e) valueCompare (listify(eval e))
       | Union_e(e1,e2) =>
           let
             val e1' = listify (eval e1)
             val e2' = listify (eval e2)
           in
             MS.fold (fn (x,b) =>
              MS.union(MS.fold (fn (y,b) => MS.add(E.eval(Union_e(x,y)),b))
                               emptyMset e2',b))
             emptyMset e1'
           end
       | Var_e(id) => type_error ("Unbound identifier: "^id)

end

