structure Util = struct
  structure A = AbSyn

  structure A = AbSyn

  fun listToString (f: 'a -> string) (l: 'a list): string =
    let
      fun rest l =
        case l of
          [] => "}"
        | [i] => (f i)^"}"
        | i::l' => (f i)^", "^(rest l')
    in
      "{"^(rest l)
    end

  fun itos x = if x >= 0 then Int.toString x else "-" ^ Int.toString(~x)

  fun prettyPrintValue(v : A.value) : string = 
    case v of
      A.Int_v x => itos x
    | A.Bag_v b => listToString itos (Multiset.toList b)

  fun insert(x,[]) = [x]
    | insert(x,h::t) = if h < x then h::(insert(x,t)) else x::h::t

  val inSort = foldl insert []

  fun valueCompare(x:A.value, y:A.value):order =
    case (x,y) of
        (A.Int_v x, A.Int_v y) => Int.compare (x,y)
      | (A.Int_v _, A.Bag_v _) => LESS
      | (A.Bag_v _, A.Int_v _) => GREATER
      (* bags of ints *)
      | (A.Bag_v x, A.Bag_v y) =>
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

  fun prettyPrintValueBag(v : A.value A.bag) : string =
  let
    val uniqueBag =
      Multiset.fold
      (fn (x,b) =>
      if not (Multiset.member(b,x))
      then Multiset.add(x,b)
      else b)
      (Multiset.empty valueCompare)
      v
  in
    let val (max_count, tot) = Multiset.fold (fn (x,(c, t)) =>
			let val f = Multiset.frequency(v, x) in
			    (if f > c then f else c, f+t)
			end) (0,0) uniqueBag
    in
	Multiset.fold
	(fn (x,b) =>
	    (let val n = Multiset.frequency(v,x) in
	    b ^ (prettyPrintValue x) ^ " : " ^ (Real.fmt (StringCvt.FIX (SOME 4)) ((Real.fromInt n)/(Real.fromInt tot))) ^ " " ^
		(if max_count < 80 then 
		    implode(List.tabulate (n,fn _ => #"#"))
		else
		    implode(List.tabulate (n * 80 div max_count,fn _ => #"$")))
	    ^"\n" end))
	""
	uniqueBag
    end
  end

end
