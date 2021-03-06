structure Multiset :> MULTISET = struct

  structure List = List

  datatype 'a tree = EMPTY | NODE of {left: 'a tree, data: 'a, right: 'a tree}

  type 'a elements = ('a * int) tree

  type 'a multiset = {elems: 'a elements, comp:('a * 'a -> order)}

  (* Rep invariant: elements are stored in a binary search tree *)

  exception NotFound
  exception Empty

  val emptyTree = EMPTY

  fun empty c = {elems=emptyTree, comp=c}

(* TODO: balancing *)
  fun treeAdd(item, tree: 'a elements, comp): 'a elements =
    case tree of
         EMPTY => NODE{left=EMPTY, data=(item,1), right=EMPTY}
       | NODE{left,data=d as (data,n),right} => NODE(
           case comp(data,item) of
                EQUAL =>  {left=left, data=(item,1 + n), right=right}
              | LESS  =>  {left=left, data=d, right=treeAdd(item,right,comp)}
              | GREATER => {left=treeAdd(item,left,comp), data=d, right=right})

  fun add (item, {elems, comp}) = {elems=treeAdd(item,elems,comp),comp=comp}

  fun delete (item, {elems, comp}) = raise Fail "Unimplemented"

  fun fromList (elems, comp) = List.foldl add (empty comp) elems

  fun isEmpty {elems, comp} = case elems of EMPTY => true | _ => false

  fun tabulate (n,f,c) = fromList ((List.tabulate)(n,f),c)

  fun foldI f acc tree =
    case tree of
         EMPTY => acc
       | NODE{left, data, right} => foldI f (f (data, foldI f acc left)) right

  fun repeat n f a b =
    case n of 0 => b
       | _ => f(a,repeat (n-1) f a b)

  fun foldIWithRep f acc tree =
    case tree of EMPTY => acc
       | NODE{left,data=(data,n),right} =>
         foldIWithRep f
         (repeat n f data (foldIWithRep f acc left)) right

  fun fold (f: 'a * 'b -> 'b) (acc: 'b)  ({elems, comp}:'a multiset) =
    foldIWithRep f acc elems

  fun toList  {elems, comp} = foldIWithRep op:: [] elems

  fun treeFind(item,tree,comp) =
    case tree of
         EMPTY => NONE
       | NODE{left,data=(data,n),right} =>
       case comp(data,item) of
            EQUAL => SOME(n)
          | LESS => treeFind(item,right,comp)
          | GREATER => treeFind(item,left,comp)

  fun frequency ({elems, comp}, item) =
    case treeFind(item,elems,comp) of
      NONE => 0
    | SOME(n) => n

  fun member (multiset, item) = frequency(multiset,item) > 0

  fun numItems {elems, comp} = foldI (fn((_,n), a) => n+a) 0 elems

  fun treeAddN((item,k), tree, comp) =
    case tree of
         EMPTY => NODE{left=EMPTY,data=(item,k),right=EMPTY}
       | NODE{left,data=d as (data,n),right} => NODE(
           case comp(data,item) of
                EQUAL =>  {left=left,data=(item,k + n),right=right}
              | LESS  =>  {left=left,data=d,right=treeAddN((item,k),right,comp)}
              | GREATER => {left=treeAddN((item,k),left,comp),data=d,right=right})

  fun treeUnion (t1, t2, comp) =
    case (t1, t2) of
         ((EMPTY, t) | (t, EMPTY)) => t
       | (NODE{left=l1, data=(d1,n1), right=r1},
          NODE{left=l2, data=(d2,n2), right=r2}) =>
            case comp(d1,d2) of
              EQUAL => NODE{left=treeUnion(l1,l2,comp),
                            data=(d1,n1+n2),
                            right=treeUnion(r1,r2,comp)}
            | LESS => treeAddN((d2,n2),
                   treeUnion(NODE{left=l1,
                                  data=(d1,n1),
                                  right=treeUnion(r1,r2,comp)},l2,comp),comp)
            | GREATER => treeUnion(t2, t1, comp)


  fun union ({elems=e1, comp=c1}, {elems=e2, comp=c2}) =
    {elems=treeUnion(e1,e2,c1),comp=c1}

  fun filter f (multiset:'a multiset as {elems, comp}) =
    let val elems' = foldI
        (fn (x, result) =>
             if f (#1 x) then treeAddN(x, result, comp)
                       else result
           )
        emptyTree
        elems
    in
       {elems = elems', comp = comp}
    end

  fun getfirst (elems:('a * int) list, n): ('a * int) list =
    raise Fail "Unimplemented"

  fun nMin n {elems, comp} =
    #1 (foldIWithRep
            (fn (x:'a, (b:'a multiset, m:int)) =>
                if m < n then (add(x,b), m+1) else (b, m))
            (empty comp, 0)
            elems)

  fun nMax n {elems, comp} = let val skip:int = (numItems ({elems=elems, comp=comp})) - n in
        #1 (foldIWithRep
              (fn (x:'a, (b:'a multiset, m:int)) =>
                if m < skip then (b, m+1) else (add(x,b), skip))
              (empty comp, 0)
              elems)
  end

  fun repOK (multiset as {elems,comp}: 'a multiset) =
      multiset

  fun map (f:'a -> 'a) ({elems, comp}:'a multiset) =
    foldIWithRep (fn (x,b) => add(f(x),b)) (empty comp) elems

  fun map' (f:'a -> 'b) (comp) ({elems, ...}:'a multiset) =
    foldIWithRep (fn (x,b) => add(f(x),b)) (empty comp) elems

end
