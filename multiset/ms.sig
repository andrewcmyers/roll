signature MULTISET = sig
  type 'a multiset
  exception NotFound
  exception Empty

  (* empty(ordf) = empty multiset w/ordering as defined by ordf. *)
  val empty : ('a * 'a -> order) -> 'a multiset

  (* tabulate(n,tabf,ordf) = {tabf(0),tabf(1)...tabf(n-1)}  *)
  val tabulate : int * (int -> 'a) * ('a * 'a -> order) -> 'a multiset

  (* add(el,multiset) = multiset with el added to it. *)
  val add : 'a * 'a multiset -> 'a multiset

  (* delete(el,multiset) = multiset with el removed from it.
   * Checks: el is in multiset. Raises NotFound otherwise. *)
  val delete : 'a * 'a multiset -> 'a multiset

  (* fromList(l,ordf) = multiset containing elements in l. *)
  val fromList : 'a list * ('a * 'a -> order) -> 'a multiset

  (* toList(multiset) = list containing elements in multiset.
   * The order of the elements in the list is unspecified.*)
  val toList : 'a multiset -> 'a list

  (* isEmpty(multiset) = "multiset is empty." *)
  val isEmpty : 'a multiset -> bool

  (* member(multiset,el) = "el is a member of multiset." *)
  val member : 'a multiset * 'a -> bool

  (* frequency(multiset,el) = # occurrences of el in multiset. *)
  val frequency : 'a multiset * 'a -> int

  (* numItems(multiset) = total # elements in multiset *)
  val numItems : 'a multiset -> int

  (* fold(f)(acc)(multiset) = folds over multiset using function f
   * and initial accumulator acc. The order of fold is
   * unspecified. *)
  val fold : ('a * 'b -> 'b) -> 'b -> 'a multiset -> 'b

  (* only from 'a -> 'a b/c don't want to have to deal w/ ordering
     function *)
  val map: ('a -> 'a) -> 'a multiset -> 'a multiset

  (* from 'a -> 'b, takes a comparison function as well  *)
  val map': ('a -> 'b) -> ('b * 'b -> order)-> 'a multiset -> 'b multiset

  (* union(multiset,multiset') = union of multiset and multiset'.
   * Requires: two compare functions are identical. *)
  val union : 'a multiset * 'a multiset -> 'a multiset

  (* filter(f)(multiset) = multiset containing all elements of multiset that
   * are accepted by f. *)
  val filter : ('a -> bool) -> 'a multiset -> 'a multiset

  (* nMin(n)(multiset)= multiset containing n smallest elements in multiset.*)
  val nMin : int -> 'a multiset -> 'a multiset

  (* nMax(n)(multiset)= multiset containing n largest elements in multiset.*)
  val nMax : int -> 'a multiset -> 'a multiset

  (* multiset maintains R.E. *)
  val repOK : 'a multiset -> 'a multiset
end
