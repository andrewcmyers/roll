structure Random :> sig
  val randRange : int * int -> int
end = struct
  structure I = IntInf

  val from = I.fromInt
  val to = I.toInt

  val seed = from 312
  val a    = from 3
  val c    = from 3
  val m    = from (valOf Int.maxInt)

  val last = ref seed

  fun randRange(x:int, y:int) = let
    val () = last := I.mod(I.+(I.*(!last,a),c),m)
  in
    (x + to( I.div(I.*(!last,from(y - x + 1)), m)))
  end

end
