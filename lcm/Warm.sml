structure Warm :> sig
  val apple : unit -> real
end = struct

  fun rand() = Random.randRange(1,10000)

  val r = Real.fromInt

  fun gcd(a,b) = if b = 0 then a else gcd(b, a mod b)

  fun apple() = let
    fun try(l:int, c:int, t:int) =
      if l = 0 then Math.sqrt(6.0/(r(c)/r(t)))
      else try(l-1,c + (if gcd(rand(),rand()) = 1 then 1 else 0), t+1)
  in
    try(10000,0,0)
  end


end
