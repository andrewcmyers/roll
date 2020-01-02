signature SAMPLE = sig
  (* Exception raised for an invalid expression *)
  exception RuntimeError

  (* Evaluates the given expression and returns a bag or int *)
  val eval : int * AbSyn.exp -> AbSyn.value AbSyn.bag
end
