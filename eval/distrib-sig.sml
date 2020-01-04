signature DISTRIBUTION = sig
  (* Exception raised for an invalid expression *)
  exception RuntimeError of string

  (* Evaluates the given expression and returns a bag or int *)
  val eval : AbSyn.exp -> AbSyn.value AbSyn.bag
end
