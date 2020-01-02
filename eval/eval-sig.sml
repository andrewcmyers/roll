signature EVALUATOR = sig
  (* Exception raised for an invalid expression *)
  exception RuntimeError

  (* Evaluates the given expression and returns a bag or int *)
  val eval : AbSyn.exp -> AbSyn.value
end
