structure Roll = struct

  structure A = AbSyn
  structure E = Evaluator
  structure S = Sample
  structure D = Distribution
  structure P = Parser
  structure U = Util

  datatype mode_type = EVAL | DIST | SAMPLE | PARSE

  type exp = AbSyn.exp
  type decl = AbSyn.decl

  val sampleSize = 100000

  fun interpreter() : unit = let
    val prev_input = ref NONE
    val defns : decl list ref = ref []
    fun trim(s: string) = (
        if (s = "") then s
        else if (String.sub(s, 0) = #" ") then
            trim(String.extract(s, 1, NONE))
        else if (String.sub(s, String.size(s) - 1) = #" ") then
            trim(String.substring(s, 0, String.size(s) - 1))
        else
            s
    )

    fun interpreterLoop(mode : mode_type) : unit = let
      (* Read input *)
      val _ = print "> "
      val input = case TextIO.inputLine TextIO.stdIn of SOME m => trim(m) | NONE => ""

      (* Prints out interpreter directives *)
      fun printDirectives() : unit =
        (
         print "Commands:\n";
         print "eval      Evaluate future expressions (default mode)\n";
         print "dist      Calculate probability distributions\n";
         print "sample    Sample probability distributions\n";
         print "parse     Parse and pretty-print expressions\n";
         print "help      Bring up this help screen\n";
         print "q, quit   Quit\n";
         print "\n";
         print "exp       Sample from, estimate, or calculate the distribution of exp,\n";
         print "          depending on the current mode. E.g., \"d6\"\n";
         print "id = exp  Define an identifier.\n"
        )

      (* Evaluates and prints result *)
      fun eval(e: AbSyn.exp) : unit = (
        print ((U.prettyPrintValue (E.eval e))^"\n")
	)

      (* Gets distribution and prints it *)
      fun dist(e : AbSyn.exp) : unit =
        print ((U.prettyPrintValueBag (D.eval e))^"\n")

      (* Samples distribution and prints it *)
      fun sample(e : AbSyn.exp) : unit =
        print ((U.prettyPrintValueBag
                    (S.eval (sampleSize, e)))^"\n")

      (* Parses and pretty-prints an expression *)
      fun parse(e: AbSyn.exp) : unit = A.ppExp e

    in
	if input = "q\n" orelse input = "quit\n" orelse input = "" then ()
	else
           let val input = (if input = "\n" then
                case !prev_input of
                  SOME i => i
                | NONE => "\n"
                else (prev_input := SOME input; input))
	       val new_mode =
		(case (input, mode) of
		    ("\n", _) => mode
		| ("eval\n", _) => (print "Evaluate mode\n"; EVAL)
		| ("dist\n", _) => (print "Distribution mode\n"; DIST)
		| ("sample\n", _) => (print ("Sample mode (" ^ (Int.toString sampleSize) ^ ")\n"); SAMPLE)
		| ("parse\n", _) => (print "Parse mode\n"; PARSE)
		| ("help\n", _) => (printDirectives(); mode)
                | _ => case P.parseString(input) of
                          AbSyn.Declaration (id, exp) => (print (id ^ " is defined.\n"); mode)
                        | AbSyn.Expression e => (
                            case mode of 
                              EVAL => (eval e; mode)
                            | DIST => (dist e; mode)
                            | SAMPLE => (sample e; mode)
                            | PARSE => (parse e; mode)
                        )
		) handle
		    (P.ParseError | E.RuntimeError | D.RuntimeError) =>
			(print "Error\n"; mode)
		  | Fail s => (print ("Exception Fail \""^s^"\"\n"); mode)
	    in
		interpreterLoop new_mode
	    end
    end
  in
    (print "Roll Interpreter. Type \"help\" for help.\n";
     interpreterLoop EVAL)
  end
end
