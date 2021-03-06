structure Roll = struct

  structure E = Evaluator
  structure S = Sample
  structure D = Distribution
  structure P = Parser
  structure U = Util
  
  open AbSyn

  datatype mode_type = INITIAL | EVAL | DIST | SAMPLE | PARSE | QUIT

  val sampleSize = 100000

  fun helpMessage() : unit =
    (
        print "Commands:\n";
        print "eval         Evaluate future expressions (default mode)\n";
        print "dist         Calculate probability distributions\n";
        print "sample       Sample probability distributions\n";
        print "parse        Parse and pretty-print expressions\n";
        print "help         Bring up this help screen\n";
        print "load <file>  Load a file of commands\n";
        print "q, quit      Quit\n";
        print "\n";
        print "exp       Sample from, estimate, or calculate the distribution of exp,\n";
        print "          depending on the current mode. E.g., \"d6\"\n";
        print "id = exp  Define an identifier.\n";
        print "\nSee index.html for more detailed help.\n"
    )

  fun interpreter(command: string option) : unit = let

    val prev_input = ref NONE
    val defns : decl list ref = ref []
    fun is_ws(c) = c = #" " orelse c = #"\n" orelse c = #"\t"
    fun trim(s: string) = (
        if (s = "") then s
        else if is_ws(String.sub(s, 0)) then
            trim(String.extract(s, 1, NONE))
        else if is_ws(String.sub(s, String.size(s) - 1)) then
            trim(String.substring(s, 0, String.size(s) - 1))
        else
            s
    )

    (* Evaluates and prints result *)
    fun eval(e: exp) : unit =
      print ((U.prettyPrintValue (E.eval e))^"\n")

    (* Gets distribution and prints it *)
    fun dist(e : exp) : unit =
      print ((U.prettyPrintValueBag (D.eval e))^"\n")

    (* Samples distribution and prints it *)
    fun sample(e : exp) : unit =
      print ((U.prettyPrintValueBag
                  (S.eval (sampleSize, e)))^"\n")

    (* Parses and pretty-prints an expression *)
    fun parse(e: exp) : unit = ppExp e

    (* Substitute one definition id=e1 into e2. *)
    fun subst_exp(e1, id, e2):exp = (
     case e2 of
       Count_e(e) => Count_e(subst_exp(e1,id,e))
     | Sum_e(x) => Sum_e(subst_exp(e1,id,x))
     | DieRoll_e(e) => DieRoll_e(subst_exp(e1,id,e))
     | Filter_e((oper,x),y) => Filter_e((oper,subst_exp(e1,id,x)),subst_exp(e1,id,y))
     | If_e((x,oper,y),a,b) =>
         If_e((subst_exp(e1,id,x),oper,subst_exp(e1,id,y)),subst_exp(e1,id,a),subst_exp(e1,id,b))
     | Int_e(x) => e2
     | Bag_e(x) => e2
     | Least_e(x,y) => Least_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Greatest_e(x,y) => Greatest_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Let_e(id',x,y) => (
         if id' = id then Let_e(id',subst_exp(e1,id,x),y)
         else Let_e(id',subst_exp(e1,id,x),subst_exp(e1,id,y))
      )
     | NTimes_e(x,y) => NTimes_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Plus_e(x,y) => Plus_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Times_e(x,y) => Times_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Compare_e(x,oper,y) => Compare_e(subst_exp(e1,id,x), oper, subst_exp(e1,id,y))
     | Var_e(id2) => if id2 = id then e1 else Var_e(id2)
     | Negative_e(x) => Negative_e(subst_exp(e1,id,x))
     | Div_e(x,y) => Div_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Mod_e(x,y) => Mod_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
     | Union_e(x,y) => Union_e(subst_exp(e1,id,x),subst_exp(e1,id,y))
    )

    (* Substitute all definitions in defns appearing in e. *)
    fun subst_defns (defs:decl list) (e:exp) : exp = (
      case defs of
        [] => e
      | (id, e2) :: rest =>
          subst_defns rest (subst_exp (e2, id, e))
    )

    fun handleExpr(input, mode) = 
      (case P.parseString(input) of
        Declaration (x, e) => (
          defns := (x, e) :: !defns;
          print (x ^ " is defined.\n");
          if mode = INITIAL then EVAL else mode
        )
      | Expression e => 
        let val e2 = subst_defns (!defns) e in
          case mode of 
            EVAL => (eval e2; mode)
          | INITIAL => (eval e2; QUIT)
          | DIST => (dist e2; mode)
          | SAMPLE => (sample e2; mode)
          | PARSE => (parse e2; mode)
          | QUIT => mode
        end
      )
      handle E.RuntimeError msg => (print("Error: " ^ msg ^ "\n"); mode)
           | D.RuntimeError msg => (print("Error: " ^ msg ^ "\n"); mode)
           | P.ParseError => mode
           | Fail s => (print ("Exception Fail \""^s^"\"\n"); mode)

    fun loadFile(filename) : unit =
      let val inp = TextIO.openIn(filename)
        fun readDefns() = (
          case TextIO.inputLine(inp) of
            NONE => ()
          | SOME line => (
              handleExpr(line, EVAL);
              readDefns()
            )
        )
      in
        readDefns()
      end

    fun stepMode mode default = case mode of
      INITIAL => default
    | _ => mode

    fun doCommand(input, mode) = (
        prev_input := SOME input;
        case (String.tokens is_ws input, mode) of
          ([], _) => mode
          | (["eval"], _) => (print "Evaluate mode\n"; EVAL)
          | (["dist"], _) => (print "Distribution mode\n"; DIST)
          | (["sample"], _) => (print ("Sample mode (" ^ (Int.toString sampleSize) ^ ")\n"); SAMPLE)
          | (["parse"], _) => (print "Parse mode\n"; PARSE)
          | (["help"], _) => (helpMessage(); stepMode mode QUIT)
          | (["quit"], _) => QUIT
          | (["q"], _) => QUIT
          | (["load", filename], _) => (loadFile(filename); stepMode mode EVAL)
          | _ => handleExpr(input, mode)
    )

    val initial_mode = case command of
        NONE => EVAL
    |   SOME str => doCommand(str, INITIAL)

    fun interpreterLoop(mode : mode_type) : unit = let
      (* Read input *)
      val _ = print "> "
      val input = case TextIO.inputLine TextIO.stdIn of SOME m => trim(m) | NONE => "quit"

      val input = if input = "" then
                    case !prev_input of
                      SOME i => i
                    | NONE => "eval"
                  else input
      val new_mode = doCommand(input, mode)
      in
        if new_mode <> QUIT then
          interpreterLoop new_mode
        else
          ()
      end
  in
    if initial_mode <> QUIT
    then (
        print "Roll Interpreter. Type \"help\" for help.\n";
        interpreterLoop initial_mode
    ) else ()
  end
end
