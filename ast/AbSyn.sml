structure AbSyn = struct

  type id = string

  datatype oper = Equal | Less | Greater | GreaterEqual | LessEqual | NotEqual

  type 'a bag = 'a Multiset.multiset

  datatype value = Int_v of int | Bag_v of int bag


  datatype exp = Int_e of int
               | Bag_e of int bag (* [int, int, int] *)
               | Var_e of id
               | DieRoll_e of exp        (* d *)
               | Plus_e of exp*exp       (* + *)
               | Negative_e of exp       (* - *)
               | Times_e of exp*exp      (* * *)
               | Div_e of exp*exp        (* / *)
               | Union_e of exp*exp      (* ++ *)
               | NTimes_e of exp*exp     (* # *)
               | Sum_e of exp            (* sum exp *)
               | Count_e of exp          (* count exp *)
               | Least_e of exp*exp      (* least e1 elements of bag e2 *)
               | Greatest_e of exp*exp   (* greatest e1 elements of bag e2 *)
               | Let_e of id*exp*exp     (* let id=exp in exp end *)
               | If_e of intcomp*exp*exp (* if exp op exp then exp else exp *)
               | Filter_e of setcomp*exp (* filter op exp exp *)
  withtype setcomp = oper * exp
  and intcomp = exp * oper * exp

  type decl = id * exp

  datatype top = Expression of exp | Declaration of decl

  (* Pretty Printer *)
  fun ppExp (e: exp): unit =
    let
      fun p d s =
        if d=0 then print (s^"\n")
        else (print " "; p (d-1) s)

      fun bagToString l =
        let
          fun rest l =
            case l of
              [] => "}"
            | [i] => (Int.toString i)^"}"
            | i::l' => (Int.toString i)^", "^(rest l')
        in
          "{"^(rest l)
        end

      fun opToString oper =
        case oper of
          Equal => "="
        | Less => "<"
        | Greater => ">"
        | GreaterEqual => ">="
        | LessEqual => "<="
        | NotEqual => "<>"

      fun pp (d: int) (e: exp): unit =
        let
          val pd = p d
          val pd2 = p (d+2)
          val pp = pp (d+2)
        in
          case e of
            Int_e i => pd ("Int_e "^(Int.toString i))
          | Bag_e l => pd ("Bag_e "^(bagToString (Multiset.toList l)))
          | Var_e id => pd ("Var_e "^id)
          | DieRoll_e e' => (pd "DieRoll_e ("; pp e'; pd ")")
          | Plus_e (e1,e2) => (pd "Plus_e ("; pp e1; pp e2; pd ")")
          | Times_e (e1,e2) => (pd "Times_e ("; pp e1; pp e2; pd ")")
          | Div_e (e1,e2) => (pd "Div_e ("; pp e1; pp e2; pd ")")
	  | Negative_e e => (pd "Negative_e ("; pp e; pd ")")
          | Union_e (e1,e2) => (pd "Union_e ("; pp e1; pp e2; pd ")")
          | NTimes_e (e1,e2) => (pd "NTimes_e ("; pp e1; pp e2; pd ")")
          | Sum_e e' => (pd "Sum_e ("; pp e'; pd ")")
          | Count_e e' => (pd "Count_e ("; pp e'; pd ")")
          | Least_e (e1,e2) => (pd "Least_e ("; pp e1; pp e2; pd ")")
          | Greatest_e (e1,e2) => (pd "Greatest_e ("; pp e1; pp e2; pd ")")
          | Let_e (id,e1,e2) => (pd ("Let_e ("); pd2 id; pp e1; pp e2; pd ")")
          | If_e ((e1,oper,e2),e3,e4) =>
              (pd "If_e ("; pp e1; pd2 (opToString oper); pp e2;
                            pp e3; pp e4; pd ")")
          | Filter_e ((oper,e1),e2) =>
              (pd "Filter_e ("; pd2 (opToString oper); pp e1; pp e2; pd ")")
        end
    in
      pp 0 e
    end

  fun subst(v,id,e2):exp =
    case e2 of
         Count_e(e) => Count_e(subst(v,id,e))
       | DieRoll_e(e) => DieRoll_e(subst(v,id,e))
       | Filter_e((oper,x),y) => Filter_e((oper,subst(v,id,x)),subst(v,id,y))
       | If_e((x,oper,y),a,b) =>
           If_e((subst(v,id,x),oper,subst(v,id,y)),subst(v,id,a),subst(v,id,b))
       | Int_e(x) => e2
       | Bag_e(x) => e2
       | Least_e(x,y) => Least_e(subst(v,id,x),subst(v,id,y))
       | Greatest_e(x,y) => Greatest_e(subst(v,id,x),subst(v,id,y))
       | Let_e(id',x,y) =>
           if id' = id then Let_e(id',subst(v,id,x),y)
           else Let_e(id',subst(v,id,x),subst(v,id,y))
       | NTimes_e(x,y) => NTimes_e(subst(v,id,x),subst(v,id,y))
       | Plus_e(x,y) => Plus_e(subst(v,id,x),subst(v,id,y))
       | Times_e(x,y) => Times_e(subst(v,id,x),subst(v,id,y))
       | Div_e(x,y) => Div_e(subst(v,id,x),subst(v,id,y))
       | Negative_e(x) => Negative_e(subst(v,id,x))
       | Sum_e(x) => Sum_e(subst(v,id,x))
       | Union_e(x,y) => Union_e(subst(v,id,x),subst(v,id,y))
       | Var_e(id') => (if id' = id then
                         case v of Int_v(x) => Int_e(x) | Bag_v(x) => Bag_e(x)
                       else
                         e2)
end


