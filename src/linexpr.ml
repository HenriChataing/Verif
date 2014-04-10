(** Linear expressions: expressions of the form
      c + a1 * x1 + a2 * x2 + .. + an * xn
 *)

type linexpr = {
  terms: (string * int) list;
  constant: int
}

(** Sum two linear expressions. *)
let add (e0: linexpr) (e1: linexpr): linexpr =
  let rec update v f = function
  | [] -> (false, [])
  | (v', c')::vs ->
      if v = v' then
        let c'' = f c' in
        (true, if c'' = 0 then vs else (v', c'')::vs)
      else
        let (b, vs) = update v f vs in
        (b, (v',c')::vs)
  in
  let ts0 = List.fold_left (fun ts (v, c) ->
    let b, ts = update v (fun c' -> c + c') ts in
    if b then ts else (v, c)::ts
  ) e0.terms e1.terms in
  { terms = ts0;
    constant = e0.constant + e1.constant }


(** Multiply a linear expression by a constant. *)
let mul (e: linexpr) (a: int): linexpr =
  { terms = List.map (fun (v,c) -> (v, a*c)) e.terms;
    constant = e.constant * a }


(** Substract two linear expressions. *)
let sub (e0: linexpr) (e1: linexpr): linexpr =
  add e0 (mul e1 (-1))


let minus (e: linexpr): linexpr =
  mul e (-1)


(** Normalize a comparison between linear expressions. *)
let comp (p: Positions.position) (op: string) (e0: linexpr) (e1: linexpr): (linexpr * string) =
  match op with
  | ">=" -> (sub e1 e0, "<=") | ">" -> (sub e1 e0, "<")
  | "==" | "!=" | "<=" | "<" -> (sub e0 e1, op)
  | _ -> Errors.fatal' p ("Illegal operator " ^ op)
 

(** Reverse a comparison. *)
let rev (e,op: linexpr * string): linexpr * string =
  match op with
  | "<" ->  (minus e, "<=") | "<=" -> (minus e, "<")
  | "==" -> (e, "!=") | "!=" -> (e, "==")
  | _ -> assert false (* Other operators eliminated by function 'comp' *)


(** Printing. *)
let string_of_linexpr (e: linexpr): string =
  let pc =
    if e.constant = 0 then "" else string_of_int e.constant
  in
  List.fold_left (fun s (x, c) ->
    if s = "" then
      match c with
      | 0 -> "" | 1 -> x | -1 -> "-" ^ x
      | _ -> string_of_int c ^ x
    else
      match c with
      | 0 -> s | 1 -> s ^ " + " ^ x | -1 -> s ^ " - " ^ x
      | c when c < 0 -> s ^ " - " ^ string_of_int (-c) ^ x
      | _ -> s ^ " + " ^ string_of_int c ^ x  
  ) pc e.terms

