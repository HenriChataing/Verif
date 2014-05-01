(** Extract the Horn clauses from the SmtLib file. *)

open SmtSyntax
open Expressions
open Types

(* Generation of clause identifiers. *)
let id_count = ref 0

let new_id () =
  incr id_count;
  !id_count - 1

(* List of smtlib operators, each given an arity and the used representation. *)
let operators = [
  "=", (2, "=="); "<>", (2, "<>");
  "<", (2, "<"); ">", (2, ">");
  "<=", (2, "<="); ">=", (2, ">=");
  "and", (2, "&&"); "or", (2, "||");
  "not", (1, "not");
  "+", (2, "+"); "-", (2, "-"); "*", (2, "*")
]


(** Partial definition of a horn clause. *)
type hclause = {
  id: int;                (* Unique identifier. *)
  variables: var list;    (* List of universaly quantified variables. *)
  preconds: Expr.t list;
  postcond: string * var list
}


(** The context of quantified variables. *)
type context = var list

(** Retrieve a variable from the context. *)
let from_context
    ?(pos: Positions.position = Positions.undefined_position)
    (ctx: context) (x: string): var =
  try
    List.find (fun v -> v.name = x) ctx
  with Not_found -> Errors.fatal' pos ("Undefined variable " ^ x)


(** Convert a sort to a primitive type. *)
let ptype_of_sort (s: sort): ptype =
  match s with
  | Sort (Simple (Symbol "Int"), []) -> TypeInt
  | Sort (Simple (Symbol "Bool"), []) -> TypeBool
  | _ -> Errors.fatal [] "This sort is not a primitive type"


(** Remove the universal quantifications and return the corresponding variables. *)
let rec strip_foralls (t: term): var list * term =
  match t with
  | Forall (_, xs, t) ->
      let xs = List.map (fun (Symbol s, t) -> { name = s; ptype = ptype_of_sort t }) xs in
      let xs',t = strip_foralls t in
      xs @ xs', t
  | _ -> [], t


(** Check whether the term is a conjunction. *)
let rec as_conjunction (t: term): term list =
  match t with
  | App (_, NonQualified (Simple (Symbol "and")), ts) ->
      List.concat (List.map as_conjunction ts)
  | _ -> [t]


(** Match the application aof a clause to its variables. *)
let as_clause (ctx: context) (t: term): string * var list =
  match t with
  | App (pos, NonQualified (Simple (Symbol c)), ts) ->
      let vs = List.map (fun t ->
        match t with
        | Ident (pos, NonQualified (Simple (Symbol s))) -> from_context ~pos:pos ctx s
        | _ -> Errors.fatal' pos "Term is not a Horn clause"
      ) ts in
      c, vs
  | _ -> Errors.fatal' (position_of_term t) "Term is not a Horn clause"


(** Convert a term to an expression. *)
let rec expr_of_term (ctx: context) (t: term): Expr.t =
  match t with
  | Prim (pos, Num n) -> Expr.Prim (pos, Literal.Int n)
  | Ident (pos, NonQualified (Simple (Symbol "true"))) ->
      Expr.Prim (pos, Literal.Bool true)
  | Ident (pos, NonQualified (Simple (Symbol "false"))) ->
      Expr.Prim (pos, Literal.Bool false)

  | Ident (pos, NonQualified (Simple (Symbol s))) ->
      Expr.Var (pos, from_context ~pos:pos ctx s)
  | App (pos, NonQualified (Simple (Symbol op)), ts) ->
      let es = List.map (expr_of_term ctx) ts in
      begin try
        let (arity, repl) = List.assoc op operators in
        match op, arity, es with
        | "-", _, [e] -> Expr.Unary (pos, "-", e)
        | _, 1, [e] -> Expr.Unary (pos, repl, e)
        | _, 2, [e0;e1] -> Expr.Binary (pos, repl, e0, e1)
        | _ -> Errors.fatal [] "Malformed term"
      with Not_found ->
        Expr.Clause (pos, op, es)
      end
  | _ -> Errors.fatal [] "Not implemented"


(** Representation of the contents of a program writen using Horn clauses. *)
type program = {
  clauses_def: (string * (int * ptype list)) list;
  clauses: hclause list;
  assertions: term list
}

(** Return the id of a clause. *)
let get_clause_id (p: program) (c: string) =
  fst (List.assoc c p.clauses_def)
    

(** Extract the horn clauses of a smtlib program. *)
let extract_hclauses (p: command list): program =
  List.fold_left (fun p c ->
    match c with
    | DeclareFun (_, Symbol n, ss,_) ->
        let id = new_id () in
        { p with
          clauses_def = (n, (id, List.map ptype_of_sort ss))::p.clauses_def }
    | Assert (pos, t) ->
      let xs, t = strip_foralls t in
      begin match t with
      | App (pos, NonQualified (Simple (Symbol "=>")),[pre; post]) ->
          let es = List.map (expr_of_term xs) (as_conjunction pre)
          and c, vs = as_clause xs post in
          let cid =
            try
              fst (List.assoc c p.clauses_def)
            with Not_found -> Errors.fatal' pos "Undefined clause name"
          in
          { p with clauses =
             { id = cid;
               variables = xs;
               preconds = es;
               postcond = c, vs }::p.clauses }
      | _ -> { p with assertions = t::p.assertions }
      end
    | _ -> p
  ) { clauses_def = []; clauses = []; assertions = [] } p


(** Print a horn clause. *)
let string_of_hclause (c: hclause): string =
  let pre = match c.preconds with
    | [] -> ""
    | e::es -> Expr.to_string e ^ List.fold_left (fun s e -> s ^ ", " ^ Expr.to_string e) "" es
  in
  let vars = match snd c.postcond with
    | [] -> ""
    | v::vs -> v.name ^ List.fold_left (fun s v -> s ^ ", " ^ v.name) "" vs
  in
  fst c.postcond ^ "(" ^ vars ^ ") :- " ^ pre


(** Cleanup a clause by removing duplicate variables. *)
let simplify_hclause (c: hclause): hclause =
  (* The list of variables that can be deleted independently from
     the other clauses. *)
  let removable = List.filter (fun v ->
    not (List.mem v (snd c.postcond))) c.variables in
  (* Equivalence classes of variables. *)
  let classes = ref [] in
  let get_class x =
    let cx, cs = List.partition (fun c -> List.mem x c) !classes in
    classes := cs;
    match cx with
    | [] -> [x]
    | cx::_ -> cx
  in
  let merge_classes x y =
    if x = y then ()
    else begin
      let cx = get_class x
      and cy = get_class y in
      classes := (cx @ cy)::!classes
    end
  in
  (* Filter the precondtions for equalities between variables, and apply these
     equalities to the equivalence classes. *)
  let pre = List.fold_left (fun pre e ->
    match e with
    | Expr.Binary (_, "==", Expr.Var (_, v0), Expr.Var (_, v1)) ->
        merge_classes v0 v1;
        pre
    | _ -> e::pre
  ) [] c.preconds in
  (* Chose a representant by class, and build the corresponding substitution. *)
  let subs = List.concat (List.map (fun c ->
    (* If possible, the representant should be a variable that cannot be deleted. *)
    let repr = try List.find (fun x -> not (List.mem x removable)) c
      with Not_found -> List.hd c
    in
    List.map (fun x -> (x, repr)) c
  ) !classes) in
  (* Apply the substitution to the remaining preconditions. *)
  let pre = List.map (Expr.subs subs) pre in
  (* Apply to the post condition. *)
  let post = List.map (fun v ->
    try List.assoc v subs with Not_found -> v) (snd c.postcond) in
  (* Remove the now useless variables (the variables that are replaced and that can be removed). *)
  let xsubs = List.map fst subs in
  let removable = List.filter (fun v -> not (List.mem v xsubs)) removable in
  (* Build the resulting clause. *)
  { id = c.id;
    variables = snd c.postcond @ removable;
    preconds = pre;
    postcond = (fst c.postcond),post }


module StringMap = Map.Make (String)


(** Build the dependency graph of the clauses. *)
let build_dependencies (p: program): (string list) StringMap.t =
  let rec depends (e: Expr.t): string list =
    match e with
    | Expr.Clause (_, c, _) -> [c]
    | Expr.Binary (_, _, e0, e1) -> depends e0 @ depends e1
    | Expr.Unary (_, _, e) -> depends e
    | _ -> []
  in
  
  List.fold_left (fun g c ->
    let dep = List.concat (List.map depends c.preconds) in
    try
      let dep' = StringMap.find (fst c.postcond) g in
      StringMap.add (fst c.postcond) (dep @ dep') g
    with Not_found ->
      StringMap.add (fst c.postcond) dep g
  ) StringMap.empty p.clauses


(** Map used to store the dependencies between variables. *)
module ArgMap = Map.Make(
  struct
    type t = string * int
    let compare (s0, i0) (s1, i1) =
      let c = compare s0 s1 in
      if c == 0 then i0 - i1
      else c
  end)

(** Use of an argument. *)
type arguse =
    Never
  | Always
  | Depends of (int * int) list

let join (u: arguse) (u': arguse): arguse =
  match u,u' with
  | Always, _ -> Always | _, Always -> Always
  | Never, _ -> u' | _, Never -> u
  | Depends u, Depends u' -> Depends (u @ u')


(** On a global scale, purge the useless clause arguments: for each
    clause declaration, looks up the unused arguments and remove them. *)
let simplify_hclauses (p: program): program =
  (* All arguments. *)
  let arg_uses = Array.make !id_count (Array.make 0 Never) in
  List.iter (fun (_, (id, arity)) ->
    arg_uses.(id) <- Array.make (List.length arity) Never
  ) p.clauses_def;

  (* Update an arugment. *)
  let update (c, i: int * int) (u: arguse): unit =
    arg_uses.(c).(i) <- join u arg_uses.(c).(i)
  in

  (* Extract the uses of variables in an expression. *)
  let rec extract_uses
      (c: int)  (* The name of the clause. *)
      (args: (var * int) list)
      (cs: (int * int) list)  (e: Expr.t): unit =
    match e with
    | Expr.Var (_,v) ->
        let u = if cs = [] then Always else Depends cs in
        List.iter (fun (v', i) ->
          if v = v' then
            update (c, i) u
        ) args
    | Expr.Binary (_, _, e0, e1) ->
        extract_uses c args cs e0; extract_uses c args cs e1
    | Expr.Unary (_,_,e) ->
        extract_uses c args cs e
    | Expr.Clause (_, c', es) ->
        let i = ref 0 in
        let id = get_clause_id p c' in
        List.iter (fun e ->
          extract_uses c args ((id, !i)::cs) e;
          incr i) es
    | Expr.Prim _ -> ()
  in

  List.iter (fun c ->
    let _,args = List.fold_left (fun (i,args) v ->
      i+1, (v,i)::args) (0, []) (snd c.postcond)
    in
    List.iter (extract_uses c.id args []) c.preconds;
  ) p.clauses;

  (* Propagate. *)
  let finished = ref false in
  while not !finished do
    finished := true;
    for c=0 to (Array.length arg_uses)-1 do
      for i=0 to (Array.length arg_uses.(c))-1 do
        match arg_uses.(c).(i) with
        | Depends args ->
            let always = List.exists (fun (c',i') -> arg_uses.(c').(i') = Always) args in
          if always then begin
            finished := false;
            arg_uses.(c).(i) <- Always
          end
        | _ -> ()
      done
    done
  done;

  (* Eliminate useless variables. *)
  let rec filter_args (e: Expr.t): Expr.t =
    match e with
    | Expr.Binary (pos, op, e0, e1) -> Expr.Binary (pos, op, filter_args e0, filter_args e1)
    | Expr.Unary (pos, op, e) -> Expr.Unary (pos, op, filter_args e)
    | Expr.Clause (pos, c, es) ->
        let id,_ = List.assoc c p.clauses_def in
        let _,es = List.fold_left (fun (i, es) e ->
          if arg_uses.(id).(i) = Always then
            i+1, (filter_args e)::es
          else
            i+1, es
        ) (0, []) es in
        Expr.Clause (pos, c, List.rev es)
    | _ -> e
  in

  let cs = List.map (fun c ->
    let pre = List.map filter_args c.preconds in
    let _,post = List.fold_left (fun (i, post) v ->
      if arg_uses.(c.id).(i) = Always then i+1,v::post else i+1, post
    ) (0, []) (snd c.postcond) in
    { c with preconds = pre; postcond = (fst c.postcond),(List.rev post) }
  ) p.clauses in
  { p with clauses = cs }
 

