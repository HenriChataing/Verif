(** Extract the Horn clauses from the SmtLib file. *)

open Smtlib
open Expressions
open Vars
open Types


(* List of smtlib operators, each given an arity and the used representation. *)
let operators = [
  "=", (2, "=="); "<>", (2, "<>");
  "<", (2, "<"); ">", (2, ">");
  "<=", (2, "<="); ">=", (2, ">=");
  "and", (2, "&&"); "or", (2, "||");
  "not", (1, "not");
  "+", (2, "+"); "-", (2, "-"); "*", (2, "*")
]


(** Definition of a horn clause. *)
type clause = {
  cname: var;               (* The clause's name. *)
  cpos: Positions.position; (* The position of the declaration. *)
  mutable variables: var list;      (* List of universally quantified variables. *)
  mutable preconds: Expr.t list;    (* Clause preconditions. *)
  mutable arguments: Expr.t list;   (* Arguments of the clause. *)
  negative: bool            (* Whether the goal is negative or positive or strict.
                               If it is negative, the fields cname and arguments are ignored. *)
}


(** Convert a sort to a primitive type. *)
let rec typ_of_sort ?(arg: sort list = []) (s: sort): typ =
  let typ =
    match s with
    | Sort ({ symbol = "Int" }, []) -> TyInt
    | Sort ({ symbol = "Bool" }, []) -> TyBool
    | _ -> Errors.fatal [] "This sort is not a primitive type"
  in
  let arg = List.map typ_of_sort arg in
  if arg = [] then typ
  else TyArrow (arg, typ)

(** Convert a primitive type to a sort. *)
let rec sort_of_typ (t: typ): sort =
  match t with
  | TyInt -> Sort (make_id "Int", [])
  | TyBool -> Sort (make_id "Bool", [])
  | TyFloat -> Sort (make_id "Float", [])
  | TyArrow (_, typ) -> sort_of_typ typ


(** Remove the prefix universal quantifications and return the corresponding variables. *)
let rec strip_foralls (t: term): var list * term =
  match t with
  | Forall (_, xs, t) ->
      let xs = List.map (fun (s, t) ->
        { vid = 0; name = s; typ = typ_of_sort t; pos = Positions.undefined_position }) xs in
      let xs',t = strip_foralls t in
      xs @ xs', t
  | _ -> [], t

(** Remove the prefix existential quantifications and return the corresponding variables. *)
let rec strip_exists (t: term): var list * term =
  match t with
  | Exists (_, xs, t) ->
      let xs = List.map (fun (s, t) ->
        { vid = 0; name = s; typ = typ_of_sort t; pos = Positions.undefined_position }) xs in
      let xs',t = strip_exists t in
      xs @ xs', t
  | _ -> [], t


(** Use the term in conjunctive form. *)
let rec as_conjunction (t: term): term list =
  match t with
  | App (_, { symbol = "and" }, ts) ->
      List.concat (List.map as_conjunction ts)
  | _ -> [t]


(** Match the application of a predicate to its variables. *)
let as_predicate (ctx: context) (t: term): var * term list =
  match t with
  | App (pos, { symbol = c }, ts) ->
      from_context ~pos:pos ctx c, ts
  | _ -> Errors.fatal' (position_of_term t) "Term is not a Horn clause"


(** Convert a term to an expression. *)
let rec expr_of_term (ctx: context) (t: term): Expr.t =
  match t with
  | Prim (pos, Num n) -> Expr.Prim (pos, Primitive.Int n)
  | Ident (pos, { symbol = "true" }) ->
      Expr.Prim (pos, Primitive.Bool true)
  | Ident (pos, { symbol = "false" }) ->
      Expr.Prim (pos, Primitive.Bool false)

  | Ident (pos, { symbol = s }) ->
      Expr.Var (pos, from_context ~pos:pos ctx s)
  | App (pos, { symbol = op }, ts) ->
      let es = List.map (expr_of_term ctx) ts in
      begin try
        let (arity, repl) = List.assoc op operators in
        match op, arity, es with
        | "-", _, [e] -> Expr.Unary (pos, "-", e)
        | _, 1, [e] -> Expr.Unary (pos, repl, e)
        | _, 2, [e0;e1] -> Expr.Binary (pos, repl, e0, e1)
        | _ -> Errors.fatal [] "Malformed term"
      with Not_found ->
        Expr.Predicate (pos, from_context ~pos:pos ctx op, es)
      end
  | Attribute (_, t, _) -> expr_of_term ctx t
  | _ -> Errors.fatal [] "Not implemented"


(** Convert an expression to a term. *)
let rec term_of_expr (e: Expr.t): term =
  match e with
  | Expr.Prim (pos, Primitive.Int n) -> Prim (pos, Num n)
  | Expr.Prim (pos, Primitive.Bool true) ->
      Ident (pos, make_id "true")
  | Expr.Prim (pos, Primitive.Bool false) ->
      Ident (pos, make_id "false")

  | Expr.Var (pos, v) -> Ident (pos, make_id v.name)

  | Expr.Unary (pos, op, e) ->
      begin try
        let op', _ = List.find (fun (_, (_, op')) -> op' = op) operators in
        App (pos, make_id op',
          [term_of_expr e])
      with Not_found ->
        Errors.fatal' pos ("Undefined operator '" ^ op ^ "'")
      end
  | Expr.Binary (pos, op, e0, e1) ->
      begin try
        let op', _ = List.find (fun (_, (_, op')) -> op' = op) operators in
        App (pos, make_id op',
          [term_of_expr e0; term_of_expr e1])
      with Not_found ->
        Errors.fatal' pos ("Undefined operator '" ^ op ^ "'")
      end
  | Expr.Predicate (pos, c, es) ->
      App (pos, make_id c.name,
        List.map term_of_expr es)

  | _ -> Errors.fatal [] "Not implemented"


(** Convert a clause to a term. *)
let term_of_clause (c: clause): term =
  let pre = match c.preconds with
    | [] -> Ident (c.cpos, make_id "true")
    | e::es ->
        List.fold_left (fun t e ->
          let te = term_of_expr e
          and p = Expr.position e in
          App (p, make_id "and", [t;te])
        ) (term_of_expr e) es
  in
  let impl =
    if c.negative then
      App (c.cpos, make_id "not", [pre])
    else begin
      let post =
        App (c.cpos,
          make_id c.cname.name,
          List.map term_of_expr c.arguments
        ) in
      App (c.cpos, make_id "=>", [pre;post])
    end
  in
  if c.variables = [] then
    impl
  else
    Forall (c.cpos,
      List.map (fun v ->
        v.name, sort_of_typ v.typ) c.variables,
      impl
    )


(** Representation of the contents of a program writen using Horn clauses. *)
type script = {
  context: var list;      (* The context of clause predicates. *)
  clauses: clause list;
  commands: command list  (* The remaining commands, set-logic and check-sat for example. *)
}


(** Extract the horn clauses of a smtlib program. *)
let extract_clauses (p: command list): script =
  List.fold_left (fun p c ->
    match c with
    (* Declaration of a predicate. *)
    | DeclareFun (pos, n, sts,st) ->
        let typ = typ_of_sort ~arg:sts st in
        let c = create_var ~pos:pos ~typ:typ n in
        { p with context = c::p.context }
    (* Declaration of a clause. *)
    | Assert (pos, t) ->
      (* Separate the quantifiers. *)
      let xs, t' = match t with
        | Forall _ -> strip_foralls t
        | App (pos, { symbol = "not" }, [t]) ->
            let xs, t' = strip_exists t in
            xs, App (pos, make_id "not", [t'])
        | _ -> [], t
      in
      (* Local context. *)
      let ctx = xs @ p.context in
      begin match t' with
      (* Basic form: P0 /\ .. /\ Pn => Q *)
      | App (pos, { symbol = "=>" }, [pre; post]) ->
          let es = List.map (expr_of_term ctx) (as_conjunction pre)
          and c, args = as_predicate ctx post in
          let args = List.map (expr_of_term ctx) args in
          { p with clauses =
             { cname = c; cpos = pos;
               variables = xs; preconds = es;
               arguments = args; negative = false }::p.clauses }
      (* Non explicit forms of horn clauses:
           not (P0 /\ .. /\ Pn /\ not Q) *)
      | App (pos, { symbol = "not" }, [conj]) ->
        let cs = as_conjunction conj in
        (* Partition the goal and the preconditions. *)
        let goals, preconds = List.partition (function
          | App (_, { symbol = "not" }, [App (_, op, _)]) ->
              List.exists (fun v -> v.name = op.symbol) p.context
          | _ -> false) cs in
        begin match goals with
        (* Exactly one goal => Positive and strict clauses. *)
        | [App (_, _, [t])] ->
            let es = List.map (expr_of_term (xs @ p.context)) preconds in
            let c,args = as_predicate (xs @ p.context) t in
            let args = List.map (expr_of_term (xs @ p.context)) args in
            { p with clauses =
              { cname = c; cpos = pos;
                variables = xs; preconds = es;
                arguments = args; negative = false }::p.clauses }
        (* No goal => Negative clauses. *)
        | [] ->
            let es = List.map (expr_of_term (xs @ p.context)) preconds in
            { p with clauses =
              { cname = dummy_var; cpos = pos;
                variables = xs; preconds = es;
                arguments = []; negative = true }::p.clauses }
        (* If more than one goal: not a horn clause. *)
        | _ -> Errors.fatal' pos "Unaccepted assertion: does not describe a Horn clause"
        end
      (* This case corresponds to clauses with no preconditions. *)
      | _ ->
          let c,args = as_predicate (xs @ p.context) t' in
          let args = List.map (expr_of_term (xs @ p.context)) args in
          { p with clauses =
            { cname = c; cpos = pos;
              variables = xs; preconds = [];
              arguments = args; negative = false }::p.clauses }
      end

    (* Other commands are justed stored unchanged in the script. *)
    | _ -> { p with commands = c::p.commands }
  ) { context = []; clauses = []; commands = [] } p


(** Pretty print a horn clause (to something resembling the syntax of Prolog) *)
let string_of_clause (c: clause): string =
  let pre = match c.preconds with
    | [] -> ""
    | e::es -> Expr.to_string e ^ List.fold_left (fun s e -> s ^ ", " ^ Expr.to_string e) "" es
  in
  let vars = match c.arguments with
    | [] -> ""
    | a::args -> List.fold_left (fun s a -> s ^ ", " ^ Expr.to_string a) (Expr.to_string a) args
  in
  if c.negative then
    " :- " ^ pre
  else
    c.cname.name ^ "(" ^ vars ^ ") :- " ^ pre


(** Cleanup a clause by substituting variable equalities. *)
let substitute_equalities (c: clause): unit =
  (* Equivalence classes of variables. *)
  let classes = ref [] in
  let get_class rep x =
    let cx, cs = List.partition (fun (r,c) ->
      if rep then Some x = r else List.mem x c) !classes in
    classes := cs;
    match cx with
    | [] -> if rep then (Some x, []) else (None, [x])
    | cx::_ -> cx
  in
  let merge_classes rx x ry y =
    if x = y then []
    else begin
      let cx = get_class rx x
      and cy = get_class ry y in
      let mxy, eq = match cx, cy with
        | (None, cx), (ry, cy) -> (ry, cx @ cy), []
        | (rx, cx), (None, cy) -> (rx, cx @ cy), []
        | (Some rx, cx), (Some ry, cy) ->
            ((Some rx, cx @ cy),
             [Expr.Binary (rx.pos, "==", Expr.Var (rx.pos,rx), Expr.Var (ry.pos, ry))])
      in
      classes := mxy::!classes;
      eq
    end
  in
  (* Extract the goal predicate's arguments (they must be variables) *)
  let args = List.map (function
      Expr.Var (_,v) -> v
    | e -> Errors.fatal' (Expr.position e) "Expected variable in clause definition"
  ) c.arguments in

  (* Filter the preconditions for equalities between variables, and apply these
     equalities to the equivalence classes. *)
  let pre = List.fold_left (fun pre e ->
    match e with
    | Expr.Binary (_, "==", Expr.Var (_, v0), Expr.Var (_, v1)) ->
        let r0 = List.mem v0 args
        and r1 = List.mem v1 args in
        let eq = merge_classes r0 v0 r1 v1 in
        eq @ pre
    | _ -> e::pre
  ) [] c.preconds in
  (* Chose a representant by class, and build the corresponding substitution. *)
  let subs = List.fold_left (fun subs (r,c) ->
    let repr = match r with
      | None -> List.hd c
      | Some r -> r
    in
    List.fold_left (fun sub x -> (x, repr)::sub) subs c
  ) [] !classes in
  (* Apply the substitution to the remaining preconditions. *)
  let pre = List.map (Expr.rename subs) pre in
  (* Apply to the post condition. *)
  let post = List.map (Expr.rename subs) c.arguments in
  (* Build the resulting clause. *)
  c.preconds <- pre;
  c.arguments <- post


(** Rename the arguments of the goal predicate to match the format X{cid}_{vid}. *)
let standardise_arguments (c: clause): unit =
  if not c.negative then begin
    let cname = c.cname in
    let typargs, typ =
      match cname.typ with
      | TyArrow (ts, t) -> (ts, t)
      | _ -> [], cname.typ
    in

    (* Unified arguments. *)
    let _,uargs = List.fold_left (fun (i, uargs) ty ->
      let v = {
        name = "X" ^ string_of_int cname.vid ^ "_" ^ string_of_int i;
        pos = cname.pos;
        typ = ty;
        vid = i
      } in
      i+1, v::uargs
    ) (0, []) typargs in
    let uargs = List.rev uargs in

    (* Replace the old by the new. *)
    let subs, eqs = List.fold_left (fun (subs, eqs) (v, e) ->
      match e with
      | Expr.Var (_, v') ->
          begin try
            let _ = List.assoc v' subs in
            subs, Expr.Binary (v.pos, "==", Expr.Var (v.pos, v), e)::eqs
          with Not_found ->
            (v', v)::subs, eqs
          end
      | _ -> subs, Expr.Binary (v.pos, "==", Expr.Var (v.pos, v), e)::eqs
    ) ([], []) (List.combine uargs c.arguments) in
    let preconds = List.map (Expr.rename subs) (c.preconds @ eqs) in
    let variables = List.filter (fun v ->
      List.for_all (fun (v', _) -> v.name != v'.name) subs
    ) c.variables @ uargs in

    let diff = List.length variables - List.length c.variables in
    Logger.log ~lvl:2 ("StdArg:" ^ c.cname.name ^": +" ^ string_of_int diff ^ "\n");
    Logger.flush ();

    c.arguments <- List.map (fun v -> Expr.Var (v.pos, v)) uargs;
    c.variables <- variables;
    c.preconds <- preconds
  end

module StringMap = Map.Make (String)


(** Use of an argument. *)
type arguse =
    Never
  | Always
  | Depends of (int * int) list  (* j-th argument of the i-th clause. *)

let join (u: arguse) (u': arguse): arguse =
  match u,u' with
  | Always, _ -> Always | _, Always -> Always
  | Never, _ -> u' | _, Never -> u
  | Depends u, Depends u' -> Depends (u @ u')


(** On a global scale, purge the useless clause arguments: for each
    clause declaration, looks up the unused (=unconstrained) arguments and remove them. *)
let minimize_clause_arguments (p: script): unit =
  (* All clause arguments. *)
  let arg_uses = Array.make (List.length p.context) (Array.make 0 Never) in
  List.iter (fun v ->
    let arity = match v.typ with
      | TyArrow (arg, _) -> List.length arg
      | _ -> 0
    in
    arg_uses.(v.vid) <- Array.make arity Never
  ) p.context;

  (* Update an arugment. *)
  let update (c, i: int * int) (u: arguse): unit =
    arg_uses.(c).(i) <- join u arg_uses.(c).(i)
  in

  (* Extract the uses of variables in an expression. *)
  let rec extract_uses
      (c: int)      (* The id of the clause. *)
      (args: (var * int) list)
      (cs: (int * int) list)  (e: Expr.t): unit =
    match e with
    | Expr.Var (_,v) ->
        let u = if cs = [] then Always else Depends cs in
        List.iter (fun (v', i) ->
          if v.name = v'.name then
            update (c, i) u
        ) args
    | Expr.Binary (_, _, e0, e1) ->
        extract_uses c args cs e0; extract_uses c args cs e1
    | Expr.Unary (_,_,e) ->
        extract_uses c args cs e
    | Expr.Predicate (_, c', es) ->
        let i = ref 0
        and id = c'.vid in
        List.iter (fun e ->
          extract_uses c args ((id, !i)::cs) e;
          incr i) es
    | Expr.Prim _ -> ()
  in

  List.iter (fun c ->
    (* Negative clauses don't give information about the arguments. *)
    if not c.negative then begin
      let _,args = List.fold_left (fun (i,args) a ->
        match a with
        | Expr.Var (_, v) -> i+1, (v,i)::args
        | _ -> update (c.cname.vid,i) Always; i+1, args
      ) (0, []) c.arguments in
      (* If two arguments are equal, then they mutually depends on themselves. *)
      List.iter (fun (v,i) ->
        List.iter (fun (v',i') ->
          if v.name = v'.name && i != i' then
            update (c.cname.vid,i) (Depends [c.cname.vid,i']);
        ) args
      ) args;
      List.iter (fun e ->
        extract_uses c.cname.vid args [] e) c.preconds
    end
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

  (* Remove superflous arguments. *)
  let rec filter_args (e: Expr.t): Expr.t =
    match e with
    | Expr.Binary (pos, op, e0, e1) -> Expr.Binary (pos, op, filter_args e0, filter_args e1)
    | Expr.Unary (pos, op, e) -> Expr.Unary (pos, op, filter_args e)
    | Expr.Predicate (pos, c, es) ->
        let id = c.vid in
        let _,es = List.fold_left (fun (i, es) e ->
          if arg_uses.(id).(i) = Always then
            i+1, (filter_args e)::es
          else
            i+1, es
        ) (0, []) es in
        Expr.Predicate (pos, c, List.rev es)
    | _ -> e
  in

  (* Update the type of the clauses. *)
  List.iter (fun c ->
    let arg, typ = match c.typ with
      | TyArrow (arg, typ) -> arg,typ
      | _ -> [], c.typ
    in
    let id = c.vid in
    let _,arg = List.fold_left (fun (i, arg) a ->
      if arg_uses.(id).(i) = Always then
        i+1, a::arg
      else
        i+1, arg
    ) (0, []) arg in
    if arg = [] then
      c.typ <- typ
    else
      c.typ <- TyArrow (List.rev arg, typ)
  ) p.context;

  (* Update the clause definitions. *)
  List.iter (fun c ->
    let pre = List.map filter_args c.preconds in
    let _,post = List.fold_left (fun (i, post) v ->
      if arg_uses.(c.cname.vid).(i) = Always then i+1,v::post else i+1, post
    ) (0, []) c.arguments in

    if not c.negative then begin
      let diff = List.length post - List.length c.arguments in
      Logger.log ~lvl:2 ("MinArg:" ^ c.cname.name ^ ": " ^ string_of_int diff ^ "\n");
      Logger.flush ();
    end;

    c.preconds <- pre;
    c.arguments <- List.rev post
  ) p.clauses


(** Remove the unconstrained variables existentially declared in a clause. *)
let minimize_clause_variables (c: clause): unit =
  let used = List.concat (List.map Expr.freevar (c.preconds @ c.arguments)) in
  let rem = List.filter (fun v -> List.mem v used) c.variables in

  let name = if c.negative then "Neg" ^ string_of_int c.cname.vid else c.cname.name in
  let diff = List.length rem - List.length c.variables in
  Logger.log ~lvl:2 ("MinVar:" ^ name ^ ": " ^ string_of_int diff ^ "\n");
  Logger.flush ();

  c.variables <- rem


(** Try reducing the number of arguments and variables. *)
let simplify_clauses (p: script): unit =
  (* Initial number of arguments. *)
  let init = List.map (fun c -> Types.nargs c.typ) p.context in
  List.iter (fun c ->
    standardise_arguments c;
    substitute_equalities c
  ) p.clauses;
  minimize_clause_arguments p;
  List.iter minimize_clause_variables p.clauses;
  (* Output percentage of deleted arguments. *)
  let per = List.fold_left2 (fun per n c ->
    per +. float_of_int (n - Types.nargs c.typ) /. float_of_int n) 0. init p.context in
  Logger.log ~lvl:1 (string_of_float (100. *. per /. float_of_int (List.length p.context)) ^ "\n")


(** Rebuild a simplified smt program. *)
let commands_of_script (p: script): command list =
  let decl = List.map (fun c ->
    let arg, typ = match c.typ with
      | TyArrow (arg, typ) -> arg, typ
      | _ -> [], c.typ
    in
    let arg = List.map sort_of_typ arg
    and typ = sort_of_typ typ in
    DeclareFun (c.pos, c.name, arg, typ)
  ) p.context in
  let asserts = List.map (fun c ->
    Assert (c.cpos, term_of_clause c)
  ) p.clauses in
  List.sort (fun c0 c1 ->
    let p0 = position_of_command c0
    and p1 = position_of_command c1 in
    compare (Positions.start_of_position p0) (Positions.start_of_position p1)
  ) (decl @ asserts @ p.commands)

