(** Conduct an analysis on the control flow graph built upon the surface syntax. *)

open Labels
open Types
open Literal
open Syntax
open Expressions
open Expr
open Linexpr
open Bexpr
open Cfg


(** Build the environment. *)
let make_environment (): Apron.Environment.t =
  let vars = Environment.typed_variables () in
  let realvars, intvars = List.partition (fun (_,t) -> t == TypeFloat) vars in
  let realtab = Array.make (List.length realvars) (Apron.Var.of_string "")
  and inttab = Array.make (List.length intvars) (Apron.Var.of_string "") in
  Utils.iteri (fun i (x,_) -> realtab.(i) <- Apron.Var.of_string x) realvars;
  Utils.iteri (fun i (x,_) -> inttab.(i) <- Apron.Var.of_string x) intvars; 

  Apron.Environment.make inttab realtab


(** Translate a linear expression. *)
let make_linexpr (env: Apron.Environment.t) (e: Linexpr.t): Apron.Linexpr1.t =
  let line = Apron.Linexpr1.make env in
  let coeff_of_literal (l: Literal.t): Apron.Coeff.t =
    match l with
    | Int n -> Apron.Coeff.s_of_int n
    | Float f -> Apron.Coeff.s_of_float f
    | _ -> Errors.fatal [Lexing.dummy_pos] "Unexpected non int or float coeff in linear expression"
  in
  let coefs = List.map (fun (x, c) ->
    coeff_of_literal c, Apron.Var.of_string x) e.terms in
  Apron.Linexpr1.set_list line coefs (Some (coeff_of_literal e.constant));
  line


(** Translate a constraint. *)
let make_constraint (env: Apron.Environment.t) (e,op: Linexpr.t * string): Apron.Lincons1.earray =
  let typ = match op with
    | "==" -> Apron.Lincons1.EQ | "!=" -> Apron.Lincons1.DISEQ
    | ">=" -> Apron.Lincons1.SUPEQ | ">" -> Apron.Lincons1.SUP | _ -> assert false
  in
  let line = make_linexpr env e in
  let c = Apron.Lincons1.make line typ in
  let arr = Apron.Lincons1.array_make env 1 in
  Apron.Lincons1.array_set arr 0 c;
  arr


(** Abstraction the program states. Only the loop header are retained. *)
type 'a abstract_info = {
  mutable value: 'a Apron.Abstract1.t option;
  (* Previous abstract value. Set only for loop headers. *)
  mutable prev_value: 'a Apron.Abstract1.t option;
  (* Number of visits of the label. *)
  mutable marker: int;
  cfg_info: (Apron.Linexpr1.t, Apron.Lincons1.earray) label_info
}

type 'a abstract_state = ('a abstract_info) array


(** Display the result of the analysis. *)
let print_abstract (state: 'a abstract_state): unit =
  Array.iteri (fun l info ->
    print_string ("L" ^ string_of_int l ^ ": ");
    if info.cfg_info.loop_header != None then print_string "loop";
    print_newline ();
    match info.value with
    | None -> ()
    | Some d ->
        Apron.Abstract1.print Format.std_formatter d;
        Format.pp_print_flush Format.std_formatter ();
        print_newline ()
  ) state


(** Creation of the initial state (top at state 0, bottom everywhere else).  *)
let make_initial_state
    (man: 'a Apron.Manager.t)
    (env: Apron.Environment.t)
    (cfg: Linexpr.t Cfg.t): 'a abstract_state =
  (* Convert the information contained in cfg_info. *)
  let rec econvert
      (info: (Linexpr.t, Linexpr.t * string) label_info)
    : (Apron.Linexpr1.t, Apron.Lincons1.earray) label_info =
    { info with successors = List.map (fun (c, l) ->
       (match c with
        | Cond b -> Cond (bconvert b)
        | Assign (x, e) -> Assign (x, make_linexpr env e)), l
      ) info.successors }
  and bconvert (b: (Linexpr.t * string) Bexpr.t): Apron.Lincons1.earray Bexpr.t =
    match b with
    | Atom (e, op) -> Atom (make_constraint env (e, op))
    | Conj bs -> Conj (List.map bconvert bs)
    | Disj bs -> Disj (List.map bconvert bs)
    | Top -> Top | Bot -> Bot
  in 

  Array.mapi (fun l info ->
    { value = None;
      prev_value = None;
      marker = 0;
      cfg_info = econvert info }
  ) cfg.labels


(* Compute the effect of a condition on an absract value. *)
let rec with_cond
    (man: 'a Apron.Manager.t)
    (env: Apron.Environment.t)
    (b: Apron.Lincons1.earray Bexpr.t)
    (d: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
  match b with
  | Top -> d | Bot -> Apron.Abstract1.bottom man env
  | Conj bs ->
      List.fold_left (fun d b ->
        with_cond man env b d) d bs
  | Disj bs ->
      List.fold_left (fun d' b ->
        Apron.Abstract1.join man d' (with_cond man env b d)) d bs
  | Atom c ->
      Apron.Abstract1.meet_lincons_array man d c

(* Compute the effect of a command on an abstract value. *)
let with_command
    (man: 'a Apron.Manager.t)
    (env: Apron.Environment.t)
    (c: (Apron.Linexpr1.t, Apron.Lincons1.earray) command)
    (d: 'a Apron.Abstract1.t): 'a Apron.Abstract1.t =
  match c with
  | Cond b -> with_cond man env b d
  | Assign (x, e) ->
      let v = Apron.Var.of_string x.name in
      Apron.Abstract1.assign_linexpr man d v e None


(** Abstract interpretation. Starting from the loop headers (and the initial program point),
    propagate the domains. *)
let perform_analysis
    (man: 'a Apron.Manager.t)
    (env: Apron.Environment.t)
    (state: 'a abstract_state): unit =
  try
    (* Reset all the markers until the label l1. *)
    let rec reset_markers (l0: label) (l1: label): unit =
      let info = state.(l0.id) in
      if info.marker != 0 then begin
        info.marker <- 0;
        info.value <- None;
        if l0 != l1 then List.iter (fun (_,l) -> reset_markers l l1) info.cfg_info.successors
      end

    (* Join on a label. *)
    and join_and_continue
        (onlyjoin: bool)
        (l1: label) (lims: label option * label option)
        (d0: 'a Apron.Abstract1.t): unit =
      let info = state.(l1.id) in
      let d1 =
        if info.marker = 0 then d0
        else
          match info.value with
          | None -> d0
          | Some d1 -> Apron.Abstract1.join man d0 d1
      in
      info.marker <- info.marker + 1;
      if onlyjoin then
        info.value <- Some d1
      else begin
        (* Check for loop headers. *)
        match info.cfg_info.loop_header with
        | None ->
            (* Finished gathering the results from the predecessor labels. *)
            if info.marker = List.length info.cfg_info.predecessors then begin
              info.value <- None; 
              iterate l1 lims d1
            (* Still gathering results. *)
            end else
              info.value <- Some d1
        | Some le -> 
            find_loop_invariant l1 le d1;
            begin match state.(le.id).value with
            | None -> ()
            | Some d -> iterate le lims d
            end
      end

    (* Perform the algorithm starting from label [l0], stopping at label [l1]. *)
    and iterate
        (l0: label)
        (lentry, lexit: label option * label option)
        (d0: 'a Apron.Abstract1.t): unit =
      List.iter (fun (c, l1) ->
        let d1 = with_command man env c d0 in
        join_and_continue (Some l1 = lexit || Some l1 = lentry) l1 (lentry,lexit) d1
      ) state.(l0.id).cfg_info.successors

    (* Find the loop invariant. *)
    and find_loop_invariant (l0: label) (l1: label) (d0: 'a Apron.Abstract1.t): unit =
      let info = state.(l0.id)
      and invariant = ref (Apron.Abstract1.bottom man env)
      and d = ref d0 in
      while not (Apron.Abstract1.is_eq man !d !invariant) do
        reset_markers l0 l1; info.marker <- 1; info.value <- Some d0;
        iterate l0 (Some l0, Some l1) !d;
        invariant := !d;
        d := (match info.value with Some d -> d | None -> !d);
        (* Widen. *)
        d := Apron.Abstract1.widening man !invariant !d
      done;
      (* Refine the invariant, by doing another loop iteration. *)
      reset_markers l0 l1; info.marker <- 1; info.value <- Some d0;
      iterate l0 (Some l0, Some l1) !invariant;
      invariant := (match info.value with None -> !invariant | Some d -> d);

      (* Compute the value at the exit point (another loop iteration). *)
      reset_markers l0 l1; info.marker <- 1; info.value <- None;
      iterate l0 (Some l0, Some l1) !invariant;

      (* Final settings. *)
      info.value <- Some !invariant;
    in

    (* Main loop. *)
    iterate Labels.init_label (None,None) (Apron.Abstract1.top man env)
  
  with Not_found -> assert false


(** Put everything together, and perform an analysis of a control flow graph. *)
let analyze (cfg: Linexpr.t Cfg.t): (Polka.loose Polka.t) abstract_state =
  (* Creation of the manager. *)
  let man = Polka.manager_alloc_loose () in
  (* Creation of the environment. *)
  let env = make_environment () in
  (* Creation of the initial state. *)
  let state = make_initial_state man env cfg in
  (* Analysis. *)
  perform_analysis man env state;
  state


