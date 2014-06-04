(** Build and analyze the dependency graph of the horn clauses. *)

open Apron
open Expressions
open Expr
open Bexpr
open Utils
open Smtlib
open Vars
open Types
open Positions


(** Parametric definition of clauses. *)
type ('a, 'v, 'p) gen_clause = {
  cname: var;               (* The clause's predicate. *)
  cpos: position;           (* The position of the declaration. *)
  mutable variables: 'v;    (* List of universally quantified variables. *)
  mutable preconds: 'p;     (* Clause preconditions. *)
  mutable arguments: 'a;    (* Arguments of the clause. *)
  negative: bool            (* Whether the goal is negative or positive or strict.
                               If it is negative, the fields cname and arguments are ignored. *)
}

(** Parametric definition of predicates. *)
type ('a, 'e, 'c) gen_predicate = {
  mutable pname: var;          (* The name of the predicate. *)
  mutable arguments': 'a;      (* The arguments of the predicate. *)
  mutable environment: 'e;     (* The associated Apron environment. *)
  mutable clauses: 'c list;    (* The list of clauses whose goal is the predicate. *)

  mutable mark: int;           (* A boolean marker. *)
  mutable widen: bool;         (* Identify widening points. *)
  mutable head: bool;          (* Identify the starting points of the analysis. *)

  mutable children: int list;  (* The list of predicates that depends upon this node. *)
  mutable ancestors: int list; (* The number of dependencies. *)
  mutable fromloops: int list; (* List the loops this point is part of. *)

  mutable valid: bool          (* When the predicate has been inlined, it shouldn't be accessed. *)
}

(** Group the predicates definitions in a dependency graph. *)
type ('p, 'c) gen_script = {
  context: var list;           (* List of predicates. *)
  predicates: 'p array;        (* Definition of each predicate. *)
  negatives: 'c list;          (* Remaining, negative clauses. *)
  commands: command list;      (* Remaining commands. *)
  mutable reducible: bool      (* True iff the CFG is reducible. *)
}


(** Several functions querying the control graph. *)
let fromloop (script: ('p,'c) gen_script) (loop: int) (c: int) =
  List.mem loop script.predicates.(c).fromloops


(** The iteration function of the static analysis. *)
let rec iterate (predicates: 'p array) (update: int -> unit) (c: int): unit =
  predicates.(c).mark <- predicates.(c).mark + 1;
  (* Waiting for additional values. *)
  if predicates.(c).mark < List.length predicates.(c).ancestors then begin
    (* node is widening point => continue. *)
    if predicates.(c).widen then begin
      update c;
      List.iter (iterate predicates update) predicates.(c).children
    end
  (* All values are here. *)
  end else if predicates.(c).head ||
              predicates.(c).mark = List.length predicates.(c).ancestors then begin
    (* Compute the value at c. *)
    update c;
    (* Continue only if the point is not a widening point. *)
    if not predicates.(c).widen then
      List.iter (iterate predicates update) predicates.(c).children
  (* Already went through here, stop. *)
  end else ()


(** Generic graph analysis. *)
let analyze
   ?(donarrow: bool = true)                        (* Deactivate narrowing. *)
    (script: ('p,'c) gen_script)                   (* Control flow graph. *)
    (update: ?dowiden: bool -> int -> bool)        (* Node update. The function should return true iff
                                                      the state has been updated. *)
  : unit =
  let preds = script.predicates in
  (* Loop ancestors. *)
  let loop_ancestors (loop: int) (c: int): int =
    List.fold_left (fun n c' ->
      if List.mem loop preds.(c').fromloops then n else n+1) 0 preds.(c).ancestors
  in
  (* Clear the marks. *)
  let clear_marks (loop: int): unit =
    for i=0 to (Array.length preds)-1 do
      if i = loop then
        preds.(i).mark <- loop_ancestors loop i
      else if List.mem loop preds.(i).fromloops then
        preds.(i).mark <- 0
    done
  in

  (* Iterate. *)
  let rec iterate (loop: int option) (c: int): unit =
    begin match loop with
    | None ->
        Logger.log ~mode:"abstract-debug" ("Iterate: [] " ^ preds.(c).pname.name ^ "\n")
    | Some cloop ->
        Logger.log ~mode:"abstract-debug"
          ("Iterate: [" ^ preds.(cloop).pname.name ^ "] " ^ preds.(c).pname.name ^ "\n")
    end;

    preds.(c).mark <- preds.(c).mark + 1;

    (* Loop header. *)
    if preds.(c).widen then begin
      (* Reached the stop point. *)
      if loop = Some c then ()
      (* Launch another loop iteration. *)
      else if preds.(c).mark = loop_ancestors c c then begin
        (* Widen and stabilize. *)
        while update c do
          clear_marks c;
          Utils.iteron (fromloop script c) (iterate (Some c)) preds.(c).children;
        done;
        (* Narrow. *)
        if donarrow then begin
          Utils.iteron (fromloop script c) (iterate (Some c)) preds.(c).children;
          let _ = update ~dowiden:false c in clear_marks c
        end;
        (* Leave the loop. *)
        for c'=0 to (Array.length preds)-1 do
          if fromloop script c c' then
            Utils.iteron (fun c' -> not (fromloop script c c')) (iterate loop) preds.(c').children
        done
      end

    (* Not a loop header. *)
    end else begin
      (* Waiting for additional values. *)
      if preds.(c).mark < List.length preds.(c).ancestors then ()
      (* All values are here. *)
      else if preds.(c).mark = List.length preds.(c).ancestors then begin
        (* Compute the value at c. *)
        let _ = update c in
        match loop with
        | None -> List.iter (iterate loop) preds.(c).children
        | Some cloop -> Utils.iteron (fromloop script cloop) (iterate loop) preds.(c).children
      end
    end
  in

  (** REDUCIBLE GRAPHS **)
  if script.reducible then begin
    (* Start the iteration at all header points. *)
    for i=0 to (Array.length preds)-1 do
      preds.(i).mark <- if preds.(i).head then -1 else 0;
    done;
    for i=0 to (Array.length preds)-1 do
      if preds.(i).head then iterate None i
    done;

  (** NON REDUCIBLE GRAPHS **)
  end else begin
    let stable = ref false in
    while not !stable do
      stable := true;
      for i=0 to (Array.length preds)-1 do
        if preds.(i).valid then begin
          if update i then
            stable := false
        end
      done
    done
  end


