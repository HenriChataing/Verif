(** Definition and construction of the control flow graph. *)

open Positions

open Labels
open Syntax
open Environment

open Bexpr
open Linexpr

(** Edges of the graph are labelled by commands. *)
type command =
    Cond of (linexpr * string) bexpr
  | Assign of string * linexpr


(** Description of the graph. *)

module LABEL = struct
  type t = label
  let compare l0 l1 = l0.id - l1.id
end

module LabelMap = Map.Make (LABEL)

type label_info = {
  mutable loop_header: bool;                     (* Identify widening points. *)
  mutable do_widen: bool;                        (* When to perform the widening. *)
  mutable successors: (command * label) list;
  mutable predecessors: label list
}

type cfg = label_info LabelMap.t


(** Conversion of expressions to linear expressions.
    Any non linear expression raises an error. *)
let rec to_linexpr (e: expression): linexpr =
  match e with
  | Var (p, v) -> { terms = [find_var p v, 1]; constant = 0 }
  | Binary (_, "+", e0, e1) -> add (to_linexpr e0) (to_linexpr e1)
  | Binary (_, "-", e0, e1) -> add (to_linexpr e0) (mul (to_linexpr e1) (-1))
  | Binary (_, "*", e, Prim (_, Int a)) -> mul (to_linexpr e) a
  | Binary (_, "*", Prim (_, Int a), e) -> mul (to_linexpr e) a
  | Unary (_, "-", e) -> mul (to_linexpr e) (-1)
  | Prim (_, Int i) -> { terms = []; constant = i }
  | _ -> Errors.fatal' (position_of_expression e) "This expression is not linear"


(** Conversion of expressions to boolean expressions.
    Any non boolean expression raises an error. *)
let rec to_bexpr (e: expression): (linexpr * string) bexpr =
  match e with
  | Binary (_, "&&", e0, e1) -> band (to_bexpr e0) (to_bexpr e1)
  | Binary (_, "||", e0, e1) -> bor (to_bexpr e0) (to_bexpr e1)
  | Binary (p, op, e0, e1) ->
      let le0 = to_linexpr e0
      and le1 = to_linexpr e1 in
      Atom (comp p op le0 le1)
  | Prim (_, Bool b) -> if b then Top else Bot
  | _ -> Errors.fatal' (position_of_expression e) "This expression is not boolean"

(** Construction of the graph. *)

(* Initialize a label in the graph.
   The boolean flag identifies loop headers. *)
let init_label (l: label) (cfg: cfg): cfg =
  LabelMap.add l {
    loop_header = false;
    do_widen = false;
    successors = [];
    predecessors = []
  } cfg

(* Insert a command in the graph. *)
let insert (l0: label) (c,l1: command * label) (cfg: cfg): unit =
  try
    let info0 = LabelMap.find l0 cfg
    and info1 = LabelMap.find l1 cfg in
    info1.predecessors <- l0::info1.predecessors;
    info0.successors <- (c,l1)::info0.successors
  with Not_found ->
    assert false

(* Mark a label as loop header. *)
let set_loop_header (l: label) (cfg: cfg): unit =
  try
    let info = LabelMap.find l cfg in
    info.loop_header <- true
  with Not_found ->
    assert false

(* Current labels of destination of break and continue expressions. *)
let break_label = ref undefined_label
let continue_label = ref undefined_label

(* Exception to be thrown after a break or continue statement. *)
exception Loop_interruption of cfg

(* Insert an instruction in the graph. *)
let rec insert_instruction (l0: label) (l1: label) (instr: instruction) (cfg: cfg): cfg =
  match instr with
  (* This case corresponds to a simple variable declaration (no initialization).
     TODO: find a way to remove the useless branching condition. *)
  | Declare (p, t, x, None) ->
      create_var x t;
      insert l0 (Cond Top, l1) cfg;
      cfg
  | Declare (p, t, x, Some e) ->
      create_var x t;
      insert_instruction l0 l1 (Syntax.Assign (p, x, e)) cfg
  | Syntax.Assign (p, x, e) ->
      let x' = find_var p x in
      insert l0 (Assign (x', to_linexpr e), l1) cfg;
      cfg
  | If (p, e, (p0, b0), None) ->
      let l2 = new_label (start_of_position p0) in
      let cfg = init_label l2 cfg in
      let e = to_bexpr e in
      insert l0 (Cond e, l2) cfg;
      insert l0 (Cond (bnot rev e), l1) cfg;
      in_scope (fun _ -> insert_block l2 l1 b0 cfg)
  | If (p, e, (p0,b0), Some (p1, b1)) ->
      let l2 = new_label (start_of_position p0)
      and l3 = new_label (start_of_position p1) in
      let cfg = init_label l3 (init_label l2 cfg) in
      let e = to_bexpr e in
      insert l0 (Cond e, l2) cfg;
      insert l0 (Cond (bnot rev e), l3) cfg;
      let cfg = in_scope (fun _ -> insert_block l3 l1 b1 cfg) in
      in_scope (fun _ -> insert_block l2 l1 b0 cfg)
  | While (p, e, (p',b)) ->
      let l2 = new_label (start_of_position p') in
      let cfg = init_label l2 cfg in
      let e = to_bexpr e in
      set_loop_header l0 cfg;
      insert l0 (Cond e, l2) cfg;
      insert l0 (Cond (bnot rev e), l1) cfg;
      break_label := l1;
      continue_label := l0;
      in_scope (fun _ -> insert_block l2 l0 b cfg)
  (* This case correspond to blocks that contain only one break or continue statement.
     TODO: simplify this and remove the useless branching condition. *)
  | Break p -> insert l0 (Cond Top, !break_label) cfg; cfg
  | Continue p -> insert l0 (Cond Top, !continue_label) cfg; cfg

(* Insert a block in the graph. *)
and insert_block (l0: label) (l1: label) (is: instruction list) (cfg: cfg): cfg =
  match is with
  | [] -> cfg
  | [i] -> insert_instruction l0 l1 i cfg
  | i::(Break _)::_ -> insert_instruction l0 !break_label i cfg
  | i::(Continue _)::_ -> insert_instruction l0 !continue_label i cfg
  | i::is ->
      let l2 = new_label (end_of_position (position_of_instruction i)) in
      let cfg = init_label l2 cfg in
      let cfg = insert_instruction l0 l2 i cfg in
      insert_block l2 l1 is cfg


(** Main function, build the control flow graph of a program. *)
let build_cfg (p,b: block): cfg =
  reset_labels ();
  let l0 = new_label (start_of_position p)
  and l1 = new_label (end_of_position p) in
  let cfg = init_label l0 (init_label l1 LabelMap.empty) in
  insert_block l0 l1 b cfg


(** Printing. *)

let string_of_command (c: command): string =
  match c with
  | Cond e ->
      let string_of_a (e,op) = string_of_linexpr e ^ " " ^ op ^ " 0" in
      string_of_bexpr string_of_a e ^ " ?"
  | Assign (x,e) -> x ^ " = " ^ string_of_linexpr e

let print_cfg (cfg: cfg): unit =
  LabelMap.iter (fun l0 info ->
    print_string (string_of_label l0 ^ ":");
    print_newline ();
    List.iter (fun (c, l1) ->
      print_string ("  " ^ string_of_command c ^ " -> " ^ string_of_label l1);
      print_newline ()
    ) info.successors
  ) cfg

