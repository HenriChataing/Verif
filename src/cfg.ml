(** Definition and construction of the control flow graph. *)

open Positions

open Labels
open Syntax
open Environment


(** Edges of the graph are labelled by commands. *)
type command =
    Cond of expression
  | Assign of string * expression


(** Description of the graph. *)

module LABEL = struct
  type t = label
  let compare l0 l1 = l0.id - l1.id
end

module LabelMap = Map.Make (LABEL)

type cfg = ((command * label) list) LabelMap.t


(** Conversion of expressions. *)

let rec convert (e: expression): expression =
  match e with
  | Var (p, v) -> Var (p, find_var p v)
  | Binary (p, op, e0, e1) -> Binary (p, op, convert e0, convert e1)
  | Unary (p, op, e) -> Unary (p, op, convert e)
  | _ -> e


(** Construction of the graph. *)

(* Insert a command in the graph. *)
let insert (l: label) (c: command * label) (cfg: cfg): cfg =
  try
    let cs = LabelMap.find l cfg in
    LabelMap.add l (c::cs) cfg
  with Not_found ->
    LabelMap.add l [c] cfg

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
      insert l0 (Cond btrue, l1) cfg
  | Declare (p, t, x, Some e) ->
      create_var x t;
      insert_instruction l0 l1 (Syntax.Assign (p, x, e)) cfg
  | Syntax.Assign (p, x, e) ->
      let x' = find_var p x in
      insert l0 (Assign (x', convert e), l1) cfg
  | If (p, e, (p0, b0), None) ->
      let l2 = new_label (start_of_position p0) in
      let e = convert e in
      let cfg = insert l0 (Cond e, l2) cfg in
      let cfg = insert l0 (Cond (bnot e), l1) cfg in
      in_scope (fun _ -> insert_block l2 l1 b0 cfg)
  | If (p, e, (p0,b0), Some (p1, b1)) ->
      let l2 = new_label (start_of_position p0)
      and l3 = new_label (start_of_position p1) in
      let e = convert e in
      let cfg = insert l0 (Cond e, l2) cfg in
      let cfg = insert l0 (Cond (bnot e), l3) cfg in
      let cfg = in_scope (fun _ -> insert_block l3 l1 b1 cfg) in
      in_scope (fun _ -> insert_block l2 l1 b0 cfg)
  | While (p, e, (p',b)) ->
      let l2 = new_label (start_of_position p') in
      let e = convert e in
      let cfg = insert l0 (Cond e, l2) cfg in
      let cfg = insert l0 (Cond (bnot e), l1) cfg in
      break_label := l1;
      continue_label := l0;
      in_scope (fun _ -> insert_block l2 l0 b cfg)
  (* This case correspond to blocks that contain only one break or continue statement.
     TODO: simplify this and remove the useless branching condition. *)
  | Break p -> insert l0 (Cond btrue, !break_label) cfg
  | Continue p -> insert l0 (Cond btrue, !continue_label) cfg

(* Insert a block in the graph. *)
and insert_block (l0: label) (l1: label) (is: instruction list) (cfg: cfg): cfg =
  match is with
  | [] -> cfg
  | [i] -> insert_instruction l0 l1 i cfg
  | i::(Break _)::_ -> insert_instruction l0 !break_label i cfg
  | i::(Continue _)::_ -> insert_instruction l0 !continue_label i cfg
  | i::is ->
      let l2 = new_label (end_of_position (position_of_instruction i)) in
      let cfg = insert_instruction l0 l2 i cfg in
      insert_block l2 l1 is cfg


(** Main function, build the control flow graph of a program. *)
let build_cfg (p,b: block): cfg =
  reset_labels ();
  let l0 = new_label (start_of_position p)
  and l1 = new_label (end_of_position p) in
  insert_block l0 l1 b LabelMap.empty


(** Printing. *)

let string_of_command (c: command): string =
  match c with
  | Cond e -> string_of_expression e ^ " ?"
  | Assign (x,e) -> x ^ " = " ^ string_of_expression e

let print_cfg (cfg: cfg): unit =
  LabelMap.iter (fun l0 cs ->
    print_string (string_of_label l0 ^ ":");
    print_newline ();
    List.iter (fun (c, l1) ->
      print_string ("  " ^ string_of_command c ^ " -> " ^ string_of_label l1);
      print_newline ()
    ) cs
  ) cfg

