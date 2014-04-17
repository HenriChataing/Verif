(** Definition and construction of the control flow graph. *)

open Positions

open Labels
open Syntax
open Expressions
open Bexpr
open Linexpr


(** Edges of the graph are labelled by commands. *)
type ('e, 'b) command =
    Cond of 'b Bexpr.t
  | Assign of var * 'e


(** Description of the graph. *)

module LABEL = struct
  type t = label
  let compare l0 l1 = l0.id - l1.id
end

module LabelMap = Map.Make (LABEL)

type ('e, 'b) label_info = {
  mutable loop_header: label option;             (* Identify widening points. *)
  mutable do_widen: bool;                        (* When to perform the widening. *)
  mutable successors: (('e, 'b) command * label) list;
  mutable predecessors: label list
}

type 'e cfg = ('e, 'e * string) label_info LabelMap.t


(** Representation of the control flow graph. *)
type 'e t = {
  labels: (('e, 'e * string) label_info) array;  (* Definition of the graph. *)
  entry_point: label;                            (* Entry point of the graph. *)
  size: int
}


(** Conversion of expressions to linear expressions.
    Any non linear expression raises an error. *)
let rec to_linexpr (e: expression): Linexpr.t =
  match e with
  | Var (p, v) -> 
      { terms = [v.name, Int 1]; constant = Int 0 }
  | Binary (_, "+", e0, e1) -> add (to_linexpr e0) (to_linexpr e1)
  | Binary (_, "-", e0, e1) -> add (to_linexpr e0) (minus (to_linexpr e1))
  | Binary (_, "*", e, Prim (_, a)) -> mul (to_linexpr e) a
  | Binary (_, "*", Prim (_, a), e) -> mul (to_linexpr e) a
  | Unary (_, "-", e) -> minus (to_linexpr e)
  | Prim (_, a) -> { terms = []; constant = a }
  | _ -> Errors.fatal' (position_of_expression e) "This expression is not linear"


(** Conversion of expressions to boolean expressions.
    Any non boolean expression raises an error. *)
let rec to_bexpr (e: expression): (Linexpr.t * string) Bexpr.t =
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
let init_label (l: label) (cfg: 'e cfg): 'e cfg =
  LabelMap.add l {
    loop_header = None;
    do_widen = false;
    successors = [];
    predecessors = []
  } cfg

(* Insert a command in the graph. *)
let insert (l0: label) (c,l1: ('e, 'e * string) command * label) (cfg: 'e cfg): unit =
  try
    let info0 = LabelMap.find l0 cfg
    and info1 = LabelMap.find l1 cfg in
    info1.predecessors <- l0::info1.predecessors;
    info0.successors <- (c,l1)::info0.successors
  with Not_found ->
    assert false

(* Mark a label as loop header. *)
let set_loop_header (l: label) (lexit: label) (cfg: 'e cfg): unit =
  try
    let info = LabelMap.find l cfg in
    info.loop_header <- Some lexit
  with Not_found ->
    assert false

(* Current labels of destination of break and continue expressions. *)
let break_label = ref undefined_label
let continue_label = ref undefined_label

(* Exception to be thrown after a break or continue statement. *)
exception Loop_interruption of expression cfg

(* Insert an instruction in the graph. *)
let rec insert_instruction
    (l0: label) (l1: label)
    (instr: instruction)
    (cfg: Linexpr.t cfg): Linexpr.t cfg =
  match instr with
  | Declare (p, x, None) ->
      insert l0 (Cond Top, l1) cfg;
      cfg
  | Declare (p, x, Some e) ->
      insert_instruction l0 l1 (Syntax.Assign (p, x, e)) cfg
  | Syntax.Assign (p, x, e) ->
      insert l0 (Assign (x, to_linexpr e), l1) cfg;
      cfg

  | If (p, e, (p0, b0), None) ->
      let e = to_bexpr e in
      let cfg = insert_branch l0 l1 (Cond e) (p0, b0) cfg in
      insert l0 (Cond (bnot rev e), l1) cfg;
      cfg 
  | If (p, e, (p0,b0), Some (p1, b1)) ->
      let e = to_bexpr e in
      let cfg = insert_branch l0 l1 (Cond e) (p0, b0) cfg in
      insert_branch l0 l1 (Cond (bnot rev e)) (p1, b1) cfg
  | While (p, e, (p',b)) ->
      let e = to_bexpr e in
      break_label := l1;
      continue_label := l0;
      set_loop_header l0 l1 cfg;
      insert l0 (Cond (bnot rev e), l1) cfg;
      insert_branch l0 l0 (Cond e) (p', b) cfg

  | Break p -> insert l0 (Cond Top, !break_label) cfg; cfg
  | Continue p -> insert l0 (Cond Top, !continue_label) cfg; cfg

(* Insert a branch condition. *)
and insert_branch
    (l0: label) (l1: label)
    (c: (Linexpr.t, Linexpr.t * string) command)
    (p, b: block)
    (cfg: Linexpr.t cfg): Linexpr.t cfg =
  match b with
  | (Break _)::_ -> insert l0 (c, !break_label) cfg; cfg
  | (Continue _)::_ -> insert l0 (c, !continue_label) cfg; cfg
  | _ ->
      let l2 = new_label (start_of_position p) in
      let cfg = init_label l2 cfg in
      let cfg = insert_block l2 l1 b cfg in
      insert l0 (c, l2) cfg; cfg

(* Insert a block in the graph. *)
and insert_block (l0: label) (l1: label) (is: instruction list) (cfg: Linexpr.t cfg): Linexpr.t cfg =
  match is with
  | [] -> cfg
  | (Declare (p, x, None))::is ->
      insert_block l0 l1 is cfg
  | [i] -> insert_instruction l0 l1 i cfg
  | i::(Break _)::_ -> insert_instruction l0 !break_label i cfg
  | i::(Continue _)::_ -> insert_instruction l0 !continue_label i cfg
  | i::is ->
      let l2 = new_label (end_of_position (position_of_instruction i)) in
      let cfg = init_label l2 cfg in
      let cfg = insert_instruction l0 l2 i cfg in
      insert_block l2 l1 is cfg


(** Main function, build the control flow graph of a program. *)
let build_cfg (p,b: block): Linexpr.t t =
  reset_labels ();
  let l0 = new_label (start_of_position p)
  and l1 = new_label (end_of_position p) in
  let cfg = init_label l0 (init_label l1 LabelMap.empty) in
  let cfg = insert_block l0 l1 b cfg in
  (* Make it an array. *)
  let arr = Array.make (LabelMap.cardinal cfg) {
    loop_header = None;
    do_widen = false;
    successors = [];
    predecessors = []
  } in
  LabelMap.iter (fun l info ->
    arr.(l.id) <- info) cfg;
  { labels = arr; entry_point = l0; size = LabelMap.cardinal cfg }


(** Printing. *)

let string_of_command (c: (Linexpr.t, Linexpr.t * string) command): string =
  match c with
  | Cond e ->
      let string_of_a (e,op) = Linexpr.to_string e ^ " " ^ op ^ " 0" in
      Bexpr.to_string string_of_a e ^ " ?"
  | Assign (x,e) -> x.name ^ " = " ^ Linexpr.to_string e

let print_cfg (cfg: Linexpr.t t): unit =
  Array.iteri (fun l info ->
    print_string ("L" ^ string_of_int l ^ ":");
    print_newline ();
    List.iter (fun (c, l1) ->
      print_string ("  " ^ string_of_command c ^ " -> " ^ string_of_label l1);
      print_newline ()
    ) info.successors
  ) cfg.labels

