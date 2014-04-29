(** Definition and construction of the control flow graph. *)

open Positions

open Labels
open Literal
open Syntax
open Expressions
open Expr
open Bexpr
open Linexpr


(** Edges of the graph are labelled by commands. *)
type ('e, 'b) command =
  (* Conditional jump. *)
    Cond of 'b Bexpr.t
  (* Parallel assignment of variables. *)
  | Assign of (var * 'e) list


(** Description of the graph. *)

module LABEL = struct
  type t = label
  let compare l0 l1 = l0.id - l1.id
end

module LabelMap = Map.Make (LABEL)

type ('e, 'b) label_info = {
  position: Lexing.position;                               (* The position in the file. *)
  mutable loop_header: label option;                       (* Identify widening points. *)
  (* A condition to prove. Rather than the condition itself, its negation is kept. *)
  mutable goal_condition: ('b Bexpr.t) option;
  mutable successors: (('e, 'b) command * label) list;
  mutable predecessors: (('e, 'b) command * label) list
}

type 'e cfg = ('e, 'e * string) label_info LabelMap.t


(** Representation of the control flow graph. *)
type 'e t = {
  labels: (('e, 'e * string) label_info) array;  (* Definition of the graph. *)
  entry_point: label;                            (* Entry point of the graph. *)
  size: int
}


(** Construction of the graph. *)

(* Initialize a label in the graph.
   The boolean flag identifies loop headers. *)
let init_label (l: label) (cfg: 'e cfg): 'e cfg =
  LabelMap.add l {
    position = l.Labels.position;
    loop_header = None;
    goal_condition = None;
    successors = [];
    predecessors = []
  } cfg

(* Insert a command in the graph. *)
let insert (l0: label) (c,l1: ('e, 'e * string) command * label) (cfg: 'e cfg): unit =
  try
    let info0 = LabelMap.find l0 cfg
    and info1 = LabelMap.find l1 cfg in
    info1.predecessors <- (c, l0)::info1.predecessors;
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

(* Add a goal (negated) to a label. *)
let add_goal (l: label) (goal: ('e * string) Bexpr.t) (cfg: 'e cfg): unit =
  try
    let info = LabelMap.find l cfg in
    let goal = Bexpr.bnot Bexpr.rev goal in
    match info.goal_condition with
    | None -> info.goal_condition <- Some goal
    | Some (Disj bs) -> info.goal_condition <- Some (Disj (goal::bs))
    | Some b -> info.goal_condition <- Some (Disj [b; goal])
  with Not_found ->
    assert false

(* Current labels of destination of break and continue expressions. *)
let break_label = ref undefined_label
let continue_label = ref undefined_label

(* Exception to be thrown after a break or continue statement. *)
exception Loop_interruption of Expr.t cfg

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
      insert l0 (Assign [x, Linexpr.of_expr e], l1) cfg;
      cfg

  | If (p, e, (p0, b0), None) ->
      let e = Bexpr.of_expr e in
      let cfg = insert_branch l0 l1 (Cond e) (p0, b0) cfg in
      insert l0 (Cond (bnot rev e), l1) cfg;
      cfg 
  | If (p, e, (p0,b0), Some (p1, b1)) ->
      let e = Bexpr.of_expr e in
      let cfg = insert_branch l0 l1 (Cond e) (p0, b0) cfg in
      insert_branch l0 l1 (Cond (bnot rev e)) (p1, b1) cfg
  | While (p, e, (p',b)) ->
      let e = Bexpr.of_expr e in
      break_label := l1;
      continue_label := l0;
      set_loop_header l0 l1 cfg;
      insert l0 (Cond (bnot rev e), l1) cfg;
      insert_branch l0 l0 (Cond e) (p', b) cfg
  
  (* Assertions are turned into conditional jumps. *)
  | Assert (p, e) ->
      let e = Bexpr.of_expr e in
      insert l0 (Cond e, l1) cfg;
      cfg
  (* Goal conditions are inserted into the label's information. *)
  | Prove (p, e) ->
      let e = Bexpr.of_expr e in
      add_goal l0 e cfg;
      cfg

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
  (* Goal conditions are inserted into the label's information. *)
  | (Prove (p, e))::is ->
      add_goal l0 (Bexpr.of_expr e) cfg;
      insert_block l0 l1 is cfg
  | i::[Prove (p,e)] ->
      add_goal l1 (Bexpr.of_expr e) cfg;
      insert_instruction l0 l1 i cfg
  | [i] -> insert_instruction l0 l1 i cfg
  (* Intercept break and continue *)
  | i::(Break _)::_ -> insert_instruction l0 !break_label i cfg
  | i::(Continue _)::_ -> insert_instruction l0 !continue_label i cfg
  (* Intersect consecutive parallel assignements. *)
  | (Syntax.Assign (p,x,e))::is -> begin
    let rec fold_assignments xs es is =
      match is with
      | [] -> (List.combine xs es), []
      | (Syntax.Assign (p, x, e))::is' ->
          let le = Linexpr.of_expr e in
          let le = Linexpr.subs (List.combine xs es) le in
          fold_assignments (x::xs) (le::es) is'
      | _ -> (List.combine xs es), is
    in
    let ass, is = fold_assignments [x] [Linexpr.of_expr e] is in
    match is with
    | [] -> insert l0 (Assign ass, l1) cfg; cfg
    | i::_ ->
      let l2 = new_label (start_of_position (position_of_instruction i)) in
      let cfg = init_label l2 cfg in
      insert l0 (Assign ass, l2) cfg;
      insert_block l2 l1 is cfg
    end

  (* Remaining cases. *)
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
    position = Lexing.dummy_pos;
    loop_header = None;
    goal_condition = None;
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
  | Assign ((x,e)::xs) ->
      x.name ^ " = " ^ Linexpr.to_string e ^
      List.fold_left (fun s (x,e) ->
        s ^ "; " ^ x.name ^ " = " ^ Linexpr.to_string e
      ) "" xs
  | Assign [] -> ""

let print_cfg (cfg: Linexpr.t t): unit =
  let string_of_a (e,op) = Linexpr.to_string e ^ " " ^ op ^ " 0" in
  Array.iteri (fun l info ->
    let plbl = "L" ^ string_of_int l ^ ": " in
    let pspc = String.make (String.length plbl) ' ' in
    let lblok = ref false in
    begin match info.goal_condition with
    | None -> ()
    | Some e ->
        lblok := true;
        print_string plbl;
        print_string ("ngoal: " ^ Bexpr.to_string string_of_a e);
        print_newline ();
    end;
    List.iter (fun (c, l1) ->
      if !lblok then
        print_string pspc
      else begin
        lblok := true;
        print_string plbl
      end;
      print_string (string_of_command c ^ " -> " ^ string_of_label l1);
      print_newline ()
    ) info.successors
  ) cfg.labels

