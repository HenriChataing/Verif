(** Variables and generation of unique variable names. This module also
    includes means of constructing scopes. *)

open Syntax
open Expressions
open Expr
open Types
open Literal


(** Record the type of definition of each variable. *)
let variable_types: (string * ptype) list ref = ref []

(** The scopes forming the environment. *)
let environment: (string * var) list list ref = ref [[]]

(** Generation of variables. A counter is associated with each prefix. *)
let variable_counters: (string * int) list ref = ref []


exception Found of var

(** Check for the existance of a variable in the current environment. *)
let find_var (p: Positions.position) (v: string): var =
  try
    List.iter (fun scope -> 
      try
        raise (Found (List.assoc v scope))
      with Not_found -> ()
    ) !environment;
    Errors.fatal' p ("Undefined variable " ^ v)
  with Found v' -> v' 


(** Create a new variable. *)
let create_var (v: string) (t: ptype): var =
  let v' = try
    let c = List.assoc v !variable_counters in
    variable_counters := List.map (fun (v', c') ->
      if v = v' then (v, c+1) else (v',c')
    ) !variable_counters;
    { name = v ^ "#" ^ string_of_int c; ptype = t }
  with Not_found ->
    variable_counters := (v, 1)::!variable_counters;
    { name = v; ptype = t }
  in
  variable_types := (v'.name,t)::!variable_types;
  (match !environment with
  | [] -> ()
  | s::ss -> environment := ((v,v')::s)::ss);
  v'

(** Open a new scope. *)
let open_scope (): unit =
  environment := []::!environment


(** Close the top scope. *)
let close_scope (): unit =
  match !environment with
  | [] -> ()
  | _::ss -> environment := ss


(** Do some operation in a temporary scope. This is the same as doing
  open_scope (); let result = f; close_scope (); result *)
let in_scope (f: unit -> 'a): 'a =
  open_scope ();
  let result = f () in
  close_scope ();
  result


(** Return the list of all typed variables. *)
let typed_variables (): (string * ptype) list =
  !variable_types


(* Raise an exception if the types differ. *)
let checktype (p: Positions.position) (t: ptype) (t': ptype) =
  if not (subtype t t') then Errors.fatal' p
    ("This expression has type " ^ string_of_ptype t ^
    "\n but an expression was expected of type " ^ string_of_ptype t')

(** Type checking and scope analysis, in an expression. *)
let rec typecheck_expr (e: Expr.t) (expected: ptype): Expr.t =
  match e with
  | Var (p, x) -> let x' = find_var p x.name in
      checktype p x'.ptype expected;
      Var (p, x')
  | Prim (p, l) ->
      checktype p (typeof l) expected;
      e
  | Binary (p, ("+" as op), e0, e1) | Binary (p, ("-" as op), e0, e1)
  | Binary (p, ("*" as op), e0, e1) ->
      checktype p TypeInt expected;
      let e0 = typecheck_expr e0 expected
      and e1 = typecheck_expr e1 expected in
      Binary (p, op, e0, e1)
  | Binary (p, ("==" as op), e0, e1) | Binary (p, ("!=" as op), e0, e1)
  | Binary (p, ("<=" as op), e0, e1) | Binary (p, (">=" as op), e0, e1)
  | Binary (p, ("<" as op), e0, e1) | Binary (p, (">" as op), e0, e1) ->
      checktype p TypeBool expected;
      let e0 = typecheck_expr e0 TypeFloat
      and e1 = typecheck_expr e1 TypeFloat in
      Binary (p, op, e0, e1)
  | Binary (p, ("&&" as op), e0, e1) | Binary (p, ("||" as op), e0, e1) ->
      checktype p TypeBool expected;
      let e0 = typecheck_expr e0 TypeBool
      and e1 = typecheck_expr e1 TypeBool in
      Binary (p, op, e0, e1)
  | Unary (p, "-", e) ->
      checktype p TypeInt expected;
      let e = typecheck_expr e expected in
      Unary (p, "-", e)
  | _ -> e

(** Type checking and scope analysis, in an instruction. *)
let rec typecheck_instruction (i: instruction): instruction =
  match i with
  | Declare (p, x, None) ->
      let x' = create_var x.name x.ptype in
      Declare (p, x', None)
  | Declare (p, x, Some e) ->
      let e' = typecheck_expr e x.ptype
      and x' = create_var x.name x.ptype in
      Declare (p, x', Some e')
  | Assign (p, x, e) ->
      let x' = find_var p x.name in
      let e' = typecheck_expr e x'.ptype in
      Assign (p, x', e')
  | If (p, e, (p0, b0), None) ->
      let e' = typecheck_expr e TypeBool
      and b0' = in_scope (fun _ -> typecheck_block b0) in
      If (p, e', (p0, b0'), None)
  | If (p, e, (p0, b0), Some (p1, b1)) ->
      let e' = typecheck_expr e TypeBool
      and b0' = in_scope (fun _ -> typecheck_block b0)
      and b1' = in_scope (fun _ -> typecheck_block b1) in
      If (p, e', (p0, b0'), Some (p1, b1'))
  | While (p, e, (p0, b0)) ->
      let e' = typecheck_expr e TypeBool
      and b0' = in_scope (fun _ -> typecheck_block b0) in
      While (p, e', (p0, b0'))
  | _ -> i

(** Type checking and scope analysis of a block. *)
and typecheck_block (is: instruction list): instruction list =
  match is with
  | [] -> []
  | (Break p)::_ -> [Break p]
  | (Continue p)::_ -> [Continue p]
  | i::is ->
      let i' = typecheck_instruction i
      and is' = typecheck_block is in
      i'::is'

(** Type checking and scope analysis of a program. *)
let typecheck (p,b: block): block =
  let b' = typecheck_block b in
  p,b'

 
