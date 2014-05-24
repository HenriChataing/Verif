(** Common variables. *)

open Utils
open Types
open Positions

(** Variables. *)
type var = {
  vid: int;               (* Unique identifier. *)
  name: string;           (* The name of the variable. *)
  pos: position;          (* The place of declaration of the variable. *)
  mutable typ: typ        (* The type of declaration. *)
}

(** Dummy variable. *)
let dummy_var: var = {
  vid = 0; name = "";
  pos = undefined_position;
  typ = TyBool
}


(** A variable pool, used to generate freshly named variables. *)
type pool = { mutable prefixes: (string * int ref) list }

(* Fix the delimitor. *)
let delimitor = "$"

(* Variable pool, used to generate fresh names. *)
let pool = { prefixes = [] }


(** Create a fresh variable, with the given prefix. Additional
  information can be added to the variable using optional arguments. *)
let fresh_var
    ?(pos: position = undefined_position)
    ?(typ: typ = TyBool)
    ?(name: string = "")
    (p: string): var =
  let nm = if name = "" then p else name in
  let id = try
      let nr = List.assoc p pool.prefixes in
      incr nr; !nr-1
    with Not_found -> begin
      pool.prefixes <- (p, ref 1)::pool.prefixes; 0
    end in
  { name = nm; pos = pos; typ = typ; vid = id }


(** Name with index appended. *)
let vname (v: var): string =
  v.name ^ delimitor ^ string_of_int v.vid

(** By convention, argument names begin with
  the character 'A', so they can be identified. *)
let isarg (v: var): bool =
  v.name.[0] = 'A'

(** The context of existing variables. *)
type context = (string * var) list

(** Retrieve a variable from the context. *)
let from_context
    (ctx: context)
    ?(pos: position = undefined_position)
    (n: string): var =
  try
    List.assoc n ctx
  with Not_found -> Errors.fatal' pos ("Undefined variable " ^ n)


(** Union find on variables. *)

(** Variables used in the union find algorithm. *)
type 'a uvar = {
  var: var;
  mutable rank: int;
  mutable parent: ('a, 'a uvar) either
}

let make_uvar (x: var) (v: 'a): 'a uvar =
  { var = x;
    rank = 0;
    parent = Left v;
  }

let rec find (x: 'a uvar): 'a uvar =
  match x.parent with
  | Left _ -> x
  | Right p ->
      let r = find p in
      x.parent <- Right r;
      r

let rec update (x: 'a uvar) (f: 'a -> 'a): unit =
  match x.parent with
  | Left v -> x.parent <- Left (f v)
  | Right p ->
      let r = find p in
      update r f;
      x.parent <- Right r

let rec value (x: 'a uvar): 'a =
  match x.parent with
  | Left v -> v
  | Right p ->
      let r = find p in
      x.parent <- Right r;
      value r

let union ?(join: 'a -> 'a -> 'a = fun x _ -> x) (x: 'a uvar) (y: 'a uvar): unit =
  let rx = find x
  and ry = find y in
  let v =
    match rx.parent, ry.parent with
    | Left vx, Left vy -> join vx vy
    | _ -> assert false
  in
  if rx == ry then
    ()
  else if rx.rank < ry.rank then begin
    rx.parent <- Right ry;
    ry.parent <- Left v;
    ry.rank <- ry.rank + 1
  end else begin
    ry.parent <- Right rx;
    rx.parent <- Left v;
    rx.rank <- rx.rank + 1
  end

