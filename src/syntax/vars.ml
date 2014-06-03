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

