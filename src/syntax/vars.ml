(** Common variables. *)

open Types
open Positions

(* Variable counter, used to generate ids. *)
let counter = ref 0

(** Reset the variable counter. *)
let reset (): unit =
  counter := 0


(** Variables. *)
type var = {
  vid: int;               (* Unique identifier. *)
  name: string;           (* The name of the variable. *)
  pos: position;          (* The place of declaration of the variable. *)
  mutable typ: typ    (* The type of declaration. *)
}

(** Dummy variable. *)
let dummy_var: var = {
  vid = 0; name = "";
  pos = undefined_position;
  typ = TyBool
}

(** Create a new named variable with a fresh id. *)
let create_var
    ?(pos: position = undefined_position)
    ?(typ: typ = TyBool)
    (n: string): var =
  let id = !counter in
  counter := id+1;
  { vid = id; name = n; pos = pos; typ = typ } 


(** The context of existing variables. *)
type context = var list

(** Retrieve a variable from the context. *)
let from_context
    (ctx: context)
    ?(pos: position = undefined_position)
    (n: string): var =
  try
    List.find (fun x -> x.name = n) ctx
  with Not_found -> Errors.fatal' pos ("Undefined variable " ^ n)

