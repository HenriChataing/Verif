(** Union find on variables. *)

open Utils
open Vars


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


(** Application to a constant domain. *)

type cvalue =
    Bot                   (* Unconstrained. *)
  | Const of Primitive.t  (* Equal to some primitive value. *)
  | Top                   (* Constrained, but value unknown. *)

let join (u: cvalue) (u': cvalue): cvalue =
  match u,u' with
  | Bot, _ -> u' | _, Bot -> u
  | Top, _ | _, Top -> Top
  | Const p, Const p' -> if p = p' then Const p else Top

type cstate = cvalue uvar array


(* Make fresh union variables for the arguments of a predicate. *)
let make_uvars (xs: var list): cstate =
  Array.of_list (List.map (fun x -> make_uvar x Bot) xs)


(* Update the usage of an argument in a local state. *)
let update_state (x: var) (v: cvalue) (lstate: cstate): unit =
  update lstate.(x.vid) (join v)


(* Reset a local state. *)
let reset (lstate: cstate): unit =
  for i=0 to (Array.length lstate)-1 do
    lstate.(i).parent <- Utils.Left Bot
  done


(* Merge two equivalence classes == insert an equality. *)
let equals (x: var) (y: var) (lstate: cstate): unit =
  union ~join:join lstate.(x.vid) lstate.(y.vid)



