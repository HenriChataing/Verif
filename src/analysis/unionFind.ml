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


(** Application to constant domain analysis. *)

exception Incoherent of var

type cvalue =
    Bot                   (* Unconstrained. *)
  | Const of Primitive.t  (* Equal to some primitive value. *)
  | Top                   (* Constrained, but value unknown. *)

let join (u: cvalue) (u': cvalue): cvalue =
  match u,u' with
  | Bot, _ -> u' | _, Bot -> u
  | Top, _ | _, Top -> Top
  | Const p, Const p' -> if p = p' then Const p else Top

(* States corresponding to predicates. *)
type cstate = cvalue uvar array


(* Make fresh union variables for the arguments of a predicate. *)
let make_state (xs: var list): cstate =
  Array.of_list (List.map (fun x -> make_uvar x Bot) xs)


(* Update the usage of an argument in a local state. *)
let settop (x: var) (lstate: cstate): unit =
  update lstate.(x.vid) (fun p ->
    match p with
    | Const _ -> p
    | _ -> Top)


(* Set a variable to a constant value. *)
let set (x: var) (p: Primitive.t) (lstate: cstate): unit =
  update lstate.(x.vid) (fun p' ->
    match p' with
    | Const p' when p <> p' -> raise (Incoherent x)
    | _ -> Const p)


(* Reset a local state. *)
let reset (lstate: cstate): unit =
  for i=0 to (Array.length lstate)-1 do
    lstate.(i).parent <- Utils.Left Bot
  done


(* Merge two equivalence classes == insert an equality. *)
let equals (x: var) (y: var) (lstate: cstate): unit =
  union ~join:join lstate.(x.vid) lstate.(y.vid)


(* Join the partitions of two local states (as coming from different clauses). *)
let join_states (lstate0: cstate) (lstate1: cstate) (dest: cstate): unit =
  let size = Array.length lstate0 in
  (* Reset the destination. *)
  for i=0 to size-1 do
    dest.(i).parent <- Utils.Left (join (value lstate0.(i)) (value lstate1.(i)))
  done;
  (* Derive the equivalence classes. *)
  for i=0 to size-2 do
    for j=i+1 to size-1 do
      (* i and j are in the same class iff they are in both states. *)
      if find lstate0.(i) == find lstate0.(j) &&
         find lstate1.(i) == find lstate1.(j) then
        union dest.(i) dest.(j)
    done
  done

(* Compare two states. *)
let eq_states (lstate0: cstate) (lstate1: cstate): bool =
  let eq = ref true in
  (* Compare the values. *)
  for i=0 to (Array.length lstate0)-1 do
    if value lstate0.(i) <> value lstate1.(i) then eq := false
  done;
  (* Compare the classes. *)
  if !eq then begin
    for i=0 to (Array.length lstate0)-2 do
      for j=i+1 to (Array.length lstate0)-1 do
        let same0 = find lstate0.(i) == find lstate0.(j)
        and same1 = find lstate1.(i) == find lstate1.(j) in
        if same0 <> same1 then eq := false
      done
    done
  end;
  !eq

