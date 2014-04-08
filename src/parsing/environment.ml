(** Variables and generation of unique variable names. This module also
    includes means of constructing scopes. *)


(** Record the type of definition of each variable. *)
let variable_types: (string * Syntax.ptype) list ref = ref []

(** The scopes forming the environment. *)
let environment: (string * string) list list ref = ref [[]]

(** Generation of variables. A counter is associated with each prefix. *)
let variable_counters: (string * int) list ref = ref []


exception Found of string

(** Check for the existance of a variable in the current environment. *)
let find_var (p: Positions.position) (v: string): string =
  try
    List.iter (fun scope -> 
      try
        raise (Found (List.assoc v scope))
      with Not_found -> ()
    ) !environment;
    Errors.fatal' p ("Undefined variable " ^ v)
  with Found v' -> v' 


(** Create a new variable. *)
let create_var (v: string) (t: Syntax.ptype): unit =
  let v' = try
    let c = List.assoc v !variable_counters in
    variable_counters := List.map (fun (v', c') ->
      if v = v' then (v, c+1) else (v',c')
    ) !variable_counters;
    v ^ "#" ^ string_of_int c
  with Not_found ->
    variable_counters := (v, 1)::!variable_counters;
    v
  in
  variable_types := (v',t)::!variable_types;
  match !environment with
  | [] -> ()
  | s::ss -> environment := ((v,v')::s)::ss
 

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
let typed_variables (): (string * Syntax.ptype) list =
  !variable_types

