(** Primitive types. *)

type ptype =
    TypeInt
  | TypeFloat
  | TypeBool
  | TypeArrow of ptype list * ptype


(** Printing. *)
let rec string_of_ptype (t: ptype): string =
  match t with
  | TypeInt -> "int" | TypeBool -> "bool"
  | TypeFloat -> "float"
  | TypeArrow (arg, typ) ->
      let parg = List.map (fun a ->
        match a with
        | TypeArrow _ -> "(" ^ string_of_ptype a ^ ")"
        | _ -> string_of_ptype a
      ) arg in
      List.fold_right (fun a s -> a ^ " -> " ^ s) parg (string_of_ptype typ)

(** Return [true] iff [t0] is contained in [t1]. *)
let subtype (t0: ptype) (t1: ptype) =
  match t0, t1 with
  | TypeInt, TypeInt -> true | TypeInt, TypeFloat -> true
  | TypeFloat, TypeFloat -> true
  | TypeBool, TypeBool -> true
  | _ -> false  (* Subtyping is not implemented for arrow types. *)


