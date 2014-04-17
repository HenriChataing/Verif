(** Primitive types. *)

type ptype =
    TypeInt
  | TypeFloat
  | TypeBool


(** Printing. *)
let string_of_ptype (t: ptype): string =
  match t with
  | TypeInt -> "int" | TypeBool -> "bool"
  | TypeFloat -> "float"

(** Return [true] iff [t0] is contained in [t1]. *)
let subtype (t0: ptype) (t1: ptype) =
  match t0, t1 with
  | TypeInt, TypeInt -> true | TypeInt, TypeFloat -> true
  | TypeFloat, TypeFloat -> true
  | TypeBool, TypeBool -> true
  | _ -> false


