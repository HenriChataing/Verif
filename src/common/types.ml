(** Primitive types. *)

type typ =
    TyInt
  | TyFloat
  | TyBool
  | TyArrow of typ list * typ

(** Printing. *)
let rec string_of_typ (t: typ): string =
  match t with
  | TyInt -> "int" | TyBool -> "bool"
  | TyFloat -> "float"
  | TyArrow (arg, typ) ->
      let parg = List.map (fun a ->
        match a with
        | TyArrow _ -> "(" ^ string_of_typ a ^ ")"
        | _ -> string_of_typ a
      ) arg in
      List.fold_right (fun a s -> a ^ " -> " ^ s) parg (string_of_typ typ)

(** Return [true] iff [t0] is contained in [t1]. *)
let subtype (t0: typ) (t1: typ) =
  match t0, t1 with
  | TyInt, TyInt -> true | TyInt, TyFloat -> true
  | TyFloat, TyFloat -> true
  | TyBool, TyBool -> true
  | _ -> false  (* Subtyping is not implemented for arrow types. *)

(** Return the smallest type containing both proposed types. *)
let join (t0: typ) (t1: typ) =
  match t0, t1 with
  | TyArrow _, _ | _, TyArrow _ -> Errors.fatal [] "Join is not implemented on arrow types."
  | TyFloat, _ | _, TyFloat -> TyFloat
  | TyInt, _ | _, TyInt -> TyInt
  | _ -> TyBool

(** Conversion to Apron types. *)
let typ_of_typ (t: typ): Apron.Texpr1.typ =
  match t with
  | TyInt -> Apron.Texpr1.Int
  | TyFloat -> Apron.Texpr1.Real
  | _ -> Errors.fatal [] "This type has no Apron equivalent."

(** Return the number of arguments. *)
let nargs (t: typ): int =
  match t with
  | TyArrow (args, _) -> List.length args
  | _ -> 0

