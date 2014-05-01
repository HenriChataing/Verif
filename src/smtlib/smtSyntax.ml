(** Syntax of SMT-lib v2. *)

open Positions

(** Symbols. *)
type symbol = Symbol of string
type keyword = Keyword of string


(** Identifiers. *)
type identifier =
    Simple of symbol
  | Indexed of symbol * int list

(** Sorts. *)
type sort = Sort of identifier * sort list

(** Qualified identifiers. *)
type qidentifier =
    NonQualified of identifier
  | Qualified of identifier * sort

(** Type of primitives. *)
type primitive =
    Num of int
  | Dec of string
  | Bin of string
  | Hex of string
  | Str of string

(** Attributes. *)
type attribute_value =
    VPrim of primitive
  | VSym of symbol
  | VApp of attribute_value list

type attribute = keyword * attribute_value option

(** Type of terms. *)
type term =
    (* Group all the primitives together. *)
    Prim of position * primitive
    (* Identifiers. *)
  | Ident of position * qidentifier
    (* Existential quantifiers. *)
  | Exists of position * (symbol * sort) list * term
    (* Universal qantifiers. *)
  | Forall of position * (symbol * sort) list * term
    (* Let-bindings. *)
  | Let of position * (symbol * term) list * term
    (* Function applications. *)
  | App of position * qidentifier * term list
    (* Expressions with attributes. *)
  | Attribute of position * term * attribute list


(** Return the position of a term. *)
let position_of_term (t: term) =
  match t with
  | Prim (p, _) | Ident (p, _)
  | Exists (p, _, _) | Forall (p, _, _)
  | Let (p, _, _) | App (p, _, _)
  | Attribute (p, _, _) -> p


(** Type of standard SMTLib commands. *)
type command =
    SetLogic of position * symbol
  | DeclareFun of position * symbol * sort list * sort
  | DefineFun of position * symbol * (symbol * sort) list * sort * term
  | DeclareSort of position * symbol * int
  | DefineSort of position * symbol * symbol list * term
  | Assert of position * term
  | GetAssertions of position
  | CheckSat of position
  | GetProof of position
  | GetUnsatCore of position
  | GetValue of position * term list
  | GetAssignment of position
  | Push of position * int
  | Pop of position * int
  | GetOption of position * keyword
  | SetOption of position * attribute
  | GetInfo of position * keyword
  | SetInfo of position * attribute
  | Exit of position


