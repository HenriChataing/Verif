(** Syntax of SMT-lib v2. *)

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

(** Type of expressions. *)
type expression =
    (* Group all the primitives together. *)
    Prim of primitive
    (* Identifiers. *)
  | Ident of qidentifier
    (* Existential quantifiers. *)
  | Exists of (symbol * sort) list * expression
    (* Universal qantifiers. *)
  | Forall of (symbol * sort) list * expression
    (* Let-bindings. *)
  | Let of (symbol * expression) list * expression
    (* Function applications. *)
  | App of qidentifier * expression list
    (* Expressions with attributes. *)
  | Attribute of expression * attribute list


(** Type of standard SMTLib commands. *)
type command =
    SetLogic of symbol
  | DeclareFun of symbol * sort list * sort
  | DefineFun of symbol * (symbol * sort) list * sort * expression
  | DeclareSort of symbol * int
  | DefineSort of symbol * symbol list * expression
  | Assert of expression
  | GetAssertions
  | CheckSat
  | GetProof
  | GetUnsatCore
  | GetValue of expression list
  | GetAssignment
  | Push of int
  | Pop of int
  | GetOption of keyword
  | SetOption of keyword * unit (* TODO Change the type of the operand. *)
  | GetInfo of keyword
  | SetInfo of keyword * unit (* TODO Change the type of the operand. *)
  | Exit


