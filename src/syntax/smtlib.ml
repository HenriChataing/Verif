(** Syntax of SmtLib v2. *)

open Positions

(** Symbols. *)
type symbol = string
type keyword = string


(** Sorts. *)
type sort = Sort of identifier * sort list

(** Indexed identifiers. *)
and identifier = {
  symbol: symbol;
  indexes: int list;
  mutable sort: sort option
}

(** Print a sort. *)
let rec string_of_sort (Sort (id, ss): sort): string =
  match ss with
  | [] -> string_of_identifier id
  | _ ->
      "(" ^ string_of_identifier id ^
      List.fold_left (fun s st -> s ^ " " ^ string_of_sort st) "" ss ^ ")"

(** Print an identifier, abiding by the syntax of SmtLib. *)
and string_of_identifier (id: identifier): string =
  let unsorted =
    match id.indexes with
    | [] -> id.symbol
    | ixs ->
        "(_" ^ List.fold_left (fun s ix -> s ^ " " ^ string_of_int ix) id.symbol ixs ^ ")"
  in
  match id.sort with
  | None -> unsorted
  | Some s -> "(as " ^ unsorted ^ " " ^ string_of_sort s ^ ")"

(** Create a named, unindexed and unsorted identifier. *)
let make_id (s: string): identifier =
  { symbol = s;
    indexes = [];
    sort = None
  }


(** Type of primitives. *)
type primitive =
    Num of int
  | Dec of string
  | Bin of string
  | Hex of string
  | Str of string

(** Print a primitive value. *)
let string_of_primitive (p: primitive): string =
  match p with
  | Num n -> string_of_int n
  | Dec d -> d | Bin b -> b | Hex h -> h
  | Str s -> "\"" ^ s ^ "\""


(** Attributes. *)
type attribute_value =
    VPrim of primitive
  | VSym of symbol
  | VApp of attribute_value list

type attribute = keyword * attribute_value option

(** Print an attribute, abiding by the syntax of SmtLib. *)
let string_of_attribute (k,v: attribute): string =
  let rec string_of_val (v: attribute_value): string =
    match v with
    | VPrim p -> string_of_primitive p
    | VSym s -> s
    | VApp vs -> "(" ^ List.fold_left (fun s v -> s ^ " " ^ string_of_val v) "" vs ^ " )"
  in
  match v with
  | None -> k
  | Some v -> k ^ " " ^ string_of_val v


(** Definition of terms. *)
type term =
    (* Group all the primitives together. *)
    Prim of position * primitive
    (* Identifiers. *)
  | Ident of position * identifier
    (* Existential quantifiers. *)
  | Exists of position * (symbol * sort) list * term
    (* Universal qantifiers. *)
  | Forall of position * (symbol * sort) list * term
    (* Let-bindings. *)
  | Let of position * (symbol * term) list * term
    (* Function applications. *)
  | App of position * identifier * term list
    (* Expressions with attributes. *)
  | Attribute of position * term * attribute list

(** Return the position of a term. *)
let position_of_term (t: term) =
  match t with
  | Prim (p, _) | Ident (p, _)
  | Exists (p, _, _) | Forall (p, _, _)
  | Let (p, _, _) | App (p, _, _)
  | Attribute (p, _, _) -> p

(** Print a term. The output respects the syntax of smtlib. *)
let rec string_of_term (t: term): string =
  match t with
  | Prim (_, p) -> string_of_primitive p
  | Ident (_, qid) -> string_of_identifier qid
  | Exists (_, vs, t) ->
    "(exists (" ^ List.fold_left (fun s (v, st) ->
      s ^ " (" ^ v ^ " " ^ string_of_sort st ^ ")") "" vs ^ " ) " ^ string_of_term t ^ ")"
  | Forall (_, vs, t) ->
    "(forall (" ^ List.fold_left (fun s (v, st) ->
      s ^ " (" ^ v ^ " " ^ string_of_sort st ^ ")") "" vs ^ " ) " ^ string_of_term t ^ ")"
  | Let (_, vs, t) ->
    "(let (" ^ List.fold_left (fun s (v, t) ->
      s ^ " (" ^ v ^ " " ^ string_of_term t ^ ")") "" vs ^ " ) " ^ string_of_term t ^ ")"
  | App (_, qid, []) ->
      string_of_identifier qid
  | App (_, qid, ts) ->
    "(" ^ string_of_identifier qid ^
    List.fold_left (fun s t -> s ^ " " ^ string_of_term t) "" ts ^ ")"
  | Attribute (_,t,_) -> string_of_term t


(** Type of standard SmtLib commands. *)
type command =
    SetLogic of position * symbol
  | DeclareFun of position * symbol * sort list * sort
  | DefineFun of position * symbol * (symbol * sort) list * sort * term
  | DeclareSort of position * symbol * int
  | DefineSort of position * symbol * symbol list * sort
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

(** Return the position of a command. *)
let position_of_command (c: command) =
  match c with
  | SetLogic (p,_) | DeclareFun (p,_,_,_) | DefineFun (p,_,_,_,_) 
  | DeclareSort (p,_,_) | DefineSort (p,_,_,_) | Assert (p,_) 
  | GetAssertions p | CheckSat p | GetProof p | GetUnsatCore p 
  | GetValue (p,_) | GetAssignment p | Push (p,_) | Pop (p,_) 
  | GetOption (p,_) | SetOption (p,_) | GetInfo (p,_) | SetInfo (p,_)
  | Exit p -> p

(** Print a command, abiding by the syntax of SmtLib. *)
let string_of_command (c: command): string =
  match c with
  | SetLogic (_, s) -> "(set-logic " ^ s ^ ")"
  | DeclareFun (_, s, sts, st) ->
      "(declare-fun " ^ s ^ " (" ^ List.fold_left (fun s st -> s ^ " " ^ string_of_sort st) "" sts ^
      " ) " ^ string_of_sort st ^ ")"
  | DefineFun (_, s, sts, st, t) ->
      "(define-fun " ^ s ^ " (" ^
      List.fold_left (fun s (sym, st) -> s ^ " (" ^ sym ^ " " ^ string_of_sort st ^ ")") "" sts ^
      " ) " ^ string_of_sort st ^ " " ^ string_of_term t ^ ")"
  | DeclareSort (_, s, n) -> "(declare-sort " ^ s ^ " " ^ string_of_int n ^ ")"
  | DefineSort (_, s, ss, st) ->
      "(define-sort " ^ s ^ " (" ^ List.fold_left (fun s sym -> s ^ " " ^ sym) "" ss ^ " ) " ^
      string_of_sort st ^ ")"
  | Assert (_, t) -> "(assert " ^ string_of_term t ^ ")"
  | GetAssertions _ -> "(get-assertions)"
  | CheckSat _ -> "(check-sat)"
  | GetProof _ -> "(get-proof)"
  | GetUnsatCore _ -> "(get-unsat-core)"
  | GetValue (_, ts) ->
      "(get-value (" ^ List.fold_left (fun s t -> s ^ " " ^ string_of_term t) "" ts ^ " ))"
  | GetAssignment _ -> "(get-assignment)"
  | Push (_, n) -> "(push " ^ string_of_int n ^ ")"
  | Pop (_, n) -> "(pop " ^ string_of_int n ^ ")"
  | GetOption (_, k) -> "(get-option " ^ k ^ ")"
  | SetOption (_, a) -> "(set-option " ^ string_of_attribute a ^ ")"
  | GetInfo (_, k) -> "(get-info " ^ k ^ ")"
  | SetInfo (_, a) -> "(set-info " ^ string_of_attribute a ^ ")"
  | Exit _ -> "(exit)"


(** Print a complete script. *)
let print_script (fout: Format.formatter) (cs: command list): unit =
  List.iter (fun c ->
    Format.pp_print_string fout (string_of_command c);
    Format.pp_print_newline fout ()
  ) cs;
  Format.pp_print_flush fout ()

