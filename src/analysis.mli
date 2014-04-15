(** Conduct an analysis on the control flow graph built upon the surface syntax. *)

open Expressions
open Cfg


(** Build the environment. *)
val make_environment: unit -> Apron.Environment.t

(** Translate a linear expression. *)
val make_linexpr: Apron.Environment.t -> Linexpr.t -> Apron.Linexpr1.t

(** Translate a constraint. *)
val make_constraint: Apron.Environment.t -> (Linexpr.t * string) -> Apron.Lincons1.earray


(** Abstraction the program states. Only the loop header are retained. *)
type 'a abstract_info = {
  mutable value: 'a Apron.Abstract1.t option;
  (* Previous abstract value. Set only for loop headers. *)
  mutable prev_value: 'a Apron.Abstract1.t option;
  (* Number of visits of the label. *)
  mutable marker: int;
  cfg_info: (Apron.Linexpr1.t, Apron.Lincons1.earray) label_info
}

type 'a abstract_state = ('a abstract_info) array


(** Creation of the initial state (top at state 0, bottom everywhere else).  *)
val make_initial_state:
    'a Apron.Manager.t ->
    Apron.Environment.t ->
    Linexpr.t Cfg.t ->
    'a abstract_state

(** Abstract interpretation. Starting from the loop headers (and the initial program point),
    propagate the domains. *)
val perform_analysis:
    'a Apron.Manager.t ->
    Apron.Environment.t ->
    'a abstract_state ->
    unit

(** Put everything together, and perform an analysis of a control flow graph. *)
val analyze: Linexpr.t Cfg.t -> (Polka.loose Polka.t) abstract_state

(** Display the result of the analysis. *)
val print_abstract: 'a abstract_state -> unit

