(** Implement a logging tool. *)


(** Set the verbose level. *)
val set_verbose: int -> unit

(** Set the formatter. *)
val set_formatter: Format.formatter -> unit

(** Return the used formatter. *)
val get_formatter: unit -> Format.formatter

(** Flush the formatter. *)
val flush: unit -> unit

(** Logging function. *)
val log: ?lvl: int -> string -> unit

(** Logging function, specialized in printing abstract values. *)
val loga: ?lvl: int ->'a Apron.Abstract1.t -> unit

(** Write a line break. *)
val newline: ?lvl: int -> unit -> unit

