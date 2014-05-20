(** Implement a logging tool. *)


(** Set the verbose level. *)
val set_verbose: int -> unit

(** Set the formatter. *)
val set_formatter: Format.formatter -> unit

(** Activate a display mode. *)
val display: string -> unit

(** Return the used formatter. *)
val get_formatter: unit -> Format.formatter

(** Flush the formatter. *)
val flush: unit -> unit

(** Logging function. *)
val log: ?mode: string -> ?lvl: int -> string -> unit

(** Logging function, specialized in printing abstract values. *)
val loga: ?mode: string -> ?lvl: int ->'a Apron.Abstract1.t -> unit

(** Write a line break. *)
val newline: ?mode: string -> ?lvl: int -> unit -> unit

