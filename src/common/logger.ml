(** Implement a logging tool. *)

(** The current formatter. *)
let formatter: Format.formatter ref = ref Format.std_formatter

(** The current verbose level. *)
let verbose: int ref = ref 1


(** Set the verbose level. *)
let set_verbose (v: int): unit =
  verbose := v

(** Set the formatter. *)
let set_formatter (fmt: Format.formatter): unit =
  formatter := fmt

(** Return the used formatter. *)
let get_formatter (): Format.formatter =
  !formatter


(** Flush the formatter. *)
let flush (): unit =
  Format.pp_print_flush !formatter ()

(** Logging function. *)
let log ?(lvl: int = 0) (msg: string): unit =
  if lvl < !verbose then
    Format.pp_print_string !formatter msg

(** Logging function, specialized in printing abstract values. *)
let loga ?(lvl: int = 0) (d: 'a Apron.Abstract1.t): unit =
  if lvl < !verbose then
    Apron.Abstract1.print !formatter d

(** Write a line break. *)
let newline ?(lvl: int = 0) (): unit =
  if lvl < !verbose then
    Format.pp_print_newline !formatter ()

