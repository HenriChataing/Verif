(** Implement a logging tool. *)

(** The current formatter. *)
let formatter: Format.formatter ref = ref Format.std_formatter

(** The current verbose level. *)
let verbose: int ref = ref 1

(** The list of actived display modes. *)
let displayed: string list ref = ref []


(** Activate a display mode. *)
let display (m: string): unit =
  displayed := m::!displayed

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

(** Selective execution of a chunk of code. *)
let execute ?(mode: string = "") ?(lvl: int = 0) (f: unit -> unit): unit =
  if mode <> "" then begin
    if List.mem mode !displayed then
      f ()
  end else if lvl < !verbose then
    f ()

(** Logging function. The mode, if any, has priority over the level. *)
let log ?(mode: string = "") ?(lvl: int = 0) (msg: string): unit =
  if mode <> "" then begin
    if List.mem mode !displayed then
      Format.pp_print_string !formatter msg
  end else if lvl < !verbose then
    Format.pp_print_string !formatter msg

(** Logging function, specialized in printing abstract values. *)
let loga ?(mode: string = "") ?(lvl: int = 0) (d: 'a Apron.Abstract1.t): unit =
  if mode <> "" then begin
    if List.mem mode !displayed then
      Apron.Abstract1.print !formatter d
  end else if lvl < !verbose then
    Apron.Abstract1.print !formatter d

(** Write a line break. *)
let newline ?(mode: string = "") ?(lvl: int = 0) (): unit =
  if mode <> "" then begin
    if List.mem mode !displayed then
      Format.pp_print_newline !formatter ()
  end else if lvl < !verbose then
    Format.pp_print_newline !formatter ()

