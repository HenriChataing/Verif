(** Declaration of some useful functions.
    Since the version of ocaml on my computer is 3.12, some standard functions
    had to be recoded. *)

let iteri (f: int -> 'a -> unit) (xs: 'a list): unit =
  let rec iteraux i xs =
    match xs with
    | [] -> ()
    | x::xs -> f i x; iteraux (i+1) xs
  in
  iteraux 0 xs

