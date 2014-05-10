(** Declaration of some useful functions.
    Since the version of ocaml on my computer is 3.12, some standard functions
    had to be recoded. *)

type ('a,'b) either =
    Left of 'a
  | Right of 'b


let iteri (f: int -> 'a -> unit) (xs: 'a list): unit =
  let rec iteraux i xs =
    match xs with
    | [] -> ()
    | x::xs -> f i x; iteraux (i+1) xs
  in
  iteraux 0 xs

let mapi (f: int -> 'a -> 'b) (xs: 'a list): 'b list =
  let rec mapiaux i xs acc =
    match xs with
    | [] -> List.rev acc
    | x::xs -> mapiaux (i+1) xs ((f i x)::acc)
  in
  mapiaux 0 xs []

let rec insert (i: int) (is: int list): int list =
  match is with
  | [] -> [i]
  | j::is when i<j -> i::j::is
  | j::is when i=j -> j::is
  | j::is -> j::(insert i is)

