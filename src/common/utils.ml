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
  let rec mapaux i xs acc =
    match xs with
    | [] -> List.rev acc
    | x::xs -> mapaux (i+1) xs ((f i x)::acc)
  in
  mapaux 0 xs []

let filteri (f: int -> 'a -> bool) (xs: 'a list): 'a list =
  let rec filteraux i xs acc =
    match xs with
    | [] -> List.rev acc
    | x::xs ->
        if f i x then filteraux (i+1) xs (x::acc)
        else filteraux (i+1) xs acc
  in
  filteraux 0 xs []

let rec insert (i: int) (is: int list): int list =
  match is with
  | [] -> [i]
  | j::is when i<j -> i::j::is
  | j::is when i=j -> j::is
  | j::is -> j::(insert i is)

let rec delete (i: int) (is: int list): int list =
  match is with
  | [] -> []
  | i'::is ->
      if i' = i then is
      else if i' < i then i'::(delete i is)
      else i'::is

let rec union (xs: int list) (ys: int list): int list =
  match xs, ys with
  | [], _ -> ys
  | _, [] -> xs
  | x::xs, y::ys ->
      if x = y then x::(union xs ys)
      else if x < y then x::(union xs (y::ys))
      else y::(union (x::xs) ys)

let difference (xs: 'a list) (ys: 'a list): 'a list =
  List.filter (fun x -> not (List.mem x ys)) xs

