type t =
  | Bool
  | Int
  | IntArray
  | Fun of (t * string option ref) * t
  | Var of int * t option ref

val newtype : unit -> t
val is_btype : t -> bool
val equal : t -> t -> bool
val type_str : t -> string
