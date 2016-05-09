type t =
  | Bool
  | Int
  | Fun of (t * string) * t
  | Var of int * t option ref

val newtype : unit -> t
val type_str : t -> string
