(* Base Type. *)
type t =
  | Bool
  | Int
  | Fun of (t * string) * t
  | Var of int * t option ref

let nextid = ref 1

let newtype () =
  let id = !nextid in
    nextid := id+1;
    Var(id, ref None)

let rec is_funtype = function
  | Fun(_) -> true
  | Var(_, { contents = Some t'}) -> is_funtype t'
  | _ -> false

let rec type_str = function
  | Bool -> "bool"
  | Int -> "int"
  | Fun((t1, x), t2) ->
      (if is_funtype t1 then
         x ^ ":(" ^ type_str t1 ^ ") -> " ^ type_str t2
       else
         x ^ ":" ^ type_str t1 ^ " -> " ^ type_str t2)
  | Var(_, { contents = Some t'}) -> "V(" ^ type_str t' ^ ")"
  | Var(i,_) -> string_of_int i
