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

let rec equal t1 t2 =
  match t1, t2 with
    | Bool, Bool
    | Int, Int -> true
    | Fun((ta1, _), td1), Fun((ta2, _), td2) -> equal ta1 ta2 && equal td1 td2
    | Var(_,{ contents = Some t1' }), _ -> equal t1' t2
    | _, Var(_,{ contents = Some t2' }) -> equal t1 t2'
    | Var(i,_), Var(j,_) when i = j -> true
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
