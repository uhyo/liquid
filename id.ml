type t = string

let nextid = ref 1

(* 型名から変数名を生み出す *)
let gentmp t =
  let id = !nextid in
    nextid := id+1;
  let c = (match t with
             | BType.Bool -> "b"
             | BType.Int -> "i"
             | BType.Fun(_) -> "f"
             | _ -> "x") in
    Printf.sprintf "%s.%d" c id
