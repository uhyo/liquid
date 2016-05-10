type t = string

let nextid = ref 1

(* あらたな変数名を生み出す *)
let genid prefix =
  let id = !nextid in
    nextid := id+1;
    Printf.sprintf "%s.%d" prefix id
(* 型名から変数名を生み出す *)
let gentmp t =
  let c = (match t with
             | BType.Bool -> "b"
             | BType.Int -> "i"
             | BType.Fun(_) -> "f"
             | _ -> "x") in
    genid c
