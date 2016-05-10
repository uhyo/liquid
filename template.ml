(* Templates. *)

(* Type of templates.
 * Pending substitutions + Type. *)
type t = (Id.t * Id.t) list * LType.t

(* Generate template from BType. *)
let of_b t = ([], LType.skeleton t)
(* Template from LType. *)
let of_l t = ([], t)

(* Push new pending substitution *)
let subst ((x, y) as st) (sts, t) =
  if x = y then
    (sts, t)
  else
    (st::sts, t)

(* Resolve pending substitution. *)
let resolve (sts, t) =
  List.fold_right
    (fun (x, y) t -> LType.subst x y t)
    sts
    t

(* Dependent Type for constant. *)
let c_bool v = ([], LType.c_bool v)
let c_int v = ([], LType.c_int v)

let rec type_str t =
  let result = Buffer.create 256 in
  let (sts, lt) = t in
    List.iter
      (fun (x, y) ->
         Buffer.add_string result ("[" ^ x ^ "/" ^ y ^ "]"))
      sts;
    Buffer.add_string result (LType.type_str lt);
    Buffer.contents result
