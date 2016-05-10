(* Liquid Type. *)
open Q

(* Refinement predicates. *)
type rfs =
  | RFVar of int * rfs option ref
  | RFQs of Q.t list

(* Liquid Type. *)
type t = 
  | Base of BType.t * rfs
  | Fun of (t * Id.t) * t

let nextid = ref 1

(* generate Liquid type variables. *)
let gen_rfs () =
  let id = !nextid in
    nextid := id + 1;
    RFVar(id, ref None)

(* Shape of Liquid Type. *)
let rec shape t =
  match t with
    | Base(b, _) -> b
    | Fun((tx, x), td) -> BType.Fun((shape tx, x), shape td)

(* Dependent Type for Constant. *)
let c_bool v =
  if v then
    (* true *)
    Base(BType.Bool, RFQs [QNu])
  else
    (* false *)
    Base(BType.Bool, RFQs [QNot(QNu)])

let c_int v = Base(BType.Int, RFQs [QEq(QNu, QInt v)])

(* Liquid Type skeleton. *)
let rec skeleton t =
  match t with
    | BType.Bool
    | BType.Int ->
        Base(t, gen_rfs())
    | BType.Fun((t1, x), t2) ->
        Fun((skeleton t1, x), skeleton t2)
    | _ -> failwith ("FOOOOOO " ^ BType.type_str t)

(* Substitution.
 * (xをyに代入) *)
let rec subst x y t =
  match t with
    | Base(b, rfs) ->
        Base(b, subst_rfs x y rfs)
    | Fun((ta, a), td) ->
        let a' = if a = y then x else a in
        let ta' = subst x y ta in
        let td' = subst x y td in
          Fun((ta', a'), td')
and subst_rfs x y rfs =
  match rfs with
    | RFVar(i, { contents = Some rfs' }) ->
        RFVar(i, ref (Some (subst_rfs x y rfs')))
    | RFVar(_) -> rfs
    | RFQs qs ->
        let qs' = List.map (Q.subst x y) qs in
          RFQs qs'


(* Sting of Refinement predicates. *)
let rec rfs_str = function
  | RFQs [] -> "true"
  | RFQs qs ->
      let result = Buffer.create 256 in
        List.iteri
          (fun i q ->
             if i > 0 then Buffer.add_string result " ∧ ";
             Buffer.add_string result ("(" ^ Q.q_str q ^ ")"))
          qs;
        Buffer.contents result
  | RFVar(i, { contents = None }) ->
      "κ" ^ string_of_int i
  | RFVar(_, { contents = Some rf }) -> rfs_str rf

let rec is_funtype = function
  | Fun(_) -> true
  | _ -> false

let rec type_str t =
  match t with
    | Base(bt,k) ->
        "{ν : " ^ BType.type_str bt ^ " | " ^ rfs_str k ^ "}"
    | Fun((t1, x), t2) ->
        (if is_funtype t1 then
           x ^ ":(" ^ type_str t1 ^ ") -> " ^ type_str t2
         else
           x ^ ":" ^ type_str t1 ^ " -> " ^ type_str t2)
