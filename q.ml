(* Static Qualifiers. *)
type t =
  | QNu
  | QStar
  | QVar of Id.t
  | QBool of bool
  | QInt of int
  | QNot of t
  | QIff of t * t
  | QEq of t * t
  | QLt of t * t
  | QLe of t * t
  | QGt of t * t
  | QGe of t * t

(* 今回のQualifiers *)
let default_qs = [
  QLe(QInt 0, QNu);
  QLe(QStar, QNu);
  QLt(QNu, QStar)
]

(*yにxを代入*)
let rec subst x y q =
  match q with
    | QVar z when y = z -> QVar x
    | QNot q1 -> QNot (subst x y q1)
    | QIff(q1, q2) -> QIff(subst x y q1, subst x y q2)
    | QEq(q1, q2) -> QEq(subst x y q1, subst x y q2)
    | QLt(q1, q2) -> QLt(subst x y q1, subst x y q2)
    | QLe(q1, q2) -> QLe(subst x y q1, subst x y q2)
    | QGt(q1, q2) -> QGt(subst x y q1, subst x y q2)
    | QGe(q1, q2) -> QGe(subst x y q1, subst x y q2)
    | _ -> q

let rec q_str = function
  | QNu -> "ν"
  | QVar x -> x
  | QBool v -> string_of_bool v
  | QInt v -> string_of_int v
  | QNot q -> "not(" ^ q_str q ^ ")"
  | QIff(q1, q2) -> q_str q1 ^ " ⇔ " ^ q_str q2
  | QEq(q1, q2) -> q_str q1 ^ " = " ^ q_str q2
  | QLt(q1, q2) -> q_str q1 ^ " < " ^ q_str q2
  | QLe(q1, q2) -> q_str q1 ^ " ≤ " ^ q_str q2
  | QGt(q1, q2) -> q_str q1 ^ " > " ^ q_str q2
  | QGe(q1, q2) -> q_str q1 ^ " ≥ " ^ q_str q2


