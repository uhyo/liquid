(* Static Qualifiers. *)
type t =
  | QNu
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


