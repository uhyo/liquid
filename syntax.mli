(* 'aは型のつもり *)
type 'a t =
  | Bool of bool
  | Int  of int
  | Var  of Id.t
  | Lambda of ('a * Id.t) * 'a t
  | App of 'a t * 'a t
  | If of 'a t * 'a t * 'a t
  | Let of ('a * Id.t) * 'a t * 'a t
  (* 関数名 * 関数定義 * inの後 *)
  | LetRec of ('a * Id.t) * (('a * Id.t) * 'a t) * 'a t
  (* some syntax sugars *)
  | Lt of 'a t * 'a t
  | Gt of 'a t * 'a t
  | Le of 'a t * 'a t
  | Ge of 'a t * 'a t
  | Not of 'a t

val tree : ('a -> string) -> 'a t -> string
