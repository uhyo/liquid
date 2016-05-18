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
  | Add of 'a t * 'a t
  | Sub of 'a t * 'a t

let tree type_str = Tree.make (function
                                 | Bool v -> ("BOOL " ^ string_of_bool v, [], [])
                                 | Int v -> ("INT " ^ string_of_int v, [], [])
                                 | Var x -> ("VAR " ^ x, [], [])
                                 | Lambda((t,x), e) -> ("FUN (" ^ x ^ " : " ^ type_str t ^ ")", [], [e])
                                 | App(e, f) -> ("APP", [], [e; f])
                                 | If(e, f, g) -> ("IF", [], [e; f; g])
                                 | Let((t,x), e, f) -> ("LET " ^ x ^ " : " ^ type_str t, [f], [e])
                                 | LetRec((tx,x), ((ty, y), e), f) -> ("LETREC " ^ x ^ " : "^ type_str tx, [f], [Lambda((ty, y), e)])
                                 | Lt(e, f) -> ("LT", [], [e; f])
                                 | Gt(e, f) -> ("GT", [], [e; f])
                                 | Le(e, f) -> ("LE", [], [e; f])
                                 | Ge(e, f) -> ("GE", [], [e; f])
                                 | Not e    -> ("NOT",[], [e])
                                 | Add(e, f) -> ("ADD", [], [e; f])
                                 | Sub(e, f) -> ("SUB", [], [e; f])
) "  "
