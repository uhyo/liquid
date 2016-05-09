(* K-Normalized terms. *)

type 'a t =
  | Bool of bool
  | Int of int
  | Var of Id.t
  | Lambda of ('a * Id.t) * 'a t
  | App of Id.t * Id.t
  | If of Id.t * 'a t * 'a t
  | Let of ('a * Id.t) * 'a t * 'a t
  | LetRec of ('a * Id.t) * (('a * Id.t) * 'a t) * 'a t

(* letを挿入してコールバックに変数名を渡す *)
let insert_let (e, t) f =
  match e with
    | Var x ->
        (* 変数を参照してるだけだから何も挿入する必要がなかった *)
        f x
    | _ ->
        (* eをletに入れる *)
        let x = Id.gentmp t in
        (* 先を作ってもらう *)
        let e', t' = f x in
        (* Letを前に挿入する *)
          (Let((t, x), e, e'), t')
(* BType.t Syntax.t -> BType.t kNormal.t *)
let rec g env = function
  | Syntax.Bool v -> (Bool v, BType.Bool)
  | Syntax.Int v  -> (Int v, BType.Int)
  | Syntax.Var x  -> (Var x, M.find x env)
  | Syntax.Lambda((tx, x), e) ->
      let env' = M.add x tx env in
      let (e', te) = g env' e in
      (Lambda((tx, x), e'), BType.Fun((tx, x), te))
  | Syntax.App(e1, e2) ->
      let (e1', t1) as et1 = g env e1 in
      (* 関数の戻り値の型を取り出す *)
      (match t1 with
         | BType.Fun(_, td) ->
             insert_let
               et1
               (fun x ->
                  let et2 = g env e2 in
                  insert_let
                    et2
                    (fun y ->
                       (* 返り値の型はtd *)
                       (App(x, y), td)))
         | _ -> assert false)
  | Syntax.If(e1, e2, e3) ->
      let et1 = g env e1 in
        insert_let
          et1
          (fun x ->
             let (e2', t2) = g env e2 in
             let (e3', _)  = g env e3 in
               (If(x, e2', e3'), t2))
  | Syntax.Let((tx, x), e1, e2) ->
      let (e1', _) = g env e1 in
      let env' = M.add x tx env in
      let (e2', t2) = g env' e2 in
        (Let((tx, x), e1', e2'), t2)

  | Syntax.LetRec((tx, x), ((ty, y), e1), e2) ->
      let env' = M.add x tx env in
      let env''= M.add y ty env in
      let e1', _ = g env'' e1 in
      let e2', t2 = g env' e2 in
        (LetRec((tx, x), ((ty, y), e1'), e2'), t2)
(* いろいろなSyntaxを組み込み関数化 *)
  | Syntax.Lt(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Builtin.lt, e1), e2))
  | Syntax.Le(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Builtin.le, e1), e2))
  | Syntax.Gt(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Builtin.gt, e1), e2))
  | Syntax.Ge(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Builtin.ge, e1), e2))
  | Syntax.Not e1 ->
      g env (Syntax.App(Syntax.Var Builtin.not, e1))

let f e = fst (g Builtin.btypes e)

let tree type_str = Tree.make (function
                                 | Bool v -> ("BOOL " ^ string_of_bool v, [], [])
                                 | Int v -> ("INT " ^ string_of_int v, [], [])
                                 | Var x -> ("VAR " ^ x, [], [])
                                 | Lambda((t,x), e) -> ("FUN (" ^ x ^ " : " ^ type_str t ^ ")", [], [e])
                                 | App(x, y) -> ("APP " ^ x ^ " " ^ y, [], [])
                                 | If(x, f, g) -> ("IF " ^ x, [], [f; g])
                                 | Let((t,x), e, f) -> ("LET " ^ x ^ " : " ^ type_str t, [f], [e])
                                 | LetRec((tx,x), ((ty, y), e), f) -> ("LETREC " ^ x ^ " : "^ type_str tx, [f], [Lambda((ty, y), e)])) "  "
