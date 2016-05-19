(* K-Normalized terms. *)

type t =
  | Bool of bool
  | Int of int
  | Var of Id.t
  | Lambda of (BType.t * Id.t) * t
  | RecLambda of (BType.t * Id.t) * (BType.t * Id.t) * t
  | App of t * t
  | If of t * t * t
  | Let of (BType.t * Id.t) * t * t

(* 式の変数を式で置き換え *)
let rec subst ((ex, x) as st) e =
  match e with
    | Bool _ | Int _ -> e
    | Var y when x <> y -> e
    | Lambda((_, y), _) when x = y -> e
    | Var _ ->
        (* 置き換え *)
        ex
    | Lambda((ty, y), e2) ->
        Lambda((ty, y), subst st e2)
    | RecLambda((tw, w), (ty, y), e2) when x = w || x = y ->
        RecLambda((tw, w), (ty, y), e2)
    | RecLambda((tw, w), (ty, y), e2) ->
        RecLambda((tw, w), (ty, y), subst st e2)
    | If(e1, e2, e3) ->
        If(subst st e1, subst st e2, subst st e3)
    | App(e1, e2) ->
        App(subst st e1, subst st e2)
    | Let((t, y), e1, e2) when x = y ->
        (* e2は置き換えない *)
        Let((t, y), subst st e1, e2)
    | Let((t, y), e1, e2) ->
        Let((t, y), subst st e1, subst st e2)

(* 出現する変数と型を全て列挙 *)
let rec vars env e =
  match e with
    | Bool _ | Int _ | Var _ -> env
    | Lambda((t, x), e2) ->
        let env' = M.add x t env in
          vars env' e2
    | RecLambda((tx, x), (ty, y), e2) ->
        let env' = M.add x tx env in
        let env''= M.add y ty env' in
          vars env'' e2
    | App(e1, e2) ->
        let env1 = vars env e1 in
        let env2 = vars env e2 in
          M.leftunion env1 env2
    | If(e1, e2, e3) ->
        let env1 = vars env e1 in
        let env2 = vars env e2 in
        let env3 = vars env e3 in
          M.leftunion env1 (M.leftunion env2 env3)
    | Let((t, x), e1, e2) ->
        let env1 = vars env e1 in
        let env1' = M.add x t env1 in
          vars env1' e2

(* 変数含むか調べる *)
let rec varset e =
  match e with
    | Bool _ | Int _ -> S.empty
    | Var x -> S.singleton x
    | Lambda((_, x), e2) -> S.remove x (varset e2)
    | RecLambda((_, _), (_, y), e2) -> S.remove y (varset e2)
    | App(e1, e2) -> S.union (varset e1) (varset e2)
    | If(e1, e2, e3) -> S.union (varset e1) (S.union (varset e2) (varset e2))
    | Let((_, x), e1, e2) -> S.union (varset e1) (S.remove x (varset e2))


exception TypeError
(* KNormalのBTypeをinfer（しっかりcheck） *)
(* TODO: cons.mlにもあったような？ *)
let rec gettype (env: BType.t M.t) = function
  | Bool _ -> BType.Bool
  | Int _ -> BType.Int
  | Var x -> M.find x env
  | Lambda((ta, a), e1) ->
      let env' = M.add a ta env in
      let td = gettype env' e1 in
        BType.Fun((ta, ref (Some a)), td)
  | RecLambda((tx, x), (ty, y), e1) ->
      let env' = M.add x tx env in
      let env''= M.add y ty env' in
        (match tx with
           | BType.Fun ((ta, {contents = Some a}), td) when a = y && BType.equal tx ta ->
               let t1 = gettype env'' e1 in
                 (match BType.equal t1 td with
                    | false -> raise TypeError
                    | true ->
                        tx)
           | _ -> raise TypeError)
  | App(e1, e2) ->
      let t1 = gettype env e1 in
      let t2 = gettype env e2 in
        (match t1 with
           | BType.Fun((ta, _), td) when BType.equal ta t2 ->
               td
           | _ -> raise TypeError)
  | If(e1, e2, e3) ->
      let t1 = gettype env e1 in
        (match BType.equal t1 BType.Bool with
           | false -> raise TypeError
           | true ->
               let t2 = gettype env e2 in
               let t3 = gettype env e3 in
                 (if BType.equal t2 t3 then
                    t2
                  else
                    raise TypeError))
  | Let((tx, x), e1, e2) ->
      let t1 = gettype env e1 in
        (match BType.equal tx t1 with
           | false -> raise TypeError
           | true ->
               let env' = M.add x tx env in
               gettype env' e2)



(* BType.t Syntax.t -> kNormal.t *)
let rec g env = function
  | Syntax.Bool v -> (Bool v, BType.Bool)
  | Syntax.Int v  -> (Int v, BType.Int)
  | Syntax.Var x  -> (Var x, M.find x env)
  | Syntax.Lambda((tx, x), e) ->
      let env' = M.add x tx env in
      let (e', te) = g env' e in
      (Lambda((tx, x), e'), BType.Fun((tx, ref (Some x)), te))
  | Syntax.App(e1, e2) ->
      let (e1', t1) = g env e1 in
      (* 関数の戻り値の型を取り出す *)
      (match t1 with
         | BType.Fun(_, td) ->
             let (e2', _) = g env e2 in
               (App(e1', e2'), td)
         | _ -> assert false)
  | Syntax.If(e1, e2, e3) ->
      let (e1', _) = g env e1 in
      let (e2', t2) = g env e2 in
      let (e3', _)  = g env e3 in
        (If(e1', e2', e3'), t2)
  | Syntax.Let((tx, x), e1, e2) ->
      let (e1', _) = g env e1 in
      let env' = M.add x tx env in
      let (e2', t2) = g env' e2 in
        (Let((tx, x), e1', e2'), t2)

  | Syntax.LetRec((tx, x), ((ty, y), e1), e2) ->
      let env' = M.add x tx env in
      let env''= M.add y ty env' in
      let e1', _ = g env'' e1 in
      let e2', t2 = g env' e2 in
        (Let((tx, x),
             RecLambda((tx, x), (ty, y), e1'),
             e2'), t2)
(* いろいろなSyntaxを組み込み関数化 *)
  | Syntax.Lt(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Constant.lt, e1), e2))
  | Syntax.Le(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Constant.le, e1), e2))
  | Syntax.Gt(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Constant.gt, e1), e2))
  | Syntax.Ge(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Constant.ge, e1), e2))
  | Syntax.Not e1 ->
      g env (Syntax.App(Syntax.Var Constant.not, e1))
  | Syntax.Add(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Constant.add, e1), e2))
  | Syntax.Sub(e1,e2) ->
      g env (Syntax.App(Syntax.App(Syntax.Var Constant.sub, e1), e2))

let f e = fst (g Constant.btypes e)

let tree type_str = Tree.make (function
                                 | Bool v -> ("BOOL " ^ string_of_bool v, [], [])
                                 | Int v -> ("INT " ^ string_of_int v, [], [])
                                 | Var x -> ("VAR " ^ x, [], [])
                                 | Lambda((t,x), e) -> ("FUN (" ^ x ^ " : " ^ type_str t ^ ")", [], [e])
                                 | RecLambda(_,(t,x), e) -> ("RECFUN (" ^ x ^ " : " ^ type_str t ^ ")", [], [e])
                                 | App(e1, e2) -> ("APP", [], [e1; e2])
                                 | If(e1, e2, e3) -> ("IF", [], [e1; e2; e3])
                                 | Let((t,x), e, f) -> ("LET " ^ x ^ " : " ^ type_str t, [f], [e])) "  "

(* 短い文字列での表現 *)
let rec short_str e =
  match e with
    | Bool v -> string_of_bool v
    | Int v -> string_of_int v
    | Var x -> x
    | Lambda((_, x), e) -> "(fun " ^ x ^ " -> " ^ short_str e ^ ")"
    | RecLambda((_, _), (_, y), e) -> "(recfun " ^ y ^ " -> " ^ short_str e ^ ")"
    | App(e1, e2) -> "(" ^ short_str e1 ^ " " ^ short_str e2 ^ ")"
    | If(e1, e2, e3) -> "if " ^ short_str e1 ^ " then " ^ short_str e2 ^ " else " ^ short_str e3
    | Let((_, x), e1, e2) ->
        "let " ^ x ^ " = " ^ short_str e1 ^ " in " ^ short_str e2

