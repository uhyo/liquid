(* Base Type Inference *)
open Syntax

exception Unify of BType.t * BType.t
exception ReferenceError of Id.t

(* 無限型を防ぐにはoccur checkが必要 *)
(* 型変数（番号i）がt2に含まれるか? *)
let rec occur i t =
  match t with
    | BType.Fun((t1,_), t2) -> occur i t1 || occur i t2
    | BType.Var(j, _) when i=j -> true
    | BType.Var(_, { contents = None }) -> false
    | BType.Var(_, { contents = Some t' }) -> occur i t'
    | _ -> false

(* 2つの型を一致させる *)
let rec unify t1 t2 =
  match t1,t2 with
     | BType.Bool, BType.Bool
     | BType.Int, BType.Int -> ()
     | BType.Fun((t3,_), t4), BType.Fun((t5,_), t6) ->
         unify t3 t5;
         unify t4 t6
     | BType.Var(i,_), BType.Var(j,_) when i = j -> ()
     | BType.Var(_,{ contents = Some(t1') }), _ -> unify t1' t2
     | _, BType.Var(_,{ contents = Some(t2') }) -> unify t1 t2'
     | BType.Var(i,({ contents = None } as r1)), _ ->
         (* 無限型を避ける *)
         if occur i t2 then raise (Unify(t1, t2));
         r1 := Some t2
     | _, BType.Var(j,({ contents = None } as r2)) ->
         if occur j t1 then raise (Unify(t1, t2));
         r2 := Some t1
     | _, _ ->
         raise (Unify(t1,t2))

(* 型変数をunbox *)
let rec deref_t t =
  match t with
    | BType.Fun((tx, x), ty) -> BType.Fun((deref_t tx, x), deref_t ty)
    | BType.Var(_,{ contents = Some t'}) -> deref_t t'
    | _ -> t
let rec deref (e: BType.t Syntax.t) =
  match e with
    | Lambda((t, x), f) -> Lambda((deref_t t, x), deref f)
    | App(e1, e2) -> App(deref e1, deref e2)
    | If(e1, e2, e3) -> If(deref e1, deref e2, deref e3)
    | Let((t, x), e1, e2) ->
        Let((deref_t t, x), deref e1, deref e2)
    | LetRec((tx, x), ((ty, y), e1), e2) ->
        LetRec((deref_t tx, x), ((deref_t ty, y), deref e1), deref e2)
    | Lt(e1, e2) -> Lt(deref e1, deref e2)
    | Gt(e1, e2) -> Gt(deref e1, deref e2)
    | Le(e1, e2) -> Le(deref e1, deref e2)
    | Ge(e1, e2) -> Ge(deref e1, deref e2)
    | Not f -> Not(deref f)
    | Add(e1, e2) -> Add(deref e1, deref e2)
    | Sub(e1, e2) -> Sub(deref e1, deref e2)
    | _ -> e

(* 型環境から型推論 *)
let rec g (env: BType.t M.t) (e: BType.t Syntax.t) =
  match e with
    | Bool _ -> BType.Bool
    | Int _ -> BType.Int
    | Var x ->
        (try M.find x env
         with Not_found -> raise (ReferenceError x))
    | Lambda((t, x), e) ->
        (* xの型はt *)
        let env' = M.add x t env in
        let t2 = g env' e in
          BType.Fun((t, x), t2)
    | App(e1, e2) ->
        let t = BType.newtype() in
        let t1 = g env e1 in
        let t2 = g env e2 in
        (* e1の型はt2 -> tである *)
        unify t1 (BType.Fun((t2,"?"), t));
        t
    | If(e1, e2, e3) ->
        let t1 = g env e1 in
        (* 条件の型はBoolである *)
        unify t1 BType.Bool;
        let t2 = g env e2 in
        let t3 = g env e3 in
        (* thenもelseも型が等しい *)
        unify t2 t3;
        t2
    | Let((t, x), e1, e2) ->
        let t1 = g env e1 in
        (* xの型tはe1の型である *)
        unify t t1;
        let env' = M.add x t env in
        let t2 = g env' e2 in
          t2
    | LetRec((tx, x), ((ty, y), e1), e2) ->
        (* 再帰なので自分の型を *)
        let env' = M.add x tx env in
        let env'' = M.add y ty env' in
        let t1 = BType.newtype() in
        (* txは関数 *)
        unify tx (BType.Fun((ty, y), t1));
        let t1' = g env'' e1 in
        unify t1 t1';
        let t2 = g env' e2 in
          t2
    | Lt(e1, e2) | Gt(e1, e2)
    | Le(e1, e2) | Ge(e1, e2) ->
        (* e1もe2もint *)
        let t1 = g env e1 in
        let t2 = g env e2 in
        unify t1 BType.Int;
        unify t2 BType.Int;
        (* 返り値はBool *)
        BType.Bool
    | Add(e1, e2) | Sub(e1, e2) ->
        (* 全部int *)
        let t1 = g env e1 in
        let t2 = g env e2 in
        unify t1 BType.Int;
        unify t2 BType.Int;
        BType.Int
    | Not e1 ->
        let t1 = g env e1 in
        unify t1 BType.Bool;
        BType.Bool

let f (e: BType.t Syntax.t) =
  let env = M.empty in
    (try
       let topTyp = g env e in
         (deref e, deref_t topTyp)
     with
       | Unify(t1, t2)->
           Printf.eprintf "Unify error: %s vs %s\n" (BType.type_str t1) (BType.type_str t2); failwith "TypeError"
       | ReferenceError x ->
           Printf.eprintf "Reference error: %s\n" x; failwith "ReferenceError")
