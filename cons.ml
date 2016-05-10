(* Constraints. *)
open KNormal

(* Map whose key is int *)
module MI =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)

type t =
  (* WellFormed((a, es), t1) <=> a; es |- t1 *)
  | WellFormed of (LType.t M.t * KNormal.t list) * LType.t
  (* SubType((a, es), t1, t2) <=> a; es |- t1 <: t2 *)
  | SubType of (LType.t M.t * KNormal.t list) * LType.t * LType.t
(* Type of e is bool *)
  | BoolExp of (LType.t M.t * KNormal.t list) * KNormal.t

(* string of Cons. *)
let cons_str t =
  (* template env. *)
  let benv buf env =
    M.iter
      (fun x tx ->
         if not(M.mem x Builtin.dtypes) then
           (match tx with
              | LType.Fun(_) ->
                  Buffer.add_string buf (x ^ ": (" ^ LType.type_str tx ^ "); ")
              | _ ->
                  Buffer.add_string buf (x ^ ": " ^ LType.type_str tx ^ "; ")))
      env in
    (*Buffer.add_string buf "...; " in*)
  let bqenv buf qenv =
    List.iter
      (fun q -> Buffer.add_string buf (KNormal.short_str q ^ "; "))
      qenv in
  match t with
    | WellFormed((env, qenv), t) ->
        let result = Buffer.create 1024 in
          benv result env;
          bqenv result qenv;
          Buffer.add_string result "⊢ ";
          Buffer.add_string result (LType.type_str t);
          Buffer.contents result
    | SubType((env, qenv), t1, t2) ->
        let result = Buffer.create 1024 in
          benv result env;
          bqenv result qenv;
          Buffer.add_string result "⊢ ";
          Buffer.add_string result (LType.type_str t1);
          Buffer.add_string result " <: ";
          Buffer.add_string result (LType.type_str t2);
          Buffer.contents result
    | BoolExp((env, qenv), e) ->
        let result = Buffer.create 1024 in
          benv result env;
          bqenv result qenv;
          Buffer.add_string result "⊢ ";
          Buffer.add_string result (KNormal.short_str e);
          Buffer.add_string result " : bool";
          Buffer.contents result



(* generate constraints.
 * env: Type environment.
 * qenv: Type environment (conditions). *)
let rec g (env: LType.t M.t) (qenv: KNormal.t list) (e: KNormal.t) =
  match e with
    | Bool v -> (LType.c_bool v, [])
    | Int v -> (LType.c_int v, [])
    | Var x ->
        (* xがBase Typeか確認 *)
        let t = M.find x env in
          (match t with
             | LType.Base(bt,_) ->
                 (* 値が決まっている *)
                 let t2 = LType.c_var bt x in
                   (t2, [])
             | _ ->
                 (t, []))
    | App(e1, e2) ->
        let (t1, cs1) = g env qenv e1 in
        let (t2, cs2) = g env qenv e2 in
          (match t1 with
             | LType.Fun((ta, a), td) ->
                 (* t2はtaのsubtypeである必要がある *)
                 let c = SubType((env, qenv), t2, ta) in
                 (* 返り値の型はtdのaをe2で置き換えた型 *)
                   (LType.subst (e2, a) td, c::cs1@cs2)
             | _ ->
                 assert false)
    | If(e1, e2, e3) ->
        (* 値のML型を推論 *)
        let bt = hm (shape_env env) e in
        (* FreshなTemplateを作成 *)
        let t = LType.fresh bt in
        let (_, cs1) = g env qenv e1 in
        (* 制約を追加 *)
        let (t2, cs2) = g env (e1 :: qenv) e2 in
        (* 逆を作る *)
        let note1 = App(Var(Constant.not), e1) in
        let (t3, cs3) = g env (note1 :: qenv) e3 in
        (* if文の制約 *)
        (* Well-Formedness *)
        let cwf = WellFormed((env, qenv), t) in
        (* SubTyping *)
        let cst1 = SubType((env, e1 :: qenv), t2, t) in
        let cst2 = SubType((env, note1 :: qenv), t3, t) in
          (t, cwf::cst1::cst2:: cs1 @ cs2 @ cs3)
    | Lambda(_, e1) ->
        (* 値のML型 *)
        let bt = hm (shape_env env) e in
        (* Freshにする *)
        let t = LType.fresh bt in
        (* 引数と返り値を得る *)
          (match t with
             | LType.Fun((ta, a), td) ->
                 let env' = M.add a ta env in
                 (* bodyの制約 *)
                 let (td', cs) = g env' qenv e1 in
                 (* Lambdaの制約 *)
                 (* Well-Formedness *)
                 let cwf = WellFormed((env, qenv), t) in
                 (* subtype *)
                 let cst = SubType((env', qenv), td', td) in
                   (t, cwf::cst::cs)
             | _ -> assert false)
    | Let((tx, x), e1, e2) ->
        let bt = hm (shape_env env) e in
        let t = LType.fresh bt in
        let (t1, cs1) = g env qenv e1 in
        let env' = M.add x t1 env in
        let (t2, cs2) = g env' qenv e2 in
        (* Letの制約 *)
        let cwf = WellFormed((env, qenv), t) in
        let cst = SubType((env', qenv), t2, t) in
          (t, cwf::cst::cs1@cs2)
         
(* shape of environment. *)
and shape_env env =
  M.map LType.shape env
(* ML Type construction. *)
and hm env (e: KNormal.t) =
  match e with
    | Bool _ -> BType.Bool
    | Int _ -> BType.Int
    | Var x -> M.find x env
    | Lambda((tx, x), e1) ->
        let env' = M.add x tx env in
        let t2 = hm env' e1 in
          BType.Fun((tx, x), t2)
    | App(e1, e2) ->
        let t1 = hm env e1 in
          (match t1 with
             | BType.Fun((ta, _), td) ->
                 td
             | _ -> assert false)
    | If(x, e1, e2) ->
        hm env e1
    | Let((tx, x), e1, e2) ->
        let env' = M.add x tx env in
          hm env' e2

let f e = g Builtin.dtypes [] e


(* Split constraints. *)
let rec split cs =
  List.fold_right
    (fun c css' ->
       let c's =
         (match c with
            | WellFormed((env, qenv), t) ->
                (match t with
                   (* Funの引数は環境へ(WT-FUN) *)
                   | LType.Fun((ta, a), td) ->
                       let c' = WellFormed((M.add a ta env, qenv), td) in
                         split [c']
                   | LType.Base(bt, LType.RExp e) ->
                       (* WT-BASE *)
                       (* Liquid Type化する *)
                       let bt' = LType.Base(bt, LType.RExp(Bool true)) in
                       let c' = BoolExp((M.add Constant.nu bt' env, qenv), e) in
                         split [c']
                   | _ -> [c])
            | SubType((env, qenv), t1, t2) ->
                (match t1, t2 with
                   | (LType.Fun((ta1, a1), td1), LType.Fun((ta2, a2), td2)) when a1 = a2 ->
                       (* 関数だったら分解する（引数は逆なので注意） *)
                       let ca = SubType((env, qenv), ta2, ta1) in
                       let ca's = split [ca] in
                       let env' = M.add a2 ta2 env in
                       let cd = SubType((env', qenv), td1, td2) in
                       let cd's = split [cd] in
                         ca's @ cd's
                   | _ -> [c])
            | _ -> [c]) in
         c's@css')
    cs
    []

(* Solve constraints. *)
    (*
let rec solve (invalids: Q.t list) (valids: Q.t list) =
  match invalids with
    | [] ->
        (* すべて解決した *)
        ()
    | c::cs ->
        (* cがvalidでないconstraintだ *)
        weaken c;
          (* TODO *)
          solve cs (c::valids)
and weaken c =
  match c with
    | WellFormed((env, qenv), (sts, LType.Base(bt, LType.RFVar(i, r)))) ->
        (* 現在のQualifiers *)
        let qs = MI.find i a in

and getqs rfs =
  match rfs with
    | LType.RFVar(i, { contents = Some rfs' }) -> getqs rfs'
    | LType.RFVar(i, { contents = None }) ->
        (* 初期値 *)


     *)
