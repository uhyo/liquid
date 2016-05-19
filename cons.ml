(* Constraints. *)
open KNormal

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


(* all bindings. *)
let allenv = ref M.empty

(* generate constraints.
 * env: Type environment.
 * qenv: Type environment (conditions). 
 * toplevel: true if functions can be called with arbitary arguments.
 * fopen: true if function here is open *)
let rec g (env: LType.t M.t) (qenv: KNormal.t list) (toplevel: bool) (fopen: bool) (e: KNormal.t) =
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
        let (t1, cs1) = g env qenv toplevel false e1 in
        let (t2, cs2) = g env qenv toplevel false e2 in
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
        let (_, cs1) = g env qenv toplevel false e1 in
        (* 制約を追加 *)
        let (t2, cs2) = g env (e1 :: qenv) toplevel fopen e2 in
        (* 逆を作る *)
        let note1 = App(Var(Constant.not), e1) in
        let (t3, cs3) = g env (note1 :: qenv) toplevel fopen e3 in
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
                   (if a = "l" then
                      M.iter
                        (fun k t ->
                           Printf.printf "%s: %s\n" k (LType.type_str t))
                        env');
                 (* bodyの制約 *)
                 (* bodyの中はtoplevelではないがfopenかも *)
                 let (td', cs) = g env' qenv false fopen e1 in
                 (* Lambdaの制約 *)
                 (* Well-Formedness *)
                 let cwf = WellFormed((env, qenv), t) in
                 (* subtype *)
                 let cst = SubType((env', qenv), td', td) in
                 (* 関数がopenの場合の制約 *)
                 let copn = g_openfunc env qenv fopen t in
                   (t, cwf::cst::copn@cs)
             | _ -> assert false)
    | RecLambda((tx, x), (ty, y), e1) ->
        (* こいつ…… 自分の型を知ってやがる！ *)
        let t = LType.fresh tx in
          (match t with
             | LType.Fun((ta, a), td) ->
                 let env' = M.add x t env in
                 let env''= M.add a ta env' in
                 (* bodyの制約 *)
                 let (td', cs) = g env'' qenv false fopen e1 in
                 (* Well-Formedness *)
                 let cwf = WellFormed((env', qenv), t) in
                 (* subtype *)
                 let cst = SubType((env'', qenv), td', td) in
                 let copn = g_openfunc env' qenv fopen t in
                   (t, cwf::cst::copn@cs)
             | _ -> assert false)

    | Let((tx, x), e1, e2) ->
        let bt = hm (shape_env env) e in
        let t = LType.fresh bt in
        (* letの中はtoplevelではないがfopenに引き継がれる！ *)
        let (t1, cs1) = g env qenv false toplevel e1 in
        (* allenvに入れる *)
        allenv := M.add x t1 !allenv;
        Printf.printf "BOOKOOM %s: %s\n" x (LType.type_str t1);
        let env' = M.add x t1 env in
        let (t2, cs2) = g env' qenv toplevel fopen e2 in
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
          BType.Fun((tx, ref (Some x)), t2)
    | RecLambda((tx, x), (ty, y), e1) ->
        tx
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

(* openなfunctionの型に対して制約を発行する *)
and g_openfunc env qenv fopen t =
  if not fopen then
    []
  else
    (* openなfuncは任意の引数で呼ばれる可能性がある *)
    match t with
      | LType.Fun((ta, a), td) ->
          (* 興味があるのは関数だけ *)
          let ta' = arbitrarize ta in
          let c1 = SubType((env, qenv), ta', ta) in
            (* openなfuncから返される関数も任意の引数で呼ばれる可能性がある *)
            c1::g_openfunc env qenv fopen td
      | _ -> []
(* 型を任意化 *)
and arbitrarize t =
  match t with
    | LType.Base(bt, _) ->
        LType.Base(bt, LType.RExp [])
    | LType.Fun((ta, a), td) ->
        LType.Fun((arbitrarize ta, a), arbitrarize td)



(*ここでfopenがtrueかfalseかは諸説ある（2つしかない） *)
let f e = g Builtin.dtypes [] true false e


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
                   | LType.Base(bt, LType.RExp es) ->
                       (* WT-BASE *)
                       (* Liquid Type化する *)
                       let bt' = LType.Base(bt, LType.RExp([])) in
                         (* esは全部boolでないとだめ *)
                       let c's = List.map
                                   (fun e -> BoolExp((M.add Constant.nu bt' env, qenv), e))
                                   es in
                         split c's
                   | _ -> [c])
            | SubType((env, qenv), t1, t2) ->
                (match t1, t2 with
                   | (LType.Fun((ta1, a1), td1), LType.Fun((ta2, a2), td2)) ->
                       (* 関数だったら分解する（引数は逆なので注意） *)
                       let ca = SubType((env, qenv), ta2, ta1) in
                       let ca's = split [ca] in
                       let env' = M.add a2 ta2 env in
                       let env''= M.add a1 ta2 env' in
                       let cd = SubType((env'', qenv), td1, td2) in
                       let cd's = split [cd] in
                         ca's @ cd's
                   | (LType.Base(bt1, _), LType.Base(bt2, LType.RExp([]))) when BType.equal bt1 bt2 ->
                       (* 自明な制約は取り除く *)
                       []
                   | _ -> [c])
            | _ -> [c]) in
         c's@css')
    cs
    []
