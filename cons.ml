(* Constraints. *)
open KNormal
open Q

(* Map whose key is int *)
module MI =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)

type t =
  (* WellFormed((a, qs), t1) <=> a; qs |- t1 *)
  | WellFormed of (Template.t M.t * Q.t list) * Template.t
  (* SubType((a, qs), t1, t2) <=> a; qs |- t1 <: t2 *)
  | SubType of (Template.t M.t * Q.t list) * Template.t * Template.t

(* string of Cons. *)
let cons_str t =
  (* template env. *)
  let benv buf env =
    (*
    M.iter
      (fun x tx ->
         let (_, lt) = tx in
           match lt with
             | LType.Fun(_) ->
                 Buffer.add_string buf (x ^ ": (" ^ Template.type_str tx ^ "); ")
             | _ ->
                 Buffer.add_string buf (x ^ ": " ^ Template.type_str tx ^ "; "))
      env in
     *)
    Buffer.add_string buf "...; " in
  let bqenv buf qenv =
    List.iter
      (fun q -> Buffer.add_string buf (Q.q_str q ^ "; "))
      qenv in
  match t with
    | WellFormed((env, qenv), t) ->
        let result = Buffer.create 1024 in
          benv result env;
          bqenv result qenv;
          Buffer.add_string result "⊢ ";
          Buffer.add_string result (Template.type_str t);
          Buffer.contents result
    | SubType((env, qenv), t1, t2) ->
        let result = Buffer.create 1024 in
          benv result env;
          bqenv result qenv;
          Buffer.add_string result "⊢ ";
          Buffer.add_string result (Template.type_str t1);
          Buffer.add_string result " <: ";
          Buffer.add_string result (Template.type_str t2);
          Buffer.contents result



(* generate constraints.
 * env: Type environment.
 * qenv: Type environment (conditions). *)
let rec g (env: Template.t M.t) (qenv: Q.t list) (e: LType.t KNormal.t) =
  match e with
    | Bool v -> (Template.c_bool v, [])
    | Int v -> (Template.c_int v, [])
    | Var x ->
        (* xがBase Typeか確認 *)
        let t = M.find x env in
          (match t with
             | (_, LType.Base(bt, _)) ->
                 (* 値が決まっている *)
                 let t2 = Template.of_l (LType.Base(bt, LType.RFQs [QEq(QNu, QVar x)])) in
                   (t2, [])
             | _ ->
                 (t, []))
    | App(x, y) ->
        let (sts, tx) = M.find x env in
          (match tx with
             | LType.Fun((ta, a), td) ->
                 let (stsy, ty) = M.find y env in
                 (* tyはtaのsubtypeである必要がある *)
                 let c = SubType((env, qenv), (sts@stsy, ty), (sts, ta)) in
                 (* 返り値の型はtdのaをyで置き換えた型 *)
                   (Template.subst (y, a) (sts, td), [c])
             | _ ->
                 assert false)
    | If(x, e1, e2) ->
        (* 値のML型を推論 *)
        let bt = hm (shape_env env) e in
        (* FreshなTemplateを作成 *)
        let t = Template.of_b bt in
        (* xが表す条件を抽出 *)
        let xqs = getq (M.find x env) in
        (* 逆もつくる *)
        let xqinvs = List.map
                       (fun q -> QNot q)
                       xqs in
        (* 制約を洗い出す *)
        let (t1, cs1) = g env (xqs @ qenv) e1 in
        let (t2, cs2) = g env (xqinvs @ qenv) e2 in
        (* if文の制約 *)
        (* Well-Formedness *)
        let cwf = WellFormed((env, qenv), t) in
        (* SubTyping *)
        let cst1 = SubType((env, xqs @ qenv), t1, t) in
        let cst2 = SubType((env, xqinvs @ qenv), t2, t) in
          (t, cwf::cst1::cst2:: cs1 @ cs2)
    | Lambda(_, e1) ->
        (* 値のML型 *)
        let bt = hm (shape_env env) e in
        (* Freshにする *)
        let lt = LType.skeleton bt in
        (* 引数と返り値を得る *)
          (match lt with
             | LType.Fun((ta, a), td) ->
                 let env' = M.add a (Template.of_l ta) env in
                 (* bodyの制約 *)
                 let (td', cs) = g env' qenv e1 in
                 (* Lambdaの制約 *)
                 let t = Template.of_l lt in
                 (* Well-Formedness *)
                 let cwf = WellFormed((env, qenv), t) in
                 (* subtype *)
                 let cst = SubType((env', qenv), td', Template.of_l td) in
                   (t, cwf::cst::cs)
             | _ -> assert false)
    | Let((tx, x), e1, e2) ->
        let bt = hm (shape_env env) e in
        let (t1, cs1) = g env qenv e1 in
        let env' = M.add x t1 env in
        let (t2, cs2) = g env' qenv e2 in
        let t = Template.of_b bt in
        (* Letの制約 *)
        let cwf = WellFormed((env, qenv), t) in
        let cst = SubType((env', qenv), t2, t) in
          (t, cwf::cst::cs1@cs2)
    | LetRec _ ->
        failwith "LETRECCCCCCCCCCCCC"
         

(* Boolean variableの型から条件を抽出 *)
and getq t =
  Printf.eprintf "AAAR %s\n" (Template.type_str t);
  let rec fromrfs rfs =
    match rfs with
      | LType.RFVar(_, { contents = Some rfs' }) -> fromrfs rfs'
      | LType.RFVar _ -> []
      | LType.RFQs qs ->
          fromqs qs
  and fromqs qs =
    List.fold_right
      (fun q rs ->
         match fromq q with
           | Some q' -> q'::rs
           | _ -> rs)
      qs
      []
  and fromq q =
    (* Nuと同値な条件を引き出す *)
    match q with
      | QIff(QNu, q1)
      | QIff(q1, QNu) ->
          Some q1
      | _ -> None in
  let lt = Template.resolve t in
    match lt with
      (* 真偽値だ *)
      | LType.Base(BType.Bool, rfs) -> fromrfs rfs
      | _ -> assert false

(* shape of environment. *)
and shape_env env =
  M.map
    (fun (_, t) -> LType.shape t)
    env
(* ML Type construction. *)
and hm env (e: LType.t KNormal.t) =
  match e with
    | Bool _ -> BType.Bool
    | Int _ -> BType.Int
    | Var x -> M.find x env
    | Lambda((tx, x), e1) ->
        let tx' = LType.shape tx in
        let env' = M.add x tx' env in
        let t2 = hm env' e1 in
          BType.Fun((tx', x), t2)
    | App(x, y) ->
        let tx = M.find x env in
          (match tx with
             | BType.Fun((ta, _), td) ->
                 td
             | _ -> assert false)
    | If(x, e1, e2) ->
        hm env e1
    | Let((tx, x), e1, e2) ->
        let tx' = LType.shape tx in
        let env' = M.add x tx' env in
          hm env' e2
    | LetRec((tx, x), ((ty, y), e1), e2) ->
        let tx' = LType.shape tx in
        let env' = M.add x tx' env in
          hm env' e2

let f e = g Builtin.dtypes [] e


(* Split constraints. *)
let rec split cs =
  List.fold_right
    (fun c css' ->
       let c's =
         (match c with
            | WellFormed((env, qenv), (sts, lt)) ->
                (match lt with
                   (* Funは環境へ *)
                   | LType.Fun((ta, a), td) ->
                       let c' = WellFormed((M.add a (sts, ta) env, qenv), (sts, td)) in
                         split [c']
                   | LType.Base(BType.Bool, _) ->
                       (* XXX 大丈夫な気がするからにぎりつぶす *)
                       []
                   | _ -> [c])
            | SubType((env, qenv), (sts1, lt1), (sts2, lt2)) ->
                (match lt1, lt2 with
                   | (LType.Fun((ta1, a1), td1), LType.Fun((ta2, a2), td2)) when a1 = a2 ->
                       (* 関数だったら分解する（引数は逆なので注意） *)
                       let ca = SubType((env, qenv), (sts2, ta2), (sts1, ta1)) in
                       let ca's = split [ca] in
                       let env' = M.add a2 (sts2, ta2) env in
                       let cd = SubType((env', qenv), (sts1, td1), (sts2, td2)) in
                       let cd's = split [cd] in
                         ca's @ cd's
                   | _ -> [c])) in
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
