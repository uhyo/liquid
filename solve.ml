(* Constraint solver. *)
open KNormal
open Cons


(* 今回のQは……? *)
let default_q = [
  App(App(Var(Constant.le), Int 0),
      Var(Constant.nu));
  App(App(Var(Constant.le), Var(Constant.star)),
      Var(Constant.nu));
  App(App(Var(Constant.lt), Var(Constant.nu)),
      Var(Constant.star));
  App(App(Var(Constant.lt), Var(Constant.nu)),
      App(Var(Constant.len), Var(Constant.star)))
]

let infer_q = ref []

(* 初期のQ*を生成 *)
let inst (env: LType.t M.t) (nu_t: BType.t) =
  let vs = List.map fst (M.bindings env) in
    (* XXX starは1つのみと仮定 *)
  let vs' = List.concat
              (List.map
                 (fun e ->
                    if S.mem Constant.star (KNormal.varset e) then
                      (* eの中のstarを自由変数で置き換える *)
                      (List.map
                         (fun v ->
                            KNormal.subst (Var(v), Constant.star) e)
                         vs)
                    else
                        (* starとかなかった *)
                        [e])
                 (!infer_q)) in
  (* 型がおかしいものは除外 *)
  let env' = M.add Constant.nu nu_t (!Prover.all_env) in
  let vs'' = 
    List.filter
      (fun e ->
         try
           BType.Bool = KNormal.gettype env' e
         with
           | KNormal.TypeError
           | Not_found -> false)
      vs' in
    vs''

let show_qs (qs: KNormal.t list) =
  List.iter
    (fun e -> Printf.printf "%s\n" (KNormal.short_str e))
    qs

let rec solve (invalids: Cons.t list) (valids: Cons.t list) (assignment: (KNormal.t list) MI.t) =
  match invalids with
    | [] ->
        (* すべて解決した *)
        assignment
    | c::cs ->
        (* cはほんとうにivalidか? *)
        let v = c_valid_asgn c assignment in
          (if v then
             (* cはvalidだった！ *)
             solve cs (c::valids) assignment
           else
             (* cはinvalidだからよわくしないと *)
               let a' = weaken c assignment in
                 solve (invalids@valids) [] a')
and weaken c a =
  match c with
    | WellFormed((env, qenv), LType.Base(bt, LType.RSubst(sts, i))) ->
        Printf.printf "Weakening \027[93m%s\027[39m\n" (Cons.cons_str c);
        (* 現在のQualifiers *)
        let qs = get_asgn env bt i a in
        (* ちゃんとboolになるやつだけ残す *)
        let env' = Cons.shape_env env in
        let env'' = M.add Constant.nu bt env' in
        let qs' = List.filter
                    (fun e -> 
                       try
                         let bt = KNormal.gettype env'' e in
                           BType.equal bt BType.Bool
                       with
                         | TypeError
                         | Not_found ->
                             false)
                    qs in
          Printf.printf "qs:\n";
          show_qs qs;
          Printf.printf "qs':\n";
          show_qs qs';
        let a' = MI.add i qs' a in
          a'
    | SubType((env, qenv), (LType.Base(bt1, refm) as t1), LType.Base(bt2, LType.RSubst(sts, i))) ->
        Printf.printf "Weakening \027[93m%s\027[39m\n" (Cons.cons_str c);
        let qs = get_asgn env bt1 i a in
        (* envにaを適用 *)
        let env' = apply_asgn_env a env in
        (* t1にもaを適用 *)
        let t1' = apply_asgn_lt env a t1 in
          (match t1' with
             (* さっきパターンマッチしたしね *)
             | LType.Base(bt1, LType.RExp es) ->
                 (* debug *)
                 MI.iter
                   (fun i qs ->
                      Printf.printf "% 2d: " i;
                      List.iter
                        (fun q -> Printf.printf "%s " (KNormal.short_str q))
                        qs;
                      Printf.printf "\n")
                   a;
                   (* /debug *)
                 let qs' = List.filter
                             (fun q ->
                                (* Pending substitutionを適用 *)
                                let q' = List.fold_right
                                           KNormal.subst
                                           sts
                                           q in
                                (* validかどうか調べる *)
                                Prover.validate env' (qenv @ es) bt1 [q'])
                             qs in
                   (* debug *)
                   Printf.printf "Updating κ%d\n" i;
                   (* /debug *)
                 let a' = MI.add i qs' a in
                   a'
             | _ -> assert false)

    | _ ->
        (* 制約を満たせなかった！！！！！ *)
        Printf.printf "error: \027[91m%s\027[39m\nAssignments:\n\n" (Cons.cons_str c);
        MI.iter
          (fun i qs ->
             Printf.printf "κ%d: " i;
             if List.length qs = 0 then Printf.printf "true";
             List.iter
               (fun q -> Printf.printf "%s " (KNormal.short_str q))
               qs;
             Printf.printf "\n")
          a;
        failwith "FAILURE!!!"

(* A(c) がvalidか *)
and c_valid_asgn c a =
  let c' = (match c with
              | WellFormed((env, qenv), t) ->
                  (* aでmapする *)
                  let env' = apply_asgn_env a env in
                  let t' = apply_asgn_lt env a t in
                  WellFormed((env', qenv), t')
              | SubType((env, qenv), t1, t2) ->
                  (*
                  M.iter
                    (fun x t -> Printf.printf "HUHUHU %s: %s\n" x (LType.type_str t))
                    env;
                   *)
                  let env' = apply_asgn_env a env in
                  let t1' = apply_asgn_lt env' a t1 in
                  let t2' = apply_asgn_lt env' a t2 in
                    (*
                    M.iter
                      (fun x t -> Printf.printf "HOHOHO %s: %s\n" x (LType.type_str t))
                      env';
                     *)

                  SubType((env', qenv), t1', t2')
              | BoolExp((env, qenv), e) ->
                  let env' = apply_asgn_env a env in
                   BoolExp((env', qenv), e)) in
  let c's = Cons.split [c'] in
    List.for_all c_valid c's
                
(* Base ConsがValidか調べる *)
and c_valid c =
  match c with
    | WellFormed((env, qenv), t) ->
        (* Base CaseはBoolExpだからない XXX Polyとかあると……*)
        false
    | BoolExp((env, qenv), e) ->
        let env_shape = Cons.shape_env env in
          (try
             BType.Bool = KNormal.gettype env_shape e
           with
             | KNormal.TypeError
             | Not_found -> false)
    | SubType((env, qenv), t1, t2) ->
        match t1, t2 with
          | (LType.Base(bt1, LType.RExp es1), LType.Base(bt2, LType.RExp es2)) when BType.equal bt1 bt2 ->
              (* es1, es2をenvで制限する *)
              let env'n = M.add Constant.nu bt1 (Cons.shape_env env) in
              let es1' = limit_env_qs env'n es1 in
              let es2' = limit_env_qs env'n es2 in
                (*
                M.iter
                  (fun x t -> Printf.printf "KKOK %s: %s\n" x (LType.type_str t))
                  env;
                Printf.printf "MMOOM %s\n" (Cons.cons_str c);
                 *)
              Printf.printf "\027[93mValidity Checking for %s\027[39m\n" (Cons.cons_str c);
              Prover.validate env (qenv @ es1') bt1 es2'
          | _ -> false

(* qをenvの範囲に制限（envが知らないqは消す） *)
and limit_env_qs (env: 'a M.t) (q: KNormal.t list) =
  (* envが知っている変数のSet *)
  let envs = S.of_list (List.map fst (M.bindings env)) in
  let q' = List.filter
             (fun e -> S.subset (KNormal.varset e) envs)
             q in
    q'



(* assignmentから値を引く（ただしデフォルト値がある） *)
and get_asgn env nu_t i a =
  try
    MI.find i a
  with Not_found -> (*!default_inst*)
    inst env nu_t

(* envにaを適用 *)
and apply_asgn_env (a: (KNormal.t list) MI.t) (env: LType.t M.t) =
  M.map (apply_asgn_lt env a) env

(* LType.tにaを適用（ただしenvの範囲内で） *)
and apply_asgn_lt env (a: (KNormal.t list) MI.t) (t: LType.t) =
  match t with
    | LType.Base(bt, LType.RSubst(sts, i)) ->
        (* まずiを具体化 *)
        let qs = get_asgn env bt i a in
          (*
          List.iter
            (fun e -> Printf.printf "AOAOAOAOAO %s\n" (KNormal.short_str e))
            qs;
           *)

        (* 全てにstsを適用 *)
        let qs' = List.map
                    (List.fold_right KNormal.subst sts)
                    qs in
          LType.Base(bt, LType.RExp qs')
    | LType.Base(_) ->
        (* RSubstでないならそのまま *)
        t
    | LType.Fun((tx, x), td) ->
        let env' = M.add x tx env in
        LType.Fun((apply_asgn_lt env a tx, x), apply_asgn_lt env' a td)


let f (cs: Cons.t list) (q: KNormal.t list) (e: KNormal.t) =
  (* すべての変数を求める *)
  let env = KNormal.vars M.empty e in
  (* すべての変数とその型 *)
  let vs = M.leftunion Constant.btypes env in
  Prover.all_env := vs;
  infer_q := q;
  solve cs [] MI.empty
