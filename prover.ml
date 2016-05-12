(* Theorem prover using Z3. *)
open KNormal
open Z3
open Z3.Arithmetic
open Z3.Boolean

let debug_log = ref true

(* 諸事情によりここみ *)
let all_env = ref M.empty



(* 論理式がValidか調べる *)
(* nu_t: νの型 *)
let rec validate (env: LType.t M.t) (qenv: KNormal.t list) (nu_t: BType.t) (goal: KNormal.t list) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "\n---------- Validate ----------\nenv:\n";
  let env' = M.filter
               (fun x t ->
                  match t with
                    | LType.Base(_, LType.RExp _) ->
                        Buffer.add_string buf (Printf.sprintf "%s : %s\n" x (LType.type_str t));
                        true
                    | _ ->
                        (* 他のは使わないので *)
                        Buffer.add_string buf (Printf.sprintf "\027[90m%s : %s\027[39m\n" x (LType.type_str t));
                        false)
               env in
  Buffer.add_string buf "qenv:\n";
  List.iter
    (fun e -> Buffer.add_string buf (Printf.sprintf "%s\n" (KNormal.short_str e)))
    qenv;
  Buffer.add_string buf "goal:\n";
  List.iter
    (fun e -> Buffer.add_string buf (Printf.sprintf "%s\n" (KNormal.short_str e)))
    goal;
  Buffer.add_string buf "  ------\n";
  (* /debug log *)
  (* Z3 context *)
  let ctx = Z3.mk_context [("model", "false"); ("proof", "false")] in
  let g= Goal.mk_goal ctx false false false in
  (* envのshapeも用意しておく（ここはenv'ではなくenvで！） *)
  let env_shape = !all_env in
  (* env'の型がおかしかったらfalseを返す *)
    match List.exists
            (fun (_,t) ->
               match t with
                 | LType.Base(bt, LType.RExp q) ->
                     List.exists
                       (fun e ->
                          let env_shape' = M.add Constant.nu bt env_shape in
                            try
                              BType.Bool <> KNormal.gettype env_shape' e
                            with
                              | KNormal.TypeError
                              | Not_found -> Printf.printf "Z %s\n%s\n" (KNormal.short_str e) (Buffer.contents buf); raise (Failure "a");true)
                       q
                 | _ -> assert false)
            (M.bindings env') with
      | true ->
          (* 型がおかしいやつあった *)
          if !debug_log then
            (Buffer.add_string buf "\027[91menv bad type\027[39m\n";
             Printf.printf "%s" (Buffer.contents buf));
          false
      | false ->
          (* qenvの型もたしかめる *)
          let env_shape' = M.add Constant.nu nu_t env_shape in
          (* TODO 2箇所ある！ *)
          match List.exists
                  (fun e ->
                       try
                         BType.Bool <> KNormal.gettype env_shape' e
                       with
                         | KNormal.TypeError
                         | Not_found -> true)
                  (goal@qenv) (* goalもついでに *)
          with
            | true ->
                if !debug_log then
                  (Buffer.add_string buf "\027[91mqenv bad type\027[39m\n";
                   Printf.printf "%s" (Buffer.contents buf));
                false
            | false ->
                (* envにあるものを全部andでつなげたい *)
                let exps = ref [] in
                M.iter 
                  (fun x t ->
                     match t with
                       | LType.Base(bt, LType.RExp es) ->
                           (* これはつかうぞ *)
                           (* xが型tをもつからνの条件がxに適用される? *)
                           let es' = List.map
                                       (KNormal.subst (KNormal.Var(x), Constant.nu))
                                       es in
                             Buffer.add_string buf (Printf.sprintf "%s : \027[96m" x);
                             if es' = [] then Buffer.add_string buf "true";
                             List.iter
                               (fun e ->
                                  (* debug log *)
                                  Buffer.add_string buf (Printf.sprintf "%s " (KNormal.short_str e));
                                  (* Z3のExprに変換 *)
                                  let expr = e_to_z3expr ctx env_shape e in
                                    (* conjunctionの列をつくっている *)
                                    exps := expr :: !exps)
                               es';
                             Buffer.add_string buf "\027[39m\n"
                       | _ ->
                           ())
                  env;
                (* qenvにあるものも全部つなげたい *)
                List.iter
                  (fun e ->
                     Buffer.add_string buf (Printf.sprintf "\027[96m%s\027[39m\n" (KNormal.short_str e));
                     let expr = e_to_z3expr ctx env_shape' e in
                       exps := expr :: !exps)
                  qenv;
                (* goalをexpr化する *)
                let targetexps = List.map (e_to_z3expr ctx env_shape') goal in
                let targetexpr =
                  if List.length targetexps = 1 then
                    List.hd targetexps
                  else
                    Boolean.mk_and ctx targetexps in
                (* 示したい式作る *)
                let andexpr =
                  if List.length !exps = 1 then
                    List.hd !exps
                  else
                    Boolean.mk_and ctx (!exps) in
                let goalexpr = Boolean.mk_implies ctx andexpr targetexpr in
                (* validity checkのために逆にする *)
                let invexpr = Boolean.mk_not ctx goalexpr in
                (* Goalに追加 *)
                Goal.add g [invexpr];
                Buffer.add_string buf (Printf.sprintf "Goal (inv):\n%s\n" (Goal.to_string g));
                (* Solverを作成 *)
                let sv = Z3.Solver.mk_solver ctx None in
                (* formulaをSolverに放り込む *)
                Z3.Solver.add sv (Goal.get_formulas g);
                let st = Z3.Solver.check sv [] in
                let valid = (st = Z3.Solver.UNSATISFIABLE) in
                (*
                (* やってもらう *)
                (*
                let t1 = Tactic.and_then ctx (Tactic.mk_tactic ctx "simplify") (Tactic.mk_tactic ctx "solve-eqs") [] in
                let t2 = Tactic.repeat ctx t1 10 in
                 *)
                let t2 = Tactic.mk_tactic ctx "smt" in
                let ar = Tactic.apply t2 g None in
                Printf.printf "Result:\n%s\n" (Tactic.ApplyResult.to_string ar);
                let sat = Goal.is_decided_sat g in
                 *)
                  (if valid then
                     Buffer.add_string buf "\027[92mValid\027[39m\n"
                   else
                     Buffer.add_string buf "\027[91mNot Valid\027[39m\n");
                  Printf.printf "%s" (Buffer.contents buf);
                  valid

and e_to_z3expr ctx (env: BType.t M.t) (e: KNormal.t) =
  (* eをZ3のExprに変換する *)
  match e with
    | Bool v -> Boolean.mk_val ctx v
    | Int v -> Integer.mk_numeral_i ctx v
    | Var x -> Expr.mk_const_s ctx x (bt_to_z3sort ctx (M.find x env))
    | Lambda _ ->
        failwith "e_to_z3expr lambda"
    | Let _ ->
        failwith "e_to_z3expr let"
    | If(e1, e2, e3) ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
        let exp3 = e_to_z3expr ctx env e3 in
          mk_ite ctx exp1 exp2 exp3
    (* 組み込み関数の処理 *)
    | App(App(Var(op), e1), e2) when op = Constant.iff ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
          mk_iff ctx exp1 exp2
    | App(App(Var(op), e1), e2) when op = Constant.lt ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
          mk_lt ctx exp1 exp2
    | App(App(Var(op), e1), e2) when op = Constant.gt ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
          mk_gt ctx exp1 exp2
    | App(App(Var(op), e1), e2) when op = Constant.le ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
          mk_le ctx exp1 exp2
    | App(App(Var(op), e1), e2) when op = Constant.ge ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
          mk_ge ctx exp1 exp2
    | App(App(Var(op), e1), e2) when op = Constant.eq ->
        let exp1 = e_to_z3expr ctx env e1 in
        let exp2 = e_to_z3expr ctx env e2 in
          mk_eq ctx exp1 exp2
    | App(Var(op), e1) when op = Constant.not ->
        let exp1 = e_to_z3expr ctx env e1 in
          mk_not ctx exp1
    | App _ ->
        failwith "app"


and bt_to_z3sort ctx (t: BType.t) =
  (* BTypeをZ3のSortに変換する *)
  match t with
    | BType.Bool -> Boolean.mk_sort ctx
    | BType.Int -> Integer.mk_sort ctx
    | _ -> Sort.mk_uninterpreted_s ctx "uniterpreted_sort"
