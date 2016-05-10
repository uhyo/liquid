(* Constraint solver. *)
open KNormal
open Cons

(* Map whose key is int *)
module MI =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)

(* 今回のQは……? *)
let default_q = [
  App(App(Var(Constant.le), Int 0),
      Var(Constant.nu));
  App(App(Var(Constant.le), Var(Constant.star)),
      Var(Constant.nu));
  App(App(Var(Constant.lt), Var(Constant.nu)),
      Var(Constant.star))
]


(* 初期のQ*を生成 *)
let inst (env: LType.t M.t) (e: KNormal.t) (q: KNormal.t list) =
  let vsenv = S.of_list (List.map fst (M.bindings env)) in
  let vse = KNormal.vars e in
  let vs = S.elements (S.union vsenv vse) in
  (* XXX starは1つのみと仮定 *)
    List.concat
    (List.map
       (fun e ->
          if S.mem Constant.star (KNormal.vars e) then
            (* eの中のstarを自由変数で置き換える *)
            (List.map
              (fun v ->
                 KNormal.subst (Var(v), Constant.star) e)
              vs)
          else
            (* starとかなかった *)
            [e])
       q)

let default_inst = ref []

let show_qs (qs: KNormal.t list) =
  List.iter
    (fun e -> Printf.printf "%s\n" (KNormal.short_str e))
    qs

let rec solve (invalids: Cons.t list) (valids: Cons.t list) (assignment: (KNormal.t list) MI.t) =
  match invalids with
    | [] ->
        (* すべて解決した *)
        ()
    | c::cs ->
        (* cがvalidでないconstraintだ *)
        let a' = weaken c assignment in
          (* TODO *)
          solve cs (c::valids) a'
and weaken c a =
  match c with
    | WellFormed((env, qenv), LType.Base(bt, LType.RSubst(sts, i))) ->
        (* 現在のQualifiers *)
        let qs = get_asgn i a in
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
    | _ -> assert false

(* assignmentから値を引く（ただしデフォルト値がある） *)
and get_asgn i a =
  try
    MI.find i a
  with Not_found -> !default_inst


let f (cs: Cons.t list) (q: KNormal.t list) (e: KNormal.t) =
  default_inst := inst Builtin.dtypes e q;
  List.iter
    (fun e -> Printf.printf "%s\n" (KNormal.short_str e))
    !default_inst;
  solve cs [] MI.empty
