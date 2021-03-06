open KNormal
(* Liquid Type. *)

(* elements of refinements predicates. *)
type refm_t =
  (* ML expression. *)
  | RExp of KNormal.t list
  (* Pending Substitutions. *)
  | RSubst of (KNormal.t * Id.t) list * int

let nextid = ref 1
let genid () =
  let id = !nextid in
    nextid := id + 1;
    id

(* Liquid Types. *)
type t =
  | Base of BType.t * refm_t
  | Fun of (t * Id.t) * t

(* constant type. *)
let c_bool = function
  | true -> Base(BType.Bool,
                 RExp([Var(Constant.nu)]))
  | false -> Base(BType.Bool,
                  RExp([App(Var(Constant.not),
                            Var(Constant.nu))]))

let c_int v =
  Base(BType.Int,
       RExp([App(App(Var(Constant.eq), Var(Constant.nu)),
                 Int(v))]))
    (* var type. *)
let c_var bt x =
  let eqop = (match bt with
                | BType.Bool -> Constant.iff
                | BType.Int -> Constant.eq
                | BType.IntArray -> Constant.arrayeq
                | _ -> assert false) in
  Base(bt,
       RExp([App(App(Var(eqop), Var(Constant.nu)),
                 Var(x))]))

(* Pending substitutionをapplyする *)
let rec subst ((e, x) as st) t =
  match t with
    | Base(bt, rfm) -> Base(bt, subst_rfm st rfm)
    | Fun((ta, a), td) ->
        if a = x then
          Fun((subst st ta, a), td)
        else
          Fun((subst st ta, a), subst st td)
and subst_rfm st rfm =
  match rfm with
    | RExp es ->
        RExp(List.map (KNormal.subst st) es)
    | RSubst(sts, i) ->
        RSubst(st::sts, i)

(* FreshなTemplateを作成 *)
let rec fresh (t: BType.t) =
  match t with
    | t when BType.is_btype t -> Base(t, RSubst([], genid()))
    | BType.Fun((ta, {contents = Some x}), td) ->
        Fun((fresh ta, x), fresh td)
    | _ -> failwith "LType.fresh"

(* BTypeにもどす *)
let rec shape t =
  match t with
    | Base(bt, _) -> bt
    | Fun((bta, a), btd) ->
        BType.Fun((shape bta, ref (Some a)), shape btd)

let rec is_funtype = function
  | Fun(_) -> true
  | _ -> false

let rec refm_str t =
  match t with
    | RExp [] -> "true"
    | RExp es ->
        let result = Buffer.create 512 in
          List.iteri
            (fun i e ->
               if i > 0 then Buffer.add_string result "; ";
               Buffer.add_string result (KNormal.short_str e))
            es;
          Buffer.contents result
    | RSubst(sts, i) ->
        let result = Buffer.create 512 in
          List.iter
            (fun (e, x) ->
               let es = KNormal.short_str e in
               if es <> x then
                 Buffer.add_string result (Printf.sprintf "[%s/%s]" es x))
            sts;
          Buffer.add_string result ("κ" ^ string_of_int i);
          Buffer.contents result


let rec type_str t =
  match t with
    | Base(bt,refm) ->
        "{ν : " ^ BType.type_str bt ^ " | " ^ refm_str refm ^ "}"
    | Fun((t1, x), t2) ->
        (if is_funtype t1 then
           x ^ ":(" ^ type_str t1 ^ ") -> " ^ type_str t2
         else
           x ^ ":" ^ type_str t1 ^ " -> " ^ type_str t2)
