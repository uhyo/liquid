open Syntax

(* 式をtemplate化しつつ式の型も返す *)
let rec template_term' env e =
  match e with
    | Bool v -> (Bool v, LType.c_bool v)
    | Int v -> (Int v, LType.c_int v)
    | Var x -> (Var x, M.find x env)
    | Lambda((t, x), e2) ->
        let t1 = LType.template t in
        let env' = M.add x t1 env in
        let (e2', t2) = template_term' env' e2 in
        let e' = Lambda((t1, x), e2') in
        let t' = LType.Fun((t1, x), t2) in
          (e', t')
    | App(e1, e2) ->
        let (e1', t1) = template_term' env e1 in
        let (e2', t2) = template_term' env e2 in
          (match t1 with
             | LType.Fun(_, t1d) ->
                 (App(e1', e2'), t1d)
             | _ -> failwith "OHHHHHHHHHHHHHHH")
    | If(e1, e2, e3) ->
        let (e1', _) = template_term' env e1 in
        let (e2', t2)= template_term' env e2 in
        let (e3', _) = template_term' env e3 in
        (If(e1', e2', e3'), t2)
    | Let((_, x), e1, e2) ->
        let (e1', t1) = template_term' env e1 in
        let env' = M.add x t1 env in
        let (e2', t2) = template_term' env' e2 in
        (Let((t1, x), e1', e2'), t2)
    | LetRec((tx, x), ((ty, y), e1), e2) ->
        (* XXX ??? It should be broken *)
        let tx' = LType.template tx in
        let env' = M.add x tx' env in
        let (e1', _) = template_term' env' e1 in
        let (e2', t2) = template_term' env' e2 in
        (LetRec((tx', x), ((LType.template ty, y), e1'), e2'), t2)
    (* TODO *)
    | Lt(e1, e2) ->
        let (e1', _) = template_term' env e1 in
        let (e2', _) = template_term' env e2 in
        (Lt(e1', e2'), LType.template BType.Bool)
    | Gt(e1, e2) ->
        let (e1', _) = template_term' env e1 in
        let (e2', _) = template_term' env e2 in
        (Gt(e1', e2'), LType.template BType.Bool)
    | Le(e1, e2) ->
        let (e1', _) = template_term' env e1 in
        let (e2', _) = template_term' env e2 in
        (Le(e1', e2'), LType.template BType.Bool)
    | Ge(e1, e2) ->
        let (e1', _) = template_term' env e1 in
        let (e2', _) = template_term' env e2 in
        (Ge(e1', e2'), LType.template BType.Bool)
    | Not e1 ->
        let (e1', _) = template_term' env e1 in
        (Not(e1'), LType.template BType.Bool)

let template_term e = fst (template_term' M.empty e)
