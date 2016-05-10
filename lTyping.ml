open KNormal

(* 式と式の型情報が与えられている *)
let rec template_term e typ =
  match e with
    | Bool v -> Bool v
    | Int v -> Int v 
    | Var x -> Var x
    | Lambda((_, x), e2) ->
        (* 型情報はletから拾う *)
        (match typ with
           | LType.Fun((t, x'), t2) ->
               assert(x = x');
               let e2' = template_term e2 t2 in
                 Lambda((t, x), e2')
           | _ -> assert false)
    | App(x, y) -> App(x, y)
    | If(x, e2, e3) ->
        let e2' = template_term e2 typ in
        let e3' = template_term e3 typ in
          If(x, e2', e3')
    | Let((t, x), e1, e2) ->
        let t' = LType.template t in
        let e1' = template_term e1 t' in
        let e2' = template_term e2 typ in
          Let((t', x), e1', e2')
    | LetRec((tx, x), ((_, y), e1), e2) ->
        (* XXX ??? It should be broken *)
        let tx' = LType.template tx in
          (match tx' with
             | LType.Fun((ty, y'), td) ->
                 assert(y = y');
                 let e1' = template_term e1 td in
                 let e2' = template_term e2 typ in
                   LetRec((tx', x), ((ty, y), e1'), e2')
             | _ -> failwith "MOOOOOM")

let f (e: BType.t KNormal.t) (btyp: BType.t) =
  template_term e (LType.template btyp)
