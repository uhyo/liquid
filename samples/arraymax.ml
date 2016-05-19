let max = fun x -> fun y ->
  if x > y then x else y
                         in
let arraymax =
  let foldn = fun n -> fun b -> fun f ->
    let rec loop i = fun c ->
      if i < n then
        let fic = f i c in
          loop (i+1) fic
          else c in
      loop 0 b in
  fun a ->
    let am = fun l -> fun m ->
      let d1= arrayget a in
      let d = d1 l in
      let d2 = max d in
        d2 m in
    let l1 = len a in
    let f1 = foldn l1 in
    let f2 = f1 0 in
      f2 am in
  0

