let max = fun x -> fun y ->
  if x > y then x else y
                         in
let foldn = fun n -> fun b -> fun f ->
  let rec loop i = fun c ->
    if i < n then
      let fi = f i in
      let fic = fi c in
      loop (i+1) fic
    else c in
  loop 0 b in
let arraymax = fun a ->
  let am = fun l -> fun m -> max (sub a l) m in
    foldn (len a) 0 am in
  0

