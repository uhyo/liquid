let foldn = fun n -> fun b -> fun f ->
  let rec loop i = fun c ->
    if i < n then
      let fic = f i c in
      loop (i+1) fic
    else c in
  loop 0 b in
  foldn 10 0 (fun x -> fun y -> x+y)
