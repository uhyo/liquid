let max = fun x -> fun y ->
  if x > y then x else y
                         in
let arraymax = fun a ->
  let rec loop i = fun b ->
    if i < len a then
      (let b2 = max b (arrayget a i) in
         loop (i+1) b2)
    else
      b in
    loop 0 0 in
  0

