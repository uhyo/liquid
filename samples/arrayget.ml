let f = fun a -> fun x ->
  if 0 <= x then
    if x < len a then
      arrayget a x
    else
      0
  else
    0 in
let a1 = mkarray 10 (fun i -> i) in
  arrayget a1 3
