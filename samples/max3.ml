let max = fun x -> fun y ->
  if x > y then x else y
                         in
let max3 = fun x -> fun y -> fun z ->
  (* max (max x y) z in *)
  let w = max x y in
    max w z in
  0

