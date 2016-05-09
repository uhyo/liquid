let max = fun x -> fun y ->
  if x > y then x else y
                         in
let a = max 3 5 in
let b = max 3 in
  b
