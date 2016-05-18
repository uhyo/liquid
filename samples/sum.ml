let rec sum k =
  if k < 0 then 0 else
    let s = sum (k-1) in
      s + k in
  sum
