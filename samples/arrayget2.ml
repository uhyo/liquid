let a1 = mkarray 10 (fun i -> i+i) in
let a2 = mkarray 10 (fun j -> arrayget a1 j) in
  0
