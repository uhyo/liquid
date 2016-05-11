(* Map whose key is int *)
module MI =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)
include MI

