(* Map whose key is Id.t *)
module M =
  Map.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include M
