module S =
  Set.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include S

