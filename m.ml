(* Map whose key is Id.t *)
module M =
  Map.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include M

let of_list binds = List.fold_left
                      (fun m (k, v) ->
                         M.add k v m)
                      empty
                      binds

let leftunion m1 m2 = merge
                        (fun _ v1 v2 ->
                           match v1, v2 with
                             | (Some v, _) -> Some v
                             | (_, Some v) -> Some v
                             | _           -> None)
                        m1
                        m2

