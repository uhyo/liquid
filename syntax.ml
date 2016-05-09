type t =
  | Bool of bool

let tree = Tree.make (function
  | Bool t -> ("BOOL " ^ string_of_bool t, [])
) "  "
