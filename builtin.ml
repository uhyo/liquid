(* Built-in functions. *)
let lt = "<"
let gt = ">"
let le = "<="
let ge = ">="
let not = "not"

(* BType for built-in functions. *)
let btypes = M.of_list
               [
                 (lt, BType.Fun((BType.Int, "x"),
                      BType.Fun((BType.Int, "y"), BType.Bool)));
                 (gt, BType.Fun((BType.Int, "x"),
                      BType.Fun((BType.Int, "y"), BType.Bool)));
                 (le, BType.Fun((BType.Int, "x"),
                      BType.Fun((BType.Int, "y"), BType.Bool)));
                 (gt, BType.Fun((BType.Int, "x"),
                      BType.Fun((BType.Int, "y"), BType.Bool)));
                 (not, BType.Fun((BType.Bool, "x"), BType.Bool))
               ]

