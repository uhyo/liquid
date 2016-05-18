(* Built-in something. *)
(* special variable. *)
let nu = "ν"
let star = "*"
(* Built-in functions. *)
let iff= "⇔"
let lt = "<"
let gt = ">"
let le = "<="
let ge = ">="
let not = "not"
let eq = "="
let add = "+"
let sub = "-"

(* BType for built-in functions. *)
let btypes = M.of_list
                  [
                    (lt, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Bool)));
                    (gt, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Bool)));
                    (le, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Bool)));
                    (ge, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Bool)));
                    (eq, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Bool)));
                    (not, BType.Fun((BType.Bool, "x"), BType.Bool));
                    (add, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Int)));
                    (sub, BType.Fun((BType.Int, "x"),
                         BType.Fun((BType.Int, "y"), BType.Int)))
                  ]
