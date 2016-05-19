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


let rs x = ref (Some x)
(* BType for built-in functions. *)
let btypes = M.of_list
                  [
                    (lt, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (gt, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (le, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (ge, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (eq, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Bool)));
                    (not, BType.Fun((BType.Bool, rs "x"), BType.Bool));
                    (add, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Int)));
                    (sub, BType.Fun((BType.Int, rs "x"),
                         BType.Fun((BType.Int, rs "y"), BType.Int)))
                  ]
