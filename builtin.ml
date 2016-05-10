open Q
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

(* Dependent Type for built-in functions. *)
let lbint = LType.Base(BType.Int, LType.RFQs [])
let lbbool= LType.Base(BType.Bool, LType.RFQs [])
let dtypes = M.of_list
               (List.map
                  (fun (n, t) -> (n, Template.of_l t))
                  [
                    (lt, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RFQs [QIff(QNu, QLt(QVar("x"), QVar("y")))]))));
                    (gt, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RFQs [QIff(QNu, QGt(QVar("x"), QVar("y")))]))));
                    (le, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RFQs [QIff(QNu, QLe(QVar("x"), QVar("y")))]))));
                    (ge, LType.Fun((lbint, "x"),
                         LType.Fun((lbint, "y"),
                         LType.Base(BType.Bool, LType.RFQs [QIff(QNu, QGe(QVar("x"), QVar("y")))]))));
                    (not, LType.Fun((lbbool, "x"),
                          LType.Base(BType.Bool, LType.RFQs [QIff(QNu, QNot(QVar("x")))])))
                  ])
