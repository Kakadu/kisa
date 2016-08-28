open Printf
open FormatList
open While
open While.Stmt
open While.Expr

let expr00 = Var  "n"
let expr03 = Cons 1 
let expr01 = Binop ("*", Var "res", Var  "n")
let expr02 = Binop ("-", Var "k"  , Cons  1 )

let term01 =
  Seq [Read "n";
       Read "k";
       Assign ("res", Cons 1);
       While (Binop (">", Var "k", Cons 0),
              Seq   [Assign ("res", expr01);
                     Assign ("k"  , expr02);]);
       Write (Var "res")]

let testStmt n term =
  FormatList.default_width := n;
  Printf.printf "%s\n\n" @@
    to_string (Stmt.to_format_list term)

let testExpr n term =
  FormatList.default_width := n;
  Printf.printf "%s\n\n" @@
    to_string (Expr.to_format_list term)

let _ =
  testStmt 20 term01;
  testStmt 25 term01;
  testStmt 50 term01;
  try 
    testStmt 15 term01
  with Failure s ->
    Printf.printf "Ok\n\n"
