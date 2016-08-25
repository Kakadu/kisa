open FormatList
open Printf

module Expr =
  struct
    type t = Var   of string
           | Cons  of int
           | Binop of string * t * t

    let rec to_format_list = function
      | Var  s -> !s
      | Cons n -> !(string_of_int n)
      | Binop (c, t1, t2) ->
         let f1  = (to_format_list t1) in
         let f2  = (to_format_list t2) in
         (f1 >|< !c >|< f2) >?<
           (f1 >-< ((2 >> !c) >|< f2))
  end

module Stmt =
  struct
    type t = Read   of string
           | Write  of Expr.t
           | Assign of string * Expr.t
           | Seq    of t list 
           | If     of Expr.t * t * t
           | While  of Expr.t * t

    let rec to_format_list = function
      | Read    s    -> !"read("  >|< !s >|< !")" 
      | Write   e    -> !"write(" >|< (Expr.to_format_list e) >|< !")" 
      | Assign (s,e) -> !s >|< !" := " >|< (Expr.to_format_list e) 
      | Seq     sl   ->
         List.fold_left (fun s fs ->
             let f = to_format_list s in
             (fs >|< !"; " >|< f) >?<
               (fs >|< !";" >-< f)
           ) initial sl
      | If (e,s1,s2) ->
         let e  = Expr.to_format_list e in
         let f1 = to_format_list s1 in
         let f2 = to_format_list s2 in
         (!"if " >|< (e ^ 1) >|< !" then " >|< (f1 ^ 1) >|< !" else " >|< (f2 ^ 1)) >?<
         (!"if " >|< e >-<
            !" then " >|< f1 >-<
            !" else " >|< f2)
  end
                             
exception Not_Operation 

let op = function
  | '+' -> (+)
  | '-' -> (-)
  | '*' -> ( * )
  | '/' -> (/)
  | '%' -> (mod)
  | _   -> raise Not_Operation

let term01 =
  Seq [Read "n";
       Read "k";
       Assign ("res", Cons 1);
       While (Binop (">", Var "k", Cons 0),
              Seq   [Assign ("res", (Binop ("*", Var "res", Var  "n")));
                     Assign ("k"  , (Binop ("-", Var "k"  , Cons "1")));]);
       Write (Var "res")]

let _ =
  let res = to_string (Stmt.to_format_list term01) in
  Printf.printf "%s\n" res

