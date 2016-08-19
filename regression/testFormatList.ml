open FormatList
open Printf

module Expr =
  struct
    type t = Var  of string
           | Cons of int
           | Binop of char * t * t

    let rec to_format_list t =
      match t with
      | Var  s -> !s
      | Cons n -> !(string_of_int n)
      | Binop (c, t1, t2) ->
         let f1  = (to_format_list t1) in
         let f2  = (to_format_list t2) in
         let fop = !(Char.escaped c) in
         (f1 >|< fop >|< f2) >?<
           (f1 >-< ((2 >> fop) >|< f2))
  end

module Stmt =
  struct
    type t = Read   of string
           | Write  of Expr.t
           | Assign of string * Expr.t
           | Seq    of t * t
           | If     of Expr.t * t * t
           | While  of Expr.t * t

    let rec to_format_list t =
      match t with
      | Read    s    -> !"read("  >|< !s >|< ")" 
      | Write   e    -> !"write(" >|< (Expr.to_format_list e) >|< ")" 
      | Assign (s,e) -> !s >|< !" := " >|< (Expr.to_format_list e) 
      | Seq  (s1,s2) ->
         let f1 = to_format_list s1 in
         let f2 = to_format_list s2 in
         (f1 >|< !"; " >|< f2) >?<
           (f1 >|< !";" >-< f2)
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

let op s =
  match s with
  | '+' -> (+)
  | '-' -> (-)
  | '*' -> ( * )
  | '/' -> (/)
  | '%' -> (mod)
  | _   -> raise Not_Operation
