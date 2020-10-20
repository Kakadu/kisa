open Kisa
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
         (f1 >||< !c >||< f2) >?<
           (f1 >-< ((2 >> !c) >||< f2))
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
      | Assign (s,e) -> !s >||< !":=" >||< (Expr.to_format_list e)
      | Seq     []   -> initial
      | Seq (hd::tl) ->
         List.fold_left (fun fs s ->
             let f = to_format_list s in
             (fs >|< !";" >||< f) >?<
               (fs >|< !";" >-< f)
           ) (to_format_list hd) tl
      | If (e,s1,s2) ->
         let e  = Expr.to_format_list e in
         let f1 = to_format_list s1 in
         let f2 = to_format_list s2 in
         (!"if" >||< (e ^ 1) >||< !"then" >||< (f1 ^ 1) >||< !"else" >||< (f2 ^ 1)) >?<
         (!"if" >||< e >-<
            !"then" >||< f1 >-<
            !"else" >||< f2)
      | While (e,body) ->
         let e     = Expr.to_format_list e in
         let bodyf = to_format_list body in
         (!"while" >||< (e ^ 1) >||< !"do" >||< (bodyf ^ 1) >||< !"od") >?<
         (!"while" >||< e     >-<
            !"do"  >||< bodyf >-<
            !"od")
  end

exception Not_Operation

let op = function
  | '+' -> (+)
  | '-' -> (-)
  | '*' -> ( * )
  | '/' -> (/)
  | '%' -> (mod)
  | _   -> raise Not_Operation


let () =
  let open Stmt in
  to_format_list (While (Expr.(Binop (">", Var "x", Cons 1 )), Read "x")) |> FormatList.to_string |> print_endline
