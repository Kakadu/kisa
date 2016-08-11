(* ocamlc str.cma *)
open Str

type t = {
    height           : int;
    first_line_width : int;
    middle_width     : int;
    last_line_width  : int;
    to_text          : int -> string -> string
  }
           
let empty = {
    height           = 0;
    first_line_width = 0;
    middle_width     = 0;
    last_line_width  = 0;
    to_text          = fun s t -> t
    (* `s` stands for shift. *)
    (* `t` stands for text.  *)
  }

let line nt = {
    height           = 1;
    first_line_width = String.length nt;
    middle_width     = String.length nt;
    last_line_width  = String.length nt;
    to_text          = fun s t -> nt ^ t
  }
                
let sp n = String.make n ' '
                       
let list_max l =
  match l with
    [] ->
    failwith "Empty list as argument of list_max."
  | hd :: tl ->
     List.fold_left max hd tl

let add_above f1 f2 =
  match f1.height, f2.height with
    0, _ -> f2
  | _, 0 -> f1
  | _, _ ->
     let middle_width_new =
       match f1.height, f2.height with
         1, 1            -> f1.first_line_width
       | 1, 2            -> f2.first_line_width
       | 1, x when x > 2 -> max f2.first_line_width
                                f2.middle_width 
       | 2, 1            -> f1.last_line_width
       | x, 1 when x > 2 -> max f1.middle_width
                                f1.last_line_width
       | _               ->
          list_max [f1.middle_width;
                    f1.last_line_width;
                    f2.first_line_width;
                    f2.middle_width
                   ]
     in {
         height           = f1.height + f2.height;
         first_line_width = f1.first_line_width;
         middle_width     = middle_width_new;
         last_line_width  = f2.last_line_width;
         to_text          =
           fun s t ->
           f1.to_text
             s
             ("\n" ^ (sp s) ^ (f2.to_text s t))
       }

let add_beside f1 f2 =
  match f1.height, f2.height with
    0, _ -> f2
  | _, 0 -> f1
  | _, _ ->
     let middle_width_new = 
       match f1.height, f2.height with
         1, x when x <= 2 -> f1.first_line_width +
                               f2.first_line_width
       | 1, x when x >  2 -> f1.first_line_width +
                               f2.middle_width
       | 2, 1             -> f1.first_line_width
       | x, 1 when x >  2 -> f1.middle_width 
       | _ -> list_max [f1.middle_width;
                        f1.last_line_width +
                          f2.first_line_width;
                        f1.last_line_width +
                          f2.middle_width
                       ]
     in
     let first_line_width_new =
       if f1.height == 1 
       then f1.first_line_width +
              f2.first_line_width 
       else f1.first_line_width
     in
     {
       height           = f1.height + f2.height - 1;
       first_line_width = first_line_width_new;
       middle_width     = middle_width_new;
       last_line_width  = f1.last_line_width
                          + f2.last_line_width;
       to_text          =
         fun s t ->
         f1.to_text
           s
           (f2.to_text (s + f1.last_line_width) t)
     }

let add_fill f1 f2 shift =
  match f1.height, f2.height with
    0, _ -> f2
  | _, 0 -> f1
  | _, _ ->
     let middle_width_new = 
       match f1.height, f2.height with
         1, x when x <= 2 -> f1.first_line_width +
                               f2.first_line_width
       | 1, x when x >  2 -> shift + f2.middle_width
       | 2, x when x >  2 ->
          max (f1.last_line_width + f2.first_line_width)
              (shift + f2.middle_width)
       | 2, 1             -> f1.first_line_width
       | x, 1 when x >  2 -> f1.middle_width 
       | x, 2 when x >  2 ->
          max f1.middle_width
              (f1.last_line_width + f2.first_line_width)
       | _ -> list_max [f1.middle_width;
                        f1.last_line_width +
                          f2.first_line_width;
                        shift + f2.middle_width
                       ]
     in
     let first_line_width_new =
       if f1.height == 1 
       then f1.first_line_width +
              f2.first_line_width 
       else f1.first_line_width
     in
     let last_line_width_new =
       if f2.height == 1 
       then f2.last_line_width  +
              f1.last_line_width
       else f2.last_line_width  +
              shift 
     in
     { height           = f1.height + f2.height - 1;
       first_line_width = first_line_width_new; 
       middle_width     = middle_width_new; 
       last_line_width  = last_line_width_new; 
       to_text          =
         fun s t ->
         f1.to_text s (f2.to_text (shift + s) t)
     }
       
let to_string f = f.to_text 0 ""
let total_width f = list_max [f.first_line_width;
                              f.middle_width;
                              f.last_line_width
                             ]
                             
let of_string s =
  let lines       =
    Str.split_delim (Str.regexp "\n") s in
  let lineFormats =
    List.map line lines in
  List.fold_left add_above empty lineFormats
                 
let indent shift f = {
    height           = f.height;
    first_line_width = shift + f.first_line_width; 
    middle_width     = shift + f.middle_width; 
    last_line_width  = shift + f.last_line_width; 
    to_text          =
      fun s t -> (sp shift) ^ (f.to_text (shift + s) t)
  }
                       
let ( >|< ) a b = add_beside a b
let ( >-< ) a b = add_above  a b
let ( >/< ) a b = add_fill   a b
