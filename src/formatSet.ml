open Format
open Hashtbl

module Frame =
  struct
    type t = {
        first_line_width : int;
        middle_width     : int;
        last_line_width  : int;
      }  

    let from_format (f : Format.t) = {
        first_line_width = f.first_line_width;
        middle_width     = f.middle_width;
        last_line_width  = f.last_line_width;
      }
  end

type t = {
    width : int;                           (* maximal width  *)
    tbl   : (Frame.t, Format.t) Hashtbl.t; (* set of formats *)
}

let replace_if_smaller tbl key value =
  try
    let old_value = Hashtbl.find tbl key in
    if is_less_than value old_value
    then Hashtbl.replace tbl key value
  with Not_found -> ()

let append fs f =
  if total_width f <= fs.width
  then replace_if_smaller fs.tbl (Frame.from_format f) f;
  fs

let add_general op fs (f : Format.t) =
  let init_set = {
      width = fs.width;
      tbl   = let tbl_size = Hashtbl.length fs.tbl in
              Hashtbl.create tbl_size;
    } in
  Hashtbl.fold (fun _ tbl_f cur_set ->
                  append cur_set (op tbl_f f))
               fs.tbl init_set


let add_beside fs f       = add_general Format.add_beside fs f
let add_above  fs f       = add_general Format.add_above  fs f
let add_fill   fs f shift = add_general
                              (fun f1 f2 -> Format.add_fill f1 f2 shift)
                              fs f

let cross_general op fs1 fs2 = 
  Hashtbl.fold (fun _ tbl_f cur_set ->
                  op cur_set tbl_f)
               fs2
               { width = fs1.width;
                 tbl   = Hashtbl.copy fs1.tbl;
               }

let (>|<) fs1 fs2 = cross_general add_beside fs1 fs2
let (>-<) fs1 fs2 = cross_general add_above  fs1 fs2
let (>/<) fs1 fs2 shift =
  cross_general (fun fs f -> add_fill fs f shift)
                fs1 fs2
