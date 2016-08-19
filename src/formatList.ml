open Format
open List

let cons a lst = [a] @ lst

let map_filter (mapf : 'a -> 'b) (filterf : 'b -> bool) (l : 'a list) : 'b list =
  fold_left
    (fun lst a -> let b = mapf a in
                  if filterf b
                  then [b] @ lst
                  else lst
    )
    [] l

let add_general op width fl f =
  (* Potentially we want to add factorization here. *)
  map_filter (fun f' -> op f' f)
             (fun f  -> total_width f <= width)
             fl

let filteri filterf lst =
  let (_, result) =
    fold_left
      (fun (n, lst) a ->
        (n + 1, if filterf n then cons a lst else lst))
      (0, []) lst in
  result
             
let cross_general op width fl1 fl2 =
  let cross_lst = flatten (map (add_general op width fl1) fl2) in
  let flags = Array.make (length cross_lst) true in
  iteri
    (fun i1 f1 ->
      if flags.(i1)
      then iteri
             (fun i2 f2 ->
               if flags.(i2)
               then match compare f1 f2 with
                    | x when x < 0 -> flags.(i2) <- false
                    | x when x > 0 -> flags.(i1) <- false
                    | _ -> ()
             ) cross_lst
    ) cross_lst;
  filteri (fun i -> flags.(i)) cross_lst

type t = {
    width : int;           (* maximal width  *)
    lst   : Format.t list;
}

let (>|<) fs1 fs2 =
  { width = fs1.width;
    lst   = cross_general add_beside fs1.width fs1.lst fs2.lst;
  }
let (>-<) fs1 fs2 =
  { width = fs1.width;
    lst   = cross_general add_above fs1.width fs1.lst fs2.lst;
  }
let (>/<) fs1 fs2 shift =
  { width = fs1.width;
    lst   = cross_general (fun fs f -> add_fill fs f shift)
                          fs1.width fs1.lst fs2.lst;
  }

let pick_best t =
  try
    fold_left
      (fun f best ->
        if f.height < best.height
        then f
        else best)
      (hd t.lst) (tl t.lst)
  with Failure "hd" ->
    raise Not_found
