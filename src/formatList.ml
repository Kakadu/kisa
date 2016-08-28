open Format
open List

let cons a lst = [a] @ lst

let map_filter (mapf : 'a -> 'b) (filterf : 'b -> bool) (l : 'a list) : 'b list =
  fold_left
    (fun lst a -> let b = mapf a in
                  if filterf b
                  then cons b lst
                  else lst
    )
    [] l

let filter_map (filterf : 'a -> bool) (mapf : 'a -> 'b) (l : 'a list) : 'b list =
  fold_left
    (fun lst a -> if filterf a
                  then cons (mapf a) lst
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

let factorize lst =
  let flags = Array.make (length lst) true in
  iteri
    (fun i1 f1 ->
      if flags.(i1)
      then iteri
             (fun i2 f2 ->
               if (i1 < i2) && flags.(i2)
               then match compare f1 f2 with
                    | x when x < 0 -> flags.(i2) <- false
                    | x when x > 0 -> flags.(i1) <- false
                    | _ -> ()
             ) lst
    ) lst;
  filteri (fun i -> flags.(i)) lst
 
             
let cross_general op width fl1 fl2 =
  let cross_lst = flatten (map (add_general op width fl1) fl2) in
  factorize cross_lst
  
type t = {
    width : int;           (* maximal width  *)
    lst   : Format.t list;
}

let (>>) shift fs =
  { width = fs.width;
    lst   = filter_map (fun f -> total_width f <= fs.width - shift)
                       (indent shift)
                       fs.lst
  } 

let default_width = ref 100

(* It's important not to reorder this function
   definition and the definition of (!) operator.
 *)
let initial =
  { width = !default_width;
    lst   = [empty];
  }

let blank_line =
  { width = !default_width;
    lst   = [line ""];
  }
 
let (!) s =
  { width = !default_width;
    lst   = [of_string s];
  }
   
let (^) fs n =
  { width = fs.width;
    lst   = filter (fun f -> f.height < n) fs.lst;
  }

let (>|<) fs1 fs2 =
  { width = fs1.width;
    lst   = cross_general add_beside fs1.width fs1.lst fs2.lst;
  }
let (>||<) fs1 fs2 = fs1 >|< !" " >|< fs2

let (>-<) fs1 fs2 =
  { width = fs1.width;
    lst   = cross_general add_above fs1.width fs1.lst fs2.lst;
  }
let (>--<) fs1 fs2 = fs1 >-< blank_line >-< fs2

let (>/<) fs1 fs2 shift =
  { width = fs1.width;
    lst   = cross_general (fun fs f -> add_fill fs f shift)
                          fs1.width fs1.lst fs2.lst;
  }

(* Choice operation *)
let (>?<) fs1 fs2 =
  { width = max fs1.width fs2.width;
    lst   = factorize (fs1.lst @ fs2.lst);
  }


let pick_best t =
  try
    fold_left
      (fun best f ->
        if f.height < best.height
        then f
        else best)
      (hd t.lst) (tl t.lst)
  with Failure s ->
    failwith "Empty set of strings to choose from."

let to_string t =
  Format.to_string (pick_best t)
