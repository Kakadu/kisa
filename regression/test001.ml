open Kisa.Format
open Printf


let print_format f =
  printf "%s\n" (to_string f);
  printf "%d\n%d %d %d\n\n"
         f.height
         f.first_line_width
         f.middle_width
         f.last_line_width

let ab = of_string "a\nb"
let ab_above  = ab >-< ab
let ab_beside = ab >|< ab

let str01 = "Hello test!"
let str02 = "Hello\ntest!"
let str03 = "Goodbye\ntest!"
let str04 = "Test "
let str05 = "56\n  E\n  E"
let str06 = "E"
let str07 = "56"

let _ =
  List.iter print_format
            [of_string "";
             line str01;
             of_string str01;
             indent 0 (of_string str01);
             indent 2 (of_string str01);
             (of_string str02) >-< (of_string str03);
             (of_string str02) >-< (of_string "");
             (of_string str04) >|< (of_string str03);
             (of_string ""   ) >|< (of_string str02);
             (of_string ""   ) >|< (of_string str02);
             (of_string str05) >|< (of_string str06);
             (of_string str07) >|< (of_string str06);
             (of_string str05) >-< (of_string str06);
             (of_string str07) >-< (of_string str06);
             ((of_string str04) >/< (of_string str03)) 0;
             ((of_string str04) >/< (of_string str03)) 2;
             (of_string "a\n\n") >-< (of_string "d");
             (of_string "a\nb\n\n") >-< (of_string "d");
             (of_string "") >-< (of_string "d");
             ((of_string "a\nbcd\ne") >/< (of_string "ab\nc")) 3;
             of_string "ab\nc";
             ab;
             ab_above;
             ab_beside
            ]
