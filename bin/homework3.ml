(* tags: #recursion #pattern-matching #polymorphism *)

(* Exercise 1 *)

(** [skips lst] iterates over [lst] according to the length of [lst].
    During the first iteration, it adds a list containing every element to a result list.
    In the second iteration it adds a list of every 2nd element of the original list to
    the result list, in the third iteration, it adds a list with every 3rd element and
    so on, up to the length of the given list. *)
let skips lst =
  let rec aux n acc =
    if n <= List.length lst
    then aux (n + 1) (acc @ [ List.filteri (fun i _ -> (i + 1) mod n = 0) lst ])
    else acc
  in
  aux 1 []
;;

let _ =
  assert (
    skips [ 'A'; 'B'; 'C'; 'D' ]
    = [ [ 'A'; 'B'; 'C'; 'D' ]; [ 'B'; 'D' ]; [ 'C' ]; [ 'D' ] ]);
  assert (
    skips [ 'h'; 'e'; 'l'; 'l'; 'o'; '!' ]
    = [ [ 'h'; 'e'; 'l'; 'l'; 'o'; '!' ]
      ; [ 'e'; 'l'; '!' ]
      ; [ 'l'; '!' ]
      ; [ 'l' ]
      ; [ 'o' ]
      ; [ '!' ]
      ]);
  assert (skips [ 1 ] = [ [ 1 ] ]);
  assert (skips [ true; false ] = [ [ true; false ]; [ false ] ]);
  assert (skips [] = [])
;;

(* Exercise 2 *)

(** [local_maxima lst] extracts elements of a given list, where a value is strictly
    greater than the elements before and after it. For this, the given list is iterated
    over and the elements are tested in groups of three (x, y, z) to see if x < y > z. *)
let rec local_maxima = function
  | [] -> []
  | x :: y :: (z :: _ as rest) when y > x && y > z -> y :: local_maxima rest
  | _ :: (_ as rest) -> local_maxima rest
;;

let _ =
  assert (local_maxima [ 2; 9; 5; 6; 1 ] = [ 9; 6 ]);
  assert (local_maxima [ 2; 3; 4; 1; 5 ] = [ 4 ]);
  assert (local_maxima [ 1; 2; 3; 4; 5 ] = [])
;;

(* Exercise 3 *)

let histogram lst =
  let cnt = Array.make 10 0 in
  List.iter (fun i -> cnt.(i) <- cnt.(i) + 1) lst;
  let max = Array.fold_left (fun mx x -> max mx x) 0 cnt in
  let make_row h =
    Array.fold_left (fun st x -> st ^ if x >= h then "*" else " ") "" cnt
  in
  let rec make_rows rows h =
    if h = 0 then rows else make_rows (rows ^ make_row h ^ "\n") (h - 1)
  in
  make_rows "" max ^ "==========\n0123456789\n"
;;

let[@ocamlformat "disable"] _ =
  assert (histogram [1; 1; 1; 5]
    = " *        \n"
    ^ " *        \n"
    ^ " *   *    \n"
    ^ "==========\n"
    ^ "0123456789\n");
  assert (histogram [1; 4; 5; 4; 6; 6; 3; 4; 2; 4; 9]
    = "    *     \n"
    ^ "    *     \n"
    ^ "    * *   \n"
    ^ " ******  *\n"
    ^ "==========\n"
    ^ "0123456789\n");
