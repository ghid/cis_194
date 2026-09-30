(* Exercise 1 *)

let to_digits (n : int) : int list =
  let rec aux digits = function
    | 0 -> digits
    | n -> aux ((n mod 10) :: digits) (n / 10)
  in
  if n <= 0 then [] else aux [] n
;;

let _ =
  assert (to_digits 1234 = [ 1; 2; 3; 4 ]);
  assert (to_digits 0 = []);
  assert (to_digits (-17) = [])
;;

let to_digits_rev (n : int) : int list = to_digits n |> List.rev
let _ = assert (to_digits_rev 1234 = [ 4; 3; 2; 1 ])

(* Exercise 2 *)

let double_every_other' lst =
  let len = List.length lst in
  List.mapi (fun i x -> if (len + i) mod 2 = 0 then x * 2 else x) lst
;;

let double_every_other'' lst =
  let len = List.length lst in
  let rec aux = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: x' :: xs -> (x * 2) :: x' :: aux xs
  in
  if len > 0 && len mod 2 = 0 then aux lst else List.hd lst :: aux (List.tl lst)
;;

let double_every_other''' lst =
  let len = List.length lst in
  let rec aux i = function
    | [] -> []
    | x :: xs ->
      if (len - i - 1) mod 2 = 0 then x :: aux (i + 1) xs else (x * 2) :: aux (i + 1) xs
  in
  aux 0 lst
;;

let double_every_other'''' lst =
  let len = List.length lst in
  let rec aux n = function
    | [] -> []
    | x :: xs -> if n mod 2 = 1 then x :: aux (n + 1) xs else (2 * x) :: aux (n + 1) xs
  in
  if len mod 2 = 1 then aux 1 lst else aux 0 lst
;;

let double_every_other (lst : int list) : int list = double_every_other'''' lst

let _ =
  assert (double_every_other [ 8; 7; 6; 5 ] = [ 16; 7; 12; 5 ]);
  assert (double_every_other [ 1; 2; 3 ] = [ 1; 4; 3 ])
;;

(* Exercise 3 *)

let rec sum_digits (lst : int list) : int =
  let digit_sum n = List.fold_left ( + ) 0 (to_digits n) in
  List.fold_left ( + ) 0 (List.map digit_sum lst)
;;

let _ = assert (sum_digits [ 16; 7; 12; 5 ] = 22)

(* Exercise 4 *)

let validate (card_number : int) : bool =
  (card_number |> to_digits |> double_every_other |> sum_digits) mod 10 = 0
;;

let _ =
  assert (validate 4012888888881881 = true);
  assert (validate 4012888888881882 = false);
  assert (validate 79927398713 = true)
;;
