(* tags: #composition #wholemeal-programing #avl-tree #fold #prime *)

(* Exercise 1 *)

let even x = x mod 2 = 0
let ( /= ) x y = x <> y
let sum = Seq.fold_left ( + ) 0
let range n = List.init n (fun x -> x + 1)
let ( // ) xs ys = List.(filter (fun x -> not (mem x ys)) xs)

let rec fun1 = function
  | [] -> 1
  | x :: xs when even x -> (x - 2) * fun1 xs
  | _ :: xs -> fun1 xs
;;

let rec fun2 = function
  | 1 -> 0
  | n when even n -> n + fun2 (n / 2)
  | n -> fun2 ((3 * n) + 1)
;;

let fun1' xs =
  xs |> List.filter even |> List.map (fun x -> x - 2) |> List.fold_left ( * ) 1
;;

let _ =
  let l = [ 3; 4; 5; 6; 7; 8 ] in
  assert (fun1 l = 48);
  assert (fun1' l = fun1 l)
;;

let rec iterate f x () = Seq.Cons (x, iterate f (f x))

let fun2' n =
  let step = function
    | x when even x -> x / 2
    | x -> (3 * x) + 1
  in
  iterate step n |> Seq.take_while (( /= ) 1) |> Seq.filter even |> sum
;;

let _ =
  assert (fun2 10 = 40);
  assert (fun2' 10 = fun2 10)
;;

(* Exercise 2 *)

type 'a tree =
  | Leaf
  | Node of int * 'a tree * 'a * 'a tree

let tree_height = function
  | Leaf -> 0
  | Node (h, _, _, _) -> h
;;

let fold_tree xs =
  let rec insert_tree x = function
    | Leaf -> Node (0, Leaf, x, Leaf)
    | Node (h, t1, x', t2) ->
      let h1 = tree_height t1 in
      let h2 = tree_height t2 in
      let new_t1 = insert_tree x t1 in
      let new_t2 = insert_tree x t2 in
      let new_h1 = tree_height new_t1 in
      let new_h2 = tree_height new_t2 in
      (match () with
       | _ when h1 < h2 -> Node (h, new_t1, x', t2)
       | _ when h1 > h2 -> Node (h, t1, x', new_t2)
       | _ when new_h1 < new_h2 -> Node (h, new_t1, x', t2)
       | _ -> Node (new_h2 + 1, t1, x', new_t2))
  in
  List.fold_right (fun x tree -> insert_tree x tree) xs Leaf
;;

let _ =
  assert (
    fold_tree [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' ]
    = Node
        ( 3
        , Node
            ( 2
            , Node (0, Leaf, 'D', Leaf)
            , 'H'
            , Node (1, Leaf, 'F', Node (0, Leaf, 'B', Leaf)) )
        , 'J'
        , Node
            ( 2
            , Node (1, Leaf, 'E', Node (0, Leaf, 'A', Leaf))
            , 'I'
            , Node (1, Leaf, 'G', Node (0, Leaf, 'C', Leaf)) ) ))
;;

(* Exercise 3 *)

let xor lst = List.fold_right (fun p acc -> acc <> p) lst false

let _ =
  assert (xor [ false; true; false ] = true);
  assert (xor [ false; true; false; false; true ] = false)
;;

let map' f lst = List.fold_right (fun x acc -> f x :: acc) lst []

let _ =
  let l = [ 1; 2; 3; 4 ] in
  let square x = x * x in
  assert (map' Char.uppercase_ascii [ 'a'; 'b'; 'c' ] = [ 'A'; 'B'; 'C' ]);
  assert (map' square l = [ 1; 4; 9; 16 ]);
  assert (map' square l = List.map square l)
;;

(* Exercise 4 *)

let cart_prod xs ys = List.(concat (map (fun x -> map (fun y -> x, y) ys) xs))

let sieve_sundaram n =
  let numbers = range n in
  let sieve =
    cart_prod numbers numbers
    |> List.filter_map (fun (i, j) ->
      let k = i + j + (2 * i * j) in
      if k <= n then Some k else None)
  in
  List.map (fun x -> (2 * x) + 1) (numbers // sieve)
;;

let _ = assert (sieve_sundaram 20 = [ 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41 ])
