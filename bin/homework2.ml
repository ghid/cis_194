(* tags: #algebraic-data-type #variant #binary-tree #parsing *)

type message_type =
  | Info
  | Warning
  | Error of int

type timestamp = int

type log_message =
  | LogMessage of message_type * timestamp * string
  | Unknown of string

type message_tree =
  | Leaf
  | Node of message_tree * log_message * message_tree

(* Exercise 1 *)

let parse_message line =
  let open Scanf in
  try
    match line.[0] with
    | 'E' ->
      sscanf line "E %d %d %s@\n" (fun severity timestamp msg ->
        LogMessage (Error severity, timestamp, msg))
    | 'W' ->
      sscanf line "W %d %s@\n" (fun timestamp msg -> LogMessage (Warning, timestamp, msg))
    | 'I' ->
      sscanf line "I %d %s@\n" (fun timestamp msg -> LogMessage (Info, timestamp, msg))
    | _ -> raise (Scan_failure "Unknown line")
  with
  | Scan_failure _ -> Unknown line
;;

let _ =
  assert (parse_message "E 2 562 help help" = LogMessage (Error 2, 562, "help help"));
  assert (parse_message "I 29 la la la" = LogMessage (Info, 29, "la la la"));
  assert (
    parse_message "This is not in the right format"
    = Unknown "This is not in the right format")
;;

let parse file =
  let input = In_channel.open_text file in
  let rec read_lines acc =
    try
      let line = input_line input in
      let msg = parse_message line in
      read_lines (msg :: acc)
    with
    | End_of_file ->
      close_in input;
      List.rev acc
  in
  read_lines []
;;

let _ =
  assert (
    parse "./files/sample.log"
    = [ LogMessage (Info, 6, "Completed armadillo processing")
      ; LogMessage (Info, 1, "Nothing to report")
      ; LogMessage (Info, 4, "Everything normal")
      ; LogMessage (Info, 11, "Initiating self-destruct sequence")
      ; LogMessage (Error 70, 3, "Way too many pickles")
      ; LogMessage (Error 65, 8, "Bad pickle-flange interaction detected")
      ; LogMessage (Warning, 5, "Flange is due for a check-up")
      ; LogMessage (Info, 7, "Out for lunch, back in two time steps")
      ; LogMessage (Error 20, 2, "Too many pickles")
      ; LogMessage (Info, 9, "Back from lunch")
      ; LogMessage (Error 99, 10, "Flange failed!")
      ])
;;

(* Exercise 2 *)

let insert log_message tree =
  let get_timestamp = function
    | LogMessage (_, timestamp, _) -> timestamp
    | Unknown _ -> failwith "Unknown Log Type"
  in
  let rec ordered_insert new_msg = function
    | Leaf -> Node (Leaf, new_msg, Leaf)
    | Node (l, msg, r) ->
      (match compare (get_timestamp new_msg) (get_timestamp msg) < 0 with
       | true -> Node (ordered_insert new_msg l, msg, r)
       | false -> Node (l, msg, ordered_insert new_msg r))
  in
  match log_message with
  | LogMessage _ -> ordered_insert log_message tree
  | Unknown _ -> tree
;;

(* Exercise 3 *)

let build log_messages = List.fold_left (fun t m -> insert m t) Leaf log_messages

(* Exercise 4 *)

let in_order tree =
  let rec aux acc = function
    | Leaf -> acc
    | Node (l, msg, r) -> aux (msg :: aux acc r) l (* aux l @ [ msg ] @ aux r *)
  in
  aux [] tree
;;

(* Exercise 5 *)

let what_went_wrong log_messages =
  log_messages
  |> build
  |> in_order
  |> List.filter_map (function
    | LogMessage (Error n, _, msg_string) when n >= 50 -> Some msg_string
    | _ -> None)
;;

let _ =
  assert (
    what_went_wrong (parse "./files/sample.log")
    = [ "Way too many pickles"
      ; "Bad pickle-flange interaction detected"
      ; "Flange failed!"
      ])
;;
